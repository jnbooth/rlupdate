use proc_macro2::{TokenStream, TokenTree};
use quote::ToTokens;
use regex::{Captures, Regex};
use roxmltree::Document;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use syn::punctuated::{Pair, Punctuated};
use syn::{
    Block, Expr, ExprBreak, ExprReturn, ExprYield, Ident, ImplItem, Item, ItemMod, LitStr, Local,
    Macro, Stmt, TraitItem, TraitItemConst, TraitItemMethod, Type,
};

use super::config::Config;
use super::xmlify::{AsElement, Message, TS};

type IO<T> = Result<T, Box<dyn std::error::Error>>;

fn get_last<T, P>(mut pairs: Punctuated<T, P>) -> Option<T> {
    match pairs.pop()? {
        Pair::Punctuated(t, _) => Some(t),
        Pair::End(t) => Some(t),
    }
}

fn get_inner_type(mut ty: Box<Type>) -> Option<Ident> {
    loop {
        ty = match *ty {
            Type::Array(x) => x.elem,
            Type::Group(x) => x.elem,
            Type::Paren(x) => x.elem,
            Type::Path(x) => return Some(get_last(x.path.segments)?.ident),
            Type::Ptr(x) => x.elem,
            Type::Reference(x) => x.elem,
            Type::Slice(x) => x.elem,
            _ => return None,
        };
    }
}

pub struct Updater {
    cfg: Config,
    regex: Regex,
    ts: Option<TS>,
    root: PathBuf,
    file: PathBuf,
    relative: String,
}

impl From<Updater> for Config {
    fn from(value: Updater) -> Self {
        value.cfg
    }
}

impl Updater {
    pub fn new(cfg: Config) -> Self {
        Self {
            cfg,
            regex: Regex::new(r"\{[^\}]*\}").unwrap(),
            ts: None,
            root: PathBuf::new(),
            file: PathBuf::new(),
            relative: String::new(),
        }
    }

    pub fn focus(&mut self, ts: TS, root: &Path) {
        self.ts = Some(ts);
        self.root = root.parent().unwrap().to_owned();
    }

    pub fn write<T: Write>(&mut self, writer: T) -> Result<(), xmltree::Error> {
        let mut ts = self.ts.take().unwrap();
        for ctx in self.cfg.custom.clone() {
            ctx.merge_into(&mut ts);
        }
        if self.cfg.no_obsolete {
            ts.purge_obsolete();
        }
        if self.cfg.no_ui_lines {
            ts.purge_lines();
        }
        ts.into_element().write(writer)
    }

    pub fn process(&mut self, path: PathBuf) -> IO<()> {
        if path.is_dir() {
            self.process_dir(path)
        } else {
            self.process_file(path)
        }
    }

    fn process_dir(&mut self, path: PathBuf) -> IO<()> {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let subpath = entry.path();
            let metadata = entry.metadata()?;
            if metadata.is_dir() && !self.cfg.no_recursive {
                self.process_dir(subpath)?;
            } else {
                self.process_file(subpath)?;
            }
        }
        Ok(())
    }

    fn process_file(&mut self, path: PathBuf) -> IO<()> {
        let name = path.to_str().unwrap_or("");
        let casefold = name.to_lowercase();
        if !self.cfg.extensions.contains(&casefold) {
            return Ok(());
        }
        self.relative = pathdiff::diff_paths(&path, &self.root)
            .ok_or("Unable to construct relative path")?
            .to_str()
            .ok_or("Invalid characters in file path")?
            .to_owned();
        self.file = path;
        match self
            .file
            .extension()
            .and_then(OsStr::to_str)
            .unwrap_or("")
            .to_lowercase()
            .as_str()
        {
            "rs" => self.process_rust(),
            "ui" => self.process_ui(),
            _ => Ok(()),
        }
    }

    fn process_ui(&mut self) -> IO<()> {
        let mut src = String::new();
        File::open(&self.file)?.read_to_string(&mut src)?;
        let doc = Document::parse(&src)?;
        let name = doc
            .root_element()
            .children()
            .find(|el| el.tag_name().name() == "class")
            .and_then(|el| el.text())
            .ok_or_else(|| format!("Missing class name in {}", self.relative))?
            .to_owned();
        let ts = self.ts.as_mut().unwrap();
        for (el, text) in doc
            .descendants()
            .filter(|el| el.tag_name().name() == "string")
            .filter_map(|el| el.text().map(|text| (el, text)))
        {
            let message = Message::new(
                false,
                self.relative.clone(),
                doc.text_pos_at(el.range().start).row as usize,
                text.to_owned(),
            );
            ts.insert(name.clone(), message);
        }
        Ok(())
    }

    fn process_rust(&mut self) -> IO<()> {
        let mut src = String::new();
        File::open(&self.file)?.read_to_string(&mut src)?;
        let syntax = syn::parse_file(&src)?;
        for item in syntax.items {
            self.process_item(&None, item);
        }
        Ok(())
    }

    fn process_item(&mut self, ty: &Option<Ident>, item: Item) {
        match item {
            Item::Const(expr) => {
                self.process_expr(ty, *expr.expr);
            }
            Item::Fn(expr) => {
                self.process_block(ty, *expr.block);
            }
            Item::Impl(expr) => {
                match get_inner_type(expr.self_ty) {
                    None => {
                        for it in expr.items {
                            self.process_impl(ty, it)
                        }
                    }
                    t => {
                        for it in expr.items {
                            self.process_impl(&t, it)
                        }
                    }
                };
            }
            Item::Macro(expr) => {
                self.process_macro(ty, expr.mac);
            }
            Item::Mod(ItemMod {
                content: Some((_, items)),
                ..
            }) => {
                for it in items {
                    self.process_item(ty, it);
                }
            }
            Item::Static(expr) => {
                self.process_expr(ty, *expr.expr);
            }
            Item::Trait(expr) => {
                for it in expr.items {
                    self.process_trait(ty, it);
                }
            }
            _ => (),
        }
    }

    fn try_print(&mut self, mty: &Option<Ident>, tokens: TokenStream) -> Option<()> {
        let mut stream = tokens.into_token_stream().into_iter();
        let (numerus, token) = match stream.next()? {
            TokenTree::Literal(l) => (false, l.into_token_stream()),
            _ => (true, stream.nth(1)?.into_token_stream()),
        };
        let lit: LitStr = syn::parse2(token.into_token_stream()).ok()?;
        let line = lit.span().start().line;
        let raw = lit.value();
        let mut i = 0;
        let source = self
            .regex
            .replace_all(&raw, |_: &Captures| {
                i += 1;
                format!("%{}", i)
            })
            .into_owned();
        let ctx = mty.as_ref()?.to_string();
        if numerus || !self.cfg.plural_only {
            self.ts.as_mut().unwrap().insert(
                ctx,
                Message::new(numerus, self.relative.clone(), line, source),
            );
        }
        Some(())
    }

    fn process_macro(&mut self, ty: &Option<Ident>, mac: Macro) {
        if let Some(pathname) = get_last(mac.path.segments) {
            if pathname.ident == "tr" && self.try_print(ty, mac.tokens).is_none() {
                eprintln!("Invalid usage of tr in {:?}", ty);
            }
        }
    }

    fn process_impl(&mut self, ty: &Option<Ident>, item: ImplItem) {
        match item {
            ImplItem::Const(expr) => self.process_expr(ty, expr.expr),
            ImplItem::Method(method) => self.process_block(ty, method.block),
            ImplItem::Macro(expr) => self.process_macro(ty, expr.mac),
            _ => (),
        };
    }

    fn process_trait(&mut self, ty: &Option<Ident>, item: TraitItem) {
        match item {
            TraitItem::Const(TraitItemConst {
                default: Some((_, expr)),
                ..
            }) => self.process_expr(ty, expr),
            TraitItem::Method(TraitItemMethod {
                default: Some(block),
                ..
            }) => self.process_block(ty, block),
            TraitItem::Macro(mac) => self.process_macro(ty, mac.mac),
            _ => (),
        }
    }

    fn process_block(&mut self, ty: &Option<Ident>, block: Block) {
        for stmt in block.stmts {
            self.process_stmt(ty, stmt);
        }
    }

    fn process_stmt(&mut self, ty: &Option<Ident>, item: Stmt) {
        match item {
            Stmt::Local(Local {
                init: Some((_, expr)),
                ..
            }) => self.process_expr(ty, *expr),
            Stmt::Item(it) => self.process_item(ty, it),
            Stmt::Expr(expr) => self.process_expr(ty, expr),
            Stmt::Semi(expr, _) => self.process_expr(ty, expr),
            _ => (),
        }
    }

    fn process_expr(&mut self, ty: &Option<Ident>, mut item: Expr) {
        loop {
            item = *match item {
                Expr::Array(expr) => {
                    for it in expr.elems {
                        self.process_expr(ty, it);
                    }
                    return;
                }
                Expr::Assign(expr) => expr.right,
                Expr::AssignOp(expr) => expr.right,
                Expr::Async(expr) => return self.process_block(ty, expr.block),
                Expr::Await(expr) => expr.base,
                Expr::Binary(expr) => {
                    self.process_expr(ty, *expr.left);
                    self.process_expr(ty, *expr.right);
                    return;
                }
                Expr::Block(expr) => return self.process_block(ty, expr.block),
                Expr::Box(expr) => expr.expr,
                Expr::Break(ExprBreak { expr: Some(e), .. }) => e,
                Expr::Call(expr) => {
                    self.process_expr(ty, *expr.func);
                    for e in expr.args {
                        self.process_expr(ty, e);
                    }
                    return;
                }
                Expr::Cast(expr) => expr.expr,
                Expr::Closure(expr) => expr.body,
                Expr::Field(expr) => expr.base,
                Expr::ForLoop(expr) => {
                    self.process_expr(ty, *expr.expr);
                    self.process_block(ty, expr.body);
                    return;
                }
                Expr::Group(expr) => expr.expr,
                Expr::If(expr) => {
                    self.process_expr(ty, *expr.cond);
                    self.process_block(ty, expr.then_branch);
                    if let Some((_, branch)) = expr.else_branch {
                        self.process_expr(ty, *branch);
                    }
                    return;
                }
                Expr::Index(expr) => {
                    self.process_expr(ty, *expr.expr);
                    self.process_expr(ty, *expr.index);
                    return;
                }
                Expr::Let(expr) => expr.expr,
                Expr::Loop(expr) => return self.process_block(ty, expr.body),
                Expr::Macro(expr) => return self.process_macro(ty, expr.mac),
                Expr::Match(expr) => {
                    self.process_expr(ty, *expr.expr);
                    for arm in expr.arms {
                        self.process_expr(ty, *arm.body);
                    }
                    return;
                }
                Expr::MethodCall(expr) => {
                    self.process_expr(ty, *expr.receiver);
                    for arg in expr.args {
                        self.process_expr(ty, arg);
                    }
                    return;
                }
                Expr::Paren(expr) => expr.expr,
                Expr::Range(expr) => {
                    if let Some(from) = expr.from {
                        self.process_expr(ty, *from);
                    }
                    if let Some(to) = expr.to {
                        self.process_expr(ty, *to);
                    }
                    return;
                }
                Expr::Reference(expr) => expr.expr,
                Expr::Repeat(expr) => {
                    self.process_expr(ty, *expr.expr);
                    self.process_expr(ty, *expr.len);
                    return;
                }
                Expr::Return(ExprReturn { expr: Some(e), .. }) => e,
                Expr::Struct(expr) => {
                    for field in expr.fields {
                        self.process_expr(ty, field.expr);
                    }
                    if let Some(rest) = expr.rest {
                        self.process_expr(ty, *rest);
                    }
                    return;
                }
                Expr::Try(expr) => expr.expr,
                Expr::TryBlock(expr) => return self.process_block(ty, expr.block),
                Expr::Tuple(expr) => {
                    for elem in expr.elems {
                        self.process_expr(ty, elem);
                    }
                    return;
                }
                Expr::Unary(expr) => expr.expr,
                Expr::Unsafe(expr) => return self.process_block(ty, expr.block),
                Expr::While(expr) => {
                    self.process_expr(ty, *expr.cond);
                    self.process_block(ty, expr.body);
                    return;
                }
                Expr::Yield(ExprYield { expr: Some(e), .. }) => e,
                _ => return,
            };
        }
    }
}

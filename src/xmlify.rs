use serde::Deserialize;
use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::convert::{Infallible, TryFrom, TryInto};
use std::fmt::{self, Display, Write};
use std::str::FromStr;
use xmltree::{Element, XMLNode};

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct XmlError(&'static str);

impl Display for XmlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "XmlError({})", self.0)
    }
}
impl std::error::Error for XmlError {}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ElementBuilder(Element);

impl ElementBuilder {
    fn new(tag: &str) -> Self {
        Self(Element::new(tag))
    }
    fn build(self) -> Element {
        self.0
    }
    fn attr(mut self, key: &str, value: String) -> Self {
        self.0.attributes.insert(key.to_owned(), value);
        self
    }
    fn attr_opt(mut self, key: &str, value: Option<String>) -> Self {
        if let Some(value) = value {
            self.0.attributes.insert(key.to_owned(), value);
        }
        self
    }
    fn attr_opts<T: ToString>(mut self, key: &str, value: Option<T>) -> Self {
        if let Some(value) = value {
            self.0.attributes.insert(key.to_owned(), value.to_string());
        }
        self
    }
    fn replace<T: Into<Vec<XMLNode>>>(mut self, children: T) -> Self {
        self.0.children = children.into();
        self
    }
    fn child<T: AsElement>(mut self, child: T) -> Self {
        self.0.children.push(child.into_node());
        self
    }
    fn child_opt<T: AsElement>(mut self, child: Option<T>) -> Self {
        if let Some(child) = child {
            self.0.children.push(child.into_node());
        }
        self
    }
    fn text(mut self, s: String) -> Self {
        self.0.children.push(XMLNode::Text(s));
        self
    }
    fn extend_vec<T: AsElement, I: IntoIterator<Item = T>>(mut self, iter: I) -> Self {
        self.0
            .children
            .extend(iter.into_iter().map(AsElement::into_node));
        self
    }
    fn extend_map<K, V, I, F>(mut self, iter: I, f: F) -> Self
    where
        V: AsElement,
        I: IntoIterator<Item = (K, V)>,
        F: FnMut(&(K, V), &(K, V)) -> Ordering,
    {
        let mut tosort: Vec<_> = iter.into_iter().collect();
        tosort.sort_by(f);
        self.0
            .children
            .extend(tosort.into_iter().map(|(_, v)| v.into_node()));
        self
    }
}

#[inline(always)]
fn try_el(node: XMLNode, tag: &'static str) -> Result<Element, XmlError> {
    match node {
        XMLNode::Element(el) if el.name == tag => Ok(el),
        _ => Err(XmlError(tag)),
    }
}

#[inline(always)]
fn take_attr<T: FromStr>(el: &mut Element, key: &str) -> Result<Option<T>, T::Err> {
    match el.attributes.remove(key) {
        Some(attr) => Ok(Some(attr.parse()?)),
        None => Ok(None),
    }
}

pub trait AsElement: Sized {
    const TAG: &'static str;
    const ERR: XmlError = XmlError(Self::TAG);

    fn into_element(self) -> Element;
    fn into_node(self) -> XMLNode {
        XMLNode::Element(self.into_element())
    }
    fn parse_content(el: Element) -> Result<Self, XmlError>;
    fn parse_node(node: XMLNode) -> Result<Self, XmlError> {
        try_el(node, Self::TAG).and_then(Self::parse_content)
    }
}

macro_rules! stringwrapper {
    ($t:ty, $tag:literal) => {
        impl AsElement for $t {
            const TAG: &'static str = $tag;

            fn into_element(self) -> Element {
                ElementBuilder::new(Self::TAG).replace(self.0).build()
            }
            fn parse_content(el: Element) -> Result<Self, XmlError> {
                el.children.try_into().map(Self)
            }
        }
    };
}

// Macro used in order to escape byte entities not allowed in an xml document
// for instance, only #x9, #xA and #xD are allowed characters below #x20.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EvilString {
    CData(String),
    String(String),
    Bytes(Vec<Byte>),
}
impl From<EvilString> for Vec<XMLNode> {
    fn from(value: EvilString) -> Self {
        match value {
            EvilString::CData(s) => vec![XMLNode::CData(s)],
            EvilString::String(s) => vec![XMLNode::Text(s)],
            EvilString::Bytes(xs) => xs.into_iter().map(Byte::into_node).collect(),
        }
    }
}
impl TryFrom<Vec<XMLNode>> for EvilString {
    type Error = XmlError;

    fn try_from(mut value: Vec<XMLNode>) -> Result<Self, Self::Error> {
        if value.len() == 1 {
            match value.pop().unwrap() {
                XMLNode::CData(s) => Ok(Self::CData(s)),
                XMLNode::Text(s) => Ok(Self::String(s)),
                node => Ok(Self::Bytes(vec![Byte::parse_node(node)?])),
            }
        } else {
            value
                .into_iter()
                .map(Byte::parse_node)
                .collect::<Result<_, _>>()
                .map(Self::Bytes)
        }
    }
}
impl Default for EvilString {
    fn default() -> Self {
        EvilString::String(Default::default())
    }
}
impl From<String> for EvilString {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}
impl FromStr for EvilString {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::String(s.to_owned()))
    }
}
impl Display for EvilString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CData(s) => f.pad(s),
            Self::String(s) => f.pad(s),
            Self::Bytes(xs) => {
                for x in xs {
                    write!(f, "{}", x)?
                }
                Ok(())
            }
        }
    }
}

// value contains decimal (e.g. 1000) or hex (e.g. x3e8) unicode encoding of one char
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Byte {
    // attributes
    value: String,
}
impl AsElement for Byte {
    const TAG: &'static str = "byte";

    fn into_element(self) -> Element {
        ElementBuilder::new(Self::TAG)
            .attr("value", self.value)
            .build()
    }

    fn parse_content(mut el: Element) -> Result<Self, XmlError> {
        let value = el.attributes.remove("value").ok_or(Self::ERR)?;
        Ok(Self { value })
    }
}
impl Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (radix, value) = if self.value.starts_with("0x") {
            (16, &self.value[2..])
        } else if self.value.starts_with('x') {
            (16, &self.value[1..])
        } else {
            (10, &self.value[..])
        };
        let n = u32::from_str_radix(value, radix).map_err(|_| fmt::Error)?;
        if n == 0 {
            Ok(())
        } else {
            let aschar = char::try_from(n).map_err(|_| fmt::Error)?;
            f.write_char(aschar)
        }
    }
}

// This element wildcard is no valid DTD. No better solution available.
// extra elements may appear in TS and message elements. Each element may appear
// only once within each scope. The contents are preserved verbatim; any
// attributes are dropped. Currently recognized extra tags include:
//   extra-po-msgid_plural, extra-po-old_msgid_plural
//   extra-po-flags (comma-space separated list)
//   extra-loc-layout_id
//   extra-loc-feature
//   extra-loc-blank
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Extra {
    name: String,
    content: EvilString,
}
impl From<Extra> for XMLNode {
    fn from(value: Extra) -> Self {
        let mut el = Element::new(&value.name);
        el.children = value.content.into();
        XMLNode::Element(el)
    }
}
impl TryFrom<Element> for Extra {
    type Error = XmlError;

    fn try_from(el: Element) -> Result<Self, Self::Error> {
        let name = el.name;
        let content = el.children.try_into()?;
        Ok(Self { name, content })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TS {
    // attributes
    version: Option<String>,
    sourcelanguage: Option<String>,
    language: Option<String>,
    // children
    defaultcodec: Option<DefaultCodec>,
    dependencies: Option<Dependencies>,
    contexts: HashMap<Name, Context>,
    extra: Vec<Extra>,
}
impl AsElement for TS {
    const TAG: &'static str = "TS";

    fn into_element(self) -> Element {
        ElementBuilder::new(Self::TAG)
            .attr_opt("version", self.version)
            .attr_opt("sourcelanguage", self.sourcelanguage)
            .attr_opt("language", self.language)
            .child_opt(self.defaultcodec)
            .child_opt(self.dependencies)
            .extend_map(self.contexts, |(k1, _), (k2, _)| k1.cmp(k2))
            .build()
    }

    fn parse_content(mut el: Element) -> Result<Self, XmlError> {
        let mut this = Self {
            version: el.attributes.remove("version"),
            sourcelanguage: el.attributes.remove("sourcelanguage"),
            language: el.attributes.remove("language"),
            defaultcodec: None,
            dependencies: None,
            contexts: HashMap::new(),
            extra: Vec::new(),
        };
        for child in el.children {
            if let XMLNode::Element(child) = child {
                if child.name == DefaultCodec::TAG {
                    this.defaultcodec = Some(DefaultCodec::parse_content(child)?);
                } else if child.name == Dependencies::TAG {
                    this.dependencies = Some(Dependencies::parse_content(child)?);
                } else if child.name == Context::TAG {
                    let context = Context::parse_content(child)?;
                    this.contexts.insert(context.name.clone(), context);
                } else if child.name.starts_with("extra-") {
                    this.extra.push(Extra::try_from(child)?);
                } else {
                    return Err(Self::ERR);
                }
            }
        }
        Ok(this)
    }
}
impl TS {
    pub fn new(sourcelanguage: Option<String>, language: Option<String>) -> Self {
        Self {
            version: Some("2.1".into()),
            sourcelanguage,
            language,
            defaultcodec: None,
            extra: Vec::new(),
            dependencies: None,
            contexts: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, msg: Message) {
        match self.contexts.entry(Name(name.into())) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(msg);
            }
            Entry::Vacant(entry) => {
                let mut ctx = Context::new(entry.key().clone());
                ctx.insert(msg);
                entry.insert(ctx);
            }
        }
    }

    pub fn insert_all<I: IntoIterator<Item = Context>>(&mut self, iter: I) {
        for ctx in iter {
            match self.contexts.entry(ctx.name.clone()) {
                Entry::Occupied(mut entry) => {
                    let myctx = entry.get_mut();
                    for (_, message) in ctx.messages {
                        myctx.insert(message);
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(ctx);
                }
            }
        }
    }

    pub fn purge_obsolete(&mut self) {
        for ctx in self.contexts.values_mut() {
            ctx.messages.retain(|_, x| x.updated);
        }
    }

    pub fn purge_lines(&mut self) {
        for loc in self
            .contexts
            .values_mut()
            .flat_map(|ctx| ctx.messages.values_mut())
            .flat_map(|msg| msg.locations.iter_mut())
        {
            loc.line = None;
        }
    }
}

// The encoding to use in the QM file by default. Default is ISO-8859-1.
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefaultCodec(String);
impl AsElement for DefaultCodec {
    const TAG: &'static str = "defaultcodec";

    fn into_element(self) -> Element {
        ElementBuilder::new(Self::TAG).text(self.0).build()
    }
    fn parse_content(mut el: Element) -> Result<Self, XmlError> {
        if el.children.len() != 1 {
            return Err(Self::ERR);
        }
        match el.children.pop().unwrap() {
            XMLNode::Text(s) => Ok(Self(s)),
            _ => Err(Self::ERR),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Context {
    // attributes
    encoding: Option<String>,
    // children
    name: Name,
    comment: Option<Comment>,
    messages: HashMap<Source, Message>,
}
impl AsElement for Context {
    const TAG: &'static str = "context";

    fn into_element(self) -> Element {
        ElementBuilder::new(Self::TAG)
            .attr_opt("encoding", self.encoding)
            .child(self.name)
            .child_opt(self.comment)
            .extend_map(self.messages, |(_, v1), (_, v2)| v1.cmp(v2))
            .build()
    }

    fn parse_content(mut el: Element) -> Result<Self, XmlError> {
        let mut foundname = false;
        let mut this = Self {
            encoding: el.attributes.remove("encoding"),
            ..Default::default()
        };
        for child in el.children {
            if let XMLNode::Element(child) = child {
                if child.name == Name::TAG {
                    this.name = Name::parse_content(child)?;
                    foundname = true;
                } else if child.name == Comment::TAG {
                    this.comment = Some(Comment::parse_content(child)?);
                } else if child.name == Message::TAG {
                    let msg = Message::parse_content(child)?;
                    this.messages.insert(msg.source.clone(), msg);
                }
            }
        }
        if foundname {
            Ok(this)
        } else {
            Err(Self::ERR)
        }
    }
}
impl Context {
    pub fn new(name: Name) -> Self {
        Self {
            encoding: None,
            name,
            comment: None,
            messages: HashMap::new(),
        }
    }

    pub fn insert(&mut self, msg: Message) {
        match self.messages.entry(msg.source.clone()) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().update_from(msg);
            }
            Entry::Vacant(entry) => {
                entry.insert(msg);
            }
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Dependencies(Vec<Dependency>);
impl AsElement for Dependencies {
    const TAG: &'static str = "dependencies";

    fn into_element(self) -> Element {
        ElementBuilder::new(Self::TAG).extend_vec(self.0).build()
    }

    fn parse_content(el: Element) -> Result<Self, XmlError> {
        el.children
            .into_iter()
            .map(Dependency::parse_node)
            .collect::<Result<_, _>>()
            .map(Self)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Dependency {
    // attributes
    catalog: String,
}
impl AsElement for Dependency {
    const TAG: &'static str = "dependency";

    fn into_element(self) -> Element {
        ElementBuilder::new(Self::TAG)
            .attr("catalog", self.catalog)
            .build()
    }

    fn parse_content(mut el: Element) -> Result<Self, XmlError> {
        let catalog = el.attributes.remove("catalog").ok_or(Self::ERR)?;
        Ok(Self { catalog })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(EvilString);
stringwrapper!(Name, "name");

// This is "disambiguation" in the (new) API, or "msgctxt" in gettext speak
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Comment(EvilString);
stringwrapper!(Comment, "comment");

// Previous content of comment (result of merge)
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OldComment(EvilString);
stringwrapper!(OldComment, "oldcomment");

// The real comment (added by developer/designer)
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtraComment(EvilString);
stringwrapper!(ExtraComment, "extracomment");

// Comment added by translator
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TranslatorComment(EvilString);
stringwrapper!(TranslatorComment, "translatorcomment");

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Utf8 {
    True,
    False,
    Both,
}
impl Display for Utf8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(match self {
            Self::True => "true",
            Self::False => "false",
            Self::Both => "both",
        })
    }
}
impl FromStr for Utf8 {
    type Err = XmlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true" => Ok(Self::True),
            "false" => Ok(Self::False),
            "both" => Ok(Self::Both),
            _ => Err(XmlError("utf8")),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum YesNo {
    No,
    Yes,
}
impl Display for YesNo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(match self {
            Self::Yes => "yes",
            Self::No => "no",
        })
    }
}
impl FromStr for YesNo {
    type Err = XmlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "yes" => Ok(Self::Yes),
            "no" => Ok(Self::No),
            "true" => Ok(Self::Yes),
            "false" => Ok(Self::No),
            _ => Err(XmlError("yesno")),
        }
    }
}

// If utf8 is "true", the defaultcodec is overridden and the message is encoded
// in UTF-8 in the QM file. If it is "both", both source encodings are stored
// in the QM file.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Message {
    updated: bool,
    // attributes
    id: Option<String>,
    utf8: Option<Utf8>,
    numerus: Option<YesNo>,
    // children
    locations: Vec<Location>,
    source: Source,
    oldsource: Option<OldSource>,
    comment: Option<Comment>,
    oldcomment: Option<OldComment>,
    extracomment: Option<ExtraComment>,
    translatorcomment: Option<TranslatorComment>,
    translation: Option<Translation>,
    extra: Vec<Extra>,
}
impl Ord for Message {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self.locations.first(), other.locations.first()) {
            (None, None) => Ordering::Equal,
            (None, _) => Ordering::Less,
            (_, None) => Ordering::Greater,
            (Some(x), Some(y)) => x.filename.cmp(&y.filename).then(x.line.cmp(&y.line)),
        }
    }
}
impl PartialOrd for Message {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl AsElement for Message {
    const TAG: &'static str = "message";

    fn into_element(self) -> Element {
        ElementBuilder::new(Self::TAG)
            .replace(self.extra.into_iter().map(Extra::into).collect::<Vec<_>>())
            .attr_opt("id", self.id)
            .attr_opts("utf8", self.utf8)
            .attr_opts("numerus", self.numerus)
            .extend_vec(self.locations)
            .child(self.source)
            .child_opt(self.oldsource)
            .child_opt(self.comment)
            .child_opt(self.oldcomment)
            .child_opt(self.extracomment)
            .child_opt(self.translatorcomment)
            .child_opt(self.translation)
            .build()
    }

    fn parse_content(mut el: Element) -> Result<Self, XmlError> {
        let mut foundsource = false;
        let mut this = Self {
            updated: false,
            id: el.attributes.remove("id"),
            utf8: take_attr(&mut el, "utf8")?,
            numerus: take_attr(&mut el, "numerus")?,
            ..Default::default()
        };
        for child in el.children {
            if let XMLNode::Element(child) = child {
                if child.name == Location::TAG {
                    this.locations.push(Location::parse_content(child)?);
                } else if child.name == Source::TAG {
                    this.source = Source::parse_content(child)?;
                    foundsource = true;
                } else if child.name == OldSource::TAG {
                    this.oldsource = Some(OldSource::parse_content(child)?);
                } else if child.name == Comment::TAG {
                    this.comment = Some(Comment::parse_content(child)?);
                } else if child.name == OldComment::TAG {
                    this.oldcomment = Some(OldComment::parse_content(child)?);
                } else if child.name == ExtraComment::TAG {
                    this.extracomment = Some(ExtraComment::parse_content(child)?);
                } else if child.name == TranslatorComment::TAG {
                    this.translatorcomment = Some(TranslatorComment::parse_content(child)?);
                } else if child.name == Translation::TAG {
                    this.translation = Some(Translation::parse_content(child)?);
                } else if child.name.starts_with("extra-") {
                    this.extra.push(Extra::try_from(child)?);
                } else {
                    return Err(Self::ERR);
                }
            }
        }
        if foundsource {
            Ok(this)
        } else {
            Err(Self::ERR)
        }
    }
}

impl Message {
    pub fn new(numerus: bool, filename: String, line: usize, source: String) -> Self {
        let location = Location {
            filename: Some(filename),
            line: Some(line),
        };
        let numerus = if numerus { Some(YesNo::Yes) } else { None };
        Self {
            updated: true,
            numerus,
            locations: vec![location],
            source: Source(source.into()),
            translation: Some(Translation {
                type_: Some(TranslationType::Unfinished),
                variants: None,
                body: TranslationBody::String(EvilString::String(String::new())),
            }),
            ..Default::default()
        }
    }

    pub fn update_from(&mut self, other: Self) {
        self.updated = true;
        self.numerus = other.numerus;
        self.locations = other.locations;
        self.source = other.source;
    }
}

// If the line is omitted, the location specifies only a file.
//
// location supports relative specifications as well. Line numbers are
// relative (explicitly positive or negative) to the last reference to a
// given filename; each file starts with current line 0. If the filename
// is omitted, the "current" one is used. For the 1st location in a message,
// "current" is the filename used for the 1st location of the previous message.
// For subsequent locations, it is the filename used for the previous location.
// A single TS file has either all absolute or all relative locations.
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    // attributes
    filename: Option<String>,
    line: Option<usize>,
}

impl AsElement for Location {
    const TAG: &'static str = "location";

    fn into_element(self) -> Element {
        ElementBuilder::new(Self::TAG)
            .attr_opt("filename", self.filename)
            .attr_opts("line", self.line)
            .build()
    }

    fn parse_content(mut el: Element) -> Result<Self, XmlError> {
        let line = match el.attributes.remove("line") {
            None => None,
            Some(line) => Some(line.parse().map_err(|_| Self::ERR)?),
        };
        Ok(Self {
            filename: el.attributes.remove("filename"),
            line,
        })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Source(EvilString);
stringwrapper!(Source, "source");

// Previous content of source (result of merge)
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OldSource(EvilString);
stringwrapper!(OldSource, "oldsource");

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TranslationType {
    Unfinished,
    Vanished,
    Obsolete,
}
impl Display for TranslationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(match self {
            Self::Unfinished => "unfinished",
            Self::Vanished => "vanished",
            Self::Obsolete => "obsolete",
        })
    }
}
impl FromStr for TranslationType {
    type Err = XmlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "unfinished" => Ok(Self::Unfinished),
            "vanished" => Ok(Self::Vanished),
            "obsolete" => Ok(Self::Obsolete),
            _ => Err(XmlError("translationtype")),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TranslationBody {
    String(EvilString),
    NumerusForms(Vec<NumerusForm>),
    LengthVariants(Vec<LengthVariant>),
}
impl Default for TranslationBody {
    fn default() -> Self {
        Self::String(Default::default())
    }
}
impl From<TranslationBody> for Vec<XMLNode> {
    fn from(value: TranslationBody) -> Self {
        match value {
            TranslationBody::String(s) => s.into(),
            TranslationBody::NumerusForms(xs) => {
                xs.into_iter().map(NumerusForm::into_node).collect()
            }
            TranslationBody::LengthVariants(xs) => {
                xs.into_iter().map(LengthVariant::into_node).collect()
            }
        }
    }
}
impl TryFrom<Vec<XMLNode>> for TranslationBody {
    type Error = XmlError;

    fn try_from(value: Vec<XMLNode>) -> Result<Self, Self::Error> {
        match value.get(0) {
            None => Ok(Self::String(EvilString::String(String::new()))),
            Some(XMLNode::Element(el)) if el.name == LengthVariant::TAG => value
                .into_iter()
                .map(LengthVariant::parse_node)
                .collect::<Result<_, _>>()
                .map(Self::LengthVariants),
            Some(XMLNode::Element(el)) if el.name == NumerusForm::TAG => value
                .into_iter()
                .map(NumerusForm::parse_node)
                .collect::<Result<_, _>>()
                .map(Self::NumerusForms),
            _ => EvilString::try_from(value).map(Self::String),
        }
    }
}

// If no type is set, the message is "finished".
// Length variants must be ordered by falling display length.
// variants may not be yes if the message has numerus yes.
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Translation {
    // attributes
    type_: Option<TranslationType>,
    variants: Option<YesNo>,
    // children
    body: TranslationBody,
}
impl AsElement for Translation {
    const TAG: &'static str = "translation";

    fn into_element(self) -> Element {
        ElementBuilder::new(Self::TAG)
            .replace(self.body)
            .attr_opts("type", self.type_)
            .attr_opts("variants", self.variants)
            .build()
    }

    fn parse_content(mut el: Element) -> Result<Self, XmlError> {
        Ok(Self {
            type_: take_attr(&mut el, "type")?,
            variants: take_attr(&mut el, "variants")?,
            body: el.children.try_into()?,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NumerusFormBody {
    String(EvilString),
    LengthVariants(Vec<LengthVariant>),
}
impl Default for NumerusFormBody {
    fn default() -> Self {
        Self::String(Default::default())
    }
}
impl From<NumerusFormBody> for Vec<XMLNode> {
    fn from(value: NumerusFormBody) -> Self {
        match value {
            NumerusFormBody::String(s) => s.into(),
            NumerusFormBody::LengthVariants(xs) => {
                xs.into_iter().map(LengthVariant::into_node).collect()
            }
        }
    }
}
impl TryFrom<Vec<XMLNode>> for NumerusFormBody {
    type Error = XmlError;

    fn try_from(value: Vec<XMLNode>) -> Result<Self, Self::Error> {
        match value.get(0) {
            None => Ok(Self::LengthVariants(Vec::new())),
            Some(XMLNode::Element(el)) if el.name == LengthVariant::TAG => value
                .into_iter()
                .map(LengthVariant::parse_node)
                .collect::<Result<_, _>>()
                .map(Self::LengthVariants),
            _ => EvilString::try_from(value).map(Self::String),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NumerusForm {
    // attributes
    variants: Option<YesNo>,
    // children
    body: NumerusFormBody,
}

impl AsElement for NumerusForm {
    const TAG: &'static str = "numerusform";

    fn into_element(self) -> Element {
        ElementBuilder::new(Self::TAG)
            .replace(self.body)
            .attr_opts("variants", self.variants)
            .build()
    }

    fn parse_content(mut el: Element) -> Result<Self, XmlError> {
        Ok(Self {
            variants: take_attr(&mut el, "variants")?,
            body: el.children.try_into()?,
        })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LengthVariant(EvilString);
stringwrapper!(LengthVariant, "lengthvariant");

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize)]
pub struct CustomMessage {
    pub source: String,
    #[serde(default)]
    pub numerus: bool,
    #[serde(default)]
    pub filename: Option<String>,
    #[serde(default)]
    pub line: Option<usize>
}

impl From<CustomMessage> for Message {
    fn from(value: CustomMessage) -> Self {
        let numerus = if value.numerus { Some(YesNo::Yes) } else { None };
        let location = Location {
            filename: value.filename,
            line: value.line,
        };
        Self {
            updated: true,
            numerus,
            locations: vec![location],
            source: Source(value.source.into()),
            translation: Some(Translation {
                type_: Some(TranslationType::Unfinished),
                variants: None,
                body: TranslationBody::String(EvilString::String(String::new())),
            }),
            ..Default::default()
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize)]
pub struct CustomContext {
    context: String,
    messages: Vec<CustomMessage>,
}

impl CustomContext {
    pub fn merge_into(self, ts: &mut TS) {
        match ts.contexts.entry(Name(self.context.into())) {
            Entry::Occupied(mut entry) => {
                let ctx = entry.get_mut();
                for message in self.messages {
                    ctx.insert(message.into());
                }
            }
            Entry::Vacant(entry) => {
                let mut ctx = Context::new(entry.key().to_owned());
                for message in self.messages {
                    ctx.insert(message.into());
                }
                entry.insert(ctx);
            }
        }
    }
}

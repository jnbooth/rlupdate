# rlupdate
The Rust analog to Qt's [lupdate](https://doc.qt.io/qt-5/linguist-manager.html).

## Installation

```
cargo install --path .
```

## Usage

```
USAGE:
    rlupdate [FLAGS] [OPTIONS] <source>... --ts <ts>...

FLAGS:
    -h, --help            Prints help information
        --no-obsolete     Delete obsolete and vanished strings
        --no-recursive    Do not recursively scan directories
        --no-ui-lines     Do not record line numbers in references to UI files
        --plural-only     Only include plural form messages
        --silent          Do not explain what is being done
    -V, --version         Prints version information

ARGS:
    <source>...    Source file or directory

OPTIONS:
        --disable-heuristic <disable-heuristic>...
            Named merge heuristic(s) to disable [possible values: sametext, similartext, number]

        --extensions <extensions>                     Comma-separated list of files to process [default: rs,ui]
    -I, --includes <includes>...                      Additional location(s) to look for include files
        --locations <locations>
            Source code reference style [possible values: absolute, relative, none]

        --source-language <source-language>           Source language for new files [default: POSIX]
        --target-language <target-language>           Translation language for new files
        --ts <ts>...                                  output file(s)
```

## How It Works

rlupdate generates [Qt Linguist](https://doc.qt.io/qt-5/qtlinguist-index.html) .ts files out of [Qt Designer](https://doc.qt.io/qt-5/qtdesigner-manual.html) .ui files and .rs Rust source files. It handles .ui files pretty much the same way as Qt's official lupdate does. For .rs files, it looks for usage of a `tr!` macro, which is up to the user to implement. `tr!` invocations should fit one of two patterns:

- `tr!("<string literal>", ...)`: Basic translation
- `tr!(<numeric literal or variable>, "<string literal>", ...): Numerus translation, where the first field is the [numerus](https://doc.qt.io/archives/qq/qq19-plurals.html)

As with lupdate, the [translation context](https://doc.qt.io/qt-5/i18n-source-translation.html) is the name of the object in which `tr!` is invoked. This means `tr!`s will only be processed if they are called inside a base impl, rather than a trait or a top-level function.

# Configuration

Unlike lupdate, rlupdate can read its configuration from a file. If a file named "rlupdate.yaml" is present in its working directory, rlupdate will parse its configuration from that file instead of command-line arguments. Instead of running `rlupdate src/ --ts=resources/app_en.ts`, you could create the following rlupdate.yaml file:

```yaml
sources:
- src/
ts:
- resources/app_en.ts
```

Then you could simply run `rlupdate` and the result would be the same.

The main advantage of YAML configuration is that you can add custom translation keys that rlupdate would not be able to find on its own. For example, you might have a function that generates one of several strings and then passes it to a widget for translation. In your rlupdate.yaml file, you could then add this:

```yaml
custom:
- context: "<name of widget doing the translation, e.g. MyWidget>"
  messages:
  - filename: "../src/client/mod.rs"
    line: 50
    source: "my string 1"
    numerus: false
  # note: fields are optional
  - filename: "../src/client/mod.rs"
    source: "my string 2"
```

## What "tr" Macro?

That's up to you to implement. But if you're curious, here's how I've been doing it:

```rs
use cpp_core::{CppBox, Ptr};
use qt_core::{QCoreApplication, QString};
use std::ffi::{CStr, CString};
use std::fmt::{self, Write};
use std::os::raw::{c_char, c_int};

/// Any object that calls tr! should implement this trait.
/// It's probably best to let a procedural macro derive this automatically rather than messing with
/// stuff like [`std::ffi::CStr::from_bytes_with_nul_unchecked`] on a one-by-one basis.
pub trait TrContext {
    fn class_name() -> &'static CStr;
}

/// Like [`std::format!`], but the string literal for formatting is translated through Qt's linguist
/// framework and the output is a `CppBox<QString>` instead of a `String`.
///
/// For example, `tr!("translate {}, {:#X}, {}", "something", 2, false)`
/// will ask the Qt engine to translate "translate %1, %2, %3". The result will be passed the
/// arguments "something", "0x2", and "false".
/// `tr!` uses [`std::format_args!`] under the hood, so formatting errors will be caught at compile
/// time.
#[macro_export]
macro_rules! tr {
    // simple translation of a string literal
    ($s:literal) => (
        $crate::tr::translate(Self::class_name(), $s)
    );
    // translation of a string formatted with arguments
    ($fmt:literal,$($arg:tt)*) => (
        $crate::tr::fmt(Self::class_name(), std::format_args!($fmt,$($arg)*))
    );
    // simple translation of a string literal with a numerus
    ($n:expr,$s:literal) => (
        $crate::tr::translate_amount(Self::class_name(), $s, $n as std::os::raw::c_int)
    );
    // translation of a string with a numerus, formatted with arguments
    ($n:expr,$s:literal,$($arg:tt)*) => (
        $crate::tr::fmt_amount(Self::class_name(), std::format_args!($s, $($arg)*), $n as std::os::raw::c_int)
    );
}

/// Separates [`fmt::Arguments`] into the key and its arguments.
struct ArgumentWalker {
    on_arg: bool,
    key: String,
    args: Vec<String>,
}

impl fmt::Write for ArgumentWalker {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if self.on_arg {
            self.args.push(s.to_owned());
            self.key.push('%');
            self.key.push_str(&self.args.len().to_string());
        } else {
            self.key.push_str(s);
        }
        self.on_arg = !self.on_arg;
        Ok(())
    }
}

const ERROR: &str = "Unexpected error while formatting a string for translation";
/// If `n` is `Some`, formats a numerus translation. If `n` is `None`, formats without a numerus.
fn fmt_either(context: &CStr, args: fmt::Arguments, n: Option<c_int>) -> CppBox<QString> {
    let mut walker = ArgumentWalker {
        on_arg: false,
        key: String::new(),
        args: Vec::new(),
    };
    walker.write_fmt(args).expect(ERROR);
    let ckey = CString::new(walker.key).expect(ERROR);
    let mut qkey = unsafe {
        match n {
            None => QCoreApplication::translate_2a(context.as_ptr(), ckey.as_ptr()),
            Some(n) => QCoreApplication::translate_4a(
                context.as_ptr(),
                ckey.as_ptr(),
                Ptr::<c_char>::null().as_raw_ptr(),
                n,
            ),
        }
    };
    for arg in walker.args {
        let qarg = QString::from_std_str(&arg);
        qkey = unsafe { qkey.arg_q_string(&qarg) };
    }
    qkey
}
/// Translates arguments without a numerus.
pub fn fmt(context: &CStr, args: fmt::Arguments) -> CppBox<QString> {
    fmt_either(context, args, None)
}
/// Translates arguments with a provided numerus.
pub fn fmt_amount(context: &CStr, args: fmt::Arguments, n: c_int) -> CppBox<QString> {
    fmt_either(context, args, Some(n))
}
/// Translates a bare string literal without a numerus.
pub fn translate(context: &CStr, s: &str) -> CppBox<QString> {
    let cstr = CString::new(s).expect(ERROR);
    unsafe { QCoreApplication::translate_2a(context.as_ptr(), cstr.as_ptr()) }
}
/// Translates a bare string literal with a numerus.
pub fn translate_amount(context: &CStr, s: &str, n: c_int) -> CppBox<QString> {
    let cstr = CString::new(s).expect(ERROR);
    unsafe {
        QCoreApplication::translate_4a(
            context.as_ptr(),
            cstr.as_ptr(),
            Ptr::<c_char>::null().as_raw_ptr(),
            n,
        )
    }
}
```

use crate::xmlify::CustomContext;
use serde::Deserialize;
use std::collections::HashSet;
use std::fmt::{self, Display};
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

fn parse_extensions(src: &str) -> HashSet<String> {
    src.split(',').map(|x| x.strip_prefix('.').unwrap_or(x).to_lowercase()).collect()
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LocationMode {
    Absolute,
    Relative,
    None,
}
impl Default for LocationMode {
    fn default() -> Self {
        Self::Absolute
    }
}
impl Display for LocationMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        f.pad(match self {
            Self::Absolute => "absolute",
            Self::Relative => "relative",
            Self::None => "none",
        })
    }
}

impl FromStr for LocationMode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "absolute" => Ok(Self::Absolute),
            "relative" => Ok(Self::Relative),
            "none" => Ok(Self::None),
            _ => Err("Expected absolute, relative, or none".to_owned()),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Heuristic {
    SameText,
    SimilarText,
    Number,
}
impl FromStr for Heuristic {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "sametext" => Ok(Self::SameText),
            "similartext" => Ok(Self::SimilarText),
            "number" => Ok(Self::Number),
            _ => Err("Expected absolute, relative, or none".to_owned()),
        }
    }
}

fn rsui() -> HashSet<String> {
    ["rs","ui"].iter().map(ToString::to_string).collect()
}
fn posix() -> String {
    "POSIX".to_owned()
}


#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, StructOpt)]
pub struct Config {
    /// Delete obsolete and vanished strings
    #[serde(default)]
    #[structopt(long)]
    pub no_obsolete: bool,
    /// Comma-separated list of files to process
    #[serde(default = "rsui")]
    #[structopt(long, default_value = "rs,ui", parse(from_str = parse_extensions))]
    pub extensions: HashSet<String>,
    /// Only include plural form messages
    #[serde(default)]
    #[structopt(long)]
    pub plural_only: bool,
    /// Do not explain what is being done
    #[serde(default)]
    #[structopt(long)]
    pub silent: bool,
    /// Do not recursively scan directories
    #[serde(default)]
    #[structopt(long)]
    pub no_recursive: bool,
    /// Additional location(s) to look for include files
    #[serde(default)]
    #[structopt(short = "I", long)]
    pub includes: Vec<PathBuf>,
    /// Source code reference style
    #[serde(default)]
    #[structopt(long, possible_values = &["absolute","relative","none"])]
    pub locations: Option<LocationMode>,
    /// Do not record line numbers in references to UI files
    #[serde(default)]
    #[structopt(long)]
    pub no_ui_lines: bool,
    /// Named merge heuristic(s) to disable
    #[serde(default)]
    #[structopt(long, possible_values = &["sametext", "similartext", "number"])]
    disable_heuristic: Vec<Heuristic>,
    /// Source language for new files
    #[serde(default = "posix")]
    #[structopt(long, default_value = "POSIX")]
    pub source_language: String,
    /// Translation language for new files
    #[serde(default)]
    #[structopt(long)]
    pub target_language: Option<String>,
    /// output file(s)
    #[structopt(long, required = true)]
    pub ts: Vec<PathBuf>,
    /// Source file or directory
    #[structopt(min_values = 1, max_values = 1, required = true)]
    #[serde(rename = "sources")]
    pub source: Vec<PathBuf>,
    #[serde(default)]
    #[structopt(skip)]
    pub custom: Vec<CustomContext>,
}

use std::ffi::OsStr;
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::{env, io};
use structopt::StructOpt;
use xmltree::Element;

extern crate rlupdate;
use rlupdate::{AsElement, Config, Updater, TS};

/// Attempts to canonicalize all paths. Fails if any path fails to canonicalize.
fn to_canon<T: AsRef<Path>, I: Iterator<Item = T>>(it: I) -> Result<Vec<PathBuf>, io::Error> {
    it.map(fs::canonicalize).collect()
}

/// Where to look for a configuration file.
const YAML: &str = "rlupdate.yaml";

/// Run the program and format the output if an error occurs.
fn main() -> Result<(), String> {
    run_main().map_err(|x| x.to_string())
}

/// Run the program.
fn run_main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml = if env::args().count() <= 1 {
        fs::read_dir(env::current_dir()?)?
            .filter_map(Result::ok)
            .find(|x| x.file_name() == OsStr::new(YAML))
    } else {
        None
    };
    let cfg = match yaml {
        Some(x) => serde_yaml::from_reader(File::open(x.path())?)?,
        None => Config::from_args(),
    };
    let infiles = to_canon(cfg.source.iter().chain(cfg.includes.iter()))?;
    let outfiles = to_canon(cfg.ts.iter())?;
    let sourcelang = cfg.source_language.clone();
    let targetlang = cfg.target_language.clone();
    let mut updater = Updater::new(cfg);
    for outfile in outfiles {
        let ts: TS = match File::open(&outfile) {
            Ok(file) => TS::parse_content(Element::parse(file)?)?,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                TS::new(Some(sourcelang.clone()), targetlang.clone())
            }
            Err(e) => return Err(Box::new(e)),
        };
        updater.focus(ts, &outfile);
        for infile in &infiles {
            updater.process(infile.clone())?;
        }
        let mut output = File::create(&outfile)?;
        updater.write(&mut output)?;
    }
    Ok(())
}

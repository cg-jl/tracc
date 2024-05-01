use structopt::StructOpt;
use tracc::ast::Program;

use tracc::error::SourceMetadata;
use tracc::grammar::Parser;

use tracing_subscriber::fmt;

// TODO(#3): structured formatting lib (error,warning,note,help, etc)
// TODO(#4): create test crate

fn main() {
    if let Err(ref e) = run() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn run() -> Result<(), anyhow::Error> {
    use std::fs;
    use std::io::Write;

    let opt = Opt::from_args();

    if let Some((_, filter)) = std::env::vars().find(|x| x.0 == "TRACC_TRACE") {
        fmt::Subscriber::builder()
            .with_ansi(true)
            .pretty()
            .with_env_filter(filter)
            .init();
    }

    // TermLogger::init(
    //     if opt.verbose {
    //         log::LevelFilter::Trace
    //     } else {
    //         log::LevelFilter::Info
    //     },
    //     Default::default(),
    //     TerminalMode::Mixed,
    //     simplelog::ColorChoice::Auto,
    // )?;
    let filename = opt.file;
    let file = fs::read_to_string(&filename)?;
    let out_file = opt.output.unwrap_or_else(|| filename.with_extension("s"));
    let meta = SourceMetadata::new(&file).with_file(filename);
    let program: Program = Parser::new(&meta).parse()?;
    tracing::debug!(target: "main::grammar", "parsed: {program:?}");
    let (ir, function_names) = match tracc::ir::generate::compile_program(program, &meta) {
        Ok(v) => v,
        Err(e) => {
            for error in e {
                eprintln!("{}", error);
            }
            std::process::exit(1);
        }
    };

    let mut ir = tracc::ir::fold::constant_fold(ir);

    tracing::debug!(target: "main::ir", "IR: {ir:?}");

    if !opt.fno_stack_lvn {
        tracc::ir::mem::expand_reads(&mut ir);
        ir = tracc::ir::fold::constant_fold(ir);
        tracing::debug!(target: "main::ir", "IR: {ir:?}");
    }

    let output = tracc::asmgen::codegen(ir, function_names).cons(
        tracc::asmgen::assembly::Directive::Architecture("armv8-a".into()),
    );

    //tracc::asmgen::registers::debug_what_im_doing(&ir);

    // let program = tracc::variables::convert_program(program, &meta)?;
    // let output = program.compile();
    let mut file = fs::File::create(out_file)?;
    for x in output {
        writeln!(file, "{}", x)?;
    }

    Ok(())
}

#[derive(Debug, StructOpt)]
struct Opt {
    /// The file to compile
    #[structopt(parse(from_os_str))]
    file: std::path::PathBuf,
    /// The (optional) output file
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    output: Option<std::path::PathBuf>,

    /// If set, disables stack-based local value numbering
    #[structopt(long = "fno-stack-lvn")]
    fno_stack_lvn: bool,
}

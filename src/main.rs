use std::error::Error;
use structopt::StructOpt;
use tracc::ast::Program;

use tracc::error::SourceMetadata;
use tracc::grammar::Parser;

// TODO(#3): structured formatting lib (error,warning,note,help, etc)
// TODO(#4): create test crate

fn main() {
    if let Err(ref e) = run() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    use std::fs;
    use std::io::Write;

    let opt = Opt::from_args();
    let filename = opt.file;
    let file = fs::read_to_string(&filename)?;
    let out_file = opt.output.unwrap_or_else(|| filename.with_extension("s"));
    let meta = SourceMetadata::new(&file).with_file(filename);
    let program: Program = Parser::new(&meta).parse()?;
    let (function_name, ir) = tracc::intermediate::generate::compile_program(program, &meta)?;

    let output = tracc::codegen::codegen_function(
        function_name.to_string(),
        dbg!(tracc::intermediate::fold::constant_fold(dbg!(ir))),
    )
    .cons(tracc::codegen::assembly::Directive::Architecture(
        "armv8-a".into(),
    ));

    //tracc::codegen::registers::debug_what_im_doing(&ir);
    // dbg!(memory_map, stack_size);

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
}

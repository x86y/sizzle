use parser::pest_parser::ast::*;
use std::io;

fn main() -> io::Result<()> {
    loop {
        let mut inp = String::new();
        let stdin = io::stdin();
        stdin.read_line(&mut inp)?;
        inp.pop();
        println!("{}", pest_parse(&inp).unwrap().ast);
    }
}

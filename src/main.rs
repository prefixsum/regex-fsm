mod grammar;

fn main() {
    println!("Enter a regular expression:");
    let mut expression = String::new();
    std::io::stdin()
        .read_line(&mut expression)
        .expect("Failed to read line");

    let expression = expression.trim();
    let expression_tree = grammar::Expression::new(expression);

    println!("Parsed Expression: {:?}", expression_tree);
}

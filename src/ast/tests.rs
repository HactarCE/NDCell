use super::*;

#[test]
#[allow(irrefutable_let_patterns)]
fn test_ast() {
    let source_code = "
// @states [ #dead, #live ]
@transition {
    become #(1 - (2 + -3) + 12)
}
";

    let tokens = tokens::tokenize(source_code).expect("Tokenization failed");
    let ast = TokenFeeder::from(&tokens[..])
        .program()
        .expect("AST generation failed");

    let mut correct = false;
    if let Directive::Transition(ast) = &ast[0].inner {
        if let Statement::Become(ast) = &ast[0].inner {
            if let Expr::Tag(ast) = &ast.inner {
                if let Expr::Add(lhs, rhs) = &ast.inner {
                    if let (Expr::Sub(lhs, rhs), Expr::Int(12)) = (&lhs.inner, &rhs.inner) {
                        if let (Expr::Int(1), Expr::Add(lhs, rhs)) = (&lhs.inner, &rhs.inner) {
                            if let (Expr::Int(2), Expr::Neg(ast)) = (&lhs.inner, &rhs.inner) {
                                if let Expr::Int(3) = &ast.inner {
                                    correct = true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    if !correct {
        println!("Tokens:");
        println!("{:?}", tokens);
        println!();
        println!("AST:");
        println!("{:?}", ast);
        println!();
        panic!("AST is incorrect");
    }
}

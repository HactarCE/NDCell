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
        .directives()
        .expect("AST generation failed");

    let mut correct = false;
    if let Directive::Transition(ast) = &ast[0].inner {
        if let Statement::Become(ast) = &ast[0].inner {
            if let Expr::Tag(ast) = &ast.inner {
                if let Expr::Op(lhs, Op::Add, rhs) = &ast.inner {
                    if let (Expr::Op(lhs, Op::Sub, rhs), Expr::Int(12)) = (&lhs.inner, &rhs.inner) {
                        if let (Expr::Int(1), Expr::Op(lhs, Op::Add, rhs)) =
                            (&lhs.inner, &rhs.inner)
                        {
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

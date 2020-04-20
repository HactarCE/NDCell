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

    let ast = make_program(source_code)
        .expect("AST generation failed")
        .transition_fn;

    let mut correct = false;
    use untyped::*;
    if let Statement::Become(ast) = &ast[0].inner {
        if let Expr::Tag(ast) = &ast.inner {
            if let Expr::Op {
                lhs,
                op: Op::Math(MathOp::Add),
                rhs,
            } = &ast.inner
            {
                if let (
                    Expr::Op {
                        lhs,
                        op: Op::Math(MathOp::Sub),
                        rhs,
                    },
                    Expr::Int(12),
                ) = (&lhs.inner, &rhs.inner)
                {
                    if let (
                        Expr::Int(1),
                        Expr::Op {
                            lhs,
                            op: Op::Math(MathOp::Add),
                            rhs,
                        },
                    ) = (&lhs.inner, &rhs.inner)
                    {
                        if let (Expr::Int(2), Expr::Int(-3)) = (&lhs.inner, &rhs.inner) {
                            correct = true;
                        }
                    }
                }
            }
        }
    }
    if !correct {
        println!("AST:");
        println!("{:#?}", ast);
        println!();
        panic!("AST is incorrect");
    }
}

use super::*;
use crate::data::RtVal::{Integer, Vector};

#[test]
fn test_vector_set_construct() {
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "x0..x1")])
        .assert_interpreted_test_cases(test_cases![
            // Test `Vector..Vector`
            (Vector(vec![1, 2]), Vector(vec![3, 4]))
            => Ok("[1, 2]..[3, 4]"),

            (Vector(vec![1, 4]), Vector(vec![3, 2]))
            => Ok("[1, 2]..[3, 4]"),

            (Vector(vec![1, 4]), Vector(vec![3, 2, 9]))
            => Ok("[1, 2, 0]..[3, 4, 9]"),

            (Vector(vec![3, 2, -1]), Vector(vec![1, 4]))
            => Ok("[1, 2, -1]..[3, 4, 0]"),

            (Vector(vec![6, 7, 0]), Vector(vec![6, 7]))
            => Ok("[6, 7, 0]..[6, 7, 0]"),

            // Test `Vector..Integer`
            (Vector(vec![6, 7, 0]), Integer(0))
            => Ok("[0, 0, 0]..[6, 7, 0]"),

            (Vector(vec![1, 12]), Integer(6))
            => Ok("[1, 6]..[6, 12]"),

            // Test `Integer..Vector`
            (Integer(0), Vector(vec![6, 7, 0]))
            => Ok("[0, 0, 0]..[6, 7, 0]"),

            (Integer(6), Vector(vec![1, 12]))
            => Ok("[1, 6]..[6, 12]"),
        ]);

    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "vecset()")])
        .assert_interpreted_test_cases(test_cases![() => Ok("vec2set()")]);
    TestProgram::new()
        .with_setup("@ndim 4")
        .with_result_expressions(&[(Type::VectorSet(None), "vecset()")])
        .assert_interpreted_test_cases(test_cases![() => Ok("vec4set()")]);
    TestProgram::new()
        .with_setup("@ndim 4")
        .with_result_expressions(&[(Type::VectorSet(None), "vecset(len=6)")])
        .assert_interpreted_test_cases(test_cases![() => Ok("vec6set()")]);
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "vec1set()")])
        .assert_interpreted_test_cases(test_cases![() => Ok("vec1set()")]);
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "vecset(len=1)")])
        .assert_interpreted_test_cases(test_cases![() => Ok("vec1set()")]);
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "vec7set()")])
        .assert_interpreted_test_cases::<&str>(test_cases![
            () => Err("no such function" @ "vec7set"),
        ]);
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "vecset(len=7)")])
        .assert_interpreted_test_cases::<&str>(test_cases![
            () => Err("set can only be constructed from values of types Integer, Cell, or Vector with length at most 6; not Vector[7]" @ "7"),
        ]);
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "vecset(len=900)")])
        .assert_interpreted_test_cases::<&str>(test_cases![
            () => Err("length of a vector must be an integer from 1 to 256" @ "900"),
        ]);
}

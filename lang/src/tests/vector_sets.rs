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
        .with_result_expressions(&[(Type::VectorSet(None), "VectorSet.empty")])
        .assert_interpreted_test_cases(test_cases![() => Ok("VectorSet[2].empty")]);
    TestProgram::new()
        .with_setup("@ndim 4")
        .with_result_expressions(&[(Type::VectorSet(None), "VectorSet.empty")])
        .assert_interpreted_test_cases(test_cases![() => Ok("VectorSet[4].empty")]);
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "VectorSet[1].empty")])
        .assert_interpreted_test_cases(test_cases![() => Ok("VectorSet[1].empty")]);
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "VectorSet[90].empty")])
        .assert_interpreted_test_cases::<&str>(
            test_cases![() => Err("set can only be constructed from values of types Integer, Cell, or Vector with length at most 6; not Vector[90]" @ "90")]
        );
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "VectorSet[900].empty")])
        .assert_interpreted_test_cases::<&str>(
            test_cases![() => Err("length of a vector must be an integer from 1 to 256" @ "900")],
        );
}

use super::*;
use crate::data::RtVal::{Integer, Vector};

#[test]
fn test_vector_set_construct() {
    TestProgram::new()
        .with_result_expressions(&[(Type::VectorSet(None), "x0..x1")])
        .assert_interpreted_test_cases(vec![
            // Test `Vector..Vector`
            (
                vec![Vector(vec![1, 2]), Vector(vec![3, 4])],
                Ok(vec!["[1, 2]..[3, 4]"]),
            ),
            (
                vec![Vector(vec![1, 4]), Vector(vec![3, 2])],
                Ok(vec!["[1, 2]..[3, 4]"]),
            ),
            (
                vec![Vector(vec![1, 4]), Vector(vec![3, 2, 9])],
                Ok(vec!["[1, 2, 0]..[3, 4, 9]"]),
            ),
            (
                vec![Vector(vec![3, 2, -1]), Vector(vec![1, 4])],
                Ok(vec!["[1, 2, -1]..[3, 4, 0]"]),
            ),
            (
                vec![Vector(vec![6, 7, 0]), Vector(vec![6, 7])],
                Ok(vec!["[6, 7, 0]..[6, 7, 0]"]),
            ),
            // Test `Vector..Integer`
            (
                vec![Vector(vec![6, 7, 0]), Integer(0)],
                Ok(vec!["[0, 0, 0]..[6, 7, 0]"]),
            ),
            (
                vec![Vector(vec![1, 12]), Integer(6)],
                Ok(vec!["[1, 6]..[6, 12]"]),
            ),
            // Test `Integer..Vector`
            (
                vec![Integer(0), Vector(vec![6, 7, 0])],
                Ok(vec!["[0, 0, 0]..[6, 7, 0]"]),
            ),
            (
                vec![Integer(6), Vector(vec![1, 12])],
                Ok(vec!["[1, 6]..[6, 12]"]),
            ),
        ]);
}

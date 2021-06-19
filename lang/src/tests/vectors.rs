use super::*;
use crate::data::RtVal::{Integer, Vector};

#[test]
fn test_vector_new() {
    let test_cases = vec![(vec![], Ok(vec![Vector(vec![0; 10])]))];

    TestProgram::new()
        .with_result_expressions(&[(Type::Vector(Some(10)), "vec10()")])
        .assert_test_cases(test_cases.clone());
    TestProgram::new()
        .with_result_expressions(&[(Type::Vector(Some(10)), "vec(len=10)")])
        .assert_test_cases(test_cases);
}

#[test]
fn test_vector_construct() {
    // Test vector constructor with explicit length.
    test_vector_construct_expr(vec![0; 10], "vec10()");
    test_vector_construct_expr(vec![0; 10], "vec(len=10)");

    // Test limits of `len`.
    test_vector_construct_expr(vec![0; 1], "vec(len=1)");
    test_vector_construct_expr(vec![0; 256], "vec(len=256)");
    let vec_len_msg = "the length of a vector must be an integer from 1 to 256";
    for &len_str in &["0", "257", "-1"] {
        let src = format!("v = vec(len={})", len_str);
        TestProgram::new()
            .with_exec(&src)
            .assert_compile_errors(vec![(len_str, vec_len_msg)]);
    }

    // Test vector constructor with integer value and explicit length.
    test_vector_construct_expr(vec![-3; 10], "vec10(-3)");
    test_vector_construct_expr(vec![-3; 10], "vec(len=10, -3)");
    test_vector_construct_expr(vec![-3; 10], "vec(-3, len=10)");

    // Test vector constructor based on `NDIM`.
    test_vector_construct_expr(vec![0; 2], "vec()");
    test_vector_construct_expr(vec![9; 2], "vec(9)");

    // Test vector constructor with integers.
    test_vector_construct_expr(vec![5], "[5]");
    test_vector_construct_expr(vec![1, 2, 3], "[1, 2, 3]");

    // Test vector constructor with vectors.
    test_vector_construct_expr((1..=6).collect(), "[[1, 2], 3, [4, 5, 6]]");
    test_vector_construct_expr((1..=6).collect(), "[[[1, 2], 3, [4, 5], 6]]");
}

#[test]
fn test_vector_len_cast() {
    // Test conversion from an existing vector of the same length.
    test_vector_construct_expr(vec![1, 2], "vec([1, 2], len=2)");
    // Test conversion from an existing shorter vector.
    test_vector_construct_expr(vec![1, 2, 0, 0, 0], "vec([1, 2], len=5)");
    // Test conversion from an existing longer vector.
    test_vector_construct_expr(vec![1, 2], "vec([1, 2, 3, 4], len=2)");
}

fn test_vector_construct_expr(expected: Vec<LangInt>, expr: &str) {
    let vec_len = expected.len();
    let inputs = vec![];
    let output = Ok(vec![Vector(expected)]);
    TestProgram::new()
        .with_result_expressions(&[(Type::Vector(Some(vec_len)), expr)])
        .assert_test_cases(vec![(inputs, output)]);
}

// #[test]
// fn test_vector_access() {
//     // Test in-bounds access.
//     let mut f = compile_test_fn(
//         "@function Int test() {
//             v = [1, 10, 100]
//             return v.y - v.x
//         }",
//     );
//     let expected = Ok(ConstValue::Int(9));
//     assert_fn_result(&mut f, &mut [], expected);

//     // Test out of bounds access (positive).
//     let mut f = compile_test_fn(
//         "@function Int test() {
//             v = [1, 10, 100]
//             return v.w + v[9999]
//         }",
//     );
//     let expected = Ok(ConstValue::Int(0));
//     assert_fn_result(&mut f, &mut [], expected);

//     // Test out of bounds access (negative).
//     let mut f = compile_test_fn(
//         "@function Int test() {
//             v = [1, 10, 100]
//             return v[-1]
//         }",
//     );
//     let expected = Err(("v[-1]", "Index out of bounds"));
//     assert_fn_result(&mut f, &mut [], expected);

//     // Test in-bounds modification.
//     let mut f = compile_test_fn(
//         "@function Vec3 test() {
//             v = [1, 10, 100]
//             v.x -= 4
//             v[2] = 88
//             return v
//         }",
//     );
//     let expected = Ok(ConstValue::Vector(vec![-3, 10, 88]));
//     assert_fn_result(&mut f, &mut [], expected);

//     // Test out of bounds modification.
//     let mut f = compile_test_fn(
//         "@function Int test() {
//             v = [1, 10, 100]
//             v.w = 0
//         }",
//     );
//     let expected = Err(("v.w", "Index out of bounds"));
//     assert_fn_result(&mut f, &mut [], expected);

//     // Test length.
//     let mut f = compile_test_fn(
//         "@function Void test() {
//             v3 = [1, 10, 100]
//             assert v3.len == 3
//             v2 = [4, 3]
//             assert v2.len == 2
//             v10 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
//             assert v10.len == 10
//         }",
//     );
//     let expected = Ok(ConstValue::Void);
//     assert_fn_result(&mut f, &mut [], expected);
// }

// #[test]
// fn test_vector_cmp() {
//     let test_ints = [-10, 0, 10];
//     let test_vec2s = iproduct!(&test_ints, &test_ints)
//         .map(|(&a, &b)| ConstValue::Vector(vec![a, b]))
//         .collect_vec();
//     let test_vec3s = iproduct!(&test_ints, &test_ints, &test_ints)
//         .map(|(&a, &b, &c)| ConstValue::Vector(vec![a, b, c]))
//         .collect_vec();

//     let mut cmp_same_fn = compile_test_fn(
//         "@function Void test(Vec3 a, Vec3 b) {
//                 assert (a == b) == (a.x == b.x and a.y == b.y and a.z == b.z)
//                 assert (a != b) == (a.x != b.x or  a.y != b.y or  a.z != b.z)
//                 assert (a >  b) == (a.x >  b.x and a.y >  b.y and a.z >  b.z)
//                 assert (a <  b) == (a.x <  b.x and a.y <  b.y and a.z <  b.z)
//                 assert (a >= b) == (a.x >= b.x and a.y >= b.y and a.z >= b.z)
//                 assert (a <= b) == (a.x <= b.x and a.y <= b.y and a.z <= b.z)
//             }",
//     );
//     for a in &test_vec3s {
//         for b in &test_vec3s {
//             assert_fn_result(
//                 &mut cmp_same_fn,
//                 &mut [a.clone(), b.clone()],
//                 Ok(ConstValue::Void),
//             );
//         }
//     }

//     let mut cmp_mixed_fn = compile_test_fn(
//         "@function Void test(Vec2 a, Vec3 b) {
//             assert (a == b) == (b == a) == (a.x == b.x and a.y == b.y and 0 == b.z)
//             assert (a != b) == (b != a) == (a.x != b.x or  a.y != b.y or  0 != b.z)
//             assert (a >  b) == (b <  a) == (a.x >  b.x and a.y >  b.y and 0 >  b.z)
//             assert (a <  b) == (b >  a) == (a.x <  b.x and a.y <  b.y and 0 <  b.z)
//             assert (a >= b) == (b <= a) == (a.x >= b.x and a.y >= b.y and 0 >= b.z)
//             assert (a <= b) == (b >= a) == (a.x <= b.x and a.y <= b.y and 0 <= b.z)
//         }",
//     );
//     for a in &test_vec2s {
//         for b in &test_vec3s {
//             assert_fn_result(
//                 &mut cmp_mixed_fn,
//                 &mut [a.clone(), b.clone()],
//                 Ok(ConstValue::Void),
//             );
//         }
//     }
// }

// #[test]
// fn test_vector_ops() {
//     // Test division by zero.
//     let mut f = compile_test_fn(
//         "@function Int test() {
//             b = [4, 5, 6] / [1, 2, 0]
//         }",
//     );
//     let expected = Err(("[4, 5, 6] / [1, 2, 0]", "Divide by zero"));
//     assert_fn_result(&mut f, &mut [], expected);
//     let mut f = compile_test_fn(
//         "@function Int test() {
//             b = [4, 5, 6] % [1, 0, 2]
//         }",
//     );
//     let expected = Err(("[4, 5, 6] % [1, 0, 2]", "Divide by zero"));
//     assert_fn_result(&mut f, &mut [], expected);

//     // Test all the stuff that shouldn't fail
//     let mut f = compile_test_fn(
//         "@function Void test() {
//             // Test vector equality
//             assert [-1, 2] == [-1, 2]
//             assert [-1, 2] != [4, 2]
//             assert [-1, 2] != [-1, 3]

//             // Test vector equality with different lengths
//             assert [-1, 2] == [-1, 2, 0]
//             assert [-1, 2, 0] == [-1, 2]
//             assert [-1, 2] != [-1, 2, 1]

//             // Test product and sum.
//             v = [-4, 5, 95]
//             assert v.product == -4 * 5 * 95
//             assert v.sum == -4 + 5 + 95

//             // Test arithmetic operations between vector and integer
//             v += 7
//             assert v == [3, 12, 102]
//             v /= 3
//             assert v == [1, 4, 34]
//             v *= 2
//             assert v == [2, 8, 68]
//             v %= 10
//             assert v == [2, 8, 8]
//             v -= 10
//             assert v == [-8, -2, -2]
//             assert -v == [8, 2, 2]

//             // Test operations between vectors
//             assert [1, 2, 3] * [2, 5, -1] == [2, 10, -3]

//             // Test operations between vectors of different lengths

//             // Addition
//             a = [1, 2] + [4, 5, 6]
//             b = [4, 5, 6] + [1, 2]
//             assert a.len == b.len == 3
//             assert a == b == [5, 7, 6]

//             // Subtraction
//             a = [1, 2] - [4, 5, 6]
//             b = [4, 5, 6] - [1, 2]
//             assert a.len == b.len == 3
//             assert a == -b == [-3, -3, -6]

//             // Multiplication
//             c = [1, 2] * [4, 5, 6]
//             d = [4, 5, 6] * [1, 2]
//             assert c.len == d.len == 2
//             assert c == d == [4, 10]
//             assert c.product == d.product == 40

//             // Division
//             c = [10, 20] / [4, 5, 6]
//             d = [40, 50, 60] / [1, 2]
//             assert c.len == d.len == 2
//             assert c == [2, 4]
//             assert d == [40, 25]
//             assert c.product == 8
//             assert d.product == 40 * 25

//             // Modulo
//             c = [10, 20] % [4, 5, 6]
//             d = [42, 50, 61] % [10, 20]
//             assert c.len == d.len == 2
//             assert c == [2, 0]
//             assert d == [2, 10]
//         }",
//     );
//     assert_fn_result(&mut f, &mut [], Ok(ConstValue::Void));
// }

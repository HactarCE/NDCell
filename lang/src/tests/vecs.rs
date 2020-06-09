use itertools::Itertools;

use super::{assert_fn_result, compile_test_fn, ConstValue};

#[test]
fn test_vector_access() {
    // Test in-bounds access.
    let mut f = compile_test_fn(
        "@function int test() {
            set v = [1, 10, 100]
            return v.y - v.x
        }",
    );
    let expected = Ok(ConstValue::Int(9));
    assert_fn_result(&mut f, &[], expected);

    // Test out of bounds access (positive).
    let mut f = compile_test_fn(
        "@function int test() {
            set v = [1, 10, 100]
            return v.w + v[9999]
        }",
    );
    let expected = Ok(ConstValue::Int(0));
    assert_fn_result(&mut f, &[], expected);

    // Test out of bounds access (negative).
    let mut f = compile_test_fn(
        "@function int test() {
            set v = [1, 10, 100]
            return v[-1]
        }",
    );
    let expected = Err(("-1", "Index out of bounds"));
    assert_fn_result(&mut f, &[], expected);

    // Test in-bounds modification.
    let mut f = compile_test_fn(
        "@function vec3 test() {
            set v = [1, 10, 100]
            set v.x -= 4
            set v[2] = 88
            return v
        }",
    );
    let expected = Ok(ConstValue::Vector(vec![-3, 10, 88]));
    assert_fn_result(&mut f, &[], expected);

    // Test out of bounds modification.
    let mut f = compile_test_fn(
        "@function int test() {
            set v = [1, 10, 100]
            set v.w = 0
        }",
    );
    let expected = Err(("v.w", "Index out of bounds"));
    assert_fn_result(&mut f, &[], expected);

    // Test length.
    let mut f = compile_test_fn(
        "@function int test() {
            set v3 = [1, 10, 100]
            assert v3.len == 3
            set v2 = [4, 3]
            assert v2.len == 2
            set v10 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
            assert v10.len == 10
        }",
    );
    let expected = Ok(ConstValue::Int(0));
    assert_fn_result(&mut f, &[], expected);
}

#[test]
fn test_vector_cmp() {
    let test_ints = [-10, 0, 10];
    let test_vec2s = iproduct!(&test_ints, &test_ints)
        .map(|(&a, &b)| ConstValue::Vector(vec![a, b]))
        .collect_vec();
    let test_vec3s = iproduct!(&test_ints, &test_ints, &test_ints)
        .map(|(&a, &b, &c)| ConstValue::Vector(vec![a, b, c]))
        .collect_vec();

    let mut cmp_same_fn = compile_test_fn(
        "@function int test(vec3 a, vec3 b) {
                assert (a == b) == (a.x == b.x and a.y == b.y and a.z == b.z)
                assert (a != b) == (a.x != b.x or  a.y != b.y or  a.z != b.z)
                assert (a >  b) == (a.x >  b.x and a.y >  b.y and a.z >  b.z)
                assert (a <  b) == (a.x <  b.x and a.y <  b.y and a.z <  b.z)
                assert (a >= b) == (a.x >= b.x and a.y >= b.y and a.z >= b.z)
                assert (a <= b) == (a.x <= b.x and a.y <= b.y and a.z <= b.z)
            }",
    );
    for a in &test_vec3s {
        for b in &test_vec3s {
            assert_fn_result(
                &mut cmp_same_fn,
                &[a.clone(), b.clone()],
                Ok(ConstValue::Int(0)),
            );
        }
    }

    let mut cmp_mixed_fn = compile_test_fn(
        "@function int test(vec2 a, vec3 b) {
            assert (a == b) == (b == a) == (a.x == b.x and a.y == b.y and 0 == b.z)
            assert (a != b) == (b != a) == (a.x != b.x or  a.y != b.y or  0 != b.z)
            assert (a >  b) == (b <  a) == (a.x >  b.x and a.y >  b.y and 0 >  b.z)
            assert (a <  b) == (b >  a) == (a.x <  b.x and a.y <  b.y and 0 <  b.z)
            assert (a >= b) == (b <= a) == (a.x >= b.x and a.y >= b.y and 0 >= b.z)
            assert (a <= b) == (b >= a) == (a.x <= b.x and a.y <= b.y and 0 <= b.z)
        }",
    );
    for a in &test_vec2s {
        for b in &test_vec3s {
            assert_fn_result(
                &mut cmp_mixed_fn,
                &[a.clone(), b.clone()],
                Ok(ConstValue::Int(0)),
            );
        }
    }
}

#[test]
fn test_vector_ops() {
    // Test division by zero.
    let mut f = compile_test_fn(
        "@function int test() {
            set b = [4, 5, 6] / [1, 2, 0]
        }",
    );
    let expected = Err(("[4, 5, 6] / [1, 2, 0]", "Divide by zero"));
    assert_fn_result(&mut f, &[], expected);
    let mut f = compile_test_fn(
        "@function int test() {
            set b = [4, 5, 6] % [1, 0, 2]
        }",
    );
    let expected = Err(("[4, 5, 6] % [1, 0, 2]", "Divide by zero"));
    assert_fn_result(&mut f, &[], expected);

    // Test all the stuff that shouldn't fail
    let mut f = compile_test_fn(
        "@function int test() {
            // Test vector equality
            assert [-1, 2] == [-1, 2]
            assert [-1, 2] != [4, 2]
            assert [-1, 2] != [-1, 3]

            // Test vector equality with different lengths
            assert [-1, 2] == [-1, 2, 0]
            assert [-1, 2, 0] == [-1, 2]
            assert [-1, 2] != [-1, 2, 1]

            // Test product and sum.
            set v = [-4, 5, 95]
            assert v.product == -4 * 5 * 95
            assert v.sum == -4 + 5 + 95

            // Test arithmetic operations between vector and integer
            set v += 7
            assert v == [3, 12, 102]
            set v /= 3
            assert v == [1, 4, 34]
            set v *= 2
            assert v == [2, 8, 68]
            set v %= 10
            assert v == [2, 8, 8]
            set v -= 10
            assert v == [-8, -2, -2]
            assert -v == [8, 2, 2]

            // Test operations between vectors
            assert [1, 2, 3] * [2, 5, -1] == [2, 10, -3]

            // Test operations between vectors of different lengths

            // Addition
            set a = [1, 2] + [4, 5, 6]
            set b = [4, 5, 6] + [1, 2]
            assert a.len == b.len == 3
            assert a == b == [5, 7, 6]

            // Subtraction
            set a = [1, 2] - [4, 5, 6]
            set b = [4, 5, 6] - [1, 2]
            assert a.len == b.len == 3
            assert a == -b == [-3, -3, -6]

            // Multiplication
            set c = [1, 2] * [4, 5, 6]
            set d = [4, 5, 6] * [1, 2]
            assert c.len == d.len == 2
            assert c == d == [4, 10]
            assert c.product == d.product == 40

            // Division
            set c = [10, 20] / [4, 5, 6]
            set d = [40, 50, 60] / [1, 2]
            assert c.len == d.len == 2
            assert c == [2, 4]
            assert d == [40, 25]
            assert c.product == 8
            assert d.product == 40 * 25

            // Modulo
            set c = [10, 20] % [4, 5, 6]
            set d = [42, 50, 61] % [10, 20]
            assert c.len == d.len == 2
            assert c == [2, 0]
            assert d == [2, 10]
        }",
    );
    assert_fn_result(&mut f, &[], Ok(ConstValue::Int(0)));
}

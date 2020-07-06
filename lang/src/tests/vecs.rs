use itertools::Itertools;

use super::{assert_fn_result, compile_test_fn, ConstValue};

#[test]
fn test_vector_new() {
    let mut f = compile_test_fn("@function Vec10 test() { return Vec10.new }");
    assert_fn_result(&mut f, &[], Ok(ConstValue::Vector(vec![0; 10])));
}

#[test]
fn test_vector_construct() {
    // Test with length and value specified.
    let mut f = compile_test_fn("@function Vec10 test() { return Vec10(-3) }");
    assert_fn_result(&mut f, &[], Ok(ConstValue::Vector(vec![-3; 10])));

    // Test with length specified.
    let mut f = compile_test_fn("@function Vector4 test() { return Vec4() }");
    assert_fn_result(&mut f, &[], Ok(ConstValue::Vector(vec![0; 4])));

    // Test with value specified.
    let mut f = compile_test_fn("@ndim 3 @function Vec test() { return Vec(999) }");
    assert_fn_result(&mut f, &[], Ok(ConstValue::Vector(vec![999; 3])));

    // Test with length and value both inferred.
    let mut f = compile_test_fn("@ndim 2 @function Vector test() { return Vec() }");
    assert_fn_result(&mut f, &[], Ok(ConstValue::Vector(vec![0; 2])));

    // Test conversion from an existing vector of the same length.
    let mut f = compile_test_fn("@function Vec4 test() { return Vector4([10, 20, 30, 40]) }");
    assert_fn_result(&mut f, &[], Ok(ConstValue::Vector(vec![10, 20, 30, 40])));

    // Test conversion from an existing shorter vector.
    let mut f = compile_test_fn("@ndim 4 @function Vec test() { return Vector([10, 20]) }");
    assert_fn_result(&mut f, &[], Ok(ConstValue::Vector(vec![10, 20, 0, 0])));

    // Test conversion from an existing longer vector.
    let mut f = compile_test_fn("@function Vec4 test() { return Vec4([10, 20, 30, 40, 50, 60]) }");
    assert_fn_result(&mut f, &[], Ok(ConstValue::Vector(vec![10, 20, 30, 40])));
}

#[test]
fn test_vector_len_cast() {
    // Test conversion from an existing vector of the same length.
    let mut f = compile_test_fn("@function Vec4 test(Vec4 rhs) { return Vec4(0) + rhs }");
    assert_fn_result(
        &mut f,
        &[ConstValue::Vector(vec![10, 20, 30, 40])],
        Ok(ConstValue::Vector(vec![10, 20, 30, 40])),
    );

    // Test conversion from an existing shorter vector.
    let mut f = compile_test_fn("@function Vec4 test(Vec2 rhs) { return Vec4(0) + rhs }");
    assert_fn_result(
        &mut f,
        &[ConstValue::Vector(vec![10, 20])],
        Ok(ConstValue::Vector(vec![10, 20, 0, 0])),
    );

    // Test conversion from an existing longer vector.
    let mut f = compile_test_fn("@function Vec4 test(Vec6 rhs) { return Vec4(1) * rhs }");
    assert_fn_result(
        &mut f,
        &[ConstValue::Vector(vec![10, 20, 30, 40, 50, 60])],
        Ok(ConstValue::Vector(vec![10, 20, 30, 40])),
    );
}

#[test]
fn test_vector_access() {
    // Test in-bounds access.
    let mut f = compile_test_fn(
        "@function Int test() {
            v = [1, 10, 100]
            return v.y - v.x
        }",
    );
    let expected = Ok(ConstValue::Int(9));
    assert_fn_result(&mut f, &[], expected);

    // Test out of bounds access (positive).
    let mut f = compile_test_fn(
        "@function Int test() {
            v = [1, 10, 100]
            return v.w + v[9999]
        }",
    );
    let expected = Ok(ConstValue::Int(0));
    assert_fn_result(&mut f, &[], expected);

    // Test out of bounds access (negative).
    let mut f = compile_test_fn(
        "@function Int test() {
            v = [1, 10, 100]
            return v[-1]
        }",
    );
    let expected = Err(("-1", "Index out of bounds"));
    assert_fn_result(&mut f, &[], expected);

    // Test in-bounds modification.
    let mut f = compile_test_fn(
        "@function Vec3 test() {
            v = [1, 10, 100]
            v.x -= 4
            v[2] = 88
            return v
        }",
    );
    let expected = Ok(ConstValue::Vector(vec![-3, 10, 88]));
    assert_fn_result(&mut f, &[], expected);

    // Test out of bounds modification.
    let mut f = compile_test_fn(
        "@function Int test() {
            v = [1, 10, 100]
            v.w = 0
        }",
    );
    let expected = Err(("v.w", "Index out of bounds"));
    assert_fn_result(&mut f, &[], expected);

    // Test length.
    let mut f = compile_test_fn(
        "@function Int test() {
            v3 = [1, 10, 100]
            assert v3.len == 3
            v2 = [4, 3]
            assert v2.len == 2
            v10 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
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
        "@function Int test(Vec3 a, Vec3 b) {
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
        "@function Int test(Vec2 a, Vec3 b) {
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
        "@function Int test() {
            b = [4, 5, 6] / [1, 2, 0]
        }",
    );
    let expected = Err(("[4, 5, 6] / [1, 2, 0]", "Divide by zero"));
    assert_fn_result(&mut f, &[], expected);
    let mut f = compile_test_fn(
        "@function Int test() {
            b = [4, 5, 6] % [1, 0, 2]
        }",
    );
    let expected = Err(("[4, 5, 6] % [1, 0, 2]", "Divide by zero"));
    assert_fn_result(&mut f, &[], expected);

    // Test all the stuff that shouldn't fail
    let mut f = compile_test_fn(
        "@function Int test() {
            // Test vector equality
            assert [-1, 2] == [-1, 2]
            assert [-1, 2] != [4, 2]
            assert [-1, 2] != [-1, 3]

            // Test vector equality with different lengths
            assert [-1, 2] == [-1, 2, 0]
            assert [-1, 2, 0] == [-1, 2]
            assert [-1, 2] != [-1, 2, 1]

            // Test product and sum.
            v = [-4, 5, 95]
            assert v.product == -4 * 5 * 95
            assert v.sum == -4 + 5 + 95

            // Test arithmetic operations between vector and integer
            v += 7
            assert v == [3, 12, 102]
            v /= 3
            assert v == [1, 4, 34]
            v *= 2
            assert v == [2, 8, 68]
            v %= 10
            assert v == [2, 8, 8]
            v -= 10
            assert v == [-8, -2, -2]
            assert -v == [8, 2, 2]

            // Test operations between vectors
            assert [1, 2, 3] * [2, 5, -1] == [2, 10, -3]

            // Test operations between vectors of different lengths

            // Addition
            a = [1, 2] + [4, 5, 6]
            b = [4, 5, 6] + [1, 2]
            assert a.len == b.len == 3
            assert a == b == [5, 7, 6]

            // Subtraction
            a = [1, 2] - [4, 5, 6]
            b = [4, 5, 6] - [1, 2]
            assert a.len == b.len == 3
            assert a == -b == [-3, -3, -6]

            // Multiplication
            c = [1, 2] * [4, 5, 6]
            d = [4, 5, 6] * [1, 2]
            assert c.len == d.len == 2
            assert c == d == [4, 10]
            assert c.product == d.product == 40

            // Division
            c = [10, 20] / [4, 5, 6]
            d = [40, 50, 60] / [1, 2]
            assert c.len == d.len == 2
            assert c == [2, 4]
            assert d == [40, 25]
            assert c.product == 8
            assert d.product == 40 * 25

            // Modulo
            c = [10, 20] % [4, 5, 6]
            d = [42, 50, 61] % [10, 20]
            assert c.len == d.len == 2
            assert c == [2, 0]
            assert d == [2, 10]
        }",
    );
    assert_fn_result(&mut f, &[], Ok(ConstValue::Int(0)));
}

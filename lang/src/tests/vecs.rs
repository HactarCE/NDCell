use super::{assert_compile_error, assert_fn_result, compile_test_fn, ConstValue};

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
            v.w = 0
        }",
    );
    let expected = Err(("w", "Index out of bounds"));
    assert_fn_result(&mut f, &[], expected);

    // Test length.
    let mut f = compile_test_fn(
        "@function int test() {
            set v3 = [1, 10, 100]
            assert v.len == 3
            set v2 = [4, 3]
            assert v.len == 2
            set v10 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
            assert v.len == 10
        }",
    );
    let expected = Ok(ConstValue::Int(0));
    assert_fn_result(&mut f, &[], expected);
}

#[test]
fn test_vector_ops() {
    // Test compile-time division by zero.
    let source_code = "@function int test() {
        set b = [4, 5, 6] / [1, 2]
    }";
    let expected = (
        "[4, 5, 6] / [1, 2]",
        "Vector length mismatch causes divide by zero",
    );
    assert_compile_error(source_code, expected);

    let source_code = "@function int test() {
        set b = [4, 5, 6] % [1, 2]
    }";
    let expected = (
        "[4, 5, 6] % [1, 2]",
        "Vector length mismatch causes divide by zero",
    );
    assert_compile_error(source_code, expected);

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
            set v = [1, 10, 100]
            assert v.product == 3 * 12 * 102
            assert v.sum == 3 + 12 + 102

            // Test arithmetic operations between vector and integer
            v += [2, 2, 2]
            assert v == [3, 12, 102]
            v /= 3
            assert v == [1, 4, 34]
            v *= 1
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
            assert c.len == 2
            assert c == [2, 4]
            assert c.product == 8

            // Modulo
            set c = [10, 20] % [4, 5, 6]
            assert c.len == 2
            assert c == [2, 0]
        }",
    );
    assert_fn_result(&mut f, &[], Ok(ConstValue::Int(0)));
}

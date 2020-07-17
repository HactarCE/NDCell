use super::{assert_fn_result, compile_test_fn, ConstValue, LangInt};

fn vec_args(arg1: &[LangInt], arg2: &[LangInt]) -> Vec<ConstValue> {
    vec![
        ConstValue::Vector(arg1.to_vec()),
        ConstValue::Vector(arg2.to_vec()),
    ]
}
fn ok_rect<T>(start: &[LangInt], end: &[LangInt]) -> Result<ConstValue, T> {
    Ok(ConstValue::Rectangle(start.to_vec(), end.to_vec()))
}

#[test]
fn test_rectangle_new() {
    let mut f = compile_test_fn("@function Rect3 test() { return Rect3.new }");
    assert_fn_result(&mut f, &mut [], ok_rect(&[0; 3], &[0; 3]));
}

#[test]
fn test_rectangle_construct() {
    // Test with values.
    let mut f = compile_test_fn("@function Rect2 test(Vec2 a, Vec2 b) { return a..b }");
    assert_fn_result(
        &mut f,
        &mut vec_args(&[1, 10], &[-3, 12]),
        ok_rect(&[1, 10], &[-3, 12]),
    );
}

#[test]
fn test_rectangle_construct_brackets() {
    // Test construction from integer ranges.
    let mut f = compile_test_fn(
        "@function Rect3 test() {
            return [1, 2, 3..10]
        }",
    );
    assert_fn_result(&mut f, &mut [], ok_rect(&[1, 2, 3], &[1, 2, 10]));
}

#[test]
fn test_rectangle_constructor() {
    // Test construction using rect().
    let mut f = compile_test_fn(
        "@function Rect3 test() {
            return rect3(10)
        }",
    );
    assert_fn_result(&mut f, &mut [], ok_rect(&[10; 3], &[10; 3]));
    let mut f = compile_test_fn(
        "@function Rect5 test() {
            return rect5(-2..4)
        }",
    );
    assert_fn_result(&mut f, &mut [], ok_rect(&[-2; 5], &[4; 5]));
}

#[test]
fn test_rectangle_construct_dim_infer() {
    // Test that number of dimensions can be inferred.
    let mut f = compile_test_fn("@ndim 4 @function Rect test() { return Vec(10)..Vec(5) }");
    assert_fn_result(&mut f, &mut [], ok_rect(&[10; 4], &[5; 4]));
}

#[test]
fn test_rectangle_construct_literals() {
    // Test that literals are parsed correctly.
    let mut f = compile_test_fn("@function Rect3 test() { return [1, 2, 3]..[4, 5, 6] }");
    assert_fn_result(&mut f, &mut [], ok_rect(&[1, 2, 3], &[4, 5, 6]));

    // Test mismatched types.
    let mut f = compile_test_fn(
        "@function Void test() {
            // Construct using integer
            assert [40, 40, 40]..[10, 20, 30] == 40..[10, 20, 30]
            assert [10, 20, 30]..[40, 40, 40] == [10, 20, 30]..40
            // Construct using vector of a different length
            assert [10, 20, 30]..[40, 50] == [10, 20, 30]..[40, 50, 0]
            assert [10, 20]..[30, 40, 50] == [10, 20, 0]..[30, 40, 50]
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Void));
}

#[test]
fn test_rectangle_properties() {
    // Test start and end.
    let mut f = compile_test_fn(
        "@function Vec4 test(Vec2 a, Vec2 b) {
            r = a..b
            r.start += 5
            r.start.x *= 2
            r.start.y *= 3
            r.end -= 20
            r.end.x *= 5
            r.end.y *= 6
            return [r.start, r.end]
        }",
    );
    let (mut ax, mut ay, mut bx, mut by) = (10, 11, 12, 13);
    let mut args = vec_args(&[ax, ay], &[bx, by]);
    ax += 5;
    ay += 5;
    ax *= 2;
    ay *= 3;
    bx -= 20;
    by -= 20;
    bx *= 5;
    by *= 6;
    assert_fn_result(
        &mut f,
        &mut args,
        Ok(ConstValue::Vector(vec![ax, ay, bx, by])),
    );

    // Test axes.
    let mut f = compile_test_fn(
        "@function Vec4 test(Vec2 a, Vec2 b) {
            r = a..b
            r.x += 5
            r.x.start *= 2
            r.x.end *= 3
            r.y -= 20
            r.y.start *= 5
            r.y.end *= 6
            r.x.step = 10 // should have no effect
            return [r.start, r.end]
        }",
    );
    let (mut ax, mut ay, mut bx, mut by) = (10, 11, 12, 13);
    let mut args = vec_args(&[ax, ay], &[bx, by]);
    ax += 5;
    bx += 5;
    ax *= 2;
    bx *= 3;
    ay -= 20;
    by -= 20;
    ay *= 5;
    by *= 6;
    assert_fn_result(
        &mut f,
        &mut args,
        Ok(ConstValue::Vector(vec![ax, ay, bx, by])),
    );

    // Test direct assignment.
    let mut f = compile_test_fn(
        "@function Rect2 test(Vec2 a, Vec2 b) {
            r = a..b
            r.x = (100..120).by(5) // step should have no effect
            r.end = vec2(-8)
            return r
        }",
    );
    #[allow(unused_assignments)]
    let (mut ax, ay, mut bx, mut by) = (10, 11, 12, 13);
    let mut args = vec_args(&[ax, ay], &[bx, by]);
    ax = 100;
    // bx = 120;
    bx = -8;
    by = -8;
    assert_fn_result(&mut f, &mut args, ok_rect(&[ax, ay], &[bx, by]));
}

#[test]
fn test_rectangle_ops() {
    let mut f = compile_test_fn(
        "@function Void test() {
            tmp = (-10..5).by(3)
            a = rect5(tmp)
            assert a.ndim == 5
            assert a == +a == rect5(-10..5)
            assert -10..5 == a.x != tmp

            a = -a
            assert a == 10..-5
            a *= -2
            assert a == -20..10

            // Extend vector with zeros
            a += [10, 20, 50]
            assert a.ndim == 5
            assert a == [-10..20, 0..30, 30..60, -20..10, -20..10]

            // Extend rectangle with zeros
            b = a - vec6(30)
            assert b.ndim == 6
            assert b == [-40..-10, -30..0, 0..30, -50..-20, -50..-20, -30..-30]

            // Truncate rectangle
            c = b * vec2(1)
            assert c.ndim == 2
            assert c == [-40..-10, -30..0]

            // Truncate vector
            c *= -vec5(10)
            assert c.ndim == 2
            assert c == [400..100, 300..0]

            // Use integers
            c -= 200
            assert c.ndim == 2
            assert c == [200..-100, 100..-200]
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Void));
}

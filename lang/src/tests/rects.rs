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
    assert_fn_result(&mut f, &[], ok_rect(&[0; 3], &[0; 3]));
}

#[test]
fn test_rectangle_construct() {
    // Test with values.
    let mut f = compile_test_fn("@function Rect2 test(Vec2 a, Vec2 b) { return a..b }");
    assert_fn_result(
        &mut f,
        &vec_args(&[1, 10], &[-3, 12]),
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
    assert_fn_result(&mut f, &[], ok_rect(&[1, 2, 3], &[1, 2, 10]));
}

#[test]
fn test_rectangle_constructor() {
    // Test construction using rect().
    let mut f = compile_test_fn(
        "@function Rect3 test() {
            return rect3(10)
        }",
    );
    assert_fn_result(&mut f, &[], ok_rect(&[10; 3], &[10; 3]));
    let mut f = compile_test_fn(
        "@function Rect5 test() {
            return rect5(-2..4)
        }",
    );
    assert_fn_result(&mut f, &[], ok_rect(&[-2; 5], &[4; 5]));
}

#[test]
fn test_rectangle_construct_dim_infer() {
    // Test that number of dimensions can be inferred.
    let mut f = compile_test_fn("@ndim 4 @function Rect test() { return Vec(10)..Vec(5) }");
    assert_fn_result(&mut f, &[], ok_rect(&[10; 4], &[5; 4]));
}

#[test]
fn test_rectangle_construct_literals() {
    // Test that literals are parsed correctly.
    let mut f = compile_test_fn("@function Rect3 test() { return [1, 2, 3]..[4, 5, 6] }");
    assert_fn_result(&mut f, &[], ok_rect(&[1, 2, 3], &[4, 5, 6]));

    // Test mismatched types.
    let mut f = compile_test_fn(
        "@function Int test() {
            // Construct using integer
            assert [40, 40, 40]..[10, 20, 30] == 40..[10, 20, 30]
            assert [10, 20, 30]..[40, 40, 40] == [10, 20, 30]..40
            // Construct using vector of a different length
            assert [10, 20, 30]..[40, 50] == [10, 20, 30]..[40, 50, 0]
            assert [10, 20]..[30, 40, 50] == [10, 20, 0]..[30, 40, 50]
        }",
    );
    assert_fn_result(&mut f, &[], Ok(ConstValue::Int(0)));
}

#[test]
fn test_rectangle_properties() {
    // Test start and end.
    let mut f = compile_test_fn(
        "@function Vec4 test(Vec2 a, Vec2 b) {
            set r = a..b
            set r.start += 5
            set r.start.x *= 2
            set r.start.y *= 3
            set r.end -= 20
            set r.end.x *= 5
            set r.end.y *= 6
            return [r.start, r.end]
        }",
    );
    let (mut ax, mut ay, mut bx, mut by) = (10, 11, 12, 13);
    let args = vec_args(&[ax, ay], &[bx, by]);
    ax += 5;
    ay += 5;
    ax *= 2;
    ay *= 3;
    bx -= 20;
    by -= 20;
    bx *= 5;
    by *= 6;
    assert_fn_result(&mut f, &args, Ok(ConstValue::Vector(vec![ax, ay, bx, by])));

    // Test axes.
    let mut f = compile_test_fn(
        "@function Vec4 test(Vec2 a, Vec2 b) {
            set r = a..b
            set r.x += 5
            set r.x.start *= 2
            set r.x.end *= 3
            set r.y -= 20
            set r.y.start *= 5
            set r.y.end *= 6
            set r.x.step = 10 // should have no effect
            return [r.x, r.y]
        }",
    );
    let (mut ax, mut ay, mut bx, mut by) = (10, 11, 12, 13);
    let args = vec_args(&[ax, ay], &[bx, by]);
    ax += 5;
    bx += 5;
    ax *= 2;
    bx *= 3;
    ay -= 20;
    by -= 20;
    ay *= 5;
    by *= 6;
    assert_fn_result(&mut f, &args, Ok(ConstValue::Vector(vec![ax, bx, ay, by])));

    // Test direct assignment.
    let mut f = compile_test_fn(
        "@function Rect2 test(Vec2 a, Vec2 b) {
            set r = a..b
            set r.x = (100..120).by(5) // step should have no effect
            set r.end = vec2(-8)
            return r
        }",
    );
    #[allow(unused_assignments)]
    let (mut ax, ay, mut bx, mut by) = (10, 11, 12, 13);
    let args = vec_args(&[ax, ay], &[bx, by]);
    ax = 100;
    // bx = 120;
    bx = -8;
    by = -8;
    assert_fn_result(&mut f, &args, ok_rect(&[ax, ay], &[bx, by]));
}

#[test]
fn test_rectangle_ops() {
    let mut f = compile_test_fn(
        "@function Int test() {
            set tmp = (-5..10).by(3)
            set r = rect5(tmp)
            assert r == +r == rect5(-5..10)
            assert -5..10 == r.x != tmp

            set r = -r
            assert r == 10..-5
            set r *= -2
            assert r == -20..10

            // Extend vector with zeros
            set r += [10, 20, 50];
            assert r == [-10..20, 0..30, 30..60, -20..10, -20..10]

            // Truncate vector
            set r -= vec6(30)
            assert r == [-40..-10, -30..0, 0..30, -50..-20, -50..-20]

            // Truncate rectangle
            set r *= vec2(1)
            assert r == [-40..-10, -30..0]
        }",
    );
    assert_fn_result(&mut f, &[], Ok(ConstValue::Int(0)));
}

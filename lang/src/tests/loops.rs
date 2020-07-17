use super::{
    assert_compile_error, assert_fn_compile_error, assert_fn_result, compile_test_fn, ConstValue,
};

#[test]
fn test_break_outside_loop() {
    assert_fn_compile_error(
        Some("test"),
        "@function Void test() { break }",
        ("break", "This is only allowed a loop"),
    );
}

#[test]
fn test_continue_outside_loop() {
    assert_fn_compile_error(
        Some("test"),
        "@function Void test() { continue }",
        ("continue", "This is only allowed a loop"),
    );
}

#[test]
fn test_iter_int() {
    assert_compile_error(
        "@function Void test() {
            for x in 3 {
                // yikes
            }
        }",
        (
            "3",
            "Type error: expected type that can be iterated over but got Integer",
        ),
    );
}

#[test]
fn test_iter_var_type_mismatch() {
    assert_compile_error(
        "@function Void test() {
            x = #0
            for x in 1..10 {
                // yikes
            }
        }",
        ("x", "Type error: expected Integer but got CellState"),
    );
}

#[test]
fn test_iter_vec() {
    let mut f = compile_test_fn(
        "@function Void test() {
            count = 0
            v = [12830, 480914, -4281, 24801, 14918]
            for value in v {
                assert v[count] == value
                count += 1
            }
            assert count == v.len
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Void));
}

#[test]
fn test_iter_int_range() {
    // Ascending, step = +1
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in 1..10 {
                count += 1
                assert count == x
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(10)));

    // Ascending, step = +2
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in (1..10).by(+2) {
                count += 1
                assert count * 2 - 1 == x
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(5)));

    // Ascending, step = 0
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in (1..10).by(0) {
                error \"should not execute\"
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(0)));

    // Ascending, step = -1
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in (1..10).by(-1) {
                error \"should not execute\"
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(0)));

    // Descending, step = -1
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in 10..1 {
                count += 1
                assert 11 - count == x
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(10)));

    // Descending, step = -2
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in (10..1).by(-2) {
                count += 1
                assert 12 - count * 2 == x
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(5)));

    // Descending, step = 0
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in (10..1).by(0) {
                error \"should not execute\"
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(0)));

    // Descending, step = +1
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in (10..1).by(+1) {
                error \"should not execute\"
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(0)));

    // start = end, step = +1
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in (5..5).by(+1) {
                count += 1
                assert x == 5
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(1)));

    // start = end, step = -1
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in (5..5).by(-1) {
                count += 1
                assert x == 5
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(1)));

    // start = end, step = 0
    let mut f = compile_test_fn(
        "@function Int test() {
            count = 0
            for x in (5..5).by(0) {
                error \"should not execute\"
            }
            return count
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Int(0)));
}

#[test]
fn test_iter_rectangle() {
    let mut f = compile_test_fn(
        "@function Void test() {
          r = [1, 50, 100]..[5, 10, 500]
          total = 0
          expected = r.start

          for pos in r {
            total += 1
            assert pos == expected

            if expected.x == r.end.x {
              expected.x = r.start.x
              if expected.y == r.end.y {
                expected.y = r.start.y
                if expected.z == r.end.z {
                  expected.z = -42 // should be last iteration
                } else { expected.z += 1 }
              } else { expected.y -= 1 }
            } else { expected.x += 1 }
          }

          assert total == 5 * 41 * 401
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Void));
}

#[test]
fn test_iter_cell_state_filter() {
    let mut f = compile_test_fn(
        "@states 200
        @function Void test() {
          f = #0 | #1 | #2 | #10 | #100
          total = 0

          for state in f {
            total += 1
            if total == 1 { assert state == #0 }
            if total == 2 { assert state == #1 }
            if total == 3 { assert state == #2 }
            if total == 4 { assert state == #10 }
            if total == 5 { assert state == #100 }
          }

          assert total == 5
        }",
    );
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Void));
}

#[test]
fn test_iter_break_and_continue() {
    let mut f = compile_test_fn(
        "@function Vec4 test() {
            ret = vec4()
            for i in 1..10 {
                ret.x += 1
                if i is 6..8 {
                    continue
                }
                for j in 11..20 {
                    ret.y += 1
                    if i is 3..7 {
                        break
                    }
                    for k in 21..30 {
                        ret.z += 1
                        if j < 3 and k == 25 {
                            break
                        }
                        ret.w += 1
                    }
                }
            }
            return ret
        }",
    );
    let mut expected = vec![0; 4];
    for i in 1..=10 {
        expected[0] += 1;
        if (6..=8).contains(&i) {
            continue;
        }
        for j in 11..=20 {
            expected[1] += 1;
            if (3..=7).contains(&i) {
                break;
            }
            for k in 21..=30 {
                expected[2] += 1;
                if j < 3 && k == 25 {
                    break;
                }
                expected[3] += 1;
            }
        }
    }
    assert_fn_result(&mut f, &mut [], Ok(ConstValue::Vector(expected)))
}

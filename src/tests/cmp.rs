use super::assert_output;

#[test]
fn test_cmp() {
    assert_output(
        Ok(1),
        "
        @transition {
            set x = 3
            set y = 4
            set z = 4
            if y == z {
                if x == y {
                } else {
                    if x != y {
                        if y != z {
                        } else {
                            if x < y < z {
                            } else if x < y <= z {
                                if x <= z {
                                    become #(y >= x)
                                }
                            }
                        }
                    }
                }
            }
            become #0
        }",
    );
}

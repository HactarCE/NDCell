use super::assert_output;

#[test]
fn test_branch_nonzero() {
    assert_output(
        Ok(10),
        "
        @transition {
            if 0 {
            } else {
                if 1 {
                    if 2 {
                        if -1 {
                            become #10
                        }
                    }
                }
            }
            become #0
        }",
    );
}

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

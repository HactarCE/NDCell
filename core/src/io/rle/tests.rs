use proptest::*;

use super::*;
use crate::axis::Axis::{U, V, W, Y, Z};
use crate::prelude::*;

#[test]
fn test_rle_item() {
    // Ensure that u8->string and string->u8 is reversible.
    for i in 0..=255 {
        let s = RleItem::Cell(i).to_string();
        println!("{:?}", s);
        assert_eq!(Ok(RleItem::Cell(i)), s.parse());
        // Spot-check a few examples.
        match i {
            0 => assert_eq!(".", s),
            1 => assert_eq!("A", s),
            2 => assert_eq!("B", s),
            24 => assert_eq!("X", s),
            25 => assert_eq!("pA", s),
            240 => assert_eq!("xX", s),
            241 => assert_eq!("yA", s),
            255 => assert_eq!("yO", s),
            _ => (),
        }
    }

    // Check `false`.
    let s = format!("{:b}", RleItem::Cell(0_u8));
    assert_eq!("b", s);
    assert_eq!(Ok(RleItem::Cell(0_u8)), s.parse());
    // Check `true`.
    let s = format!("{:b}", RleItem::Cell(1_u8));
    assert_eq!("o", s);
    assert_eq!(Ok(RleItem::Cell(1_u8)), s.parse());

    // Check row endings.
    let s = RleItem::Next(Y).to_string();
    assert_eq!("$", s);
    assert_eq!(Ok(RleItem::Next(Y)), s.parse());

    let s = RleItem::Next(Z).to_string();
    assert_eq!("/", s);
    assert_eq!(Ok(RleItem::Next(Z)), s.parse());

    let s = RleItem::Next(W).to_string();
    assert_eq!("%W", s);
    assert_eq!(Ok(RleItem::Next(W)), s.parse());

    let s = RleItem::Next(U).to_string();
    assert_eq!("%U", s);
    assert_eq!(Ok(RleItem::Next(U)), s.parse());

    let s = RleItem::Next(V).to_string();
    assert_eq!("%V", s);
    assert_eq!(Ok(RleItem::Next(V)), s.parse());

    // Check EOF.
    let s = RleItem::End.to_string();
    assert_eq!("!", s);
    assert_eq!(Ok(RleItem::End), s.parse());
}

proptest! {
    /// Tests that the RLE parser fails correctly and does not panic for 0-, 1-,
    /// 2-, and 3-character strings.
    #[test]
    fn test_rle_fail(s in ".{0,3}") {
        // This should not panic.
        if let Ok(item) = s.parse::<RleItem>() {
            // If it successfully parsed, we should be able to get back the
            // original string.
            if s == "b" || s == "o"  {
                // binary cell state
                assert_eq!(s, format!("{:b}", item));
            } else {
                assert_eq!(s, item.to_string());
            }
        }
    }
}

// Load and save a glider.
#[test]
fn test_cxrle_2d() {
    let imported = Rle::from_string_to_ndtree(
        "
        #CXRLE Pos=8,-8
        # Comment
        # Comment 2
        x = 5, y = 7, rule = Life
        # more
        4$2b
        bo$2b2b
        o$2b3o!

        #Another Comment 3
        #Comment 4
        ",
    )
    .expect("Failed to import RLE");
    println!("imported\n{}\n", imported);
    assert_eq!(5, imported.root_ref().population().to_usize().unwrap());
    assert_eq!(1, imported.get_cell(&NdVec::big([11, 4])));
    assert_eq!(1, imported.get_cell(&NdVec::big([12, 3])));
    assert_eq!(1, imported.get_cell(&NdVec::big([10, 2])));
    assert_eq!(1, imported.get_cell(&NdVec::big([11, 2])));
    assert_eq!(1, imported.get_cell(&NdVec::big([12, 2])));
    let exported = Rle::from_ndtree_to_string(&imported, None, TwoState::TwoStates)
        .expect("Failed to export RLE");
    assert_eq!(
        "\
#CXRLE Pos=10,-4
x = 3, y = 3
bo$2bo$3o!
",
        exported,
    );
    let reimported = Rle::from_string_to_ndtree(&exported).expect("Failed to reimport RLE output");
    println!("reimported\n{}\n", reimported);
    // `NdTree`s with different node pools may be unequal, so we serialize
    // before comparing.
    assert_eq!(imported.to_string(), reimported.to_string());
}

#[test]
fn test_empty_rle() {
    let ndtree: NdTree2D =
        Rle::from_string_to_ndtree("x = 0, y = 0\n!").expect("Failed to import RLE");
    assert!(ndtree.root_ref().is_empty());
}

#[test]
fn test_rle_rect() {
    let mut ndtree = NdTree2D::new();
    // Make a diagonal line.
    for i in -20..=20 {
        ndtree.set_cell(&NdVec::big([i, i]), (i + 21) as u8);
    }

    println!("{}", ndtree);

    for start in -0..=20 {
        for end in start..=20 {
            println!("{}..={}", start, end);

            let rect = NdRect::span(NdVec::big([start, -20]), NdVec::big([end, 20]));
            let rle_string =
                Rle::from_ndtree_to_string(&ndtree, Some(rect), TwoState::MoreStates).unwrap();
            let reimported: NdTree2D = Rle::from_string_to_ndtree(&rle_string).unwrap();

            let expected_pop: BigUint = BigUint::from_isize(end - start + 1).unwrap();
            let actual_pop = reimported.root_ref().population();
            assert_eq!(expected_pop, actual_pop);

            for i in start..=end {
                assert_eq!((i + 21) as u8, reimported.get_cell(&NdVec::big([i, i])));
            }
        }
    }
}

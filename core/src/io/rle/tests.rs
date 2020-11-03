use proptest::*;

use super::*;
use crate::axis::Axis::{U, V, W, X, Y, Z};
use crate::ndrect::IRect3D;
use crate::ndtree::{CachedNodeRefTrait, NdTree2D};
use crate::ndvec::NdVec;
use crate::num::ToPrimitive;

#[test]
fn test_rle_rect_iter() {
    let rect = IRect3D::span(NdVec([-10, 0, 10]), NdVec([-8, 3, 11])).to_bigrect();

    let mut rle_rect_iter = RleRectIter::new(rect.clone());

    use RleRectIterItem::{Next, Pos};

    for z in rect.axis_range(Z).rev() {
        for y in rect.axis_range(Y).rev() {
            for x in rect.axis_range(X) {
                let pos = NdVec([x, y.clone(), z.clone()]);
                assert_eq!(Some(Pos(pos)), rle_rect_iter.next());
            }
            if y != rect.min()[Y] {
                assert_eq!(Some(Next(Y)), rle_rect_iter.next());
            }
        }
        if z != rect.min()[Z] {
            assert_eq!(Some(Next(Z)), rle_rect_iter.next());
        }
    }
    assert_eq!(None, rle_rect_iter.next());
}

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
    let imported = NdTree2D::from_rle_str(
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
    let node_cache = imported.cache().read();
    assert_eq!(
        5,
        imported
            .root()
            .as_ref(&node_cache)
            .population()
            .to_usize()
            .unwrap()
    );
    assert_eq!(1, imported.get_cell(&node_cache, &NdVec::big([11, 4])));
    assert_eq!(1, imported.get_cell(&node_cache, &NdVec::big([12, 3])));
    assert_eq!(1, imported.get_cell(&node_cache, &NdVec::big([10, 2])));
    assert_eq!(1, imported.get_cell(&node_cache, &NdVec::big([11, 2])));
    assert_eq!(1, imported.get_cell(&node_cache, &NdVec::big([12, 2])));
    let exported = imported
        .to_rle(None)
        .expect("Failed to export RLE")
        .to_string_2_state();
    assert_eq!(
        "\
#CXRLE Pos=10,-4
x = 3, y = 3
bo$2bo$3o!
",
        exported,
    );
    let reimported = NdTree2D::from_rle_str(&exported).expect("Failed to reimport RLE output");
    println!("reimported\n{}\n", reimported);
    // `NdTree`s with different caches are considered separate, so we
    // serialize before comparing.
    assert_eq!(imported.to_string(), reimported.to_string());
}

#[test]
fn test_empty_rle() {
    let ndtree: NdTree2D = NdTree2D::from_rle_str("x = 0, y = 0\n!").expect("Failed to import RLE");
    let cache = ndtree.cache().read();
    assert!(ndtree.root().as_ref(&cache).is_empty());
}

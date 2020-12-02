use std::sync::Arc;

use crate::prelude::*;
use crate::sim::rule::DummyRule;

#[test]
fn test_macrocell_empty() {
    _test_macrocell_empty_nd::<Dim1D>();
    _test_macrocell_empty_nd::<Dim2D>();
    _test_macrocell_empty_nd::<Dim3D>();
    _test_macrocell_empty_nd::<Dim4D>();
    _test_macrocell_empty_nd::<Dim5D>();
    _test_macrocell_empty_nd::<Dim6D>();
}

fn _test_macrocell_empty_nd<D: Dim>() {
    const EMPTY_MACROCELL: &str = "[M2] (some program)\n";
    let rule: Arc<dyn NdRule<D>> = Arc::new(DummyRule);

    let ndtree = NdTree::<D>::from_macrocell_str(EMPTY_MACROCELL).unwrap();
    assert!(ndtree.root_ref().is_empty());
    let expected = format!("{}\n", crate::MACROCELL_HEADER);
    assert_eq!(expected, ndtree.to_macrocell().to_string());

    let automaton =
        NdAutomaton::<D>::from_macrocell_str(EMPTY_MACROCELL, |_| Ok(Arc::clone(&rule))).unwrap();
    assert!(automaton.tree.root_ref().is_empty());
    assert!(Arc::ptr_eq(&automaton.rule, &rule));
}

#[test]
fn test_macrocell_2d() {
    // from https://www.conwaylife.com/ref/lexicon/lex_m.htm#macrocell
    // but fixed to use `*` instead of `O`
    const MACROCELL: &str = "\
[M2] (golly 3.0)
#R B3/S23
#O -100 400
.**.**$*.*.*.*$**...**$$**...**$*.*.*.*$.**.**$
4 0 1 1 1
5 2 0 2 2
6 3 3 0 3
7 4 4 4 4
";
    const RLE: &str = "\
x = 127, y = 127, rule = B3/S23
9b2ob2o27b2ob2o27b2ob2o27b2ob2o$8bobobobo25bobobobo25bobobobo25bobobob
o$8b2o3b2o25b2o3b2o25b2o3b2o25b2o3b2o2$8b2o3b2o25b2o3b2o25b2o3b2o25b2o
3b2o$8bobobobo25bobobobo25bobobobo25bobobobo$9b2ob2o27b2ob2o27b2ob2o
27b2ob2o2$b2ob2o3b2ob2o19b2ob2o3b2ob2o19b2ob2o3b2ob2o19b2ob2o3b2ob2o$o
bobobobobobobo17bobobobobobobobo17bobobobobobobobo17bobobobobobobobo$
2o3b2ob2o3b2o17b2o3b2ob2o3b2o17b2o3b2ob2o3b2o17b2o3b2ob2o3b2o2$2o3b2ob
2o3b2o17b2o3b2ob2o3b2o17b2o3b2ob2o3b2o17b2o3b2ob2o3b2o$obobobobobobobo
17bobobobobobobobo17bobobobobobobobo17bobobobobobobobo$b2ob2o3b2ob2o
19b2ob2o3b2ob2o19b2ob2o3b2ob2o19b2ob2o3b2ob2o2$9b2ob2o11b2ob2o11b2ob2o
11b2ob2o11b2ob2o11b2ob2o11b2ob2o11b2ob2o$8bobobobo9bobobobo9bobobobo9b
obobobo9bobobobo9bobobobo9bobobobo9bobobobo$8b2o3b2o9b2o3b2o9b2o3b2o9b
2o3b2o9b2o3b2o9b2o3b2o9b2o3b2o9b2o3b2o2$8b2o3b2o9b2o3b2o9b2o3b2o9b2o3b
2o9b2o3b2o9b2o3b2o9b2o3b2o9b2o3b2o$8bobobobo9bobobobo9bobobobo9bobobob
o9bobobobo9bobobobo9bobobobo9bobobobo$9b2ob2o11b2ob2o11b2ob2o11b2ob2o
11b2ob2o11b2ob2o11b2ob2o11b2ob2o2$b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b
2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b
2ob2o$obobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobob
obobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobo$2o3b2o
b2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2o
b2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2o2$2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b
2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b
2ob2o3b2o$obobobobobobobobobobobobobobobobobobobobobobobobobobobobobob
obobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobo$b
2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b
2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o2$41b2ob2o59b2ob2o$40bobobobo
57bobobobo$40b2o3b2o57b2o3b2o2$40b2o3b2o57b2o3b2o$40bobobobo57bobobobo
$41b2ob2o59b2ob2o2$33b2ob2o3b2ob2o51b2ob2o3b2ob2o$32bobobobobobobobo
49bobobobobobobobo$32b2o3b2ob2o3b2o49b2o3b2ob2o3b2o2$32b2o3b2ob2o3b2o
49b2o3b2ob2o3b2o$32bobobobobobobobo49bobobobobobobobo$33b2ob2o3b2ob2o
51b2ob2o3b2ob2o2$41b2ob2o11b2ob2o43b2ob2o11b2ob2o$40bobobobo9bobobobo
41bobobobo9bobobobo$40b2o3b2o9b2o3b2o41b2o3b2o9b2o3b2o2$40b2o3b2o9b2o
3b2o41b2o3b2o9b2o3b2o$40bobobobo9bobobobo41bobobobo9bobobobo$41b2ob2o
11b2ob2o43b2ob2o11b2ob2o2$33b2ob2o3b2ob2o3b2ob2o3b2ob2o35b2ob2o3b2ob2o
3b2ob2o3b2ob2o$32bobobobobobobobobobobobobobobobo33bobobobobobobobobob
obobobobobobo$32b2o3b2ob2o3b2ob2o3b2ob2o3b2o33b2o3b2ob2o3b2ob2o3b2ob2o
3b2o2$32b2o3b2ob2o3b2ob2o3b2ob2o3b2o33b2o3b2ob2o3b2ob2o3b2ob2o3b2o$32b
obobobobobobobobobobobobobobobo33bobobobobobobobobobobobobobobobo$33b
2ob2o3b2ob2o3b2ob2o3b2ob2o35b2ob2o3b2ob2o3b2ob2o3b2ob2o2$9b2ob2o27b2ob
2o27b2ob2o27b2ob2o$8bobobobo25bobobobo25bobobobo25bobobobo$8b2o3b2o25b
2o3b2o25b2o3b2o25b2o3b2o2$8b2o3b2o25b2o3b2o25b2o3b2o25b2o3b2o$8bobobob
o25bobobobo25bobobobo25bobobobo$9b2ob2o27b2ob2o27b2ob2o27b2ob2o2$b2ob
2o3b2ob2o19b2ob2o3b2ob2o19b2ob2o3b2ob2o19b2ob2o3b2ob2o$obobobobobobobo
17bobobobobobobobo17bobobobobobobobo17bobobobobobobobo$2o3b2ob2o3b2o
17b2o3b2ob2o3b2o17b2o3b2ob2o3b2o17b2o3b2ob2o3b2o2$2o3b2ob2o3b2o17b2o3b
2ob2o3b2o17b2o3b2ob2o3b2o17b2o3b2ob2o3b2o$obobobobobobobo17bobobobobob
obobo17bobobobobobobobo17bobobobobobobobo$b2ob2o3b2ob2o19b2ob2o3b2ob2o
19b2ob2o3b2ob2o19b2ob2o3b2ob2o2$9b2ob2o11b2ob2o11b2ob2o11b2ob2o11b2ob
2o11b2ob2o11b2ob2o11b2ob2o$8bobobobo9bobobobo9bobobobo9bobobobo9bobobo
bo9bobobobo9bobobobo9bobobobo$8b2o3b2o9b2o3b2o9b2o3b2o9b2o3b2o9b2o3b2o
9b2o3b2o9b2o3b2o9b2o3b2o2$8b2o3b2o9b2o3b2o9b2o3b2o9b2o3b2o9b2o3b2o9b2o
3b2o9b2o3b2o9b2o3b2o$8bobobobo9bobobobo9bobobobo9bobobobo9bobobobo9bob
obobo9bobobobo9bobobobo$9b2ob2o11b2ob2o11b2ob2o11b2ob2o11b2ob2o11b2ob
2o11b2ob2o11b2ob2o2$b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b
2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o$obobobob
obobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobob
obobobobobobobobobobobobobobobobobobobobobobobobo$2o3b2ob2o3b2ob2o3b2o
b2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2o
b2o3b2ob2o3b2ob2o3b2o2$2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b
2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2o$obob
obobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobobob
obobobobobobobobobobobobobobobobobobobobobobobobobobo$b2ob2o3b2ob2o3b
2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b2ob2o3b
2ob2o3b2ob2o3b2ob2o3b2ob2o2$41b2ob2o59b2ob2o$40bobobobo57bobobobo$40b
2o3b2o57b2o3b2o2$40b2o3b2o57b2o3b2o$40bobobobo57bobobobo$41b2ob2o59b2o
b2o2$33b2ob2o3b2ob2o51b2ob2o3b2ob2o$32bobobobobobobobo49bobobobobobobo
bo$32b2o3b2ob2o3b2o49b2o3b2ob2o3b2o2$32b2o3b2ob2o3b2o49b2o3b2ob2o3b2o$
32bobobobobobobobo49bobobobobobobobo$33b2ob2o3b2ob2o51b2ob2o3b2ob2o2$
41b2ob2o11b2ob2o43b2ob2o11b2ob2o$40bobobobo9bobobobo41bobobobo9bobobob
o$40b2o3b2o9b2o3b2o41b2o3b2o9b2o3b2o2$40b2o3b2o9b2o3b2o41b2o3b2o9b2o3b
2o$40bobobobo9bobobobo41bobobobo9bobobobo$41b2ob2o11b2ob2o43b2ob2o11b
2ob2o2$33b2ob2o3b2ob2o3b2ob2o3b2ob2o35b2ob2o3b2ob2o3b2ob2o3b2ob2o$32bo
bobobobobobobobobobobobobobobo33bobobobobobobobobobobobobobobobo$32b2o
3b2ob2o3b2ob2o3b2ob2o3b2o33b2o3b2ob2o3b2ob2o3b2ob2o3b2o2$32b2o3b2ob2o
3b2ob2o3b2ob2o3b2o33b2o3b2ob2o3b2ob2o3b2ob2o3b2o$32bobobobobobobobobob
obobobobobobo33bobobobobobobobobobobobobobobobo$33b2ob2o3b2ob2o3b2ob2o
3b2ob2o35b2ob2o3b2ob2o3b2ob2o3b2ob2o!
";

    let ndtree = NdTree2D::from_macrocell_str(MACROCELL).unwrap();
    assert_eq!(BigUint::from(2592_usize), ndtree.root_ref().population(),);
    assert_eq!(
        RLE,
        ndtree
            .to_rle(None)
            .unwrap()
            .with_rule(Some("B3/S23"))
            .without_cxrle()
            .to_string_2_state(),
    );
    assert_eq!(
        format!(
            "{}{}",
            crate::MACROCELL_HEADER,
            "
#R B3/S23
#O -100 400
1 0 1 1 0
1 1 0 1 0
1 1 1 0 0
2 1 2 3 0
1 1 1 1 0
1 0 0 1 0
1 0 1 0 0
1 1 0 0 0
2 5 6 7 8
3 4 9 9 4
4 0 10 10 10
5 11 0 11 11
6 12 12 0 12
7 13 13 13 13
",
        ),
        ndtree.to_macrocell().with_rule(Some("B3/S23")).to_string(),
    );
}

pub struct AlternatingRuleString(Vec<RuleString>);

pub struct RuleString {
    pub transition_rules: Vec<TransitionRules>,
    pub b: Option<Conditions>,
    pub s: Option<Conditions>,

    pub ba: Option<Conditions>,
    pub bb: Option<Conditions>,
    pub ta: Option<Conditions>,
    pub tb: Option<Conditions>,
    pub sa: Option<Conditions>,
    pub sb: Option<Conditions>,

    pub c: Option<usize>,

    pub d: Option<Vec<usize>>,

    pub f: Option<Conditions>,
    pub k: Option<Conditions>,
    pub l: Option<Conditions>,

    pub n: Option<Neighborhood>,
    pub r: Option<usize>,

    pub ndim: Option<usize>,

    pub topology: Option<Topology>,
}

pub enum Conditions {
    Hensel(HenselConditions),
    Totalistic(Vec<RangeInclusive<usize>>),
}
pub struct HenselConditions();

pub enum Neighborhood {}

pub enum Topology {
    Basic(Vec<AxisTopology>),
    Sphere(usize), // only valid in 2D
}
#[derive(Debug, Default, Copy, Clone)]
pub struct AxisTopology {
    pub size: Option<usize>,  // None = infinite
    pub flip: bool,           // only valid in 2D
    pub shift: Option<isize>, // only valid in 2D
}
impl AxisTopology {
    pub fn inf() -> Self {
        Self::default()
    }
    pub fn finite(size: usize) -> Self {
        Self {
            size: Some(size),
            ..Default::default()
        }
    }
}
impl Topology {
    pub fn plane(w: usize, h: usize) -> Self {
        Self::Basic(vec![AxisTopology::finite(w), AxisTopology::finite(h)])
    }
}

compile_error!("don't leave this in here");

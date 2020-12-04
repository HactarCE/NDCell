//! Formats for exporting/importing cellular automata.

use std::fmt;
use std::str::FromStr;
use std::sync::Arc;

pub mod macrocell;
pub mod rle;
mod utils;

use crate::automaton::{Automaton, NdAutomaton};
use crate::dim::Dim;
use crate::ndrect::BigRect;
use crate::ndtree::{NdTree, Region, SharedNodePool};
use crate::num::BigInt;
use crate::sim::rule::{NdRule, Rule};
pub use macrocell::{Macrocell, MacrocellError, MacrocellResult};
pub use rle::{Rle, RleError, RleResult};

/// Format that an automaton can be exported to or imported from.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CaFormat {
    /// N-dimensional generalization of Golly extended run-length encoding.
    Rle,
    /// N-dimensional generalization of Golly Macrocell format.
    Macrocell,
}
impl fmt::Display for CaFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CaFormat::Rle => write!(f, "RLE"),
            CaFormat::Macrocell => write!(f, "Macrocell"),
        }
    }
}

/// Error produced during automaton export/import.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum CaFormatError {
    RleError(RleError),
    MacrocellError(MacrocellError),
}
impl From<RleError> for CaFormatError {
    fn from(e: RleError) -> Self {
        Self::RleError(e)
    }
}
impl From<MacrocellError> for CaFormatError {
    fn from(e: MacrocellError) -> Self {
        Self::MacrocellError(e)
    }
}
impl fmt::Display for CaFormatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RleError(e) => write!(f, "RLE error: {}", e),
            Self::MacrocellError(e) => write!(f, "Macrocell error: {}", e),
        }
    }
}

/// Exports an automaton to a string using a particular format.
pub fn export_ndautomaton_to_string<D: Dim>(
    automaton: &NdAutomaton<D>,
    format: CaFormat,
) -> Result<String, CaFormatError> {
    match format {
        CaFormat::Rle => Rle::from_ndautomaton_to_string(automaton, None).map_err(Into::into),
        CaFormat::Macrocell => {
            Macrocell::from_ndautomaton_to_string(automaton, None).map_err(Into::into)
        }
    }
}
/// Exports an automaton of any dimensionality to a string using a particular
/// format.
pub fn export_automaton_to_string(
    automaton: &Automaton,
    format: CaFormat,
) -> Result<String, CaFormatError> {
    match format {
        CaFormat::Rle => Rle::from_automaton_to_string(automaton).map_err(Into::into),
        CaFormat::Macrocell => Macrocell::from_automaton_to_string(automaton).map_err(Into::into),
    }
}
/// Imports an automaton from a string using the first format that works,
/// returning the error resulting from each attempt if none succeeded.
pub fn import_automaton_from_string<R: ResolveRule + Clone>(
    s: &str,
    resolve_rule: R,
) -> Result<Result<Automaton, R::Err>, Vec<CaFormatError>> {
    let mut errors: Vec<CaFormatError> = vec![];
    match Rle::from_string_to_automaton(s, resolve_rule.clone()) {
        Ok(a) => return Ok(a),
        Err(e) => errors.push(e.into()),
    };
    match Macrocell::from_string_to_automaton(s, resolve_rule.clone()) {
        Ok(a) => return Ok(a),
        Err(e) => errors.push(e.into()),
    };
    Err(errors)
}

/// Whether a rule has more than two states.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TwoState {
    /// The rule has exactly two states.
    TwoStates,
    /// The rule has more than two states.
    MoreStates,
}
impl Default for TwoState {
    fn default() -> Self {
        Self::MoreStates
    }
}
impl TwoState {
    /// Returns a `TwoState` instance based on the number of states in a rule.
    pub fn from_rule<D: Dim>(r: &dyn NdRule<D>) -> Self {
        if r.max_state() == 1_u8 {
            Self::TwoStates
        } else {
            Self::MoreStates
        }
    }
}

/// Converter from rule name to rule.
pub trait ResolveRule {
    /// Error returned if the rule name is invalid.
    type Err;

    /// Returns a rule based on a rule name.
    fn resolve_rule(self, s: Option<&str>) -> Result<Rule, Self::Err>;
}
impl<F, E> ResolveRule for F
where
    F: FnOnce(Option<&str>) -> Result<Rule, E>,
{
    type Err = E;

    fn resolve_rule(self, s: Option<&str>) -> Result<Rule, Self::Err> {
        self(s)
    }
}
impl ResolveRule for Rule {
    type Err = std::convert::Infallible;

    fn resolve_rule(self, _: Option<&str>) -> Result<Rule, Self::Err> {
        Ok(self)
    }
}
impl<D: Dim> ResolveRule for Arc<dyn NdRule<D>> {
    type Err = std::convert::Infallible;

    fn resolve_rule(self, _: Option<&str>) -> Result<Rule, Self::Err> {
        Ok(self.into())
    }
}

/// Trait for string representations of CA patterns.
///
/// The error returned from all these methods is the same one that `from_str()`
/// returns.
pub trait SerializablePattern: FromStr + ToString {
    /// Serializes the pattern for an automaton with exactly two states.
    fn to_string_2_state(&self) -> String {
        self.to_string()
    }

    /// Returns the name of the rule.
    fn rule(&self) -> Option<&str>;
    /// Sets the rule name.
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn with_rule(self, rule: Option<impl ToString>) -> Self;

    /// Returns the number of generations.
    fn generation(&self) -> BigInt;
    /// Sets the number of generations.
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn with_generation(self, generation: BigInt) -> Self;

    /// Returns the user comments.
    fn comments(&self) -> &str;
    /// Returns the user comments.
    fn comments_mut(&mut self) -> &mut String;
    /// Sets the user comments.
    #[must_use = "This method returns a new value instead of mutating its input"]
    fn with_comments(mut self, comments: impl ToString) -> Self {
        *self.comments_mut() = comments.to_string();
        self
    }

    /// Returns the region bounding the pattern, used mainly for copy/paste.
    fn region<D: Dim>(&self) -> Result<Region<D>, Self::Err>;

    /// Converts the serializable pattern into an automaton of unknown
    /// dimensionality, given a closure to resolve the name of a rule.
    fn to_automaton<R: ResolveRule>(
        &self,
        rule_resolver: R,
    ) -> Result<Result<Automaton, R::Err>, Self::Err> {
        match rule_resolver.resolve_rule(self.rule()) {
            Ok(Rule::Rule1D(rule)) => self
                .to_ndautomaton(rule, SharedNodePool::new())
                .map(Automaton::Automaton1D)
                .map(Ok),
            Ok(Rule::Rule2D(rule)) => self
                .to_ndautomaton(rule, SharedNodePool::new())
                .map(Automaton::Automaton2D)
                .map(Ok),
            Ok(Rule::Rule3D(rule)) => self
                .to_ndautomaton(rule, SharedNodePool::new())
                .map(Automaton::Automaton3D)
                .map(Ok),
            Ok(Rule::Rule4D(rule)) => self
                .to_ndautomaton(rule, SharedNodePool::new())
                .map(Automaton::Automaton4D)
                .map(Ok),
            Ok(Rule::Rule5D(rule)) => self
                .to_ndautomaton(rule, SharedNodePool::new())
                .map(Automaton::Automaton5D)
                .map(Ok),
            Ok(Rule::Rule6D(rule)) => self
                .to_ndautomaton(rule, SharedNodePool::new())
                .map(Automaton::Automaton6D)
                .map(Ok),
            Err(rule_resolution_error) => Ok(Err(rule_resolution_error)),
        }
    }
    /// Converts the serializable pattern into an automaton, given a rule
    /// (ignoring the serialized rule name).
    fn to_ndautomaton<D: Dim>(
        &self,
        rule: Arc<dyn NdRule<D>>,
        node_pool: SharedNodePool<D>,
    ) -> Result<NdAutomaton<D>, Self::Err> {
        Ok(NdAutomaton {
            ndtree: self.to_ndtree(node_pool)?,
            rule,
            generations: self.generation(),
            comments: self.comments().to_owned(),
        })
    }
    /// Converts the serializable pattern into an ND-tree.
    fn to_ndtree<D: Dim>(&self, node_pool: SharedNodePool<D>) -> Result<NdTree<D>, Self::Err>;

    /// Converts an automaton of any dimensionality into a serializable format.
    fn from_automaton(automaton: &Automaton) -> Result<Self, Self::Err> {
        match automaton {
            Automaton::Automaton1D(a) => Self::from_ndautomaton(a, None),
            Automaton::Automaton2D(a) => Self::from_ndautomaton(a, None),
            Automaton::Automaton3D(a) => Self::from_ndautomaton(a, None),
            Automaton::Automaton4D(a) => Self::from_ndautomaton(a, None),
            Automaton::Automaton5D(a) => Self::from_ndautomaton(a, None),
            Automaton::Automaton6D(a) => Self::from_ndautomaton(a, None),
        }
    }
    /// Converts an automaton into a serializable format. If `rect` is `None`,
    /// the entire grid is exported.
    fn from_ndautomaton<D: Dim>(
        automaton: &NdAutomaton<D>,
        rect: Option<BigRect<D>>,
    ) -> Result<Self, Self::Err> {
        Ok(Self::from_ndtree(&automaton.ndtree, rect)?
            .with_rule(Some(&automaton.rule))
            .with_generation(automaton.generations.clone())
            .with_comments(automaton.comments.clone()))
    }
    /// Converts part of an ND-tree into a serializable format. If `rect` is
    /// `None`, the entire grid is exported.
    fn from_ndtree<D: Dim>(ndtree: &NdTree<D>, rect: Option<BigRect<D>>)
        -> Result<Self, Self::Err>;

    /// Deserializes an automaton of unknown dimensionality, given a closure to
    /// resolve the name of a rule.
    fn from_string_to_automaton<R: ResolveRule>(
        s: &str,
        resolve_rule: R,
    ) -> Result<Result<Automaton, R::Err>, Self::Err> {
        s.parse::<Self>()?.to_automaton(resolve_rule)
    }
    /// Deserializes an automaton, given a rule (ignoring the serialized rule
    /// name).
    fn from_string_to_ndautomaton<D: Dim>(
        s: &str,
        rule: Arc<dyn NdRule<D>>,
    ) -> Result<NdAutomaton<D>, Self::Err> {
        s.parse::<Self>()?
            .to_ndautomaton(rule, SharedNodePool::new())
    }
    /// Deserializes an ND-tree.
    fn from_string_to_ndtree<D: Dim>(s: &str) -> Result<NdTree<D>, Self::Err> {
        s.parse::<Self>()?.to_ndtree(SharedNodePool::new())
    }

    /// Serializes an automaton of any dimensionality.
    fn from_automaton_to_string(automaton: &Automaton) -> Result<String, Self::Err> {
        match automaton {
            Automaton::Automaton1D(a) => Self::from_ndautomaton_to_string(a, None),
            Automaton::Automaton2D(a) => Self::from_ndautomaton_to_string(a, None),
            Automaton::Automaton3D(a) => Self::from_ndautomaton_to_string(a, None),
            Automaton::Automaton4D(a) => Self::from_ndautomaton_to_string(a, None),
            Automaton::Automaton5D(a) => Self::from_ndautomaton_to_string(a, None),
            Automaton::Automaton6D(a) => Self::from_ndautomaton_to_string(a, None),
        }
    }
    /// Serializes part of an automaton. If `rect` is `None`, the entire grid is
    /// exported.
    fn from_ndautomaton_to_string<D: Dim>(
        automaton: &NdAutomaton<D>,
        rect: Option<BigRect<D>>,
    ) -> Result<String, Self::Err> {
        let this = Self::from_ndautomaton(automaton, rect)?;
        let two_states = TwoState::from_rule(&*automaton.rule);
        match two_states {
            TwoState::TwoStates => Ok(this.to_string_2_state()),
            TwoState::MoreStates => Ok(this.to_string()),
        }
    }
    /// Serializes part of an ND-tree. If `rect` is `None`, the entire grid is
    /// exported.
    fn from_ndtree_to_string<D: Dim>(
        ndtree: &NdTree<D>,
        rect: Option<BigRect<D>>,
        two_states: TwoState,
    ) -> Result<String, Self::Err> {
        let this = Self::from_ndtree(ndtree, rect)?;
        match two_states {
            TwoState::TwoStates => Ok(this.to_string_2_state()),
            TwoState::MoreStates => Ok(this.to_string()),
        }
    }
}

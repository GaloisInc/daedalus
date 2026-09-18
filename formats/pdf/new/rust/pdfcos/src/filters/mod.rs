//! Stream filter support shared by native PDF primitives.

mod predictor;

pub(crate) use predictor::apply_predictor;

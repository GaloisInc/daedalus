

macro_rules! Declare {
  ($nm: ident, $rep: ty, $repi: ty, $w: literal) => {
    #[derive(Copy,Clone,core::cmp::PartialEq,core::cmp::Eq,core::cmp::PartialOrd,core::cmp::Ord)]
    pub struct $nm($rep);
    // XXX
  }
}

Declare!(U1, u8, i8, 1);
Declare!(U2, u8, i8, 2);
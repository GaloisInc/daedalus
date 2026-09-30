use crate::text_extract_parsers::Fonts;
use daedalus_pdf_cos::Ref;
use std::collections::BTreeMap;

#[derive(Clone, Copy)]
pub(crate) struct Matrix {
    pub(crate) a: f64,
    pub(crate) b: f64,
    pub(crate) c: f64,
    pub(crate) d: f64,
    pub(crate) e: f64,
    pub(crate) f: f64,
}

impl Matrix {
    pub(crate) const IDENTITY: Self = Self {
        a: 1.0,
        b: 0.0,
        c: 0.0,
        d: 1.0,
        e: 0.0,
        f: 0.0,
    };
}

#[derive(Clone)]
pub(crate) struct GraphicsState {
    pub(crate) ctm: Matrix,
    pub(crate) font: Option<Fonts::Font>,
    pub(crate) font_size: Option<f64>,
    pub(crate) character_spacing: f64,
    pub(crate) word_spacing: f64,
    pub(crate) horizontal_scaling: f64,
    pub(crate) leading: f64,
    pub(crate) rendering_mode: u8,
    pub(crate) text_rise: f64,
}

impl Default for GraphicsState {
    fn default() -> Self {
        Self {
            ctm: Matrix::IDENTITY,
            font: None,
            font_size: None,
            character_spacing: 0.0,
            word_spacing: 0.0,
            horizontal_scaling: 100.0,
            leading: 0.0,
            rendering_mode: 0,
            text_rise: 0.0,
        }
    }
}

pub(crate) struct ExtractionState {
    pub(crate) graphics: GraphicsState,
    pub(crate) graphics_stack: Vec<GraphicsState>,
    pub(crate) in_text: bool,
    pub(crate) text_matrix: Matrix,
    pub(crate) text_line_matrix: Matrix,
    pub(crate) font_cache: BTreeMap<Ref, Fonts::Font>,
    pub(crate) output: Vec<u16>,
}

impl ExtractionState {
    pub(crate) fn new() -> Self {
        Self {
            graphics: GraphicsState::default(),
            graphics_stack: Vec::new(),
            in_text: false,
            text_matrix: Matrix::IDENTITY,
            text_line_matrix: Matrix::IDENTITY,
            font_cache: BTreeMap::new(),
            output: Vec::new(),
        }
    }

    pub(crate) fn reset_for_page(&mut self) {
        self.graphics = GraphicsState::default();
        self.graphics_stack.clear();
        self.in_text = false;
        self.text_matrix = Matrix::IDENTITY;
        self.text_line_matrix = Matrix::IDENTITY;
    }
}

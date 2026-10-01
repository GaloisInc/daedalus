use crate::text_extract_parsers::Fonts;
use daedalus_pdf_cos::Ref;
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Point {
    pub x: f64,
    pub y: f64,
}

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

    pub(crate) fn multiply(self, right: Self) -> Self {
        Self {
            a: self.a * right.a + self.b * right.c,
            b: self.a * right.b + self.b * right.d,
            c: self.c * right.a + self.d * right.c,
            d: self.c * right.b + self.d * right.d,
            e: self.e * right.a + self.f * right.c + right.e,
            f: self.e * right.b + self.f * right.d + right.f,
        }
    }

    pub(crate) fn transform_point(self, point: Point) -> Option<Point> {
        let transformed = Point {
            x: point.x * self.a + point.y * self.c + self.e,
            y: point.x * self.b + point.y * self.d + self.f,
        };
        (transformed.x.is_finite() && transformed.y.is_finite()).then_some(transformed)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BoundingBox {
    pub min: Point,
    pub max: Point,
}

impl BoundingBox {
    /// The points must be the four corners of a rectangle.
    pub(crate) fn from_corners(corners: [Point; 4]) -> Self {
        let first = corners[0];
        let mut bounds = Self {
            min: first,
            max: first,
        };

        for point in &corners[1..] {
            bounds.min.x = bounds.min.x.min(point.x);
            bounds.min.y = bounds.min.y.min(point.y);
            bounds.max.x = bounds.max.x.max(point.x);
            bounds.max.y = bounds.max.y.max(point.y);
        }

        bounds
    }

    pub(crate) fn transform(self, matrix: Matrix) -> Option<Self> {
        let corners = [
            self.min,
            Point {
                x: self.max.x,
                y: self.min.y,
            },
            self.max,
            Point {
                x: self.min.x,
                y: self.max.y,
            },
        ];
        let transformed = [
            matrix.transform_point(corners[0])?,
            matrix.transform_point(corners[1])?,
            matrix.transform_point(corners[2])?,
            matrix.transform_point(corners[3])?,
        ];

        Some(Self::from_corners(transformed))
    }

    pub(crate) fn union(self, other: Self) -> Self {
        Self {
            min: Point {
                x: self.min.x.min(other.min.x),
                y: self.min.y.min(other.min.y),
            },
            max: Point {
                x: self.max.x.max(other.max.x),
                y: self.max.y.max(other.max.y),
            },
        }
    }
}

pub(crate) struct RawTextChunk {
    pub(crate) page_number: u64,
    pub(crate) text: Vec<u16>,
    pub(crate) bounding_box: Option<BoundingBox>,
}

#[derive(Clone, Copy)]
pub(crate) enum ChunkGeometry {
    Empty,
    Bounds(BoundingBox),
    Unknown,
}

pub(crate) struct ChunkBuilder {
    pub(crate) text: Vec<u16>,
    pub(crate) geometry: ChunkGeometry,
}

#[derive(Clone)]
pub(crate) struct GraphicsState {
    pub(crate) ctm: Option<Matrix>,         // Current transformation matrix.
    pub(crate) font: Option<Fonts::Font>,   // Currently selected text font.
    pub(crate) font_size: Option<f64>,      // Font size in text-space units.
    pub(crate) character_spacing: f64,      // Extra character spacing in text-space units.
    pub(crate) word_spacing: f64,           // Extra word spacing in text-space units.
    pub(crate) horizontal_scaling: f64,     // Percentage; 100 means normal width.
    pub(crate) leading: f64,                // Line spacing in text-space units.
    pub(crate) rendering_mode: u8,          // Text painting and clipping mode.
    pub(crate) text_rise: f64,              // Baseline displacement in text-space units.
}

impl Default for GraphicsState {
    fn default() -> Self {
        Self {
            ctm: Some(Matrix::IDENTITY),
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
    pub(crate) text_matrix: Option<Matrix>,
    pub(crate) text_line_matrix: Option<Matrix>,
    pub(crate) font_cache: BTreeMap<Ref, Fonts::Font>,
    pub(crate) chunks: Vec<RawTextChunk>,
    pub(crate) current_chunk: Option<ChunkBuilder>,
    pub(crate) current_page_number: Option<u64>,
    pub(crate) malformed_operators: u64,
}

impl ExtractionState {
    pub(crate) fn new() -> Self {
        Self {
            graphics: GraphicsState::default(),
            graphics_stack: Vec::new(),
            text_matrix: Some(Matrix::IDENTITY),
            text_line_matrix: Some(Matrix::IDENTITY),
            font_cache: BTreeMap::new(),
            chunks: Vec::new(),
            current_chunk: None,
            current_page_number: None,
            malformed_operators: 0,
        }
    }

    pub(crate) fn reset_for_page(&mut self) {
        self.graphics = GraphicsState::default();
        self.graphics_stack.clear();
        self.text_matrix = Some(Matrix::IDENTITY);
        self.text_line_matrix = Some(Matrix::IDENTITY);
        self.current_chunk = None;
    }

    pub(crate) fn set_current_page(&mut self, page_number: u64) {
        assert!(
            self.current_chunk.is_none(),
            "cannot change pages while a text chunk is active"
        );
        self.current_page_number = Some(page_number);
    }

    pub(crate) fn begin_chunk(&mut self) {
        assert!(
            self.current_chunk.is_none(),
            "cannot begin a text chunk while another chunk is active"
        );
        self.current_chunk = Some(ChunkBuilder {
            text: Vec::new(),
            geometry: ChunkGeometry::Empty,
        });
    }

    pub(crate) fn append_to_chunk(&mut self, text: &[u16], bounds: Option<BoundingBox>) {
        let chunk = self
            .current_chunk
            .as_mut()
            .expect("cannot append text without an active chunk");

        chunk.text.extend_from_slice(text);
        chunk.geometry = match (chunk.geometry, bounds) {
            (ChunkGeometry::Unknown, _) | (_, None) => ChunkGeometry::Unknown,
            (ChunkGeometry::Empty, Some(bounds)) => ChunkGeometry::Bounds(bounds),
            (ChunkGeometry::Bounds(current), Some(bounds)) => {
                ChunkGeometry::Bounds(current.union(bounds))
            }
        };
    }

    pub(crate) fn finish_chunk(&mut self) {
        let chunk = self
            .current_chunk
            .take()
            .expect("cannot finish a text chunk when no chunk is active");
        let bounding_box = match chunk.geometry {
            ChunkGeometry::Bounds(bounds) => Some(bounds),
            ChunkGeometry::Empty | ChunkGeometry::Unknown => None,
        };
        let page_number = self
            .current_page_number
            .expect("cannot finish a text chunk without a current page");
        self.chunks.push(RawTextChunk {
            page_number,
            text: chunk.text,
            bounding_box,
        });
    }
}

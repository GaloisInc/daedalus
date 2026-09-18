use daedalus_pdf_cos::PdfCos;

pub struct TextExtractState {
    pub(crate) pdf: PdfCos,
    pub(crate) emitted: Vec<u32>,
}

impl TextExtractState {
    pub fn new(pdf: PdfCos) -> Self {
        Self {
            pdf,
            emitted: Vec::new(),
        }
    }

    pub fn output(&self) -> &[u32] {
        &self.emitted
    }

    pub fn clear_output(&mut self) {
        self.emitted.clear();
    }

    pub fn take_output(&mut self) -> Vec<u32> {
        std::mem::take(&mut self.emitted)
    }
}

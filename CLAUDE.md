# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`pdf-extract` is a Rust library for extracting content (primarily text) from PDF files. It uses `lopdf` as the underlying PDF parser and implements custom logic for font handling, encoding, and text extraction.

## Build and Test Commands

```bash
# Build the library
cargo build

# Build in release mode (includes debug symbols)
cargo build --release

# Run all tests
cargo test

# Run tests with logging output
cargo test -- --nocapture

# Run a specific test
cargo test extract_expected_text

# Run the extraction example
cargo run --example extract <pdf-file> [output-format]
# output-format: txt (default), html, or svg
```

## Architecture

### Core Extraction Pipeline

1. **PDF Loading**: Uses `lopdf::Document` to load and parse PDF structure
2. **Font Processing**: Multiple font types are handled through the `PdfFont` trait:
   - `PdfSimpleFont`: Type1 and other simple fonts with single-byte encodings
   - `PdfCIDFont`: Type0 fonts (CID-keyed fonts for multi-byte character encodings)
   - `PdfType3Font`: Type3 fonts (user-defined character shapes)
3. **Encoding Resolution**: Complex encoding chain converts PDF character codes to Unicode:
   - Built-in encodings (StandardEncoding, MacRomanEncoding, WinAnsiEncoding, etc.)
   - Font-specific encodings from font dictionaries
   - ToUnicode CMaps for explicit character mappings
   - Core font metrics with glyph name tables
4. **Content Stream Processing**: Interprets PDF content operators to extract positioned text
5. **Output Generation**: Produces text through the `OutputDev` trait

### Key Modules

- **`lib.rs`**: Main extraction logic, font handling, content stream processing
- **`core_fonts.rs`**: Metrics for PDF's 14 standard fonts (generated data)
- **`glyphnames.rs`**: Adobe Glyph List mapping glyph names to Unicode (generated data)
- **`zapfglyphnames.rs`**: ZapfDingbats-specific glyph mappings (generated data)
- **`encodings.rs`**: Standard encoding tables (MacRoman, WinAnsi, etc.)

### Font Handling

The library implements a trait-based font system:

```rust
trait PdfFont {
    fn get_width(&self, char_code: CharCode) -> f64;
    fn decode_char(&self, char_code: CharCode) -> String;
}
```

Font creation uses `make_font()` which inspects the font's Subtype to instantiate the appropriate implementation. Each font type handles character-to-Unicode mapping differently:

- **Simple fonts**: Use encoding tables (256 entries) that may come from standard encodings, font dictionaries, or ToUnicode CMaps
- **CID fonts**: Use CMap resources to map character codes to CIDs, then optionally to Unicode via ToUnicode CMaps
- **Type3 fonts**: Similar to simple fonts but with user-defined glyphs

### Output Formats

The `OutputDev` trait abstracts output generation:

- **`PlainTextOutput`**: Extracts plain text with basic positioning
- **`HTMLOutput`**: Generates absolutely-positioned HTML divs preserving layout
- **`SVGOutput`**: Produces SVG with positioned text elements

### Public API

Main entry points (in `src/lib.rs`):

- `extract_text(path)`: Extract all text from a PDF file
- `extract_text_from_mem(buffer)`: Extract from in-memory PDF data
- `extract_text_by_pages(path)`: Returns Vec of strings, one per page
- `extract_text_encrypted(path, password)`: Handle encrypted PDFs
- `output_doc(doc, output)`: Low-level API using OutputDev trait
- `print_metadata(doc)`: Print PDF metadata to stdout

## Testing

Test files are in `tests/docs/`. Files ending in `.pdf.link` contain URLs to external PDFs that are downloaded on-demand to `tests/docs_cache/` during test execution.

The test suite uses the `test-log` crate to enable logging output during tests. Use `RUST_LOG` environment variable to control log level:

```bash
RUST_LOG=debug cargo test
```

## Common Development Patterns

### Debugging Text Extraction Issues

The codebase uses a `dlog!` macro for debug logging (currently disabled by default). To enable, modify the macro definition in `src/lib.rs:73-76`. The `log` crate is also used with `warn!`, `error!`, and `debug!` macros.

### Encoding Fallbacks

When character encoding fails, the code typically:
1. Tries ToUnicode CMap first (if present)
2. Falls back to font's built-in encoding
3. For core fonts, uses glyph name tables
4. Last resort: uses `encoding_rs` to guess encoding (look for "falling back to encoding" debug messages)

### Graphics State Management

Content stream processing maintains two key state structures:
- `GraphicsState`: CTM (current transformation matrix), color spaces, soft masks
- `TextState`: Font, font size, text matrix, character/word spacing, horizontal scaling

These are manipulated by PDF operators (e.g., `Tm` sets text matrix, `Tf` sets font).

## Version and Publishing

Current version: 0.10.0

The CI workflow includes semver checking via `cargo-semver-checks-action` to prevent accidental breaking changes.

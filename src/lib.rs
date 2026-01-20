extern crate lopdf;

use adobe_cmap_parser::{ByteMapping, CodeRange, CIDRange};
use encoding_rs::UTF_16BE;
use lopdf::content::Content;
pub use lopdf::*;
use euclid::*;
use lopdf::encryption::DecryptionError;
use std::fmt::{Debug, Formatter};
extern crate encoding_rs;
extern crate euclid;
extern crate adobe_cmap_parser;
extern crate type1_encoding_parser;
extern crate unicode_normalization;
use euclid::vec2;
use unicode_normalization::UnicodeNormalization;
use std::fmt;
use std::str;
use std::fs::File;
use std::slice::Iter;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::rc::Rc;
use std::marker::PhantomData;
use std::result::Result;
use log::{warn, error, debug};
mod core_fonts;
mod glyphnames;
mod zapfglyphnames;
mod encodings;

pub struct Space;
pub type Transform = Transform2D<f64, Space, Space>;

#[derive(Debug)]
pub enum OutputError
{
    FormatError(std::fmt::Error),
    IoError(std::io::Error),
    PdfError(lopdf::Error)
}

impl std::fmt::Display for OutputError
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            OutputError::FormatError(e) => write!(f, "Formating error: {}", e),
            OutputError::IoError(e) => write!(f, "IO error: {}", e),
            OutputError::PdfError(e) => write!(f, "PDF error: {}", e)
        }
    }
}

impl std::error::Error for OutputError{}

impl From<std::fmt::Error> for OutputError {
    fn from(e: std::fmt::Error) -> Self {
        OutputError::FormatError(e)
    }
}

impl From<std::io::Error> for OutputError {
    fn from(e: std::io::Error) -> Self {
        OutputError::IoError(e)
    }
}

impl From<lopdf::Error> for OutputError {
    fn from(e: lopdf::Error) -> Self {
        OutputError::PdfError(e)
    }
}

macro_rules! dlog {
    ($($e:expr),*) => { {$(let _ = $e;)*} }
    //($($t:tt)*) => { println!($($t)*) }
}

fn get_info(doc: &Document) -> Option<&Dictionary> {
    match doc.trailer.get(b"Info") {
        Ok(&Object::Reference(ref id)) => {
            match doc.get_object(*id) {
                Ok(&Object::Dictionary(ref info)) => { return Some(info); }
                _ => {}
            }
        }
        _ => {}
    }
    None
}

fn get_catalog(doc: &Document) -> &Dictionary {
    match doc.trailer.get(b"Root").unwrap() {
        &Object::Reference(ref id) => {
            match doc.get_object(*id) {
                Ok(&Object::Dictionary(ref catalog)) => { return catalog; }
                _ => {}
            }
        }
        _ => {}
    }
    panic!();
}

fn get_pages(doc: &Document) -> &Dictionary {
    let catalog = get_catalog(doc);
    match catalog.get(b"Pages").unwrap() {
        &Object::Reference(ref id) => {
            match doc.get_object(*id) {
                Ok(&Object::Dictionary(ref pages)) => { return pages; }
                other => {dlog!("pages: {:?}", other)}
            }
        }
        other => { dlog!("pages: {:?}", other)}
    }
    dlog!("catalog {:?}", catalog);
    panic!();
}

#[allow(non_upper_case_globals)]
const PDFDocEncoding: &'static [u16] = &[
    0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 0x0008,
    0x0009, 0x000a, 0x000b, 0x000c, 0x000d, 0x000e, 0x000f, 0x0010, 0x0011,
    0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017, 0x02d8, 0x02c7, 0x02c6,
    0x02d9, 0x02dd, 0x02db, 0x02da, 0x02dc, 0x0020, 0x0021, 0x0022, 0x0023,
    0x0024, 0x0025, 0x0026, 0x0027, 0x0028, 0x0029, 0x002a, 0x002b, 0x002c,
    0x002d, 0x002e, 0x002f, 0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035,
    0x0036, 0x0037, 0x0038, 0x0039, 0x003a, 0x003b, 0x003c, 0x003d, 0x003e,
    0x003f, 0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047,
    0x0048, 0x0049, 0x004a, 0x004b, 0x004c, 0x004d, 0x004e, 0x004f, 0x0050,
    0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 0x0058, 0x0059,
    0x005a, 0x005b, 0x005c, 0x005d, 0x005e, 0x005f, 0x0060, 0x0061, 0x0062,
    0x0063, 0x0064, 0x0065, 0x0066, 0x0067, 0x0068, 0x0069, 0x006a, 0x006b,
    0x006c, 0x006d, 0x006e, 0x006f, 0x0070, 0x0071, 0x0072, 0x0073, 0x0074,
    0x0075, 0x0076, 0x0077, 0x0078, 0x0079, 0x007a, 0x007b, 0x007c, 0x007d,
    0x007e, 0x0000, 0x2022, 0x2020, 0x2021, 0x2026, 0x2014, 0x2013, 0x0192,
    0x2044, 0x2039, 0x203a, 0x2212, 0x2030, 0x201e, 0x201c, 0x201d, 0x2018,
    0x2019, 0x201a, 0x2122, 0xfb01, 0xfb02, 0x0141, 0x0152, 0x0160, 0x0178,
    0x017d, 0x0131, 0x0142, 0x0153, 0x0161, 0x017e, 0x0000, 0x20ac, 0x00a1,
    0x00a2, 0x00a3, 0x00a4, 0x00a5, 0x00a6, 0x00a7, 0x00a8, 0x00a9, 0x00aa,
    0x00ab, 0x00ac, 0x0000, 0x00ae, 0x00af, 0x00b0, 0x00b1, 0x00b2, 0x00b3,
    0x00b4, 0x00b5, 0x00b6, 0x00b7, 0x00b8, 0x00b9, 0x00ba, 0x00bb, 0x00bc,
    0x00bd, 0x00be, 0x00bf, 0x00c0, 0x00c1, 0x00c2, 0x00c3, 0x00c4, 0x00c5,
    0x00c6, 0x00c7, 0x00c8, 0x00c9, 0x00ca, 0x00cb, 0x00cc, 0x00cd, 0x00ce,
    0x00cf, 0x00d0, 0x00d1, 0x00d2, 0x00d3, 0x00d4, 0x00d5, 0x00d6, 0x00d7,
    0x00d8, 0x00d9, 0x00da, 0x00db, 0x00dc, 0x00dd, 0x00de, 0x00df, 0x00e0,
    0x00e1, 0x00e2, 0x00e3, 0x00e4, 0x00e5, 0x00e6, 0x00e7, 0x00e8, 0x00e9,
    0x00ea, 0x00eb, 0x00ec, 0x00ed, 0x00ee, 0x00ef, 0x00f0, 0x00f1, 0x00f2,
    0x00f3, 0x00f4, 0x00f5, 0x00f6, 0x00f7, 0x00f8, 0x00f9, 0x00fa, 0x00fb,
    0x00fc, 0x00fd, 0x00fe, 0x00ff];

fn pdf_to_utf8(s: &[u8]) -> String {
    if s.len() > 2 && s[0] == 0xfe && s[1] == 0xff {
        return UTF_16BE.decode_without_bom_handling_and_without_replacement(&s[2..]).unwrap().to_string()
    } else {
        let r : Vec<u8> = s.iter().map(|x| *x).flat_map(|x| {
               let k = PDFDocEncoding[x as usize];
               vec![(k>>8) as u8, k as u8].into_iter()}).collect();
        return UTF_16BE.decode_without_bom_handling_and_without_replacement(&r).unwrap().to_string()
    }
}

fn to_utf8(encoding: &[u16], s: &[u8]) -> String {
    if s.len() > 2 && s[0] == 0xfe && s[1] == 0xff {
        return UTF_16BE.decode_without_bom_handling_and_without_replacement(&s[2..]).unwrap().to_string()
    } else {
        let r : Vec<u8> = s.iter().map(|x| *x).flat_map(|x| {
            let k = encoding[x as usize];
            vec![(k>>8) as u8, k as u8].into_iter()}).collect();
        return UTF_16BE.decode_without_bom_handling_and_without_replacement(&r).unwrap().to_string()
    }
}


fn maybe_deref<'a>(doc: &'a Document, o: &'a Object) -> &'a Object {
    match o {
        &Object::Reference(r) => doc.get_object(r).expect("missing object reference"),
        _ => o
    }
}

fn maybe_get_obj<'a>(doc: &'a Document, dict: &'a Dictionary, key: &[u8]) -> Option<&'a Object> {
    dict.get(key).map(|o| maybe_deref(doc, o)).ok()
}

// an intermediate trait that can be used to chain conversions that may have failed
trait FromOptObj<'a> {
    fn from_opt_obj(doc: &'a Document, obj: Option<&'a Object>, key: &[u8]) -> Self;
}

// conditionally convert to Self returns None if the conversion failed
trait FromObj<'a> where Self: std::marker::Sized {
    fn from_obj(doc: &'a Document, obj: &'a Object) -> Option<Self>;
}

impl<'a, T: FromObj<'a>> FromOptObj<'a> for Option<T> {
    fn from_opt_obj(doc: &'a Document, obj: Option<&'a Object>, _key: &[u8]) -> Self {
        obj.and_then(|x| T::from_obj(doc,x))
    }
}

impl<'a, T: FromObj<'a>> FromOptObj<'a> for T {
    fn from_opt_obj(doc: &'a Document, obj: Option<&'a Object>, key: &[u8]) -> Self {
        T::from_obj(doc, obj.expect(&String::from_utf8_lossy(key))).expect("wrong type")
    }
}

// we follow the same conventions as pdfium for when to support indirect objects:
// on arrays, streams and dicts
impl<'a, T: FromObj<'a>> FromObj<'a> for Vec<T> {
    fn from_obj(doc: &'a Document, obj: &'a Object) -> Option<Self> {
        maybe_deref(doc, obj).as_array().map(|x| x.iter()
            .map(|x| T::from_obj(doc, x).expect("wrong type"))
            .collect()).ok()
    }
}

// XXX: These will panic if we don't have the right number of items
// we don't want to do that
impl<'a, T: FromObj<'a>> FromObj<'a> for [T; 4] {
    fn from_obj(doc: &'a Document, obj: &'a Object) -> Option<Self> {
        maybe_deref(doc, obj).as_array().map(|x| {
            let mut all = x.iter()
                .map(|x| T::from_obj(doc, x).expect("wrong type"));
            [all.next().unwrap(), all.next().unwrap(), all.next().unwrap(), all.next().unwrap()]
        }).ok()
    }
}

impl<'a, T: FromObj<'a>> FromObj<'a> for [T; 3] {
    fn from_obj(doc: &'a Document, obj: &'a Object) -> Option<Self> {
        maybe_deref(doc, obj).as_array().map(|x| {
            let mut all = x.iter()
                .map(|x| T::from_obj(doc, x).expect("wrong type"));
            [all.next().unwrap(), all.next().unwrap(), all.next().unwrap()]
        }).ok()
    }
}

impl<'a> FromObj<'a> for f64 {
    fn from_obj(_doc: &Document, obj: &Object) -> Option<Self> {
        match obj {
            &Object::Integer(i) => Some(i as f64),
            &Object::Real(f) => Some(f.into()),
            _ => None
        }
    }
}

impl<'a> FromObj<'a> for i64 {
    fn from_obj(_doc: &Document, obj: &Object) -> Option<Self> {
        match obj {
            &Object::Integer(i) => Some(i),
            _ => None
        }
    }
}

impl<'a> FromObj<'a> for &'a Dictionary {
    fn from_obj(doc: &'a Document, obj: &'a Object) -> Option<&'a Dictionary> {
        maybe_deref(doc, obj).as_dict().ok()
    }
}

impl<'a> FromObj<'a> for &'a Stream {
    fn from_obj(doc: &'a Document, obj: &'a Object) -> Option<&'a Stream> {
        maybe_deref(doc, obj).as_stream().ok()
    }
}

impl<'a> FromObj<'a> for &'a Object {
    fn from_obj(doc: &'a Document, obj: &'a Object) -> Option<&'a Object> {
        Some(maybe_deref(doc, obj))
    }
}

fn get<'a, T: FromOptObj<'a>>(doc: &'a Document, dict: &'a Dictionary, key: &[u8]) -> T {
    T::from_opt_obj(doc, dict.get(key).ok(), key)
}

fn maybe_get<'a, T: FromObj<'a>>(doc: &'a Document, dict: &'a Dictionary, key: &[u8]) -> Option<T> {
    maybe_get_obj(doc, dict, key).and_then(|o| T::from_obj(doc, o))
}

fn get_name_string<'a>(doc: &'a Document, dict: &'a Dictionary, key: &[u8]) -> String {
    maybe_get_name_string(doc, dict, key).unwrap_or_else(|| {
        warn!("Missing or invalid name field '{}' in dictionary, using empty string",
              String::from_utf8_lossy(key));
        String::new()
    })
}

fn maybe_get_name_string<'a>(doc: &'a Document, dict: &'a Dictionary, key: &[u8]) -> Option<String> {
    maybe_get_obj(doc, dict, key).and_then(|n| n.as_name().ok()).map(|n| pdf_to_utf8(n))
}

fn maybe_get_name<'a>(doc: &'a Document, dict: &'a Dictionary, key: &[u8]) -> Option<&'a [u8]> {
    maybe_get_obj(doc, dict, key).and_then(|n| n.as_name().ok())
}

fn maybe_get_array<'a>(doc: &'a Document, dict: &'a Dictionary, key: &[u8]) -> Option<&'a Vec<Object>> {
    maybe_get_obj(doc, dict, key).and_then(|n| n.as_array().ok())
}

#[derive(Clone)]
struct PdfSimpleFont<'a> {
    font: &'a Dictionary,
    doc: &'a Document,
    encoding: Option<Vec<u16>>,
    unicode_map: Option<HashMap<u32, String>>,
    widths: HashMap<CharCode, f64>, // should probably just use i32 here
    missing_width: f64,
}

#[derive(Clone)]
struct PdfType3Font<'a> {
    font: &'a Dictionary,
    doc: &'a Document,
    encoding: Option<Vec<u16>>,
    unicode_map: Option<HashMap<CharCode, String>>,
    widths: HashMap<CharCode, f64>, // should probably just use i32 here
}


fn make_font<'a>(doc: &'a Document, font: &'a Dictionary) -> Rc<dyn PdfFont + 'a> {
    let subtype = get_name_string(doc, font, b"Subtype");
    dlog!("MakeFont({})", subtype);
    if subtype == "Type0" {
        Rc::new(PdfCIDFont::new(doc, font))
    } else if subtype == "Type3" {
        Rc::new(PdfType3Font::new(doc, font))
    } else {
        Rc::new(PdfSimpleFont::new(doc, font))
    }
}

fn is_core_font(name: &str) -> bool {
    match name {
        "Courier-Bold" |
        "Courier-BoldOblique" |
        "Courier-Oblique" |
        "Courier" |
        "Helvetica-Bold" |
        "Helvetica-BoldOblique" |
        "Helvetica-Oblique" |
        "Helvetica" |
        "Symbol" |
        "Times-Bold" |
        "Times-BoldItalic" |
        "Times-Italic" |
        "Times-Roman" |
        "ZapfDingbats" => true,
        _ => false,
    }
}

fn encoding_to_unicode_table(name: &[u8]) -> Vec<u16> {
    let encoding = match &name[..] {
        b"MacRomanEncoding" => encodings::MAC_ROMAN_ENCODING,
        b"MacExpertEncoding" => encodings::MAC_EXPERT_ENCODING,
        b"WinAnsiEncoding" => encodings::WIN_ANSI_ENCODING,
        _ => panic!("unexpected encoding {:?}", pdf_to_utf8(name))
    };
    let encoding_table = encoding.iter()
        .map(|x| if let &Some(x) = x { glyphnames::name_to_unicode(x).unwrap() } else { 0 })
        .collect();
    encoding_table
}

/* "Glyphs in the font are selected by single-byte character codes obtained from a string that
    is shown by the text-showing operators. Logically, these codes index into a table of 256
    glyphs; the mapping from codes to glyphs is called the font’s encoding. Each font program
    has a built-in encoding. Under some circumstances, the encoding can be altered by means
    described in Section 5.5.5, “Character Encoding.”
*/
impl<'a> PdfSimpleFont<'a> {
    fn new(doc: &'a Document, font: &'a Dictionary) -> PdfSimpleFont<'a> {
        let base_name = get_name_string(doc, font, b"BaseFont");
        let subtype = get_name_string(doc, font, b"Subtype");
        if base_name.is_empty() {
            warn!("Font dictionary missing BaseFont field, font metrics may be inaccurate");
        }

        let encoding: Option<&Object> = get(doc, font, b"Encoding");
        dlog!("base_name {} {} enc:{:?} {:?}", base_name, subtype, encoding, font);
        let descriptor: Option<&Dictionary> = get(doc, font, b"FontDescriptor");
        let mut type1_encoding = None;
        let mut unicode_map = None;
        if let Some(descriptor) = descriptor {
            dlog!("descriptor {:?}", descriptor);
            if subtype == "Type1" {
                let file = maybe_get_obj(doc, descriptor, b"FontFile");
                match file {
                    Some(&Object::Stream(ref s)) => {
                        let s = get_contents(s);
                        //dlog!("font contents {:?}", pdf_to_utf8(&s));
                        type1_encoding = Some(type1_encoding_parser::get_encoding_map(&s).expect("encoding"));
                    }
                    _ => { dlog!("font file {:?}", file) }
                }
            } else if subtype == "TrueType" {
                let file = maybe_get_obj(doc, descriptor, b"FontFile2");
                match file {
                    Some(&Object::Stream(ref s)) => {
                        let _s = get_contents(s);
                        //File::create(format!("/tmp/{}", base_name)).unwrap().write_all(&s);
                    }
                    _ => { dlog!("font file {:?}", file) }
                }
            }

            let font_file3 = get::<Option<&Object>>(doc, descriptor, b"FontFile3");
            match font_file3 {
                Some(&Object::Stream(ref s)) => {
                    let subtype = get_name_string(doc, &s.dict, b"Subtype");
                    dlog!("font file {}, {:?}", subtype, s);
                    let s = get_contents(s);
                    if subtype == "Type1C" {
                        let table = cff_parser::Table::parse(&s).unwrap();
                        let charset = table.charset.get_table();
                        let encoding = table.encoding.get_table();
                        let mut mapping = HashMap::new();
                        for i in 0..encoding.len().min(charset.len()) {
                            let cid = encoding[i];
                            let sid = charset[i];
                            let name = cff_parser::string_by_id(&table, sid).unwrap();
                            let unicode = glyphnames::name_to_unicode(&name).or_else(|| {
                                zapfglyphnames::zapfdigbats_names_to_unicode(name)
                            });
                            if let Some(unicode) = unicode {
                                let str = String::from_utf16(&[unicode]).unwrap();
                                mapping.insert(cid as u32, str);
                            }
                        }
                        unicode_map = Some(mapping);
                        //
                        //File::create(format!("/tmp/{}", base_name)).unwrap().write_all(&s);
                    }

                    //
                    //File::create(format!("/tmp/{}", base_name)).unwrap().write_all(&s);
                }
                None => {}
                _ => { dlog!("unexpected") }
            }

            let charset = maybe_get_obj(doc, descriptor, b"CharSet");
            let _charset = match charset {
                Some(&Object::String(ref s, _)) => { Some(pdf_to_utf8(&s)) }
                _ => { None }
            };
            //dlog!("charset {:?}", charset);
        }

        let mut unicode_map = match unicode_map {
            Some(mut unicode_map) => {
                unicode_map.extend(get_unicode_map(doc, font).unwrap_or(HashMap::new()));
                Some(unicode_map)
            }
            None => {
                get_unicode_map(doc, font)
            }
        };


        let mut encoding_table = None;
        match encoding {
            Some(&Object::Name(ref encoding_name)) => {
                dlog!("encoding {:?}", pdf_to_utf8(encoding_name));
                encoding_table = Some(encoding_to_unicode_table(encoding_name));
            }
            Some(&Object::Dictionary(ref encoding)) => {
                //dlog!("Encoding {:?}", encoding);
                let mut table = if let Some(base_encoding) = maybe_get_name(doc, encoding, b"BaseEncoding") {
                    dlog!("BaseEncoding {:?}", base_encoding);
                    encoding_to_unicode_table(base_encoding)
                } else {
                    Vec::from(PDFDocEncoding)
                };
                let differences = maybe_get_array(doc, encoding, b"Differences");
                if let Some(differences) = differences {
                    dlog!("Differences");
                    let mut code = 0;
                    for o in differences {
                        let o = maybe_deref(doc, o);
                        match o {
                            &Object::Integer(i) => { code = i; },
                            &Object::Name(ref n) => {
                                let name = pdf_to_utf8(&n);
                                // XXX: names of Type1 fonts can map to arbitrary strings instead of real
                                // unicode names, so we should probably handle this differently
                                let unicode = glyphnames::name_to_unicode(&name);
                                if let Some(unicode) = unicode{
                                    table[code as usize] = unicode;
                                    if let Some(ref mut unicode_map) = unicode_map {
                                        let be = [unicode];
                                        match unicode_map.entry(code as u32) {
                                            // If there's a unicode table entry missing use one based on the name
                                            Entry::Vacant(v) => { v.insert(String::from_utf16(&be).unwrap()); }
                                            Entry::Occupied(e) => {
                                                if e.get() != &String::from_utf16(&be).unwrap() {
                                                    let normal_match  = e.get().nfkc().eq(String::from_utf16(&be).unwrap().nfkc());
                                                    if !normal_match {
                                                        warn!("Unicode mismatch {} {} {:?} {:?} {:?}", normal_match, name, e.get(), String::from_utf16(&be), be);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    match unicode_map {
                                        Some(ref mut unicode_map) if base_name.contains("FontAwesome") => {
                                            // the fontawesome tex package will use glyph names that don't have a corresponding unicode
                                            // code point, so we'll use an empty string instead. See issue #76
                                            match unicode_map.entry(code as u32) {
                                                Entry::Vacant(v) => { v.insert("".to_owned()); }
                                                Entry::Occupied(e) => {
                                                    panic!("unexpected entry in unicode map")
                                                }
                                            }
                                        }
                                        _ => {
                                            warn!("unknown glyph name '{}' for font {}", name, base_name);
                                        }
                                    }
                                }
                                dlog!("{} = {} ({:?})", code, name, unicode);
                                if let Some(ref mut unicode_map) = unicode_map {
                                    // The unicode map might not have the code in it, but the code might
                                    // not be used so we don't want to panic here.
                                    // An example of this is the 'suppress' character in the TeX Latin Modern font.
                                    // This shows up in https://arxiv.org/pdf/2405.01295v1.pdf
                                    dlog!("{} {:?}", code, unicode_map.get(&(code as u32)));
                                }
                                code += 1;
                            }
                            _ => { panic!("wrong type {:?}", o); }
                        }
                    }
                }
                // "Type" is optional
                let name = encoding.get(b"Type").and_then(|x| x.as_name()).and_then(|x| Ok(pdf_to_utf8(x)));
                dlog!("name: {}", name);

                encoding_table = Some(table);
            }
            None => {
                if let Some(type1_encoding) = type1_encoding {
                    let mut table = Vec::from(PDFDocEncoding);
                    dlog!("type1encoding");
                    for (code, name) in type1_encoding {
                        let unicode = glyphnames::name_to_unicode(&pdf_to_utf8(&name));
                        if let Some(unicode) = unicode {
                            table[code as usize] = unicode;
                        } else {
                            dlog!("unknown character {}", pdf_to_utf8(&name));
                        }
                    }
                    encoding_table = Some(table)
                } else if subtype == "TrueType" {
                    encoding_table = Some(encodings::WIN_ANSI_ENCODING.iter()
                        .map(|x| if let &Some(x) = x { glyphnames::name_to_unicode(x).unwrap() } else { 0 })
                        .collect());
                }
            }
            _ => { panic!() }
        }

        let mut width_map = HashMap::new();
        /* "Ordinarily, a font dictionary that refers to one of the standard fonts
            should omit the FirstChar, LastChar, Widths, and FontDescriptor entries.
            However, it is permissible to override a standard font by including these
            entries and embedding the font program in the PDF file."

            Note: some PDFs include a descriptor but still don't include these entries */

        // If we have widths prefer them over the core font widths. Needed for https://dkp.de/wp-content/uploads/parteitage/Sozialismusvorstellungen-der-DKP.pdf
        if let (Some(first_char), Some(last_char), Some(widths)) = (maybe_get::<i64>(doc, font, b"FirstChar"), maybe_get::<i64>(doc, font, b"LastChar"), maybe_get::<Vec<f64>>(doc, font, b"Widths")) {
            // Some PDF's don't have these like fips-197.pdf
            let mut i: i64 = 0;
            dlog!("first_char {:?}, last_char: {:?}, widths: {} {:?}", first_char, last_char, widths.len(), widths);

            for w in widths {
                width_map.insert((first_char + i) as CharCode, w);
                i += 1;
            }
            assert_eq!(first_char + i - 1, last_char);
        } else {
            let name = if is_core_font(&base_name) {
                &base_name
            } else {
                warn!("no widths and not core font {:?}", base_name);

                // This situation is handled differently by different readers
                // but basically we try to substitute the best font that we can.

                // Poppler/Xpdf:
                // this is technically an error -- the Widths entry is required
                // for all but the Base-14 fonts -- but certain PDF generators
                // apparently don't include widths for Arial and TimesNewRoman

                // Pdfium: CFX_FontMapper::FindSubstFont

                // mupdf: pdf_load_substitute_font

                // We can try to do a better job guessing at a font by looking at the flags
                // or the basename but for now we'll just use Helvetica
                "Helvetica"
            };
            for font_metrics in core_fonts::metrics().iter() {
                if font_metrics.0 == base_name {
                    if let Some(ref encoding) = encoding_table {
                        dlog!("has encoding");
                        for w in font_metrics.2 {
                            let c = glyphnames::name_to_unicode(w.2).unwrap();
                            for i in 0..encoding.len() {
                                if encoding[i] == c {
                                    width_map.insert(i as CharCode, w.1 as f64);
                                }
                            }
                        }
                    } else {
                        // Instead of using the encoding from the core font we'll just look up all
                        // of the character names. We should probably verify that this produces the
                        // same result.

                        let mut table = vec![0; 256];
                        for w in font_metrics.2 {
                            dlog!("{} {}", w.0, w.2);
                            // -1 is "not encoded"
                            if w.0 != -1 {
                                table[w.0 as usize] = if base_name == "ZapfDingbats" {
                                    zapfglyphnames::zapfdigbats_names_to_unicode(w.2).unwrap_or_else(|| panic!("bad name {:?}", w))
                                } else {
                                    glyphnames::name_to_unicode(w.2).unwrap()
                                }
                            }
                        }

                        let encoding = &table[..];
                        for w in font_metrics.2 {
                            width_map.insert(w.0 as CharCode, w.1 as f64);
                            // -1 is "not encoded"
                        }
                        encoding_table = Some(encoding.to_vec());
                    }
                    /* "Ordinarily, a font dictionary that refers to one of the standard fonts
                        should omit the FirstChar, LastChar, Widths, and FontDescriptor entries.
                        However, it is permissible to override a standard font by including these
                        entries and embedding the font program in the PDF file."

                        Note: some PDFs include a descriptor but still don't include these entries */
                    // assert!(maybe_get_obj(doc, font, b"FirstChar").is_none());
                    // assert!(maybe_get_obj(doc, font, b"LastChar").is_none());
                    // assert!(maybe_get_obj(doc, font, b"Widths").is_none());
                }
            }
        }

        let missing_width = get::<Option<f64>>(doc, font, b"MissingWidth").unwrap_or(0.);
        PdfSimpleFont {doc, font, widths: width_map, encoding: encoding_table, missing_width, unicode_map}
    }

    #[allow(dead_code)]
    fn get_type(&self) -> String {
        get_name_string(self.doc, self.font, b"Type")
    }
    #[allow(dead_code)]
    fn get_basefont(&self) -> String {
        get_name_string(self.doc, self.font, b"BaseFont")
    }
    #[allow(dead_code)]
    fn get_subtype(&self) -> String {
        get_name_string(self.doc, self.font, b"Subtype")
    }
    #[allow(dead_code)]
    fn get_widths(&self) -> Option<&Vec<Object>> {
        maybe_get_obj(self.doc, self.font, b"Widths").map(|widths| widths.as_array().expect("Widths should be an array"))
    }
    /* For type1: This entry is obsolescent and its use is no longer recommended. (See
     * implementation note 42 in Appendix H.) */
    #[allow(dead_code)]
    fn get_name(&self) -> Option<String> {
        maybe_get_name_string(self.doc, self.font, b"Name")
    }

    #[allow(dead_code)]
    fn get_descriptor(&self) -> Option<PdfFontDescriptor> {
        maybe_get_obj(self.doc, self.font, b"FontDescriptor").and_then(|desc| desc.as_dict().ok()).map(|desc| PdfFontDescriptor{desc: desc, doc: self.doc})
    }
}



impl<'a> PdfType3Font<'a> {
    fn new(doc: &'a Document, font: &'a Dictionary) -> PdfType3Font<'a> {

        let unicode_map = get_unicode_map(doc, font);
        let encoding: Option<&Object> = get(doc, font, b"Encoding");

        let encoding_table;
        match encoding {
            Some(&Object::Name(ref encoding_name)) => {
                dlog!("encoding {:?}", pdf_to_utf8(encoding_name));
                encoding_table = Some(encoding_to_unicode_table(encoding_name));
            }
            Some(&Object::Dictionary(ref encoding)) => {
                //dlog!("Encoding {:?}", encoding);
                let mut table = if let Some(base_encoding) = maybe_get_name(doc, encoding, b"BaseEncoding") {
                    dlog!("BaseEncoding {:?}", base_encoding);
                    encoding_to_unicode_table(base_encoding)
                } else {
                    Vec::from(PDFDocEncoding)
                };
                let differences = maybe_get_array(doc, encoding, b"Differences");
                if let Some(differences) = differences {
                    dlog!("Differences");
                    let mut code = 0;
                    for o in differences {
                        match o {
                            &Object::Integer(i) => { code = i; },
                            &Object::Name(ref n) => {
                                let name = pdf_to_utf8(&n);
                                // XXX: names of Type1 fonts can map to arbitrary strings instead of real
                                // unicode names, so we should probably handle this differently
                                let unicode = glyphnames::name_to_unicode(&name);
                                if let Some(unicode) = unicode{
                                    table[code as usize] = unicode;
                                }
                                dlog!("{} = {} ({:?})", code, name, unicode);
                                if let Some(ref unicode_map) = unicode_map {
                                    dlog!("{} {:?}", code, unicode_map.get(&(code as u32)));
                                }
                                code += 1;
                            }
                            _ => { panic!("wrong type"); }
                        }
                    }
                }
                let name_encoded = encoding.get(b"Type");
                if let Ok(Object::Name(name)) = name_encoded {
                    dlog!("name: {}", pdf_to_utf8(name));
                } else {
                    dlog!("name not found");
                }

                encoding_table = Some(table);
            }
            _ => { panic!() }
        }

        let first_char: i64 = get(doc, font, b"FirstChar");
        let last_char: i64 = get(doc, font, b"LastChar");
        let widths: Vec<f64> = get(doc, font, b"Widths");

        let mut width_map = HashMap::new();

        let mut i = 0;
        dlog!("first_char {:?}, last_char: {:?}, widths: {} {:?}", first_char, last_char, widths.len(), widths);

        for w in widths {
            width_map.insert((first_char + i) as CharCode, w);
            i += 1;
        }
        assert_eq!(first_char + i - 1, last_char);
        PdfType3Font {doc, font, widths: width_map, encoding: encoding_table, unicode_map}
    }
}

type CharCode = u32;

struct PdfFontIter<'a>
{
    i: Iter<'a, u8>,
    font: &'a dyn PdfFont,
}

impl<'a> Iterator for PdfFontIter<'a> {
    type Item = (CharCode, u8);
    fn next(&mut self) -> Option<(CharCode, u8)> {
        self.font.next_char(&mut self.i)
    }
}

trait PdfFont : Debug {
    fn get_width(&self, id: CharCode) -> f64;
    fn next_char(&self, iter: &mut Iter<u8>) -> Option<(CharCode, u8)>;
    fn decode_char(&self, char: CharCode) -> String;

        /*fn char_codes<'a>(&'a self, chars: &'a [u8]) -> PdfFontIter {
            let p = self;
            PdfFontIter{i: chars.iter(), font: p as &PdfFont}
        }*/

}

impl<'a> dyn PdfFont + 'a {
    fn char_codes(&'a self, chars: &'a [u8]) -> PdfFontIter {
        PdfFontIter{i: chars.iter(), font: self}
    }
    fn decode(&self, chars: &[u8]) -> String {
        let strings = self.char_codes(chars).map(|x| self.decode_char(x.0)).collect::<Vec<_>>();
        strings.join("")
    }

}


impl<'a> PdfFont for PdfSimpleFont<'a> {
    fn get_width(&self, id: CharCode) -> f64 {
        let width = self.widths.get(&id);
        if let Some(width) = width {
            return *width;
        } else {
            let mut widths = self.widths.iter().collect::<Vec<_>>();
            widths.sort_by_key(|x| x.0);
            dlog!("missing width for {} len(widths) = {}, {:?} falling back to missing_width {:?}", id, self.widths.len(), widths, self.font);
            return self.missing_width;
        }
    }
    /*fn decode(&self, chars: &[u8]) -> String {
        let encoding = self.encoding.as_ref().map(|x| &x[..]).unwrap_or(&PDFDocEncoding);
        to_utf8(encoding, chars)
    }*/

    fn next_char(&self, iter: &mut Iter<u8>) -> Option<(CharCode, u8)> {
        iter.next().map(|x| (*x as CharCode, 1))
    }
    fn decode_char(&self, char: CharCode) -> String {
        let slice = [char as u8];
        if let Some(ref unicode_map) = self.unicode_map {
            let s = unicode_map.get(&char);
            let s = match s {
                None => {
                    debug!("missing char {:?} in unicode map {:?} for {:?}", char, unicode_map, self.font);
                    // some pdf's like http://arxiv.org/pdf/2312.00064v1 are missing entries in their unicode map but do have
                    // entries in the encoding.
                    let encoding = self.encoding.as_ref().map(|x| &x[..]).expect("missing unicode map and encoding");
                    let s = to_utf8(encoding, &slice);
                    debug!("falling back to encoding {} -> {:?}", char, s);
                    s
                }
                Some(s) => { s.clone() }
            };
            return s
        }
        let encoding = self.encoding.as_ref().map(|x| &x[..]).unwrap_or(&PDFDocEncoding);
        //dlog!("char_code {:?} {:?}", char, self.encoding);
        let s = to_utf8(encoding, &slice);
        s
    }
}



impl<'a> fmt::Debug for PdfSimpleFont<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.font.fmt(f)
    }
}

impl<'a> PdfFont for PdfType3Font<'a> {
    fn get_width(&self, id: CharCode) -> f64 {
        let width = self.widths.get(&id);
        if let Some(width) = width {
            return *width;
        } else {
            panic!("missing width for {} {:?}", id, self.font);
        }
    }
    /*fn decode(&self, chars: &[u8]) -> String {
        let encoding = self.encoding.as_ref().map(|x| &x[..]).unwrap_or(&PDFDocEncoding);
        to_utf8(encoding, chars)
    }*/

    fn next_char(&self, iter: &mut Iter<u8>) -> Option<(CharCode, u8)> {
        iter.next().map(|x| (*x as CharCode, 1))
    }
    fn decode_char(&self, char: CharCode) -> String {
        let slice = [char as u8];
        if let Some(ref unicode_map) = self.unicode_map {
            let s = unicode_map.get(&char);
            let s = match s {
                None => {
                    debug!("missing char {:?} in unicode map {:?} for {:?}", char, unicode_map, self.font);
                    // some pdf's like http://arxiv.org/pdf/2312.00577v1 are missing entries in their unicode map but do have
                    // entries in the encoding.
                    let encoding = self.encoding.as_ref().map(|x| &x[..]).expect("missing unicode map and encoding");
                    let s = to_utf8(encoding, &slice);
                    debug!("falling back to encoding {} -> {:?}", char, s);
                    s
                }
                Some(s) => { s.clone() }
            };
            return s
        }
        let encoding = self.encoding.as_ref().map(|x| &x[..]).unwrap_or(&PDFDocEncoding);
        //dlog!("char_code {:?} {:?}", char, self.encoding);
        let s = to_utf8(encoding, &slice);
        s
    }
}



impl<'a> fmt::Debug for PdfType3Font<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.font.fmt(f)
    }
}

struct PdfCIDFont<'a> {
    font: &'a Dictionary,
    #[allow(dead_code)]
    doc: &'a Document,
    #[allow(dead_code)]
    encoding: ByteMapping,
    to_unicode: Option<HashMap<u32, String>>,
    widths: HashMap<CharCode, f64>, // should probably just use i32 here
    default_width: Option<f64>, // only used for CID fonts and we should probably brake out the different font types
}

fn get_unicode_map<'a>(doc: &'a Document, font: &'a Dictionary) -> Option<HashMap<u32, String>> {
    let to_unicode = maybe_get_obj(doc, font, b"ToUnicode");
    dlog!("ToUnicode: {:?}", to_unicode);
    let mut unicode_map = None;
    match to_unicode {
        Some(&Object::Stream(ref stream)) => {
            let contents = get_contents(stream);
            dlog!("Stream: {}", String::from_utf8(contents.clone()).unwrap_or_else(|_| "<binary>".to_string()));

            let cmap = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                adobe_cmap_parser::get_unicode_map(&contents)
            })) {
                Ok(Ok(cmap)) => cmap,
                Ok(Err(e)) => {
                    warn!("Failed to parse ToUnicode CMap: {:?}. Returning empty unicode map.", e);
                    return None;
                }
                Err(panic_err) => {
                    warn!("CMap parser panicked while parsing ToUnicode: {:?}. Returning empty unicode map.", panic_err);
                    return None;
                }
            };
            let mut unicode = HashMap::new();
            // "It must use the beginbfchar, endbfchar, beginbfrange, and endbfrange operators to
            // define the mapping from character codes to Unicode character sequences expressed in
            // UTF-16BE encoding."
            for (&k, v) in cmap.iter() {
                let mut be: Vec<u16> = Vec::new();
                let mut i = 0;
                if v.len() % 2 != 0 {
                    warn!("Invalid UTF-16BE data for char code {}: odd number of bytes ({}), skipping", k, v.len());
                    continue;
                }
                while i < v.len() {
                    be.push(((v[i] as u16) << 8) | v[i+1] as u16);
                    i += 2;
                }
                match &be[..] {
                    [0xd800 ..= 0xdfff] => {
                        // this range is not specified as not being encoded
                        // we ignore them so we don't an error from from_utt16
                        continue;
                    }
                    _ => {}
                }
                let s = match String::from_utf16(&be) {
                    Ok(s) => s,
                    Err(_) => {
                        debug!("Invalid UTF-16 sequence for char code {}, skipping", k);
                        continue;
                    }
                };

                unicode.insert(k, s);
            }
            unicode_map = Some(unicode);

            dlog!("map: {:?}", unicode_map);
        }
        None => { }
        Some(&Object::Name(ref name)) => {
            let name = pdf_to_utf8(name);
            if name != "Identity-H" {
                todo!("unsupported ToUnicode name: {:?}", name);
            }
        }
        _ => {
            warn!("Unsupported ToUnicode object type: {:?}, ignoring", to_unicode);
        }
    }
    unicode_map
}


impl<'a> PdfCIDFont<'a> {
    fn new(doc: &'a Document, font: &'a Dictionary) -> PdfCIDFont<'a> {
        let base_name = get_name_string(doc, font, b"BaseFont");
        let descendants = maybe_get_array(doc, font, b"DescendantFonts").expect("Descendant fonts required");
        let ciddict = maybe_deref(doc, &descendants[0]).as_dict().expect("should be CID dict");
        let encoding = maybe_get_obj(doc, font, b"Encoding").expect("Encoding required in type0 fonts");
        dlog!("base_name {} {:?}", base_name, font);

        let encoding = match encoding {
            &Object::Name(ref name) => {
                let name = pdf_to_utf8(name);
                dlog!("encoding {:?}", name);
                if name == "Identity-H" || name == "Identity-V" {
                    ByteMapping { codespace: vec![CodeRange{width: 2, start: 0, end: 0xffff }], cid: vec![CIDRange{ src_code_lo: 0, src_code_hi: 0xffff, dst_CID_lo: 0 }]}
                } else {
                    warn!("Unsupported encoding name: {}, falling back to Identity-H", name);
                    ByteMapping { codespace: vec![CodeRange{width: 2, start: 0, end: 0xffff }], cid: vec![CIDRange{ src_code_lo: 0, src_code_hi: 0xffff, dst_CID_lo: 0 }]}
                }
            }
            &Object::Stream(ref stream) => {
                let contents = get_contents(stream);
                dlog!("Stream: {}", String::from_utf8(contents.clone()).unwrap_or_else(|_| "<binary>".to_string()));
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    adobe_cmap_parser::get_byte_mapping(&contents)
                })) {
                    Ok(Ok(mapping)) => mapping,
                    Ok(Err(e)) => {
                        warn!("Failed to parse CID encoding CMap: {:?}. Falling back to Identity-H", e);
                        ByteMapping {
                            codespace: vec![CodeRange{width: 2, start: 0, end: 0xffff }],
                            cid: vec![CIDRange{ src_code_lo: 0, src_code_hi: 0xffff, dst_CID_lo: 0 }]
                        }
                    }
                    Err(panic_err) => {
                        warn!("CMap parser panicked while parsing CID encoding: {:?}. Falling back to Identity-H", panic_err);
                        ByteMapping {
                            codespace: vec![CodeRange{width: 2, start: 0, end: 0xffff }],
                            cid: vec![CIDRange{ src_code_lo: 0, src_code_hi: 0xffff, dst_CID_lo: 0 }]
                        }
                    }
                }
            }
            _ => {
                warn!("Unsupported encoding type: {:?}, falling back to Identity-H", encoding);
                ByteMapping {
                    codespace: vec![CodeRange{width: 2, start: 0, end: 0xffff }],
                    cid: vec![CIDRange{ src_code_lo: 0, src_code_hi: 0xffff, dst_CID_lo: 0 }]
                }
            }
        };

        // Sometimes a Type0 font might refer to the same underlying data as regular font. In this case we may be able to extract some encoding
        // data.
        // We should also look inside the truetype data to see if there's a cmap table. It will help us convert as well.
        // This won't work if the cmap has been subsetted. A better approach might be to hash glyph contents and use that against
        // a global library of glyph hashes
        let unicode_map = get_unicode_map(doc, font);

        dlog!("descendents {:?} {:?}", descendants, ciddict);

        let font_dict = maybe_get_obj(doc, ciddict, b"FontDescriptor").expect("required");
        dlog!("{:?}", font_dict);
        let _f = font_dict.as_dict().expect("must be dict");
        let default_width = get::<Option<i64>>(doc, ciddict, b"DW").unwrap_or(1000);
        let w: Option<Vec<&Object>> = get(doc, ciddict, b"W");
        dlog!("widths {:?}", w);
        let mut widths = HashMap::new();
        let mut i = 0;
        if let Some(w) = w {
            while i < w.len() {
                if let &Object::Array(ref wa) = w[i+1] {
                    let cid = w[i].as_i64().expect("id should be num");
                    let mut j = 0;
                    dlog!("wa: {:?} -> {:?}", cid, wa);
                    for w in wa {
                        widths.insert((cid + j) as CharCode, as_num(w) );
                        j += 1;
                    }
                    i += 2;
                } else {
                    let c_first = w[i].as_i64().expect("first should be num");
                    let c_last = w[i].as_i64().expect("last should be num");
                    let c_width = as_num(&w[i]);
                    for id in c_first..c_last {
                        widths.insert(id as CharCode, c_width);
                    }
                    i += 3;
                }
            }
        }
        PdfCIDFont{doc, font, widths, to_unicode: unicode_map, encoding, default_width: Some(default_width as f64) }
    }
}

impl<'a> PdfFont for PdfCIDFont<'a> {
    fn get_width(&self, id: CharCode) -> f64 {
        let width = self.widths.get(&id);
        if let Some(width) = width {
            dlog!("GetWidth {} -> {}", id, *width);
            return *width;
        } else {
            dlog!("missing width for {} falling back to default_width", id);
            return self.default_width.unwrap();
        }
    }/*
    fn decode(&self, chars: &[u8]) -> String {
        self.char_codes(chars);

        //let utf16 = Vec::new();

        let encoding = self.encoding.as_ref().map(|x| &x[..]).unwrap_or(&PDFDocEncoding);
        to_utf8(encoding, chars)
    }*/

    fn next_char(&self, iter: &mut Iter<u8>) -> Option<(CharCode, u8)> {
        let mut c = *iter.next()? as u32;
        let mut code = None;
        'outer: for width in 1..=4 {
            for range in &self.encoding.codespace {
                if c as u32 >= range.start && c as u32 <= range.end && range.width == width {
                    code = Some((c as u32, width));
                    break 'outer;
                }
            }
            let next = *iter.next()?;
            c = ((c as u32) << 8) | next as u32;
        }
        let code = code?;
        for range in &self.encoding.cid {
            if code.0 >= range.src_code_lo && code.0 <= range.src_code_hi {
                return Some((code.0 + range.dst_CID_lo, code.1 as u8));
            }
        }
        None
    }
    fn decode_char(&self, char: CharCode) -> String {
        let s = self.to_unicode.as_ref().and_then(|x| x.get(&char));
        if let Some(s) = s {
            s.clone()
        } else {
            dlog!("Unknown character {:?} in {:?} {:?}", char, self.font, self.to_unicode);
            "".to_string()
        }
    }
}

impl<'a> fmt::Debug for PdfCIDFont<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.font.fmt(f)
    }
}



#[derive(Copy, Clone)]
struct PdfFontDescriptor<'a> {
    desc: &'a Dictionary,
    doc: &'a Document
}

impl<'a> PdfFontDescriptor<'a> {
    #[allow(dead_code)]
    fn get_file(&self) -> Option<&'a Object> {
        maybe_get_obj(self.doc, self.desc, b"FontFile")
    }
}

impl<'a> fmt::Debug for PdfFontDescriptor<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.desc.fmt(f)
    }
}

#[derive(Clone, Debug)]
struct Type0Func {
    domain: Vec<f64>,
    range: Vec<f64>,
    contents: Vec<u8>,
    size: Vec<i64>,
    bits_per_sample: i64,
    encode: Vec<f64>,
    decode: Vec<f64>,
}

#[allow(dead_code)]
fn interpolate(x: f64, x_min: f64, _x_max: f64, y_min: f64, y_max: f64) -> f64 {
    let divisor = x - x_min;
    if divisor != 0. {
        y_min + (x - x_min) * ((y_max - y_min) / divisor)
    } else {
        // (x - x_min) will be 0 which means we want to discard the interpolation
        // and arbitrarily choose y_min to match pdfium
        y_min
    }
}

impl Type0Func {
    #[allow(dead_code)]
    fn eval(&self, _input: &[f64], _output: &mut [f64]) {
        let _n_inputs = self.domain.len() / 2;
        let _n_ouputs = self.range.len() / 2;

    }
}

#[derive(Clone, Debug)]
struct Type2Func {
    c0: Option<Vec<f64>>,
    c1: Option<Vec<f64>>,
    n: f64,
}

#[derive(Clone, Debug)]
enum Function {
    Type0(Type0Func),
    Type2(Type2Func),
    #[allow(dead_code)]
    Type3,
    #[allow(dead_code)]
    Type4(Vec<u8>)
}

impl Function {
    fn new(doc: &Document, obj: &Object) -> Function {
        let dict = match obj {
            &Object::Dictionary(ref dict) => dict,
            &Object::Stream(ref stream) => &stream.dict,
            _ => panic!()
        };
        let function_type: i64 = get(doc, dict, b"FunctionType");
        let f = match function_type {
            0 => {
                // Sampled function
                let stream = match obj {
                    &Object::Stream(ref stream) => stream,
                    _ => panic!()
                };
                let range: Vec<f64> = get(doc, dict, b"Range");
                let domain: Vec<f64> = get(doc, dict, b"Domain");
                let contents = get_contents(stream);
                let size: Vec<i64> = get(doc, dict, b"Size");
                let bits_per_sample = get(doc, dict, b"BitsPerSample");
                // We ignore 'Order' like pdfium, poppler and pdf.js

                let encode = get::<Option<Vec<f64>>>(doc, dict, b"Encode");
                // maybe there's some better way to write this.
                let encode = encode.unwrap_or_else(|| {
                    let mut default = Vec::new();
                    for i in &size {
                        default.extend([0., (i - 1) as f64].iter());
                    }
                    default
                });
                let decode = get::<Option<Vec<f64>>>(doc, dict, b"Decode").unwrap_or_else(|| range.clone());

                Function::Type0(Type0Func { domain, range, size, contents, bits_per_sample, encode, decode })
            }
            2 => {
                // Exponential interpolation function
                let c0 = get::<Option<Vec<f64>>>(doc, dict, b"C0");
                let c1 = get::<Option<Vec<f64>>>(doc, dict, b"C1");
                let n = get::<f64>(doc, dict, b"N");
                Function::Type2(Type2Func { c0, c1, n})
            }
            3 => {
                // Stitching function
                Function::Type3
            }
            4 => {
                // PostScript calculator function
                let contents = match obj {
                    &Object::Stream(ref stream) => {
                        let contents = get_contents(stream);
                        warn!("unhandled type-4 function");
                        warn!("Stream: {}", String::from_utf8(contents.clone()).unwrap());
                        contents
                    }
                    _ => { panic!("type 4 functions should be streams") }
                };
                Function::Type4(contents)
            }
            _ => { panic!("unhandled function type {}", function_type) }
        };
        f
    }
}

fn as_num(o: &Object) -> f64 {
    match o {
        &Object::Integer(i) => { i as f64 }
        &Object::Real(f) => { f.into() }
        _ => { panic!("not a number") }
    }
}

#[derive(Clone)]
struct TextState<'a>
{
    font: Option<Rc<dyn PdfFont + 'a>>,
    font_size: f64,
    character_spacing: f64,
    word_spacing: f64,
    horizontal_scaling: f64,
    leading: f64,
    rise: f64,
    tm: Transform,
}

// XXX: We'd ideally implement this without having to copy the uncompressed data
fn get_contents(contents: &Stream) -> Vec<u8> {
    if contents.filters().is_ok() {
        contents.decompressed_content().unwrap_or_else(|_|contents.content.clone())
    } else {
        contents.content.clone()
    }
}

#[derive(Clone)]
struct GraphicsState<'a>
{
    ctm: Transform,
    ts: TextState<'a>,
    smask: Option<Dictionary>,
    fill_colorspace: ColorSpace,
    fill_color: Vec<f64>,
    stroke_colorspace: ColorSpace,
    stroke_color: Vec<f64>,
    line_width: f64,
}

fn show_text(gs: &mut GraphicsState, s: &[u8],
             _tlm: &Transform,
             _flip_ctm: &Transform,
             output: &mut dyn OutputDev) -> Result<(), OutputError> {
    let ts = &mut gs.ts;
    let font = ts.font.as_ref().unwrap();
    //let encoding = font.encoding.as_ref().map(|x| &x[..]).unwrap_or(&PDFDocEncoding);
    dlog!("{:?}", font.decode(s));
    dlog!("{:?}", font.decode(s).as_bytes());
    dlog!("{:?}", s);
    output.begin_word()?;

    for (c, length) in font.char_codes(s) {
        // 5.3.3 Text Space Details
        let tsm = Transform2D::row_major(ts.horizontal_scaling,
                                                 0.,
                                                 0.,
                                                 1.0,
                                                 0.,
                                                 ts.rise);
        // Trm = Tsm × Tm × CTM
        let trm = tsm.post_transform(&ts.tm.post_transform(&gs.ctm));
        //dlog!("ctm: {:?} tm {:?}", gs.ctm, tm);
        //dlog!("current pos: {:?}", position);
        // 5.9 Extraction of Text Content


        //dlog!("w: {}", font.widths[&(*c as i64)]);
        let w0 = font.get_width(c) / 1000.;

        let mut spacing = ts.character_spacing;
        // "Word spacing is applied to every occurrence of the single-byte character code 32 in a
        //  string when using a simple font or a composite font that defines code 32 as a
        //  single-byte code. It does not apply to occurrences of the byte value 32 in
        //  multiple-byte codes."
        let is_space = c == 32 && length == 1;
        if is_space { spacing += ts.word_spacing }

        output.output_character(&trm, w0, spacing, ts.font_size, &font.decode_char(c))?;
        let tj = 0.;
        let ty = 0.;
        let tx = ts.horizontal_scaling * ((w0 - tj/1000.)* ts.font_size + spacing);
        dlog!("horizontal {} adjust {} {} {} {}", ts.horizontal_scaling, tx, w0, ts.font_size, spacing);
        // dlog!("w0: {}, tx: {}", w0, tx);
        ts.tm = ts.tm.pre_transform(&Transform2D::create_translation(tx, ty));
        let _trm = ts.tm.pre_transform(&gs.ctm);
        //dlog!("post pos: {:?}", trm);

    }
    output.end_word()?;
    Ok(())
}

#[derive(Debug, Clone, Copy)]
pub struct MediaBox {
    pub llx: f64,
    pub lly: f64,
    pub urx: f64,
    pub ury: f64
}

fn apply_state(doc: &Document, gs: &mut GraphicsState, state: &Dictionary) {
    for (k, v) in state.iter() {
        let k : &[u8] = k.as_ref();
        match k {
            b"SMask" => { match maybe_deref(doc, v)  {
                &Object::Name(ref name) => {
                    if name == b"None" {
                        gs.smask = None;
                    } else {
                        panic!("unexpected smask name")
                    }
                }
                &Object::Dictionary(ref dict) => {
                    gs.smask = Some(dict.clone());
                }
                _ => { panic!("unexpected smask type {:?}", v) }
            }}
            b"Type" => { match v {
                &Object::Name(ref name) => {
                    assert_eq!(name, b"ExtGState")
                }
                _ => { panic!("unexpected type") }
            }}
            _ => {  dlog!("unapplied state: {:?} {:?}", k, v); }
        }
    }

}

#[derive(Debug)]
pub enum PathOp {
    MoveTo(f64, f64),
    LineTo(f64, f64),
    // XXX: is it worth distinguishing the different kinds of curve ops?
    CurveTo(f64, f64, f64, f64, f64, f64),
    Rect(f64, f64, f64, f64),
    Close,
}

#[derive(Debug)]
pub struct Path {
    pub ops: Vec<PathOp>
}

impl Path {
    fn new() -> Path {
        Path { ops: Vec::new() }
    }
    fn current_point(&self) -> (f64, f64) {
        match self.ops.last().unwrap() {
            &PathOp::MoveTo(x, y) => { (x, y) }
            &PathOp::LineTo(x, y) => { (x, y) }
            &PathOp::CurveTo(_, _, _, _, x, y) => { (x, y) }
            _ => { panic!() }
        }
    }
}

#[derive(Clone, Debug)]
pub struct CalGray {
    white_point: [f64; 3],
    black_point: Option<[f64; 3]>,
    gamma: Option<f64>,
}

#[derive(Clone, Debug)]
pub struct CalRGB {
    white_point: [f64; 3],
    black_point: Option<[f64; 3]>,
    gamma: Option<[f64; 3]>,
    matrix: Option<Vec<f64>>
}

#[derive(Clone, Debug)]
pub struct Lab {
    white_point: [f64; 3],
    black_point: Option<[f64; 3]>,
    range: Option<[f64; 4]>,
}

#[derive(Clone, Debug)]
pub enum AlternateColorSpace {
    DeviceGray,
    DeviceRGB,
    DeviceCMYK,
    CalRGB(CalRGB),
    CalGray(CalGray),
    Lab(Lab),
    ICCBased(Vec<u8>)
}

#[derive(Clone)]
pub struct Separation {
    name: String,
    alternate_space: AlternateColorSpace,
    tint_transform: Box<Function>,
}

#[derive(Clone)]
pub enum ColorSpace {
    DeviceGray,
    DeviceRGB,
    DeviceCMYK,
    DeviceN,
    Pattern,
    CalRGB(CalRGB),
    CalGray(CalGray),
    Lab(Lab),
    Separation(Separation),
    ICCBased(Vec<u8>)
}

fn make_colorspace<'a>(doc: &'a Document, name: &[u8], resources: &'a Dictionary) -> ColorSpace {
    match name {
        b"DeviceGray" => ColorSpace::DeviceGray,
        b"DeviceRGB" => ColorSpace::DeviceRGB,
        b"DeviceCMYK" => ColorSpace::DeviceCMYK,
        b"Pattern" => ColorSpace::Pattern,
        _ => {
            let colorspaces: &Dictionary = get(&doc, resources, b"ColorSpace");
            let cs: &Object = maybe_get_obj(doc, colorspaces, &name[..]).unwrap_or_else(|| panic!("missing colorspace {:?}", &name[..]));
            if let Ok(cs) = cs.as_array() {
                let cs_name = pdf_to_utf8(cs[0].as_name().expect("first arg must be a name"));
                match cs_name.as_ref() {
                    "Separation" => {
                        let name = pdf_to_utf8(cs[1].as_name().expect("second arg must be a name"));
                        let alternate_space = match &maybe_deref(doc, &cs[2]) {
                            Object::Name(name) => {
                                match &name[..] {
                                    b"DeviceGray" => AlternateColorSpace::DeviceGray,
                                    b"DeviceRGB" => AlternateColorSpace::DeviceRGB,
                                    b"DeviceCMYK" => AlternateColorSpace::DeviceCMYK,
                                    _ => panic!("unexpected color space name")
                                }
                            }
                            Object::Array(cs) => {
                                let cs_name = pdf_to_utf8(cs[0].as_name().expect("first arg must be a name"));
                                match cs_name.as_ref() {
                                    "ICCBased" => {
                                        let stream = maybe_deref(doc, &cs[1]).as_stream().unwrap();
                                        dlog!("ICCBased {:?}", stream);
                                        // XXX: we're going to be continually decompressing everytime this object is referenced
                                        AlternateColorSpace::ICCBased(get_contents(stream))
                                    }
                                    "CalGray" => {
                                        let dict = cs[1].as_dict().expect("second arg must be a dict");
                                        AlternateColorSpace::CalGray(CalGray {
                                            white_point: get(&doc, dict, b"WhitePoint"),
                                            black_point: get(&doc, dict, b"BackPoint"),
                                            gamma: get(&doc, dict, b"Gamma"),
                                        })
                                    }
                                    "CalRGB" => {
                                        let dict = cs[1].as_dict().expect("second arg must be a dict");
                                        AlternateColorSpace::CalRGB(CalRGB {
                                            white_point: get(&doc, dict, b"WhitePoint"),
                                            black_point: get(&doc, dict, b"BackPoint"),
                                            gamma: get(&doc, dict, b"Gamma"),
                                            matrix: get(&doc, dict, b"Matrix"),
                                        })
                                    }
                                    "Lab" => {
                                        let dict = cs[1].as_dict().expect("second arg must be a dict");
                                        AlternateColorSpace::Lab(Lab {
                                            white_point: get(&doc, dict, b"WhitePoint"),
                                            black_point: get(&doc, dict, b"BackPoint"),
                                            range: get(&doc, dict, b"Range"),
                                        })
                                    }
                                    _ => panic!("Unexpected color space name")
                                }
                            }
                            _ => panic!("Alternate space should be name or array {:?}", cs[2])
                        };
                        let tint_transform = Box::new(Function::new(doc, maybe_deref(doc, &cs[3])));

                        dlog!("{:?} {:?} {:?}", name, alternate_space, tint_transform);
                        ColorSpace::Separation(Separation{ name, alternate_space, tint_transform})
                    }
                    "ICCBased" => {
                        let stream = maybe_deref(doc, &cs[1]).as_stream().unwrap();
                        dlog!("ICCBased {:?}", stream);
                        // XXX: we're going to be continually decompressing everytime this object is referenced
                        ColorSpace::ICCBased(get_contents(stream))
                    }
                    "CalGray" => {
                        let dict = cs[1].as_dict().expect("second arg must be a dict");
                        ColorSpace::CalGray(CalGray {
                            white_point: get(&doc, dict, b"WhitePoint"),
                            black_point: get(&doc, dict, b"BackPoint"),
                            gamma: get(&doc, dict, b"Gamma"),
                        })
                    }
                    "CalRGB" => {
                        let dict = cs[1].as_dict().expect("second arg must be a dict");
                        ColorSpace::CalRGB(CalRGB {
                            white_point: get(&doc, dict, b"WhitePoint"),
                            black_point: get(&doc, dict, b"BackPoint"),
                            gamma: get(&doc, dict, b"Gamma"),
                            matrix: get(&doc, dict, b"Matrix"),
                        })
                    }
                    "Lab" => {
                        let dict = cs[1].as_dict().expect("second arg must be a dict");
                        ColorSpace::Lab(Lab {
                            white_point: get(&doc, dict, b"WhitePoint"),
                            black_point: get(&doc, dict, b"BackPoint"),
                            range: get(&doc, dict, b"Range"),
                        })
                    }
                    "Pattern" => {
                        ColorSpace::Pattern
                    },
                    "DeviceGray" => ColorSpace::DeviceGray,
                    "DeviceRGB" => ColorSpace::DeviceRGB,
                    "DeviceCMYK" => ColorSpace::DeviceCMYK,
                    "DeviceN" => ColorSpace::DeviceN,
                    _ => {
                        panic!("color_space {:?} {:?} {:?}", name, cs_name, cs)
                    }
                }
            } else if let Ok(cs) = cs.as_name() {
                match pdf_to_utf8(cs).as_ref() {
                    "DeviceRGB" => ColorSpace::DeviceRGB,
                    "DeviceGray" => ColorSpace::DeviceGray,
                    _ => panic!()
                }
            } else {
                panic!();
            }
        }
    }
}

struct Processor<'a> {
    _none: PhantomData<&'a ()>
}

impl<'a> Processor<'a> {
    fn new() -> Processor<'a> {
        Processor { _none: PhantomData }
    }

    fn process_stream(&mut self, doc: &'a Document, content: Vec<u8>, resources: &'a Dictionary, media_box: &MediaBox, output: &mut dyn OutputDev, page_num: u32) -> Result<(), OutputError> {
        let content = Content::decode(&content).unwrap();
        let mut font_table = HashMap::new();
        let mut gs: GraphicsState = GraphicsState {
            ts: TextState {
                font: None,
                font_size: std::f64::NAN,
                character_spacing: 0.,
                word_spacing: 0.,
                horizontal_scaling: 100. / 100.,
                leading: 0.,
                rise: 0.,
                tm: Transform2D::identity(),
            },
            fill_color: Vec::new(),
            fill_colorspace: ColorSpace::DeviceGray,
            stroke_color: Vec::new(),
            stroke_colorspace: ColorSpace::DeviceGray,
            line_width: 1.,
            ctm: Transform2D::identity(),
            smask: None
        };
        //let mut ts = &mut gs.ts;
        let mut gs_stack = Vec::new();
        let mut mc_stack = Vec::new();
        // XXX: replace tlm with a point for text start
        let mut tlm = Transform2D::identity();
        let mut path = Path::new();
        let flip_ctm = Transform2D::row_major(1., 0., 0., -1., 0., media_box.ury - media_box.lly);
        dlog!("MediaBox {:?}", media_box);
        for operation in &content.operations {
            //dlog!("op: {:?}", operation);

            match operation.operator.as_ref() {
                "BT" => {
                    tlm = Transform2D::identity();
                    gs.ts.tm = tlm;
                }
                "ET" => {
                    tlm = Transform2D::identity();
                    gs.ts.tm = tlm;
                }
                "cm" => {
                    assert!(operation.operands.len() == 6);
                    let m = Transform2D::row_major(as_num(&operation.operands[0]),
                                                   as_num(&operation.operands[1]),
                                                   as_num(&operation.operands[2]),
                                                   as_num(&operation.operands[3]),
                                                   as_num(&operation.operands[4]),
                                                   as_num(&operation.operands[5]));
                    gs.ctm = gs.ctm.pre_transform(&m);
                    dlog!("matrix {:?}", gs.ctm);
                }
                "CS" => {
                    let name = operation.operands[0].as_name().unwrap();
                    gs.stroke_colorspace = make_colorspace(doc, name, resources);
                }
                "cs" => {
                    let name = operation.operands[0].as_name().unwrap();
                    gs.fill_colorspace = make_colorspace(doc, name, resources);
                }
                "SC" | "SCN" => {
                    gs.stroke_color = match gs.stroke_colorspace {
                        ColorSpace::Pattern => { dlog!("unhandled pattern color"); Vec::new() }
                        _ => { operation.operands.iter().map(|x| as_num(x)).collect() }
                    };
                }
                "sc" | "scn" => {
                    gs.fill_color = match gs.fill_colorspace {
                        ColorSpace::Pattern => { dlog!("unhandled pattern color"); Vec::new() }
                        _ => { operation.operands.iter().map(|x| as_num(x)).collect() }
                    };
                }
                "G" | "g" | "RG" | "rg" | "K" | "k" => {
                    dlog!("unhandled color operation {:?}", operation);
                }
                "TJ" => {
                    match operation.operands[0] {
                        Object::Array(ref array) => {
                            for e in array {
                                match e {
                                    &Object::String(ref s, _) => {
                                        show_text(&mut gs, s, &tlm, &flip_ctm, output)?;
                                    }
                                    &Object::Integer(i) => {
                                        let ts = &mut gs.ts;
                                        let w0 = 0.;
                                        let tj = i as f64;
                                        let ty = 0.;
                                        let tx = ts.horizontal_scaling * ((w0 - tj / 1000.) * ts.font_size);
                                        ts.tm = ts.tm.pre_transform(&Transform2D::create_translation(tx, ty));
                                        dlog!("adjust text by: {} {:?}", i, ts.tm);
                                    }
                                    &Object::Real(i) => {
                                        let ts = &mut gs.ts;
                                        let w0 = 0.;
                                        let tj = i as f64;
                                        let ty = 0.;
                                        let tx = ts.horizontal_scaling * ((w0 - tj / 1000.) * ts.font_size);
                                        ts.tm = ts.tm.pre_transform(&Transform2D::create_translation(tx, ty));
                                        dlog!("adjust text by: {} {:?}", i, ts.tm);
                                    }
                                    _ => { dlog!("kind of {:?}", e); }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                "Tj" => {
                    match operation.operands[0] {
                        Object::String(ref s, _) => {
                            show_text(&mut gs, s, &tlm, &flip_ctm, output)?;
                        }
                        _ => { panic!("unexpected Tj operand {:?}", operation) }
                    }
                }
                "Tc" => {
                    gs.ts.character_spacing = as_num(&operation.operands[0]);
                }
                "Tw" => {
                    gs.ts.word_spacing = as_num(&operation.operands[0]);
                }
                "Tz" => {
                    gs.ts.horizontal_scaling = as_num(&operation.operands[0]) / 100.;
                }
                "TL" => {
                    gs.ts.leading = as_num(&operation.operands[0]);
                }
                "Tf" => {
                    let fonts: &Dictionary = get(&doc, resources, b"Font");
                    let name = operation.operands[0].as_name().unwrap();
                    let font_dict: &Dictionary = get(doc, fonts, name);
                    let font = font_table.entry(name.to_owned()).or_insert_with(|| make_font(doc, font_dict)).clone();
                    {
                        /*let file = font.get_descriptor().and_then(|desc| desc.get_file());
                    if let Some(file) = file {
                        let file_contents = filter_data(file.as_stream().unwrap());
                        let mut cursor = Cursor::new(&file_contents[..]);
                        //let f = Font::read(&mut cursor);
                        //dlog!("font file: {:?}", f);
                    }*/
                    }
                    gs.ts.font = Some(font);

                    gs.ts.font_size = as_num(&operation.operands[1]);
                    dlog!("font {} size: {} {:?}", pdf_to_utf8(name), gs.ts.font_size, operation);

                    // Notify output device of font change (for MarkdownOutput)
                    let base_name = get_name_string(doc, font_dict, b"BaseFont");
                    output.set_font(&base_name)?;
                }
                "Ts" => {
                    gs.ts.rise = as_num(&operation.operands[0]);
                }
                "Tm" => {
                    assert!(operation.operands.len() == 6);
                    tlm = Transform2D::row_major(as_num(&operation.operands[0]),
                                                 as_num(&operation.operands[1]),
                                                 as_num(&operation.operands[2]),
                                                 as_num(&operation.operands[3]),
                                                 as_num(&operation.operands[4]),
                                                 as_num(&operation.operands[5]));
                    gs.ts.tm = tlm;
                    dlog!("Tm: matrix {:?}", gs.ts.tm);
                    output.end_line()?;
                }
                "Td" => {
                    /* Move to the start of the next line, offset from the start of the current line by (tx , ty ).
                   tx and ty are numbers expressed in unscaled text space units.
                   More precisely, this operator performs the following assignments:
                 */
                    assert!(operation.operands.len() == 2);
                    let tx = as_num(&operation.operands[0]);
                    let ty = as_num(&operation.operands[1]);
                    dlog!("translation: {} {}", tx, ty);

                    tlm = tlm.pre_transform(&Transform2D::create_translation(tx, ty));
                    gs.ts.tm = tlm;
                    dlog!("Td matrix {:?}", gs.ts.tm);
                    output.end_line()?;
                }

                "TD" => {
                    /* Move to the start of the next line, offset from the start of the current line by (tx , ty ).
                   As a side effect, this operator sets the leading parameter in the text state.
                 */
                    assert!(operation.operands.len() == 2);
                    let tx = as_num(&operation.operands[0]);
                    let ty = as_num(&operation.operands[1]);
                    dlog!("translation: {} {}", tx, ty);
                    gs.ts.leading = -ty;

                    tlm = tlm.pre_transform(&Transform2D::create_translation(tx, ty));
                    gs.ts.tm = tlm;
                    dlog!("TD matrix {:?}", gs.ts.tm);
                    output.end_line()?;
                }

                "T*" => {
                    let tx = 0.0;
                    let ty = -gs.ts.leading;

                    tlm = tlm.pre_transform(&Transform2D::create_translation(tx, ty));
                    gs.ts.tm = tlm;
                    dlog!("T* matrix {:?}", gs.ts.tm);
                    output.end_line()?;
                }
                "q" => { gs_stack.push(gs.clone()); }
                "Q" => {
                    let s = gs_stack.pop();
                    if let Some(s) = s {
                        gs = s;
                    } else {
                        warn!("No state to pop");
                    }
                }
                "gs" => {
                    let ext_gstate: &Dictionary = get(doc, resources, b"ExtGState");
                    let name = operation.operands[0].as_name().unwrap();
                    let state: &Dictionary = get(doc, ext_gstate, name);
                    apply_state(doc, &mut gs, state);
                }
                "i" => { dlog!("unhandled graphics state flattness operator {:?}", operation); }
                "w" => { gs.line_width = as_num(&operation.operands[0]); }
                "J" | "j" | "M" | "d" | "ri"  => { dlog!("unknown graphics state operator {:?}", operation); }
                "m" => { path.ops.push(PathOp::MoveTo(as_num(&operation.operands[0]), as_num(&operation.operands[1]))) }
                "l" => { path.ops.push(PathOp::LineTo(as_num(&operation.operands[0]), as_num(&operation.operands[1]))) }
                "c" => {
                    path.ops.push(PathOp::CurveTo(
                        as_num(&operation.operands[0]),
                        as_num(&operation.operands[1]),
                        as_num(&operation.operands[2]),
                        as_num(&operation.operands[3]),
                        as_num(&operation.operands[4]),
                        as_num(&operation.operands[5])))
                }
                "v" => {
                    let (x, y) = path.current_point();
                    path.ops.push(PathOp::CurveTo(
                        x,
                        y,
                        as_num(&operation.operands[0]),
                        as_num(&operation.operands[1]),
                        as_num(&operation.operands[2]),
                        as_num(&operation.operands[3])))
                }
                "y" => {
                    path.ops.push(PathOp::CurveTo(
                        as_num(&operation.operands[0]),
                        as_num(&operation.operands[1]),
                        as_num(&operation.operands[2]),
                        as_num(&operation.operands[3]),
                        as_num(&operation.operands[2]),
                        as_num(&operation.operands[3])))
                }
                "h" => { path.ops.push(PathOp::Close) }
                "re" => {
                    path.ops.push(PathOp::Rect(as_num(&operation.operands[0]),
                                               as_num(&operation.operands[1]),
                                               as_num(&operation.operands[2]),
                                               as_num(&operation.operands[3])))
                }
                "s" | "f*" | "B" | "B*" | "b" => {
                    dlog!("unhandled path op {:?}", operation);
                }
                "S" => {
                    output.stroke(&gs.ctm, &gs.stroke_colorspace, &gs.stroke_color, &path)?;
                    path.ops.clear();
                }
                "F" | "f" => {
                    output.fill(&gs.ctm, &gs.fill_colorspace, &gs.fill_color, &path)?;
                    path.ops.clear();
                }
                "W" | "w*" => { dlog!("unhandled clipping operation {:?}", operation); }
                "n" => {
                    dlog!("discard {:?}", path);
                    path.ops.clear();
                }
                "BMC" | "BDC" => {
                    mc_stack.push(operation);
                }
                "EMC" => {
                    mc_stack.pop();
                }
                "Do" => {
                    // `Do` process an entire subdocument, so we do a recursive call to `process_stream`
                    // with the subdocument content and resources
                    let xobject: &Dictionary = get(&doc, resources, b"XObject");
                    let name = operation.operands[0].as_name().unwrap();
                    let xf: &Stream = get(&doc, xobject, name);
                    let resources = maybe_get_obj(&doc, &xf.dict, b"Resources").and_then(|n| n.as_dict().ok()).unwrap_or(resources);
                    let contents = get_contents(xf);
                    self.process_stream(&doc, contents, resources, &media_box, output, page_num)?;
                }
                _ => { dlog!("unknown operation {:?}", operation); }

            }
        }
        Ok(())
    }
}


pub trait OutputDev {
    fn begin_page(&mut self, page_num: u32, media_box: &MediaBox, art_box: Option<(f64, f64, f64, f64)>)-> Result<(), OutputError>;
    fn end_page(&mut self)-> Result<(), OutputError>;
    fn output_character(&mut self, trm: &Transform, width: f64, spacing: f64, font_size: f64, char: &str) -> Result<(), OutputError>;
    fn begin_word(&mut self)-> Result<(), OutputError>;
    fn end_word(&mut self)-> Result<(), OutputError>;
    fn end_line(&mut self)-> Result<(), OutputError>;
    fn stroke(&mut self, _ctm: &Transform, _colorspace: &ColorSpace, _color: &[f64], _path: &Path)-> Result<(), OutputError> {Ok(())}
    fn fill(&mut self, _ctm: &Transform, _colorspace: &ColorSpace, _color: &[f64], _path: &Path)-> Result<(), OutputError> {Ok(())}

    /// Optional hook for font changes (default no-op for backwards compatibility)
    fn set_font(&mut self, _font_name: &str) -> Result<(), OutputError> {
        Ok(())
    }
}


pub struct HTMLOutput<'a>  {
    file: &'a mut dyn std::io::Write,
    flip_ctm: Transform,
    last_ctm: Transform,
    buf_ctm: Transform,
    buf_font_size: f64,
    buf: String
}

fn insert_nbsp(input: &str) -> String {
    let mut result = String::new();
    let mut word_end = false;
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        if c == ' ' {
            if !word_end || chars.peek().filter(|x| **x != ' ').is_none() {
                result += "&nbsp;";
            } else {
                result += " ";
            }
            word_end = false;
        } else {
            word_end = true;
            result.push(c);
        }
    }
    result
}

impl<'a> HTMLOutput<'a> {
    pub fn new(file: &mut dyn std::io::Write) -> HTMLOutput {
        HTMLOutput {
            file,
            flip_ctm: Transform2D::identity(),
            last_ctm: Transform2D::identity(),
            buf_ctm: Transform2D::identity(),
            buf: String::new(),
            buf_font_size: 0.,
        }
    }
    fn flush_string(&mut self) -> Result<(), OutputError>{
        if self.buf.len() != 0 {

            let position = self.buf_ctm.post_transform(&self.flip_ctm);
            let transformed_font_size_vec = self.buf_ctm.transform_vector(vec2(self.buf_font_size, self.buf_font_size));
            // get the length of one sized of the square with the same area with a rectangle of size (x, y)
            let transformed_font_size = (transformed_font_size_vec.x * transformed_font_size_vec.y).sqrt();
            let (x, y) = (position.m31, position.m32);
            warn!("flush {} {:?}", self.buf, (x,y));

            write!(self.file, "<div style='position: absolute; left: {}px; top: {}px; font-size: {}px'>{}</div>\n",
                   x, y, transformed_font_size, insert_nbsp(&self.buf))?;
        }
        Ok(())
    }
}

type ArtBox = (f64, f64, f64, f64);

impl<'a> OutputDev for HTMLOutput<'a> {
    fn begin_page(&mut self, page_num: u32, media_box: &MediaBox, _: Option<ArtBox>) -> Result<(), OutputError> {
        write!(self.file, "<meta charset='utf-8' /> ")?;
        write!(self.file, "<!-- page {} -->", page_num)?;
        write!(self.file, "<div id='page{}' style='position: relative; height: {}px; width: {}px; border: 1px black solid'>", page_num, media_box.ury - media_box.lly, media_box.urx - media_box.llx)?;
        self.flip_ctm = Transform::row_major(1., 0., 0., -1., 0., media_box.ury - media_box.lly);
        Ok(())
    }
    fn end_page(&mut self) -> Result<(), OutputError> {
        self.flush_string()?;
        self.buf = String::new();
        self.last_ctm = Transform::identity();
        write!(self.file, "</div>")?;
        Ok(())
    }
    fn output_character(&mut self, trm: &Transform, width: f64, spacing: f64, font_size: f64, char: &str) -> Result<(), OutputError>{
        if trm.approx_eq(&self.last_ctm) {
            let position = trm.post_transform(&self.flip_ctm);
            let (x, y) = (position.m31, position.m32);

            warn!("accum {} {:?}", char, (x,y));
            self.buf += char;
        } else {
            warn!("flush {} {:?} {:?} {} {} {}", char, trm, self.last_ctm, width, font_size, spacing);
            self.flush_string()?;
            self.buf = char.to_owned();
            self.buf_font_size = font_size;
            self.buf_ctm = *trm;
        }
        let position = trm.post_transform(&self.flip_ctm);
        let transformed_font_size_vec = trm.transform_vector(vec2(font_size, font_size));
        // get the length of one sized of the square with the same area with a rectangle of size (x, y)
        let transformed_font_size = (transformed_font_size_vec.x * transformed_font_size_vec.y).sqrt();
        let (x, y) = (position.m31, position.m32);
        write!(self.file, "<div style='position: absolute; color: red; left: {}px; top: {}px; font-size: {}px'>{}</div>",
               x, y, transformed_font_size, char)?;
        self.last_ctm = trm.pre_transform(&Transform2D::create_translation(width * font_size + spacing, 0.));

        Ok(())
    }
    fn begin_word(&mut self) -> Result<(), OutputError> {Ok(())}
    fn end_word(&mut self) -> Result<(), OutputError> {Ok(())}
    fn end_line(&mut self) -> Result<(), OutputError> {Ok(())}
}

pub struct SVGOutput<'a>  {
    file: &'a mut dyn std::io::Write
}
impl<'a> SVGOutput<'a> {
    pub fn new(file: &mut dyn std::io::Write) -> SVGOutput {
        SVGOutput{file}
    }
}

impl<'a> OutputDev for SVGOutput<'a> {
    fn begin_page(&mut self, _page_num: u32, media_box: &MediaBox, art_box: Option<(f64, f64, f64, f64)>) -> Result<(), OutputError> {
        let ver = 1.1;
        write!(self.file, "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n")?;
        if ver == 1.1 {
            write!(self.file, r#"<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">"#)?;
        } else {
            write!(self.file, r#"<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN" "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">"#)?;
        }
        if let Some(art_box) = art_box {
            let width = art_box.2 - art_box.0;
            let height = art_box.3 - art_box.1;
            let y = media_box.ury - art_box.1 - height;
            write!(self.file, "<svg width=\"{}\" height=\"{}\" xmlns=\"http://www.w3.org/2000/svg\" version=\"{}\" viewBox='{} {} {} {}'>", width, height, ver, art_box.0, y, width, height)?;
        } else {
            let width = media_box.urx - media_box.llx;
            let height = media_box.ury - media_box.lly;
            write!(self.file, "<svg width=\"{}\" height=\"{}\" xmlns=\"http://www.w3.org/2000/svg\" version=\"{}\" viewBox='{} {} {} {}'>", width, height, ver, media_box.llx, media_box.lly, width, height)?;
        }
        write!(self.file, "\n")?;
        type Mat = Transform;

        let ctm = Mat::create_scale(1., -1.).post_translate(vec2(0., media_box.ury));
        write!(self.file, "<g transform='matrix({}, {}, {}, {}, {}, {})'>\n",
               ctm.m11,
               ctm.m12,
               ctm.m21,
               ctm.m22,
               ctm.m31,
               ctm.m32,
        )?;
        Ok(())
    }
    fn end_page(&mut self) -> Result<(), OutputError>{
        write!(self.file, "</g>\n")?;
        write!(self.file, "</svg>")?;
        Ok(())
    }
    fn output_character(&mut self, _trm: &Transform, _width: f64, _spacing: f64, _font_size: f64, _char: &str) -> Result<(), OutputError>{
        Ok(())
    }
    fn begin_word(&mut self) -> Result<(), OutputError> {Ok(())}
    fn end_word(&mut self) -> Result<(), OutputError> {Ok(())}
    fn end_line(&mut self) -> Result<(), OutputError> {Ok(())}
    fn fill(&mut self, ctm: &Transform, _colorspace: &ColorSpace, _color: &[f64], path: &Path) -> Result<(), OutputError>{
        write!(self.file, "<g transform='matrix({}, {}, {}, {}, {}, {})'>",
               ctm.m11,
               ctm.m12,
               ctm.m21,
               ctm.m22,
               ctm.m31,
               ctm.m32,
        )?;

        /*if path.ops.len() == 1 {
            if let PathOp::Rect(x, y, width, height) = path.ops[0] {
                write!(self.file, "<rect x={} y={} width={} height={} />\n", x, y, width, height);
                write!(self.file, "</g>");
                return;
            }
        }*/
        let mut d = Vec::new();
        for op in &path.ops {
            match op {
                &PathOp::MoveTo(x, y) => { d.push(format!("M{} {}", x, y))}
                &PathOp::LineTo(x, y) => { d.push(format!("L{} {}", x, y))},
                &PathOp::CurveTo(x1, y1, x2, y2, x, y) => { d.push(format!("C{} {} {} {} {} {}", x1, y1, x2, y2, x, y))},
                &PathOp::Close => { d.push(format!("Z"))},
                &PathOp::Rect(x, y, width, height) => {
                    d.push(format!("M{} {}", x, y));
                    d.push(format!("L{} {}", x + width, y));
                    d.push(format!("L{} {}", x + width, y + height));
                    d.push(format!("L{} {}", x, y + height));
                    d.push(format!("Z"));
                }

            }
        }
        write!(self.file, "<path d='{}' />", d.join(" "))?;
        write!(self.file, "</g>")?;
        write!(self.file, "\n")?;
        Ok(())
    }
}

/*
File doesn't implement std::fmt::Write so we have
to do some gymnastics to accept a File or String
See https://github.com/rust-lang/rust/issues/51305
*/

pub trait ConvertToFmt {
    type Writer: std::fmt::Write;
    fn convert(self) -> Self::Writer;
}

impl<'a> ConvertToFmt for &'a mut String {
    type Writer = &'a mut String;
    fn convert(self) -> Self::Writer {
        self
    }
}

pub struct WriteAdapter<W> {
    f: W,
}

impl<W: std::io::Write> std::fmt::Write for WriteAdapter<W> {
    fn write_str(&mut self, s: &str) -> Result<(), std::fmt::Error> {
        self.f.write_all(s.as_bytes()).map_err(|_| fmt::Error)
    }
}

impl<'a> ConvertToFmt for &'a mut dyn std::io::Write {
    type Writer = WriteAdapter<Self>;
    fn convert(self) -> Self::Writer {
        WriteAdapter { f: self }
    }
}

impl<'a> ConvertToFmt for &'a mut File {
    type Writer = WriteAdapter<Self>;
    fn convert(self) -> Self::Writer {
        WriteAdapter { f: self }
    }
}

pub struct PlainTextOutput<W: ConvertToFmt>   {
    writer: W::Writer,
    last_end: f64,
    last_y: f64,
    first_char: bool,
    flip_ctm: Transform,
}

impl<W: ConvertToFmt> PlainTextOutput<W> {
    pub fn new(writer: W) -> PlainTextOutput<W> {
        PlainTextOutput{
            writer: writer.convert(),
            last_end: 100000.,
            first_char: false,
            last_y: 0.,
            flip_ctm: Transform2D::identity(),
        }
    }
}

/* There are some structural hints that PDFs can use to signal word and line endings:
 * however relying on these is not likely to be sufficient. */
impl<W: ConvertToFmt> OutputDev for PlainTextOutput<W> {
    fn begin_page(&mut self, _page_num: u32, media_box: &MediaBox, _: Option<ArtBox>) -> Result<(), OutputError> {
        self.flip_ctm = Transform2D::row_major(1., 0., 0., -1., 0., media_box.ury - media_box.lly);
        Ok(())
    }
    fn end_page(&mut self) -> Result<(), OutputError> {
        Ok(())
    }
    fn output_character(&mut self, trm: &Transform, width: f64, _spacing: f64, font_size: f64, char: &str) -> Result<(), OutputError> {
        let position = trm.post_transform(&self.flip_ctm);
        let transformed_font_size_vec = trm.transform_vector(vec2(font_size, font_size));
        // get the length of one sized of the square with the same area with a rectangle of size (x, y)
        let transformed_font_size = (transformed_font_size_vec.x*transformed_font_size_vec.y).sqrt();
        let (x, y) = (position.m31, position.m32);
        use std::fmt::Write;
        //dlog!("last_end: {} x: {}, width: {}", self.last_end, x, width);
        if self.first_char {
            if (y - self.last_y).abs() > transformed_font_size * 1.5 {
                write!(self.writer, "\n")?;
            }

            // we've moved to the left and down
            if x < self.last_end && (y - self.last_y).abs() > transformed_font_size * 0.5 {
                write!(self.writer, "\n")?;
            }

            if x > self.last_end + transformed_font_size * 0.1 {
                dlog!("width: {}, space: {}, thresh: {}", width, x - self.last_end, transformed_font_size * 0.1);
                write!(self.writer, " ")?;
            }
        }
        //let norm = unicode_normalization::UnicodeNormalization::nfkc(char);
        write!(self.writer, "{}", char)?;
        self.first_char = false;
        self.last_y = y;
        self.last_end = x + width * transformed_font_size;
        Ok(())
    }
    fn begin_word(&mut self) -> Result<(), OutputError> {
        self.first_char = true;
        Ok(())
    }
    fn end_word(&mut self) -> Result<(), OutputError> {Ok(())}
    fn end_line(&mut self) -> Result<(), OutputError>{
        //write!(self.file, "\n");
        Ok(())
    }
}

// ==================== Markdown Output Data Structures ====================

/// Font family classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FontFamily {
    /// Serif fonts (Times, Georgia, etc.)
    Serif,
    /// Sans-serif fonts (Helvetica, Arial, etc.)
    SansSerif,
    /// Monospace fonts (Courier, Consolas, etc.)
    Monospace,
    /// Symbol fonts (Symbol, ZapfDingbats)
    Symbol,
    /// Unknown or unclassified fonts
    Unknown,
}

/// Font style information extracted from font name patterns
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FontStyle {
    /// Is this a bold font? (e.g., "Times-Bold", "Helvetica-Bold")
    pub is_bold: bool,

    /// Is this an italic font? (e.g., "Times-Italic", "Courier-Oblique")
    pub is_italic: bool,

    /// Is this a monospace font? (e.g., "Courier", "Consolas", "Menlo")
    pub is_monospace: bool,

    /// Font family (normalized)
    pub family: FontFamily,
}

impl FontStyle {
    /// Parse font style from BaseFont name
    fn from_font_name(name: &str) -> Self {
        let lower = name.to_lowercase();

        // Detect bold
        let is_bold = lower.contains("bold")
            || lower.contains("-b")
            || lower.contains(",b")
            || lower.contains("heavy")
            || lower.contains("black");

        // Detect italic
        let is_italic = lower.contains("italic")
            || lower.contains("oblique")
            || lower.contains("-i")
            || lower.contains(",i");

        // Detect monospace
        let is_monospace = lower.contains("courier")
            || lower.contains("mono")
            || lower.contains("consolas")
            || lower.contains("menlo")
            || lower.contains("inconsolata")
            || lower.contains("dejavu sans mono")
            || lower.contains("source code")
            || lower.contains("ubuntu mono");

        // Determine font family
        let family = if is_monospace {
            FontFamily::Monospace
        } else if lower.contains("times")
            || lower.contains("georgia")
            || lower.contains("garamond")
            || lower.contains("baskerville") {
            FontFamily::Serif
        } else if lower.contains("helvetica")
            || lower.contains("arial")
            || lower.contains("calibri")
            || lower.contains("verdana") {
            FontFamily::SansSerif
        } else if lower.contains("symbol")
            || lower.contains("zapf")
            || lower.contains("dingbat") {
            FontFamily::Symbol
        } else {
            FontFamily::Unknown
        };

        Self { is_bold, is_italic, is_monospace, family }
    }
}

impl Default for FontStyle {
    fn default() -> Self {
        Self {
            is_bold: false,
            is_italic: false,
            is_monospace: false,
            family: FontFamily::Unknown,
        }
    }
}

/// A run of text with consistent formatting
#[derive(Debug, Clone)]
pub struct TextRun {
    /// The actual text content
    pub text: String,

    /// Font size in points (transformed)
    pub font_size: f64,

    /// Font metadata extracted from BaseFont name
    pub font_style: FontStyle,

    /// X coordinate (page-relative, post-transform)
    pub x: f64,

    /// Y coordinate (page-relative, post-transform)
    pub y: f64,

    /// Width of the text run
    pub width: f64,

    /// Character index where this run starts (for debugging)
    pub char_index: usize,
}

/// A line of text composed of one or more text runs
#[derive(Debug, Clone)]
pub struct TextLine {
    /// All text runs in this line (left-to-right order)
    pub runs: Vec<TextRun>,

    /// Average Y coordinate of the line
    pub y: f64,

    /// Leftmost X coordinate (indentation)
    pub x_start: f64,

    /// Average font size for the line
    pub avg_font_size: f64,

    /// Dominant font style for the line
    pub dominant_style: FontStyle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BulletType {
    Unordered,  // •, -, *, ◦
    Ordered,    // 1., a., i., etc.
}

/// A row in a table
#[derive(Debug, Clone)]
struct TableRow {
    cells: Vec<String>,
    y: f64,
}

/// A logical block of content (paragraph, heading, list item, code block, table)
#[derive(Debug, Clone)]
enum Block {
    Heading {
        level: u8,        // 1-6
        text: String,
    },
    Paragraph {
        lines: Vec<String>,
        indentation: f64,
    },
    ListItem {
        bullet_type: BulletType,
        indentation: f64,
        content: String,
        nested_level: usize,
    },
    CodeBlock {
        lines: Vec<String>,
    },
    Table {
        rows: Vec<TableRow>,
        column_positions: Vec<f64>,
        header_row_index: Option<usize>,
    },
}

// ============================================================================
// Stream Mode Table Detection Structures (Camelot/Nurminen's Algorithm)
// ============================================================================

/// Configuration for Stream mode table detection
#[derive(Debug, Clone)]
pub struct StreamConfig {
    /// Minimum vertical alignment length (as fraction of table height) to consider an edge
    /// Default: 0.5 (50% of table height)
    pub edge_tol: f64,

    /// Vertical tolerance for grouping text into rows (in points)
    /// Default: 2.0
    pub row_tol: f64,

    /// Horizontal tolerance for merging column boundaries (in points)
    /// Default: 5.0
    pub column_tol: f64,

    /// Minimum number of rows a text edge must intersect to be considered relevant
    /// Default: 2
    pub min_edge_intersections: usize,

    /// Minimum table dimensions (in points)
    pub min_table_width: f64,
    pub min_table_height: f64,

    /// Horizontal gap threshold for word consolidation (multiplier of font_size)
    /// Text runs within this gap will be merged into single words before table detection
    /// Default: 0.3
    pub word_consolidation_gap: f64,
}

impl Default for StreamConfig {
    fn default() -> Self {
        Self {
            edge_tol: 0.5,
            row_tol: 2.0,
            column_tol: 8.0,  // Nurminen's original value; character-level runs now consolidated before detection
            word_consolidation_gap: 0.3,  // Match existing consolidate_runs behavior
            min_edge_intersections: 4,  // Nurminen's REQUIRED_TEXT_LINES_FOR_EDGE
            min_table_width: 50.0,
            min_table_height: 30.0,
        }
    }
}

/// Type of text alignment edge
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EdgeAlignment {
    /// Left edge of text
    Left,
    /// Right edge of text
    Right,
    /// Center of text
    Center,
}

/// Bounding box for geometric operations
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BoundingBox {
    pub x_min: f64,
    pub y_min: f64,
    pub x_max: f64,
    pub y_max: f64,
}

impl BoundingBox {
    /// Create from position and dimensions
    #[inline]
    pub fn new(x: f64, y: f64, width: f64, height: f64) -> Self {
        Self {
            x_min: x,
            y_min: y,
            x_max: x + width,
            y_max: y + height,
        }
    }

    /// Check if point is inside bbox
    #[inline]
    pub fn contains_point(&self, x: f64, y: f64) -> bool {
        x >= self.x_min && x <= self.x_max && y >= self.y_min && y <= self.y_max
    }

    /// Calculate intersection area with another bbox
    pub fn intersection_area(&self, other: &BoundingBox) -> f64 {
        let x_overlap = (self.x_max.min(other.x_max) - self.x_min.max(other.x_min)).max(0.0);
        let y_overlap = (self.y_max.min(other.y_max) - self.y_min.max(other.y_min)).max(0.0);
        x_overlap * y_overlap
    }
}

/// A text element with position and dimensions
#[derive(Debug, Clone)]
pub struct TextElement {
    /// Text content
    pub text: String,
    /// Bounding box
    pub bbox: BoundingBox,
    /// Font size
    pub font_size: f64,
    /// Font style metadata
    pub font_style: FontStyle,
}

impl TextElement {
    /// Get the left edge x-coordinate
    #[inline]
    pub fn left_edge(&self) -> f64 {
        self.bbox.x_min
    }

    /// Get the right edge x-coordinate
    #[inline]
    pub fn right_edge(&self) -> f64 {
        self.bbox.x_max
    }

    /// Get the center x-coordinate
    #[inline]
    pub fn center(&self) -> f64 {
        (self.bbox.x_min + self.bbox.x_max) / 2.0
    }

    /// Get the width
    #[inline]
    pub fn width(&self) -> f64 {
        self.bbox.x_max - self.bbox.x_min
    }
}

/// Represents a vertical text alignment edge
#[derive(Debug, Clone, PartialEq)]
pub struct TextEdge {
    /// X-coordinate of the edge
    pub x: f64,
    /// Y-coordinate where edge starts (top)
    pub y_min: f64,
    /// Y-coordinate where edge ends (bottom)
    pub y_max: f64,
    /// Type of alignment this edge represents
    pub alignment: EdgeAlignment,
    /// Number of text elements aligned to this edge
    pub support_count: usize,
    /// Confidence score (0.0-1.0) based on consistency
    pub confidence: f64,
}

impl TextEdge {
    /// Get the length (height) of this edge
    #[inline]
    pub fn length(&self) -> f64 {
        self.y_max - self.y_min
    }

    /// Check if edge intersects with a horizontal band
    #[inline]
    pub fn intersects_row(&self, y: f64, tolerance: f64) -> bool {
        y >= self.y_min - tolerance && y <= self.y_max + tolerance
    }

    /// Calculate overlap with another edge
    pub fn vertical_overlap(&self, other: &TextEdge) -> f64 {
        let overlap_start = self.y_min.max(other.y_min);
        let overlap_end = self.y_max.min(other.y_max);
        (overlap_end - overlap_start).max(0.0)
    }

    /// Count how many rows this edge intersects
    pub fn count_row_intersections(&self, rows: &[TextRow]) -> usize {
        rows.iter()
            .filter(|row| row.y >= self.y_min && row.y <= self.y_max)
            .count()
    }
}

/// A row of text elements grouped by vertical position
#[derive(Debug, Clone)]
pub struct TextRow {
    /// Y-coordinate (baseline or average)
    pub y: f64,
    /// Text runs in this row (sorted by x position)
    pub elements: Vec<TextElement>,
    /// Leftmost x coordinate
    pub x_min: f64,
    /// Rightmost x coordinate
    pub x_max: f64,
}

impl TextRow {
    /// Get the width of this row
    #[inline]
    pub fn width(&self) -> f64 {
        self.x_max - self.x_min
    }

    /// Count number of word-like elements in this row
    pub fn word_count(&self) -> usize {
        self.elements.len()
    }
}

/// Table detection mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TableDetectionMode {
    /// Disable table detection
    Disabled,
    /// Legacy column clustering algorithm (backward compatibility)
    Legacy,
    /// Stream mode (Nurminen's algorithm) - text-only
    StreamMode,
}

/// Error type for table detection
#[derive(Debug, thiserror::Error)]
pub enum TableDetectionError {
    #[error("No text rows found in region")]
    NoTextRows,

    #[error("Insufficient edges: found {0}, need at least {1}")]
    InsufficientEdges(usize, usize),

    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),
}

/// Cell in a detected table
#[derive(Debug, Clone)]
pub struct TableCell {
    /// Row index (0-based)
    pub row: usize,
    /// Column index (0-based)
    pub col: usize,
    /// Text content
    pub content: String,
    /// Bounding box of the cell
    pub bbox: BoundingBox,
    /// Text elements in this cell
    pub elements: Vec<TextElement>,
}

/// Metadata about table detection
#[derive(Debug, Clone)]
pub struct TableMetadata {
    /// Detection algorithm used
    pub algorithm: TableDetectionAlgorithm,
    /// Number of columns detected
    pub num_columns: usize,
    /// Number of rows detected
    pub num_rows: usize,
    /// Statistical mode of words-per-row (if applicable)
    pub mode_words_per_row: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TableDetectionAlgorithm {
    /// Legacy column clustering
    Legacy,
    /// Stream mode (Nurminen's algorithm)
    StreamMode,
}

/// Represents a detected table region with Stream mode metadata
#[derive(Debug, Clone)]
pub struct TableRegion {
    /// Bounding box of the table
    pub bbox: BoundingBox,
    /// Column boundaries (sorted x-coordinates)
    pub columns: Vec<f64>,
    /// Row y-coordinates (sorted)
    pub rows: Vec<f64>,
    /// Text elements organized by cell
    pub cells: Vec<Vec<TableCell>>,
    /// Detected edges used for column detection
    pub edges: Vec<TextEdge>,
    /// Confidence score (0.0-1.0)
    pub confidence: f64,
    /// Detection method metadata
    pub metadata: TableMetadata,
}

/// Stream mode table detector implementing Nurminen's algorithm
pub struct StreamTableDetector<'a> {
    config: StreamConfig,
    lines: &'a [TextLine],
}

impl<'a> StreamTableDetector<'a> {
    /// Create a new StreamTableDetector
    pub fn new(config: StreamConfig, lines: &'a [TextLine]) -> Self {
        Self { config, lines }
    }

    /// Main entry point: detect all tables in the text
    pub fn detect_tables(&self) -> Result<Vec<TableRegion>, TableDetectionError> {
        // Step 1: Build text rows from lines
        let text_rows = self.build_text_rows()?;

        // Step 2: Generate text edges from aligned elements
        let text_edges = self.generate_text_edges(&text_rows)?;

        // Step 3: Select edges that intersect most rows
        let relevant_edges = self.select_relevant_edges(&text_edges, &text_rows)?;

        // Step 4: Convert edges to column boundaries
        let boundaries = self.edges_to_boundaries(&relevant_edges)?;

        // Step 5: Infer table bounding boxes
        let regions = self.infer_table_boundaries(&text_rows, &boundaries)?;

        // Step 6: Build table structures from regions
        let mut tables = Vec::new();
        for bbox in regions {
            let table = self.build_table_from_region(bbox, &text_rows, &boundaries)?;
            tables.push(table);
        }

        Ok(tables)
    }

    /// Build text rows by grouping text elements with similar y-coordinates
    fn build_text_rows(&self) -> Result<Vec<TextRow>, TableDetectionError> {
        if self.lines.is_empty() {
            return Err(TableDetectionError::NoTextRows);
        }

        // Step 1: Collect all text elements from TextLines
        let mut elements: Vec<TextElement> = Vec::new();
        for line in self.lines {
            for run in &line.runs {
                elements.push(TextElement {
                    text: run.text.clone(),
                    bbox: BoundingBox::new(run.x, run.y, run.width, run.font_size),
                    font_size: run.font_size,
                    font_style: run.font_style,
                });
            }
        }

        if elements.is_empty() {
            return Err(TableDetectionError::NoTextRows);
        }

        // Step 2: Sort by y-coordinate
        elements.sort_by(|a, b| a.bbox.y_min.total_cmp(&b.bbox.y_min));

        // Step 3: Group into rows using row_tol
        let mut rows: Vec<TextRow> = Vec::new();
        let mut current_row_elements: Vec<TextElement> = vec![elements[0].clone()];
        let mut current_y = elements[0].bbox.y_min;

        for elem in elements.into_iter().skip(1) {
            if (elem.bbox.y_min - current_y).abs() <= self.config.row_tol {
                current_row_elements.push(elem);
            } else {
                // Finalize current row
                rows.push(Self::create_text_row(current_row_elements, current_y));
                current_row_elements = vec![elem.clone()];
                current_y = elem.bbox.y_min;
            }
        }

        // Don't forget the last row
        if !current_row_elements.is_empty() {
            rows.push(Self::create_text_row(current_row_elements, current_y));
        }

        Ok(rows)
    }

    /// Helper to create a TextRow from elements
    fn create_text_row(elements: Vec<TextElement>, y: f64) -> TextRow {
        let x_min = elements
            .iter()
            .map(|e| e.bbox.x_min)
            .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap_or(0.0);
        let x_max = elements
            .iter()
            .map(|e| e.bbox.x_max)
            .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap_or(0.0);
        TextRow {
            y,
            elements,
            x_min,
            x_max,
        }
    }

    /// Generate text edges from aligned text elements
    fn generate_text_edges(&self, rows: &[TextRow]) -> Result<Vec<TextEdge>, TableDetectionError> {
        use std::collections::HashMap;

        // Step 1: Collect all edge candidates
        let mut edge_candidates: HashMap<(EdgeAlignment, i32), Vec<(f64, f64, f64)>> =
            HashMap::new();
        // Key: (alignment_type, quantized_x), Value: Vec<(exact_x, y_min, y_max)>

        for row in rows {
            for elem in &row.elements {
                // Left edge
                let left_x = elem.left_edge();
                let key_left = (
                    EdgeAlignment::Left,
                    (left_x / self.config.column_tol).round() as i32,
                );
                edge_candidates
                    .entry(key_left)
                    .or_insert_with(Vec::new)
                    .push((left_x, row.y, row.y));

                // Right edge
                let right_x = elem.right_edge();
                let key_right = (
                    EdgeAlignment::Right,
                    (right_x / self.config.column_tol).round() as i32,
                );
                edge_candidates
                    .entry(key_right)
                    .or_insert_with(Vec::new)
                    .push((right_x, row.y, row.y));

                // Center edge
                let center_x = elem.center();
                let key_center = (
                    EdgeAlignment::Center,
                    (center_x / self.config.column_tol).round() as i32,
                );
                edge_candidates
                    .entry(key_center)
                    .or_insert_with(Vec::new)
                    .push((center_x, row.y, row.y));
            }
        }

        // Step 2: Consolidate edges within column_tol
        let mut edges: Vec<TextEdge> = Vec::new();
        for ((alignment, _), positions) in edge_candidates {
            if positions.len() < 2 {
                continue; // Need at least 2 elements for an edge
            }

            // Calculate average x-coordinate
            let avg_x = positions.iter().map(|(x, _, _)| x).sum::<f64>() / positions.len() as f64;

            // Calculate y-extent
            let y_min = positions
                .iter()
                .map(|(_, y, _)| y)
                .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
                .copied()
                .unwrap();
            let y_max = positions
                .iter()
                .map(|(_, y, _)| y)
                .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
                .copied()
                .unwrap();

            // Calculate confidence (based on x-coordinate variance)
            let variance = positions
                .iter()
                .map(|(x, _, _)| (x - avg_x).powi(2))
                .sum::<f64>()
                / positions.len() as f64;
            let confidence = 1.0 / (1.0 + variance); // Higher variance = lower confidence

            edges.push(TextEdge {
                x: avg_x,
                y_min,
                y_max,
                alignment,
                support_count: positions.len(),
                confidence,
            });
        }

        Ok(edges)
    }

    /// Select edges that intersect the most rows (relevant edges)
    fn select_relevant_edges(
        &self,
        edges: &[TextEdge],
        rows: &[TextRow],
    ) -> Result<Vec<TextEdge>, TableDetectionError> {
        if rows.is_empty() {
            return Ok(Vec::new());
        }

        // Calculate table height
        let table_y_min = rows
            .iter()
            .map(|r| r.y)
            .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap();
        let table_y_max = rows
            .iter()
            .map(|r| r.y)
            .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap();
        let table_height = table_y_max - table_y_min;

        let min_edge_length = table_height * self.config.edge_tol;

        // Filter edges by length and row intersections
        let filtered_edges: Vec<TextEdge> = edges
            .iter()
            .filter(|edge| {
                let edge_length = edge.length();
                let intersections = edge.count_row_intersections(rows);

                edge_length >= min_edge_length
                    && intersections >= self.config.min_edge_intersections
            })
            .cloned()
            .collect();

        // CRITICAL: Nurminen's hierarchical edge type selection
        // Select ONE edge type based on occurrence count:
        // LEFT (if >2) > RIGHT (if >1) > CENTER (if >1)
        use std::collections::HashMap;
        let mut edge_counts: HashMap<EdgeAlignment, usize> = HashMap::new();

        for edge in &filtered_edges {
            *edge_counts.entry(edge.alignment).or_insert(0) += 1;
        }

        let left_count = edge_counts.get(&EdgeAlignment::Left).copied().unwrap_or(0);
        let right_count = edge_counts.get(&EdgeAlignment::Right).copied().unwrap_or(0);
        let center_count = edge_counts.get(&EdgeAlignment::Center).copied().unwrap_or(0);

        log::debug!("Edge type counts - Left: {}, Right: {}, Center: {}",
                   left_count, right_count, center_count);

        // Select edge type based on Nurminen's hierarchy
        let selected_alignment = if left_count > 2 {
            log::debug!("Selected LEFT edges (count={})", left_count);
            EdgeAlignment::Left
        } else if right_count > 1 {
            log::debug!("Selected RIGHT edges (count={})", right_count);
            EdgeAlignment::Right
        } else if center_count > 1 {
            log::debug!("Selected CENTER edges (count={})", center_count);
            EdgeAlignment::Center
        } else {
            // Fall back to whichever has the most
            let max_type = edge_counts.iter()
                .max_by_key(|(_, &count)| count)
                .map(|(&alignment, _)| alignment);

            if let Some(alignment) = max_type {
                log::debug!("Selected {:?} edges by fallback", alignment);
                alignment
            } else {
                return Ok(Vec::new());
            }
        };

        // Return only edges of the selected type
        let relevant_edges: Vec<TextEdge> = filtered_edges
            .into_iter()
            .filter(|edge| edge.alignment == selected_alignment)
            .collect();

        log::debug!("Selected {} edges of type {:?}", relevant_edges.len(), selected_alignment);

        Ok(relevant_edges)
    }

    /// Convert edges to column boundaries
    fn edges_to_boundaries(&self, edges: &[TextEdge]) -> Result<Vec<f64>, TableDetectionError> {
        if edges.is_empty() {
            return Err(TableDetectionError::InsufficientEdges(
                0,
                self.config.min_edge_intersections,
            ));
        }

        // Extract x-coordinates and sort
        let mut boundaries: Vec<f64> = edges.iter().map(|e| e.x).collect();
        boundaries.sort_by(|a, b| a.total_cmp(b));

        log::debug!("Before dedup: {} boundaries", boundaries.len());

        // Deduplicate within column_tol
        boundaries.dedup_by(|a, b| (*a - *b).abs() <= self.config.column_tol);

        log::debug!("After dedup: {} boundaries (column_tol={})", boundaries.len(), self.config.column_tol);
        if boundaries.len() <= 15 {
            log::debug!("All boundaries: {:?}", boundaries);
        } else {
            log::debug!("First 15 boundaries: {:?}", &boundaries[..15]);
        }

        Ok(boundaries)
    }

    /// Infer table bounding boxes from rows and boundaries
    fn infer_table_boundaries(
        &self,
        rows: &[TextRow],
        boundaries: &[f64],
    ) -> Result<Vec<BoundingBox>, TableDetectionError> {
        if rows.is_empty() || boundaries.len() < 2 {
            return Ok(Vec::new());
        }

        // Simple approach for Phase 2: treat entire region as one table
        let x_min = *boundaries.first().unwrap();
        let x_max = *boundaries.last().unwrap();
        let y_min = rows.first().unwrap().y;
        let y_max = rows.last().unwrap().y;

        let bbox = BoundingBox {
            x_min,
            y_min,
            x_max,
            y_max,
        };

        // Filter by minimum dimensions
        if (bbox.x_max - bbox.x_min) >= self.config.min_table_width
            && (bbox.y_max - bbox.y_min) >= self.config.min_table_height
        {
            Ok(vec![bbox])
        } else {
            Ok(Vec::new())
        }
    }

    /// Build a complete table structure from a region
    fn build_table_from_region(
        &self,
        bbox: BoundingBox,
        rows: &[TextRow],
        boundaries: &[f64],
    ) -> Result<TableRegion, TableDetectionError> {
        // Filter rows within bbox
        let table_rows: Vec<&TextRow> = rows
            .iter()
            .filter(|r| r.y >= bbox.y_min && r.y <= bbox.y_max)
            .collect();

        // Build cells matrix
        let mut cells: Vec<Vec<TableCell>> = Vec::new();
        for (row_idx, row) in table_rows.iter().enumerate() {
            let mut row_cells: Vec<TableCell> = Vec::new();

            for col_idx in 0..boundaries.len() - 1 {
                let col_x_min = boundaries[col_idx];
                let col_x_max = boundaries[col_idx + 1];
                let col_center = (col_x_min + col_x_max) / 2.0;

                // Find elements in this cell - check if element's START position is closest to this column
                let cell_elements: Vec<TextElement> = row
                    .elements
                    .iter()
                    .filter(|e| {
                        // Assign element to column if its left edge is within the column bounds
                        // Use a relaxed tolerance to handle slight misalignments
                        let elem_x = e.bbox.x_min;
                        elem_x >= col_x_min && elem_x < col_x_max
                    })
                    .cloned()
                    .collect();

                // Sort elements by x-coordinate to maintain left-to-right order
                let mut sorted_elements = cell_elements.clone();
                sorted_elements.sort_by(|a, b| a.bbox.x_min.total_cmp(&b.bbox.x_min));

                // Join text WITHOUT spaces - runs are already properly spaced
                let cell_content = sorted_elements
                    .iter()
                    .map(|e| e.text.as_str())
                    .collect::<Vec<_>>()
                    .join("");

                row_cells.push(TableCell {
                    row: row_idx,
                    col: col_idx,
                    content: cell_content,
                    bbox: BoundingBox {
                        x_min: col_x_min,
                        y_min: row.y,
                        x_max: col_x_max,
                        y_max: row.y + 10.0, // Estimate row height
                    },
                    elements: sorted_elements,
                });
            }

            cells.push(row_cells);
        }

        let num_columns = boundaries.len() - 1;
        let num_rows = cells.len();

        let mut region = TableRegion {
            bbox,
            columns: boundaries.to_vec(),
            rows: table_rows.iter().map(|r| r.y).collect(),
            cells,
            edges: Vec::new(),
            confidence: 0.0,
            metadata: TableMetadata {
                algorithm: TableDetectionAlgorithm::StreamMode,
                num_columns,
                num_rows,
                mode_words_per_row: None,
            },
        };

        // Calculate confidence
        region.confidence = self.calculate_table_confidence(&region);

        Ok(region)
    }

    /// Calculate confidence score for a detected table
    fn calculate_table_confidence(&self, region: &TableRegion) -> f64 {
        if region.cells.is_empty() {
            return 0.0;
        }

        // Metric 1: Cell occupancy (what % of cells have content)
        let total_cells = region.cells.len()
            * region
                .cells
                .get(0)
                .map(|r| r.len())
                .unwrap_or(0);
        let filled_cells = region
            .cells
            .iter()
            .flat_map(|row| row.iter())
            .filter(|cell| !cell.content.trim().is_empty())
            .count();

        let occupancy_score = if total_cells > 0 {
            filled_cells as f64 / total_cells as f64
        } else {
            0.0
        };

        // Metric 2: Row consistency (variance of cell counts per row)
        let row_lengths: Vec<usize> = region.cells.iter().map(|row| row.len()).collect();
        if row_lengths.is_empty() {
            return occupancy_score;
        }

        let avg_length = row_lengths.iter().sum::<usize>() as f64 / row_lengths.len() as f64;
        let variance = row_lengths
            .iter()
            .map(|&len| (len as f64 - avg_length).powi(2))
            .sum::<f64>()
            / row_lengths.len() as f64;

        let consistency_score = 1.0 / (1.0 + variance);

        // Metric 3: Edge confidence (average of all edge confidences)
        let edge_confidence = if !region.edges.is_empty() {
            region.edges.iter().map(|e| e.confidence).sum::<f64>() / region.edges.len() as f64
        } else {
            0.5 // Neutral if no edges
        };

        // Combined score (weighted average)
        (occupancy_score * 0.4 + consistency_score * 0.3 + edge_confidence * 0.3).clamp(0.0, 1.0)
    }

    /// Refine column boundaries using statistical mode analysis
    fn refine_columns_with_mode(&self, rows: &[TextRow]) -> Vec<f64> {
        use std::collections::HashMap;

        if rows.is_empty() {
            return Vec::new();
        }

        // Step 1: Calculate word count per row
        let word_counts: Vec<usize> = rows.iter().map(|row| row.word_count()).collect();

        // Step 2: Find mode (most common count)
        let mut frequency_map: HashMap<usize, usize> = HashMap::new();
        for &count in &word_counts {
            *frequency_map.entry(count).or_insert(0) += 1;
        }

        let mode = frequency_map
            .iter()
            .max_by_key(|(_, &freq)| freq)
            .map(|(&count, _)| count)
            .unwrap_or(0);

        if mode == 0 {
            return Vec::new();
        }

        // Step 3: Find rows matching mode and extract their column boundaries
        let mode_rows: Vec<&TextRow> = rows
            .iter()
            .filter(|row| row.word_count() == mode)
            .collect();

        if mode_rows.is_empty() {
            return Vec::new();
        }

        // Step 4: Cluster element positions to find column boundaries
        let mut all_positions: Vec<f64> = mode_rows
            .iter()
            .flat_map(|row| row.elements.iter().map(|e| e.left_edge()))
            .collect();

        if all_positions.is_empty() {
            return Vec::new();
        }

        all_positions.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        // Merge nearby positions
        let mut boundaries = Vec::new();
        let mut current_cluster = vec![all_positions[0]];

        for &pos in &all_positions[1..] {
            if pos - current_cluster.last().unwrap() <= self.config.column_tol {
                current_cluster.push(pos);
            } else {
                // Calculate cluster average
                let avg = current_cluster.iter().sum::<f64>() / current_cluster.len() as f64;
                boundaries.push(avg);
                current_cluster = vec![pos];
            }
        }

        // Don't forget last cluster
        if !current_cluster.is_empty() {
            boundaries.push(current_cluster.iter().sum::<f64>() / current_cluster.len() as f64);
        }

        boundaries
    }
}

/// Configuration for markdown extraction heuristics
#[derive(Debug, Clone)]
pub struct MarkdownConfig {
    /// Minimum font size increase to consider heading (multiplier)
    pub heading_font_size_threshold: f64,

    /// Maximum heading levels to detect (1-6)
    pub max_heading_level: u8,

    /// Vertical spacing threshold for paragraph breaks (multiplier of font_size)
    pub paragraph_spacing_threshold: f64,

    /// Horizontal spacing threshold for word breaks (multiplier)
    pub word_spacing_threshold: f64,

    /// Indentation threshold for list detection (points)
    pub list_indent_threshold: f64,

    /// Detect code blocks based on monospace font runs
    pub detect_code_blocks: bool,

    /// Minimum consecutive monospace lines to form code block
    pub min_code_block_lines: usize,

    /// List bullet characters to recognize
    pub bullet_chars: Vec<char>,

    // Enhanced heading detection
    /// Enable bold text as heading indicator
    pub enable_bold_heading_detection: bool,

    /// Enable all-caps text as heading indicator
    pub enable_allcaps_heading_detection: bool,

    /// Enable centered text as heading indicator
    pub enable_centered_heading_detection: bool,

    /// Minimum confidence score to consider heading (0-10)
    pub heading_confidence_threshold: u8,

    /// Maximum line width ratio for headings (0.0-1.0)
    pub max_heading_line_length_ratio: f64,

    // Table detection
    /// Enable table detection
    pub enable_table_detection: bool,

    /// Column alignment tolerance (points)
    pub table_column_tolerance: f64,

    /// Minimum rows required for table detection
    pub min_table_rows: usize,

    /// Minimum columns required for table detection
    pub min_table_columns: usize,

    /// Table detection algorithm mode
    pub table_detection_mode: TableDetectionMode,

    /// Configuration for Stream mode table detection
    pub stream_config: StreamConfig,
}

impl Default for MarkdownConfig {
    fn default() -> Self {
        Self {
            heading_font_size_threshold: 1.1,
            max_heading_level: 6,
            paragraph_spacing_threshold: 1.5,
            word_spacing_threshold: 0.3,
            list_indent_threshold: 20.0,
            detect_code_blocks: true,
            min_code_block_lines: 2,
            bullet_chars: vec!['•', '■', '▪', '◦', '◘', '○', '-', '*', '·'],

            // Enhanced heading detection
            enable_bold_heading_detection: true,
            enable_allcaps_heading_detection: true,
            enable_centered_heading_detection: true,
            heading_confidence_threshold: 4,
            max_heading_line_length_ratio: 0.8,

            // Table detection
            enable_table_detection: true,
            table_column_tolerance: 15.0,
            min_table_rows: 4,
            min_table_columns: 2,
            table_detection_mode: TableDetectionMode::StreamMode,
            stream_config: StreamConfig::default(),
        }
    }
}

/// Statistics collected per page for adaptive heuristics
#[derive(Debug, Default)]
struct PageStatistics {
    /// Font sizes seen on this page
    font_sizes: Vec<f64>,

    /// Most common font size (body text baseline)
    body_font_size: Option<f64>,

    /// Line spacings observed
    line_spacings: Vec<f64>,

    /// Average line spacing
    avg_line_spacing: f64,

    /// Indentation levels observed
    indent_levels: Vec<f64>,

    /// Page dimensions (for centering detection)
    page_width: f64,
    page_height: f64,
}

// ==================== Markdown Output Implementation ====================

/// The main MarkdownOutput implementation
pub struct MarkdownOutput<W: ConvertToFmt> {
    /// Writer for output
    writer: W::Writer,

    /// Transform for coordinate conversion
    flip_ctm: Transform,

    /// Current page number
    current_page: u32,

    /// Buffer for accumulating text runs
    run_buffer: Vec<TextRun>,

    /// Current text being accumulated within a run
    current_run_text: String,

    /// Position of last character
    last_x: f64,
    last_y: f64,

    /// Position where current run started
    run_start_x: f64,
    run_start_y: f64,

    /// Font context for current text
    current_font_style: FontStyle,
    current_font_size: f64,

    /// Character counter for debugging
    char_counter: usize,

    /// Lines assembled from runs
    assembled_lines: Vec<TextLine>,

    /// Blocks detected from lines
    blocks: Vec<Block>,

    /// Configuration for heuristics
    config: MarkdownConfig,

    /// Statistics for adaptive thresholds
    stats: PageStatistics,

    /// Stream mode table regions (if using Stream mode detection)
    stream_table_regions: Vec<TableRegion>,
}

impl<W: ConvertToFmt> MarkdownOutput<W> {
    /// Create new MarkdownOutput with default configuration
    pub fn new(writer: W) -> Self {
        Self::with_config(writer, MarkdownConfig::default())
    }

    /// Create new MarkdownOutput with custom configuration
    pub fn with_config(writer: W, config: MarkdownConfig) -> Self {
        Self {
            writer: writer.convert(),
            flip_ctm: Transform2D::identity(),
            current_page: 0,
            run_buffer: Vec::new(),
            current_run_text: String::new(),
            last_x: 0.0,
            last_y: 0.0,
            run_start_x: 0.0,
            run_start_y: 0.0,
            current_font_style: FontStyle::default(),
            current_font_size: 0.0,
            char_counter: 0,
            assembled_lines: Vec::new(),
            blocks: Vec::new(),
            config,
            stats: PageStatistics::default(),
            stream_table_regions: Vec::new(),
        }
    }

    /// Check if we need to start a new text run
    fn should_start_new_run(&self, x: f64, y: f64, font_size: f64) -> bool {
        if self.current_run_text.is_empty() {
            return false;
        }

        let y_diff = (y - self.last_y).abs();
        let x_gap = x - self.last_x;
        let font_changed = (font_size - self.current_font_size).abs() > 0.1;

        // Start new run if:
        // - Font size changed
        // - Large vertical movement
        // - Large horizontal gap (potential word spacing)
        font_changed
            || y_diff > font_size * 0.5
            || x_gap > font_size * self.config.word_spacing_threshold
    }

    /// Flush current accumulated text into a TextRun
    fn flush_current_run(&mut self) {
        if self.current_run_text.is_empty() {
            return;
        }

        let run = TextRun {
            text: std::mem::take(&mut self.current_run_text),
            font_size: self.current_font_size,
            font_style: self.current_font_style,
            x: self.run_start_x,  // Use START position, not end
            y: self.run_start_y,  // Use START position, not end
            width: self.last_x - self.run_start_x,  // Calculate actual width
            char_index: self.char_counter,
        };

        self.run_buffer.push(run);
    }

    /// Convert a text line to a string
    fn line_to_text(&self, line: &TextLine) -> String {
        let mut result = String::new();
        let mut last_end_x = 0.0;

        for run in &line.runs {
            // If there's a gap, add a space
            if !result.is_empty() && run.x > last_end_x + run.font_size * 0.2 {
                result.push(' ');
            }
            result.push_str(&run.text);
            last_end_x = run.x + run.width;
        }

        result
    }

    /// Get dominant style (most common) from runs
    fn get_dominant_style(&self, runs: &[TextRun]) -> FontStyle {
        if runs.is_empty() {
            return FontStyle::default();
        }

        // Count style occurrences
        let mut style_counts: HashMap<(bool, bool, bool), usize> = HashMap::new();

        for run in runs {
            let key = (run.font_style.is_bold, run.font_style.is_italic, run.font_style.is_monospace);
            *style_counts.entry(key).or_insert(0) += 1;
        }

        // Get most common style
        if let Some(((is_bold, is_italic, is_monospace), _)) =
            style_counts.iter().max_by_key(|&(_, count)| count) {
            FontStyle {
                is_bold: *is_bold,
                is_italic: *is_italic,
                is_monospace: *is_monospace,
                family: runs[0].font_style.family,
            }
        } else {
            runs[0].font_style
        }
    }

    /// Check if line text is all uppercase
    fn is_line_all_caps(&self, line: &TextLine) -> bool {
        let text = self.line_to_text(line);
        let alpha_chars: Vec<char> = text.chars().filter(|c| c.is_alphabetic()).collect();

        if alpha_chars.is_empty() {
            return false;
        }

        alpha_chars.iter().all(|c| c.is_uppercase())
    }

    /// Check if line appears centered on page
    fn is_line_centered(&self, line: &TextLine) -> bool {
        let page_width = self.stats.page_width;
        if page_width == 0.0 {
            return false;
        }

        let page_center = page_width / 2.0;
        let line_length = self.estimate_line_width(line);
        let line_center = line.x_start + line_length / 2.0;

        // Within 10% of page center
        (line_center - page_center).abs() < page_width * 0.1
    }

    /// Get spacing before and after a line
    fn get_surrounding_spacing(&self, line_index: usize) -> (f64, f64) {
        let spacing_before = if line_index > 0 {
            (self.assembled_lines[line_index].y
             - self.assembled_lines[line_index - 1].y).abs()
        } else {
            0.0
        };

        let spacing_after = if line_index < self.assembled_lines.len() - 1 {
            (self.assembled_lines[line_index + 1].y
             - self.assembled_lines[line_index].y).abs()
        } else {
            0.0
        };

        (spacing_before, spacing_after)
    }

    /// Estimate line width from runs
    fn estimate_line_width(&self, line: &TextLine) -> f64 {
        if line.runs.is_empty() {
            return 0.0;
        }

        let leftmost = line.runs.iter()
            .map(|r| r.x)
            .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap_or(0.0);

        let rightmost = line.runs.iter()
            .map(|r| r.x + r.width)
            .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap_or(0.0);

        rightmost - leftmost
    }

    /// Consolidate adjacent runs with configurable gap threshold
    /// Merges runs with the same style that are within gap_threshold * font_size distance
    fn consolidate_runs_configurable(runs: Vec<TextRun>, gap_threshold: f64) -> Vec<TextRun> {
        if runs.is_empty() {
            return runs;
        }

        let mut consolidated = Vec::new();
        let mut current_run = runs[0].clone();

        for run in runs.into_iter().skip(1) {
            // Calculate gap size with minimum threshold to handle zero/small font sizes
            let gap_size = (current_run.font_size * gap_threshold).max(2.0);

            // Check if runs can be merged (same style, close position)
            if run.font_style == current_run.font_style
                && (run.x - (current_run.x + current_run.width)).abs() < gap_size {
                // Merge runs
                current_run.text.push_str(&run.text);
                current_run.width = run.x + run.width - current_run.x;
            } else {
                // Save current run and start new one
                consolidated.push(current_run);
                current_run = run;
            }
        }

        consolidated.push(current_run);
        consolidated
    }

    /// Create a TextLine from a group of runs
    /// Consolidate adjacent runs with the same style
    fn consolidate_runs(runs: Vec<TextRun>) -> Vec<TextRun> {
        Self::consolidate_runs_configurable(runs, 0.3)
    }

    /// Pre-consolidate lines for table detection
    /// Merges character-level runs into word-level runs to improve column detection accuracy
    fn pre_consolidate_lines_for_table_detection(&self, lines: &[TextLine]) -> Vec<TextLine> {
        let gap_threshold = self.config.stream_config.word_consolidation_gap;

        lines.iter().map(|line| {
            // Consolidate runs using configurable threshold
            let consolidated_runs = Self::consolidate_runs_configurable(
                line.runs.clone(),
                gap_threshold
            );

            // Recalculate x_start (leftmost position)
            let x_start = consolidated_runs.iter()
                .map(|r| r.x)
                .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
                .unwrap_or(line.x_start);

            // Rebuild TextLine with consolidated runs
            TextLine {
                runs: consolidated_runs,
                y: line.y,
                x_start,
                avg_font_size: line.avg_font_size,
                dominant_style: line.dominant_style,
            }
        }).collect()
    }

    fn create_text_line(&self, runs: Vec<TextRun>) -> TextLine {
        let runs = Self::consolidate_runs(runs);
        let y = runs.iter().map(|r| r.y).sum::<f64>() / runs.len() as f64;
        let x_start = runs.iter()
            .map(|r| r.x)
            .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap_or(0.0);
        let avg_font_size = runs.iter().map(|r| r.font_size).sum::<f64>() / runs.len() as f64;

        // Determine dominant style (most common)
        let dominant_style = self.get_dominant_style(&runs);

        TextLine {
            runs,
            y,
            x_start,
            avg_font_size,
            dominant_style,
        }
    }

    /// Assemble accumulated runs into lines
    fn assemble_lines(&mut self) {
        if self.run_buffer.is_empty() {
            return;
        }

        // Sort runs by Y position (ascending), then X position (ascending)
        // Use total_cmp to handle NaN values (treats NaN as less than all other values)
        self.run_buffer.sort_by(|a, b| {
            a.y.total_cmp(&b.y).then_with(|| a.x.total_cmp(&b.x))
        });

        // Take ownership of the run buffer to avoid borrowing issues
        let runs = std::mem::take(&mut self.run_buffer);
        let mut lines: Vec<TextLine> = Vec::new();
        let mut current_line_runs: Vec<TextRun> = Vec::new();
        let mut current_y = runs[0].y;

        for run in runs {
            let y_diff = (run.y - current_y).abs();
            let font_size_threshold = run.font_size * 0.3;

            // Check if this run belongs to current line or new line
            if y_diff > font_size_threshold && !current_line_runs.is_empty() {
                // Finalize current line
                let line = self.create_text_line(std::mem::take(&mut current_line_runs));
                lines.push(line);

                // Start new line with current run
                current_y = run.y;
                current_line_runs.push(run);
            } else {
                current_line_runs.push(run);
            }
        }

        // Don't forget the last line
        if !current_line_runs.is_empty() {
            let line = self.create_text_line(current_line_runs);
            lines.push(line);
        }

        self.assembled_lines.extend(lines);
    }

    /// Calculate body font size from statistics (using median for robustness)
    fn calculate_body_font_size(&mut self) {
        if self.stats.font_sizes.is_empty() {
            return;
        }

        // Use median as body font size (more robust than mean)
        let mut sizes = self.stats.font_sizes.clone();
        sizes.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let median_idx = sizes.len() / 2;
        self.stats.body_font_size = Some(sizes[median_idx]);
    }

    /// Calculate statistics for adaptive heuristics
    fn calculate_statistics(&mut self) {
        // Collect font sizes
        for line in &self.assembled_lines {
            self.stats.font_sizes.push(line.avg_font_size);
        }

        // Calculate body font size
        self.calculate_body_font_size();

        // DEBUG: Log font size statistics
        if let Some(body_size) = self.stats.body_font_size {
            log::debug!("Body font size (median): {}", body_size);
            if let Some(&max_size) = self.stats.font_sizes.iter()
                .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)) {
                log::debug!("Max font size: {}", max_size);
                log::debug!("Ratio: {}", max_size / body_size);
            }
        }

        // Calculate page dimensions from line positions
        if !self.assembled_lines.is_empty() {
            let max_x = self.assembled_lines.iter()
                .flat_map(|line| line.runs.iter())
                .map(|run| run.x + run.width)
                .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
                .unwrap_or(0.0);

            self.stats.page_width = max_x * 1.1; // Add 10% margin

            let max_y = self.assembled_lines.iter()
                .map(|line| line.y)
                .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
                .unwrap_or(0.0);

            self.stats.page_height = max_y * 1.1;
        }

        // Collect line spacings
        for i in 1..self.assembled_lines.len() {
            let spacing = (self.assembled_lines[i].y - self.assembled_lines[i-1].y).abs();
            self.stats.line_spacings.push(spacing);
        }

        // Calculate average spacing
        if !self.stats.line_spacings.is_empty() {
            self.stats.avg_line_spacing =
                self.stats.line_spacings.iter().sum::<f64>() / self.stats.line_spacings.len() as f64;
        }

        // Collect indent levels (cluster)
        let mut indent_map: HashMap<i32, f64> = HashMap::new();
        for line in &self.assembled_lines {
            let rounded = (line.x_start / 10.0).round() as i32;
            indent_map.entry(rounded).or_insert(line.x_start);
        }
        self.stats.indent_levels = indent_map.values().copied().collect();
    }

    /// Detect if a line is a heading based on font size
    /// Detect if line is a heading using multiple heuristics
    fn detect_heading(&self, line: &TextLine, line_index: usize) -> Option<u8> {
        let text = self.line_to_text(line);
        let trimmed = text.trim();

        // Exclude patterns that shouldn't be headings

        // 1. Page numbers: "1/5", "4/5", etc.
        if trimmed.contains('/') && trimmed.len() <= 5 {
            let parts: Vec<&str> = trimmed.split('/').collect();
            if parts.len() == 2 && parts.iter().all(|p| p.trim().parse::<u32>().is_ok()) {
                return None;
            }
        }

        // 2. Pure numbers or amounts: "150k", "300k", "100%", "-50 000"
        if trimmed.len() < 30 {
            let num_chars = trimmed.chars().filter(|c| c.is_numeric()).count();
            if num_chars as f64 / trimmed.len() as f64 > 0.5 {
                return None;
            }
        }

        // 3. Too short: require minimum length
        if trimmed.len() < 5 {
            return None;
        }

        let mut score = 0u8;
        let body_size = self.stats.body_font_size.unwrap_or(12.0);
        let size_ratio = line.avg_font_size / body_size;

        // Heuristic 1: Font size increase
        if size_ratio >= 2.0 {
            score += 6;
        } else if size_ratio >= 1.7 {
            score += 5;
        } else if size_ratio >= 1.5 {
            score += 4;
        } else if size_ratio >= 1.35 {
            score += 3;
        } else if size_ratio >= 1.25 {
            score += 2;
        } else if size_ratio >= 1.1 {
            score += 1;
        }

        // Heuristic 2: Bold text
        if self.config.enable_bold_heading_detection && line.dominant_style.is_bold {
            score += 2;
        }

        // Heuristic 3: All caps
        if self.config.enable_allcaps_heading_detection && self.is_line_all_caps(line) {
            score += 2;
        }

        // Heuristic 4: Centered
        if self.config.enable_centered_heading_detection && self.is_line_centered(line) {
            score += 1;
        }

        // Heuristic 5: Whitespace context
        let (spacing_before, spacing_after) = self.get_surrounding_spacing(line_index);
        let avg_spacing = self.stats.avg_line_spacing;

        if avg_spacing > 0.0 {
            if spacing_before > avg_spacing * 2.0 {
                score += 1;
            }
            if spacing_after > avg_spacing * 1.5 {
                score += 1;
            }
        }

        // Heuristic 6: Short line (not full width)
        let line_width = self.estimate_line_width(line);
        if line_width < self.stats.page_width * self.config.max_heading_line_length_ratio {
            score += 1;
        }

        // Check threshold
        if score < self.config.heading_confidence_threshold {
            return None;
        }

        // Assign heading level based on score and size
        let level = if score >= 8 || size_ratio >= 2.0 {
            1
        } else if score >= 6 || size_ratio >= 1.7 {
            2
        } else if score >= 5 || size_ratio >= 1.5 {
            3
        } else if score >= 4 || size_ratio >= 1.35 {
            4
        } else if score >= 3 || size_ratio >= 1.25 {
            5
        } else {
            6
        };

        // Clamp to configured max level
        Some(level.min(self.config.max_heading_level))
    }

    /// Calculate nesting level based on indentation
    fn calculate_nesting_level(&self, x_start: f64) -> usize {
        if self.stats.indent_levels.is_empty() {
            return 0;
        }

        // Find closest indent level
        let mut levels = self.stats.indent_levels.clone();
        levels.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        for (idx, &level) in levels.iter().enumerate() {
            if (x_start - level).abs() < self.config.list_indent_threshold {
                return idx;
            }
        }

        0
    }

    /// Match ordered list patterns using regex
    fn match_ordered_list_pattern(&self, text: &str) -> Option<BulletType> {
        use regex::Regex;
        lazy_static::lazy_static! {
            static ref ORDERED_PATTERN: Regex = Regex::new(
                r"^(\(?\d+[\.\)]|\(?[a-z][\.\)]|\(?[A-Z][\.\)]|\(?[ivxlcdm]+[\.\)])"
            ).unwrap();
        }

        if ORDERED_PATTERN.is_match(text) {
            Some(BulletType::Ordered)
        } else {
            None
        }
    }

    /// Detect if a line is a list item
    fn detect_list_item(&self, line: &TextLine) -> Option<(BulletType, usize)> {
        let text = self.line_to_text(line);
        let trimmed = text.trim_start();

        if trimmed.is_empty() {
            return None;
        }

        let first_char = trimmed.chars().next()?;

        // Check for unordered list bullets
        if self.config.bullet_chars.contains(&first_char) {
            let nest_level = self.calculate_nesting_level(line.x_start);
            return Some((BulletType::Unordered, nest_level));
        }

        // Check for ordered list patterns: "1.", "a)", "(i)", etc.
        if let Some(bullet_type) = self.match_ordered_list_pattern(trimmed) {
            let nest_level = self.calculate_nesting_level(line.x_start);
            return Some((bullet_type, nest_level));
        }

        None
    }

    /// Convert a text line to markdown with emphasis
    fn line_to_markdown(&self, line: &TextLine) -> String {
        let mut result = String::new();
        let baseline_style = line.dominant_style;

        for run in &line.runs {
            let text = &run.text;

            // Apply emphasis if style differs from baseline
            let needs_bold = run.font_style.is_bold && !baseline_style.is_bold;
            let needs_italic = run.font_style.is_italic && !baseline_style.is_italic;
            let needs_code = run.font_style.is_monospace && !baseline_style.is_monospace;

            let formatted = if needs_code {
                format!("`{}`", text)
            } else if needs_bold && needs_italic {
                format!("***{}***", text)
            } else if needs_bold {
                format!("**{}**", text)
            } else if needs_italic {
                format!("*{}*", text)
            } else {
                text.clone()
            };

            result.push_str(&formatted);
        }

        result
    }

    /// Cluster X positions into columns
    fn cluster_columns(&self, x_positions: &[f64]) -> Vec<f64> {
        if x_positions.is_empty() {
            return Vec::new();
        }

        let mut sorted = x_positions.to_vec();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let tolerance = self.config.table_column_tolerance;
        let mut clusters = Vec::new();
        let mut current_cluster = vec![sorted[0]];

        for &pos in sorted.iter().skip(1) {
            if pos - current_cluster.last().unwrap() <= tolerance {
                current_cluster.push(pos);
            } else {
                // Finalize current cluster (use median)
                let median_idx = current_cluster.len() / 2;
                clusters.push(current_cluster[median_idx]);
                current_cluster = vec![pos];
            }
        }

        // Don't forget last cluster
        if !current_cluster.is_empty() {
            let median_idx = current_cluster.len() / 2;
            clusters.push(current_cluster[median_idx]);
        }

        clusters
    }

    /// Validate that a potential table region has valid content characteristics
    fn is_valid_table_region(&self, start: usize, end: usize) -> bool {
        let row_count = end - start + 1;

        // Reject if too few rows
        if row_count < self.config.min_table_rows {
            return false;
        }

        // Check each row for valid table characteristics
        let mut rows_with_multiple_cells = 0;
        let mut total_cell_length = 0;
        let mut non_empty_cell_count = 0;
        let mut max_columns = 0;

        for i in start..=end {
            let line = &self.assembled_lines[i];

            if line.runs.len() > max_columns {
                max_columns = line.runs.len();
            }

            // Count runs (potential cells)
            if line.runs.len() >= self.config.min_table_columns {
                rows_with_multiple_cells += 1;

                // Check cell content quality (only non-empty cells)
                for run in &line.runs {
                    let trimmed = run.text.trim();
                    if !trimmed.is_empty() {
                        total_cell_length += trimmed.len();
                        non_empty_cell_count += 1;
                    }
                }
            }
        }

        // Require most rows to have enough columns
        let multi_column_ratio = rows_with_multiple_cells as f64 / row_count as f64;
        if multi_column_ratio < 0.7 {  // 70% of rows must have multiple columns (relaxed from 80%)
            return false;
        }

        // Reject character-level tables: many columns AND very short cells
        if max_columns > 8 && non_empty_cell_count > 0 {
            let avg_cell_length = total_cell_length as f64 / non_empty_cell_count as f64;
            if avg_cell_length < 1.5 {  // Character-level tables have avg ~1.0
                return false;
            }
        }

        true
    }

    /// Detect table regions in assembled lines
    /// Legacy table detection using column clustering
    fn detect_table_regions_legacy(&self) -> Vec<(usize, usize)> {
        let mut regions = Vec::new();
        let mut i = 0;

        while i < self.assembled_lines.len() {
            // Check if line could start a table
            if self.assembled_lines[i].runs.len() >= self.config.min_table_columns {
                // Look ahead for consistent column pattern
                let mut end = i;
                let columns = self.extract_columns_from_line(&self.assembled_lines[i]);

                // Scan consecutive lines for alignment
                while end + 1 < self.assembled_lines.len() {
                    let next_line = &self.assembled_lines[end + 1];
                    if self.line_aligns_with_columns(next_line, &columns) {
                        end += 1;
                    } else {
                        break;
                    }
                }

                // Valid table if enough rows AND passes content validation
                let row_count = end - i + 1;
                if row_count >= self.config.min_table_rows && self.is_valid_table_region(i, end) {
                    regions.push((i, end));
                    i = end + 1;
                    continue;
                }
            }
            i += 1;
        }

        regions
    }

    /// Stream mode table detection using Nurminen's algorithm
    fn detect_table_regions_stream(&mut self) -> Vec<(usize, usize)> {
        log::debug!("Using Stream mode table detection");

        // PRE-CONSOLIDATE: Merge character-level runs into word-level runs
        let consolidated_lines = self.pre_consolidate_lines_for_table_detection(&self.assembled_lines);

        let total_runs_before: usize = self.assembled_lines.iter().map(|l| l.runs.len()).sum();
        let total_runs_after: usize = consolidated_lines.iter().map(|l| l.runs.len()).sum();

        log::debug!(
            "Pre-consolidation: {} lines, {} runs before, {} runs after (reduction: {:.1}%)",
            self.assembled_lines.len(),
            total_runs_before,
            total_runs_after,
            if total_runs_before > 0 {
                (1.0 - total_runs_after as f64 / total_runs_before as f64) * 100.0
            } else {
                0.0
            }
        );

        // Create StreamTableDetector with consolidated lines
        let detector = StreamTableDetector::new(
            self.config.stream_config.clone(),
            &consolidated_lines,  // Use consolidated, not original
        );

        // Call detect_tables()
        match detector.detect_tables() {
            Ok(table_regions) => {
                log::debug!("Stream mode found {} table region(s)", table_regions.len());

                // Convert TableRegion to (start_line, end_line) tuples
                let line_ranges = self.convert_table_regions_to_line_ranges(&table_regions);

                // Store the full TableRegion data for later use in extract_table()
                self.stream_table_regions = table_regions;

                line_ranges
            }
            Err(e) => {
                log::warn!("Stream table detection failed: {}", e);
                Vec::new()
            }
        }
    }

    /// Convert TableRegion bounding boxes to line index ranges
    fn convert_table_regions_to_line_ranges(&self, regions: &[TableRegion]) -> Vec<(usize, usize)> {
        let mut line_ranges = Vec::new();

        for region in regions {
            log::debug!(
                "Table region: bbox=({:.1}, {:.1}) to ({:.1}, {:.1}), confidence={:.2}",
                region.bbox.x_min,
                region.bbox.y_min,
                region.bbox.x_max,
                region.bbox.y_max,
                region.confidence
            );

            // Find lines within this table's bounding box by y-coordinate
            let mut start_line: Option<usize> = None;
            let mut end_line: Option<usize> = None;

            for (idx, line) in self.assembled_lines.iter().enumerate() {
                // Check if line's y-coordinate is within the table region
                if line.y >= region.bbox.y_min && line.y <= region.bbox.y_max {
                    if start_line.is_none() {
                        start_line = Some(idx);
                    }
                    end_line = Some(idx);
                }
            }

            if let (Some(start), Some(end)) = (start_line, end_line) {
                log::debug!("  Mapped to lines {} to {}", start, end);
                line_ranges.push((start, end));
            } else {
                log::warn!("Could not map table region to line ranges");
            }
        }

        line_ranges
    }

    /// Extract column positions from a line's runs
    fn extract_columns_from_line(&self, line: &TextLine) -> Vec<f64> {
        let x_positions: Vec<f64> = line.runs.iter().map(|r| r.x).collect();
        self.cluster_columns(&x_positions)
    }

    /// Check if line's runs align with given columns
    fn line_aligns_with_columns(&self, line: &TextLine, columns: &[f64]) -> bool {
        let tolerance = self.config.table_column_tolerance;
        let mut aligned_count = 0;

        for run in &line.runs {
            for &col_x in columns {
                if (run.x - col_x).abs() <= tolerance {
                    aligned_count += 1;
                    break;
                }
            }
        }

        // Require 65% of columns to be present (relaxed from 50% baseline)
        aligned_count >= (columns.len() * 13 + 19) / 20
    }

    /// Extract table from line range
    fn extract_table(&self, start: usize, end: usize) -> Block {
        // Check if we have Stream mode data for this table range
        // Find TableRegion that matches this line range by comparing y-coordinates
        let stream_region = self.stream_table_regions.iter().find(|region| {
            // Get y-coordinates of start and end lines
            if start >= self.assembled_lines.len() || end >= self.assembled_lines.len() {
                return false;
            }
            let start_y = self.assembled_lines[start].y;
            let end_y = self.assembled_lines[end].y;

            // Check if this region's bbox encompasses these lines
            region.bbox.y_min <= start_y.max(end_y) && region.bbox.y_max >= start_y.min(end_y)
        });

        if let Some(region) = stream_region {
            // Use Stream mode structured cell data
            log::debug!("Using Stream mode structured cells for table (lines {} to {})", start, end);

            let rows: Vec<TableRow> = region.cells.iter().map(|row_cells| {
                TableRow {
                    cells: row_cells.iter().map(|cell| cell.content.clone()).collect(),
                    y: row_cells.first().map(|c| c.bbox.y_min).unwrap_or(0.0),
                }
            }).collect();

            // Detect header row (first row that's bold or all caps)
            let header_row_index = (start..=end).position(|idx| {
                let line = &self.assembled_lines[idx];
                line.dominant_style.is_bold || self.is_line_all_caps(line)
            });

            Block::Table {
                rows,
                column_positions: region.columns.clone(),
                header_row_index,
            }
        } else {
            // Fall back to legacy algorithm
            log::debug!("Using legacy algorithm for table (lines {} to {})", start, end);

            // Collect all X positions to determine final columns
            let mut all_x_positions = Vec::new();
            for i in start..=end {
                for run in &self.assembled_lines[i].runs {
                    all_x_positions.push(run.x);
                }
            }

            let columns = self.cluster_columns(&all_x_positions);
            let mut rows = Vec::new();

            // Extract cells for each row
            for i in start..=end {
                let line = &self.assembled_lines[i];
                let cells = self.extract_row_cells(line, &columns);
                rows.push(TableRow {
                    cells,
                    y: line.y,
                });
            }

            // Detect header row (first row that's bold or all caps)
            let header_row_index = (start..=end).position(|idx| {
                let line = &self.assembled_lines[idx];
                line.dominant_style.is_bold || self.is_line_all_caps(line)
            });

            Block::Table {
                rows,
                column_positions: columns,
                header_row_index,
            }
        }
    }

    /// Extract cell content for each column in a row
    fn extract_row_cells(&self, line: &TextLine, columns: &[f64]) -> Vec<String> {
        let tolerance = self.config.table_column_tolerance;
        let mut cells = vec![String::new(); columns.len()];

        for run in &line.runs {
            // Find which column this run belongs to
            if let Some(col_idx) = columns.iter().position(|&col_x| {
                (run.x - col_x).abs() <= tolerance
            }) {
                if !cells[col_idx].is_empty() {
                    cells[col_idx].push(' ');
                }
                cells[col_idx].push_str(&run.text);
            }
        }

        cells
    }

    /// Analyze assembled lines and create blocks
    fn analyze_blocks(&mut self) {
        if self.assembled_lines.is_empty() {
            return;
        }

        // First pass: calculate statistics
        self.calculate_statistics();

        let mut blocks = Vec::new();

        // Detect table regions based on detection mode
        let table_regions = if self.config.enable_table_detection {
            match self.config.table_detection_mode {
                TableDetectionMode::Disabled => {
                    log::debug!("Table detection disabled");
                    Vec::new()
                }
                TableDetectionMode::Legacy => {
                    log::debug!("Using legacy table detection");
                    self.detect_table_regions_legacy()
                }
                TableDetectionMode::StreamMode => {
                    log::debug!("Using Stream mode table detection");
                    self.detect_table_regions_stream()
                }
            }
        } else {
            Vec::new()
        };

        // Mark which lines are part of tables
        let mut in_table = vec![false; self.assembled_lines.len()];
        for (start, end) in &table_regions {
            for i in *start..=*end {
                in_table[i] = true;
            }
        }

        let mut current_paragraph_lines: Vec<String> = Vec::new();
        let mut last_y = self.assembled_lines[0].y;
        let mut last_indent = self.assembled_lines[0].x_start;
        let mut i = 0;

        while i < self.assembled_lines.len() {
            // If this line starts a table, extract the table and skip to after it
            if in_table[i] {
                // Flush any current paragraph
                if !current_paragraph_lines.is_empty() {
                    blocks.push(Block::Paragraph {
                        lines: std::mem::take(&mut current_paragraph_lines),
                        indentation: last_indent,
                    });
                }

                // Find the table region containing this line
                if let Some(&(start, end)) = table_regions.iter().find(|(s, _)| *s == i) {
                    let table = self.extract_table(start, end);
                    blocks.push(table);
                    i = end + 1;

                    // Update tracking variables if there are more lines
                    if i < self.assembled_lines.len() {
                        last_y = self.assembled_lines[i].y;
                        last_indent = self.assembled_lines[i].x_start;
                    }
                    continue;
                } else {
                    // Shouldn't happen, but skip this line if we can't find the region
                    i += 1;
                    continue;
                }
            }

            // Normal line processing (headings, lists, paragraphs)
            let line = &self.assembled_lines[i];
            let y_gap = (line.y - last_y).abs();
            let is_large_gap = y_gap > line.avg_font_size * self.config.paragraph_spacing_threshold;
            let indent_changed = (line.x_start - last_indent).abs() > self.config.list_indent_threshold;

            // Check for heading
            if let Some(level) = self.detect_heading(line, i) {
                // Flush current paragraph
                if !current_paragraph_lines.is_empty() {
                    blocks.push(Block::Paragraph {
                        lines: std::mem::take(&mut current_paragraph_lines),
                        indentation: last_indent,
                    });
                }

                blocks.push(Block::Heading {
                    level,
                    text: self.line_to_markdown(line),
                });

                last_y = line.y;
                last_indent = line.x_start;
                i += 1;
                continue;
            }

            // Check for list item
            if let Some((bullet_type, nest_level)) = self.detect_list_item(line) {
                // Flush paragraph
                if !current_paragraph_lines.is_empty() {
                    blocks.push(Block::Paragraph {
                        lines: std::mem::take(&mut current_paragraph_lines),
                        indentation: last_indent,
                    });
                }

                blocks.push(Block::ListItem {
                    bullet_type,
                    indentation: line.x_start,
                    content: self.line_to_markdown(line),
                    nested_level: nest_level,
                });

                last_y = line.y;
                last_indent = line.x_start;
                i += 1;
                continue;
            }

            // Check for paragraph break
            if is_large_gap || indent_changed {
                if !current_paragraph_lines.is_empty() {
                    blocks.push(Block::Paragraph {
                        lines: std::mem::take(&mut current_paragraph_lines),
                        indentation: last_indent,
                    });
                }
                last_indent = line.x_start;
            }

            // Accumulate into current paragraph
            current_paragraph_lines.push(self.line_to_markdown(line));
            last_y = line.y;
            i += 1;
        }

        // Flush final paragraph
        if !current_paragraph_lines.is_empty() {
            blocks.push(Block::Paragraph {
                lines: current_paragraph_lines,
                indentation: last_indent,
            });
        }

        // Second pass: detect code blocks if enabled
        if self.config.detect_code_blocks {
            self.blocks = self.detect_code_blocks_in_blocks(blocks);
        } else {
            self.blocks = blocks;
        }
    }

    /// Post-process blocks to detect code blocks
    fn detect_code_blocks_in_blocks(&self, blocks: Vec<Block>) -> Vec<Block> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < blocks.len() {
            // Check if this is a paragraph with monospace font
            if let Block::Paragraph { ref lines, .. } = blocks[i] {
                // Count consecutive monospace paragraphs
                let mut mono_count = 0;
                let mut mono_lines = Vec::new();

                let mut j = i;
                while j < blocks.len() {
                    if let Block::Paragraph { ref lines, .. } = blocks[j] {
                        // Check if all lines in this paragraph look like code
                        let all_mono = lines.iter().all(|line| {
                            // Simple heuristic: contains backticks or looks like code
                            line.starts_with("    ") || line.contains("`")
                        });

                        if all_mono || mono_count > 0 {
                            mono_lines.extend(lines.clone());
                            mono_count += 1;
                            j += 1;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                // If we found enough consecutive blocks, make it a code block
                if mono_count >= self.config.min_code_block_lines {
                    result.push(Block::CodeBlock {
                        lines: mono_lines,
                    });
                    i = j;
                    continue;
                }
            }

            // Not a code block, add as-is
            result.push(blocks[i].clone());
            i += 1;
        }

        result
    }

    /// Write accumulated blocks as markdown
    fn write_markdown(&mut self) -> Result<(), OutputError> {
        use std::fmt::Write;

        for (idx, block) in self.blocks.iter().enumerate() {
            match block {
                Block::Heading { level, text } => {
                    let prefix = "#".repeat(*level as usize);
                    writeln!(self.writer, "{} {}", prefix, text)?;
                    writeln!(self.writer)?;  // Blank line after heading
                }

                Block::Paragraph { lines, .. } => {
                    let paragraph = lines.join(" ");
                    writeln!(self.writer, "{}", paragraph)?;
                    writeln!(self.writer)?;  // Blank line after paragraph
                }

                Block::ListItem { bullet_type, content, nested_level, .. } => {
                    let indent = "  ".repeat(*nested_level);
                    let marker = match bullet_type {
                        BulletType::Unordered => "-",
                        BulletType::Ordered => "1.",  // Could track counter
                    };
                    writeln!(self.writer, "{}{} {}", indent, marker, content)?;

                    // Don't add blank line between list items of same level
                    if let Some(next) = self.blocks.get(idx + 1) {
                        if !matches!(next, Block::ListItem { .. }) {
                            writeln!(self.writer)?;
                        }
                    }
                }

                Block::CodeBlock { lines } => {
                    writeln!(self.writer, "```")?;
                    for line in lines {
                        writeln!(self.writer, "{}", line)?;
                    }
                    writeln!(self.writer, "```")?;
                    writeln!(self.writer)?;
                }

                Block::Table { rows, header_row_index, .. } => {
                    // Markdown table format
                    if rows.is_empty() {
                        continue;
                    }

                    // Write header row (if detected) or first row
                    let header_idx = header_row_index.unwrap_or(0);
                    write!(self.writer, "|")?;
                    for cell in &rows[header_idx].cells {
                        write!(self.writer, " {} |", cell.trim())?;
                    }
                    writeln!(self.writer)?;

                    // Write separator
                    write!(self.writer, "|")?;
                    for _ in 0..rows[header_idx].cells.len() {
                        write!(self.writer, " --- |")?;
                    }
                    writeln!(self.writer)?;

                    // Write data rows
                    let start_row = if header_row_index.is_some() { header_idx + 1 } else { 1 };
                    for row in &rows[start_row..] {
                        write!(self.writer, "|")?;
                        for cell in &row.cells {
                            write!(self.writer, " {} |", cell.trim())?;
                        }
                        writeln!(self.writer)?;
                    }

                    writeln!(self.writer)?; // Blank line after table
                }
            }
        }

        Ok(())
    }
}

// Implement OutputDev trait
impl<W: ConvertToFmt> OutputDev for MarkdownOutput<W> {
    fn begin_page(&mut self, page_num: u32, media_box: &MediaBox, _art_box: Option<(f64, f64, f64, f64)>)
        -> Result<(), OutputError>
    {
        self.current_page = page_num;
        self.flip_ctm = Transform2D::row_major(1., 0., 0., -1., 0., media_box.ury - media_box.lly);

        // Reset page-level state
        self.run_buffer.clear();
        self.assembled_lines.clear();
        self.blocks.clear();
        self.stats = PageStatistics::default();
        self.char_counter = 0;
        self.current_run_text.clear();
        self.last_x = 0.0;
        self.last_y = 0.0;
        self.run_start_x = 0.0;
        self.run_start_y = 0.0;

        Ok(())
    }

    fn end_page(&mut self) -> Result<(), OutputError> {
        // Flush any remaining run
        self.flush_current_run();

        // Process accumulated runs into lines
        self.assemble_lines();

        // Analyze lines into blocks
        self.analyze_blocks();

        // Generate markdown output
        self.write_markdown()?;

        Ok(())
    }

    fn output_character(&mut self, trm: &Transform, width: f64, _spacing: f64,
                       font_size: f64, char: &str) -> Result<(), OutputError>
    {
        let position = trm.post_transform(&self.flip_ctm);
        let transformed_font_size_vec = trm.transform_vector(vec2(font_size, font_size));
        let transformed_font_size = (transformed_font_size_vec.x * transformed_font_size_vec.y).sqrt();
        let (x, y) = (position.m31, position.m32);

        // Check if we should start a new run
        let should_split = self.should_start_new_run(x, y, transformed_font_size);

        if should_split {
            self.flush_current_run();
            self.last_x = x;
            self.last_y = y;
            self.run_start_x = x;  // Capture START of new run
            self.run_start_y = y;
            self.current_font_size = transformed_font_size;
        }

        // First character of a run - capture start position
        if self.current_run_text.is_empty() {
            self.run_start_x = x;
            self.run_start_y = y;
        }

        // Accumulate character
        self.current_run_text.push_str(char);
        self.last_x = x + width * transformed_font_size;
        self.char_counter += 1;

        Ok(())
    }

    fn begin_word(&mut self) -> Result<(), OutputError> {
        Ok(())
    }

    fn end_word(&mut self) -> Result<(), OutputError> {
        Ok(())
    }

    fn end_line(&mut self) -> Result<(), OutputError> {
        self.flush_current_run();
        Ok(())
    }

    fn set_font(&mut self, font_name: &str) -> Result<(), OutputError> {
        self.current_font_style = FontStyle::from_font_name(font_name);
        Ok(())
    }
}


pub fn print_metadata(doc: &Document) {
    dlog!("Version: {}", doc.version);
    if let Some(ref info) = get_info(&doc) {
        for (k, v) in *info {
            match v {
                &Object::String(ref s, StringFormat::Literal) => { dlog!("{}: {}", pdf_to_utf8(k), pdf_to_utf8(s)); }
                _ => {}
            }
        }
    }
    dlog!("Page count: {}", get::<i64>(&doc, &get_pages(&doc), b"Count"));
    dlog!("Pages: {:?}", get_pages(&doc));
    dlog!("Type: {:?}", get_pages(&doc).get(b"Type").and_then(|x| x.as_name()).unwrap());
}

/// Extract the text from a pdf at `path` and return a `String` with the results
pub fn extract_text<P: std::convert::AsRef<std::path::Path>>(path: P) -> Result<String, OutputError> {
    let mut s = String::new();
    {
        let mut output = PlainTextOutput::new(&mut s);
        let mut doc = Document::load(path)?;
        maybe_decrypt(&mut doc)?;
        output_doc(&doc, &mut output)?;
    }
    Ok(s)
}

fn maybe_decrypt(doc: &mut Document) -> Result<(), OutputError> {
    if ! doc.is_encrypted() {
        return Ok(());
    }

    if let Err(e) = doc.decrypt("") {
        if let Error::Decryption(DecryptionError::IncorrectPassword) = e {
            error!("Encrypted documents must be decrypted with a password using {{extract_text|extract_text_from_mem|output_doc}}_encrypted")
        }

        return Err(OutputError::PdfError(e));
    }

    Ok(())
}

pub fn extract_text_encrypted<P: std::convert::AsRef<std::path::Path>>(
    path: P,
    password: &str,
) -> Result<String, OutputError> {
    let mut s = String::new();
    {
        let mut output = PlainTextOutput::new(&mut s);
        let mut doc = Document::load(path)?;
        output_doc_encrypted(&mut doc, &mut output, password)?;
    }
    Ok(s)
}

pub fn extract_text_from_mem(buffer: &[u8]) -> Result<String, OutputError> {
    let mut s = String::new();
    {
        let mut output = PlainTextOutput::new(&mut s);
        let mut doc = Document::load_mem(buffer)?;
        maybe_decrypt(&mut doc)?;
        output_doc(&doc, &mut output)?;
    }
    Ok(s)
}

pub fn extract_text_from_mem_encrypted(
    buffer: &[u8],
    password: &str,
) -> Result<String, OutputError> {
    let mut s = String::new();
    {
        let mut output = PlainTextOutput::new(&mut s);
        let mut doc = Document::load_mem(buffer)?;
        output_doc_encrypted(&mut doc, &mut output, password)?;
    }
    Ok(s)
}


fn extract_text_by_page(doc: &Document, page_num: u32) -> Result<String, OutputError> {
    let mut s = String::new();
    {
        let mut output = PlainTextOutput::new(&mut s);
        output_doc_page(doc, &mut output, page_num)?;
    }
    Ok(s)
}

/// Extract the text from a pdf at `path` and return a `Vec<String>` with the results separately by page

pub fn extract_text_by_pages<P: std::convert::AsRef<std::path::Path>>(path: P) -> Result<Vec<String>, OutputError> {
    let mut v = Vec::new();
    {
        let mut doc = Document::load(path)?;
        maybe_decrypt(&mut doc)?;
        let mut page_num = 1;
        while let Ok(content) = extract_text_by_page(&doc, page_num) {
            v.push(content);
            page_num += 1;
        }
    }
    Ok(v)
}

pub fn extract_text_by_pages_encrypted<P: std::convert::AsRef<std::path::Path>>(path: P, password: &str) -> Result<Vec<String>, OutputError> {
    let mut v = Vec::new();
    {
        let mut doc = Document::load(path)?;
        doc.decrypt(password)?;
        let mut page_num = 1;
        while let Ok(content) = extract_text_by_page(&mut doc, page_num) {
            v.push(content);
            page_num += 1;
        }
    }
    Ok(v)
}

pub fn extract_text_from_mem_by_pages(buffer: &[u8]) -> Result<Vec<String>, OutputError> {
    let mut v = Vec::new();
    {
        let mut doc = Document::load_mem(buffer)?;
        maybe_decrypt(&mut doc)?;
        let mut page_num = 1;
        while let Ok(content) = extract_text_by_page(&doc, page_num) {
            v.push(content);
            page_num += 1;
        }
    }
    Ok(v)
}

pub fn extract_text_from_mem_by_pages_encrypted(buffer: &[u8], password: &str) -> Result<Vec<String>, OutputError> {
    let mut v = Vec::new();
    {
        let mut doc = Document::load_mem(buffer)?;
        doc.decrypt(password)?;
        let mut page_num = 1;
        while let Ok(content) = extract_text_by_page(&doc, page_num) {
            v.push(content);
            page_num += 1;
        }
    }
    Ok(v)
}

// ==================== Markdown Extraction Public API ====================

/// Extract markdown from a PDF file
///
/// This function extracts text from a PDF file and formats it as markdown,
/// preserving document structure (headings, lists, emphasis, etc.) for optimal
/// LLM consumption.
///
/// # Arguments
///
/// * `path` - Path to the PDF file
///
/// # Returns
///
/// * `Result<String, OutputError>` - The extracted markdown content
///
/// # Example
///
/// ```no_run
/// use pdf_extract::extract_markdown;
///
/// let markdown = extract_markdown("document.pdf").expect("Failed to extract markdown");
/// println!("{}", markdown);
/// ```
pub fn extract_markdown<P: std::convert::AsRef<std::path::Path>>(path: P) -> Result<String, OutputError> {
    extract_markdown_with_config(path, MarkdownConfig::default())
}

/// Extract markdown from a PDF file with custom configuration
///
/// This function extracts text from a PDF file and formats it as markdown,
/// using custom configuration for heuristic detection thresholds.
///
/// # Arguments
///
/// * `path` - Path to the PDF file
/// * `config` - Configuration for markdown extraction heuristics
///
/// # Returns
///
/// * `Result<String, OutputError>` - The extracted markdown content
///
/// # Example
///
/// ```no_run
/// use pdf_extract::{extract_markdown_with_config, MarkdownConfig};
///
/// let config = MarkdownConfig {
///     max_heading_level: 4,
///     ..Default::default()
/// };
/// let markdown = extract_markdown_with_config("document.pdf", config)
///     .expect("Failed to extract markdown");
/// ```
pub fn extract_markdown_with_config<P: std::convert::AsRef<std::path::Path>>(
    path: P,
    config: MarkdownConfig
) -> Result<String, OutputError> {
    let doc = Document::load(path)?;
    let mut output_string = String::new();
    let mut md_output = MarkdownOutput::with_config(&mut output_string, config);
    output_doc(&doc, &mut md_output)?;
    Ok(output_string)
}


fn get_inherited<'a, T: FromObj<'a>>(doc: &'a Document, dict: &'a Dictionary, key: &[u8]) -> Option<T> {
    let o: Option<T> = get(doc, dict, key);
    if let Some(o) = o {
        Some(o)
    } else {
        let parent = dict.get(b"Parent")
            .and_then(|parent| parent.as_reference())
            .and_then(|id| doc.get_dictionary(id)).ok()?;
        get_inherited(doc, parent, key)
    }
}

pub fn output_doc_encrypted(
    doc: &mut Document,
    output: &mut dyn OutputDev,
    password: &str,
) -> Result<(), OutputError> {
    doc.decrypt(password)?;
    output_doc(doc, output)
}

/// Parse a given document and output it to `output`
pub fn output_doc(doc: &Document, output: &mut dyn OutputDev) -> Result<(), OutputError> {
    if doc.is_encrypted() {
        error!("Encrypted documents must be decrypted with a password using {{extract_text|extract_text_from_mem|output_doc}}_encrypted");
    }
    let empty_resources = Dictionary::new();
    let pages = doc.get_pages();
    let mut p = Processor::new();
    for dict in pages {
        let page_num = dict.0;
        let object_id = dict.1;
        output_doc_inner(page_num, object_id, doc, &mut p, output, &empty_resources)?;
    }
    Ok(())
}

pub fn output_doc_page(doc: &Document, output: &mut dyn OutputDev, page_num: u32) -> Result<(), OutputError> {
    if doc.is_encrypted() {
        error!("Encrypted documents must be decrypted with a password using {{extract_text|extract_text_from_mem|output_doc}}_encrypted");
    }
    let empty_resources = Dictionary::new();
    let pages = doc.get_pages();
    let object_id = pages.get(&page_num).ok_or(lopdf::Error::PageNumberNotFound(page_num))?;
    let mut p = Processor::new();
    output_doc_inner(page_num, *object_id, doc, &mut p, output, &empty_resources)?;
    Ok(())
}

fn output_doc_inner<'a>(page_num: u32, object_id: ObjectId, doc: &'a Document, p: & mut Processor<'a>, output: &mut dyn OutputDev, empty_resources: &'a Dictionary) -> Result<(), OutputError> {
    let page_dict = doc.get_object(object_id).unwrap().as_dict().unwrap();
    dlog!("page {} {:?}", page_num, page_dict);
    // XXX: Some pdfs lack a Resources directory
    let resources = get_inherited(doc, page_dict, b"Resources").unwrap_or(empty_resources);
    dlog!("resources {:?}", resources);
    // pdfium searches up the page tree for MediaBoxes as needed
    let media_box: Vec<f64> = get_inherited(doc, page_dict, b"MediaBox").expect("MediaBox");
    let media_box = MediaBox { llx: media_box[0], lly: media_box[1], urx: media_box[2], ury: media_box[3] };
    let art_box = get::<Option<Vec<f64>>>(&doc, page_dict, b"ArtBox")
        .map(|x| (x[0], x[1], x[2], x[3]));
    output.begin_page(page_num, &media_box, art_box)?;
    p.process_stream(&doc, doc.get_page_content(object_id).unwrap(), resources, &media_box, output, page_num)?;
    output.end_page()?;
    Ok(())
}

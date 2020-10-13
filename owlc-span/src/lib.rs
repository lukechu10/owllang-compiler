//! Utility crate for storing syntax positions.

#![allow(clippy::len_without_is_empty)]

use std::borrow::Cow;
use std::ops::Sub;
use std::rc::Rc;

/// Utility for storing position in source code. Position is represented as the position of the byte in the `SourceFile`. `BytePos` should be 0 based.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(pub u32);

impl BytePos {
    /// Create a new `SpanData` from `self` to `hi`. `lo` is inclusive and `hi` is exclusive.
    pub fn to(self, hi: BytePos) -> Span {
        Span { lo: self, hi }
    }
}
impl Sub for BytePos {
    type Output = u32;
    fn sub(self, other: BytePos) -> u32 {
        self.0 - other.0
    }
}

/// `Span` represents a region of code, used for error reporting. `Span` is represented with the starting and ending position of the region.
/// The starting position is inclusive and the ending position is exclusive.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub lo: BytePos,
    pub hi: BytePos,
}
impl Span {
    /// **Note**: This returns the length in *bytes*, not chars.
    pub fn len(&self) -> u32 {
        self.hi - self.lo
    }
}

/// Use `Rc` to prevent unnecessary duplication of the `String`.
pub type FileName = Rc<str>;

/// Represents 1 `SourceFile`. **NOTE**: A `CompilationUnit` can have multiple `SourceFile`s.
#[derive(Debug)]
pub struct SourceFile {
    /// The file name.
    pub name: FileName,
    /// The complete source code.
    pub src: Rc<str>,
    /// The locations of first char on new lines.
    pub newline_pos: Vec<BytePos>,
}

impl SourceFile {
    /// Creates a new `SourceFile` with the given `src` string and analyzes the file (records positions of newline `\n` characters).
    pub fn new(name: &str, src: &str) -> Self {
        fn get_newline_pos(src: &str) -> Vec<BytePos> {
            let mut pos = vec![BytePos(0)]; // 0 is position of first newline char

            for (index, ch) in src.char_indices() {
                if ch == '\n' {
                    pos.push(BytePos(index as u32 + 1)); // + 1 to get char after newline
                }
            }
            pos
        }

        let newline_pos = get_newline_pos(&src);

        Self {
            name: Rc::from(name),
            src: Rc::from(src),
            newline_pos,
        }
    }

    /// Returns a slice of the source code at line `line_number`. The `line_number` is 0 based.
    pub fn get_line(&self, line_number: usize) -> Option<Cow<'_, str>> {
        fn get_until_newline(src: &str, begin: usize) -> &str {
            let slice = &src[begin..];

            match slice.find('\n') {
                Some(end) => &slice[..end],
                None => &slice,
            }
        }

        let begin = self.newline_pos.get(line_number)?;
        Some(Cow::from(get_until_newline(&self.src, begin.0 as usize)))
    }

    /// Returns the number of lines in the source file.
    pub fn count_lines(&self) -> usize {
        self.newline_pos.len()
    }

    /// Returns the line of the position `pos` in the source file. The returned line number is 0 based. If the `pos` is out of bounds, should return `None`.
    pub fn lookup_line(&self, pos: BytePos) -> Option<usize> {
        if self.count_lines() == 0 || pos.0 as usize >= self.src.len() {
            None
        } else {
            // NOTE: self.newline_pos.binary_search(&pos) returns the index where the key could be inserted if not found.
            match self.newline_pos.binary_search(&pos) {
                Ok(line) => Some(line),
                Err(line) => Some(line - 1),
            }
        }
    }

    /// Returns the col of the position `pos` in the source file. The returned col number is 0 based. If the `pos` is out of bounds, should return `None`.
    pub fn lookup_col(&self, pos: BytePos) -> Option<usize> {
        let line = self.lookup_line(pos)?;
        let line_start = self.newline_pos[line];
        Some((pos - line_start) as usize)
    }
}

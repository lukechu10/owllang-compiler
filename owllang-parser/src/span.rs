use std::borrow::Cow;
use std::rc::Rc;

/// Utility for storing position in source code. Position is represented as the position of the byte in the `SourceFile`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos {
    pub pos: u32,
}

impl BytePos {
    /// Create a new `SpanData` from `self` to `hi`.
    pub fn to(self, hi: BytePos) -> SpanData {
        SpanData { lo: self, hi }
    }
}

/// `SpanData` represents a region of code, used for error reporting. `SpanData` is represented with the starting and ending position of the region.
/// The starting position is inclusive and the ending position is exclusive.
#[derive(Debug, PartialEq, Eq)]
pub struct SpanData {
    lo: BytePos,
    hi: BytePos,
}

/// Use `Rc` to prevent unnecessary duplication of the `String`.
pub type FileName = Rc<String>;

/// Represents 1 `SourceFile`. **NOTE**: A `CompilationUnit` can have multiple `SourceFile`s.
pub struct SourceFile {
    /// The file name.
    pub name: FileName,
    /// The complete source code.
    pub src: Rc<String>,
    /// The locations of newline characters in the source string.
    pub newline_pos: Vec<BytePos>,
}

impl SourceFile {
    /// Creates a new `SourceFile` with the given `src` string and analyzes the file (records positions of newline `\n` characters).
    pub fn new(name: String, src: String) -> Self {
        fn get_newline_pos(src: &String) -> Vec<BytePos> {
            let mut pos = Vec::new();

            for (index, ch) in src.char_indices() {
                if ch == '\n' {
                    pos.push(BytePos { pos: index as u32 });
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
        Some(Cow::from(get_until_newline(&self.src, begin.pos as usize)))
    }

    /// Returns the number of lines in the source file.
    pub fn count_lines(&self) -> usize {
        self.newline_pos.len()
    }

    /// Returns the line of the position `pos` in the source file. The returned line number is 0 based.
    pub fn lookup_line(&self, pos: BytePos) -> Option<usize> {
        if self.count_lines() == 0 {
            None
        } else {
            // NOTE: self.newline_pos.binary_search(&pos) returns the index where the key could be inserted if not found.
            match self.newline_pos.binary_search(&pos) {
                Ok(line) => Some(line),
                Err(line) => Some(line - 1),
            }
        }
    }
}

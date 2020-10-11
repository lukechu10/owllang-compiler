//! Tests for struct `owlc_span::SourceFile`.

use owlc_span::*;

#[test]
fn can_create_sourcefile() {
    let code = "line1\nline2\nline3\nline 4";
    let source = SourceFile::new("<tmp>", code);
    assert_eq!(source.count_lines(), 4);
    assert_eq!(
        source.newline_pos,
        vec![BytePos(0), BytePos(6), BytePos(12), BytePos(18)]
    );

    // get_line()
    assert_eq!(source.get_line(0).unwrap(), "line1");
    assert_eq!(source.get_line(2).unwrap(), "line3");
    assert_eq!(source.get_line(4), None); // NOTE: 4 is out of bounds because get_line is 0 based.

    // lookup_line()
    assert_eq!(source.lookup_line(BytePos(0)).unwrap(), 0);
    assert_eq!(source.lookup_line(BytePos(1)).unwrap(), 0);
    assert_eq!(
        source.lookup_line(BytePos(5)).unwrap(),
        0,
        "newline character should be on previous line"
    );
    assert_eq!(source.lookup_line(BytePos(6)).unwrap(), 1);
    assert_eq!(
        source.lookup_line(BytePos(100)),
        None,
        "out of bounds should return None"
    );
}

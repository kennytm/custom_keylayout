
use std::process::{Command, Stdio};
use std::io::Write;
use std::path::PathBuf;

const TEST_COMPOSE_FILE: &'static str = r#"

XCOMM These comments should be ignored.
XCOMM

XCOMM only "^1" should be recognized here, we don't know the other keys:
<Multi_key> <asciicircum> <1>           : "¹"   onesuperior # SUPERSCRIPT ONE
<Multi_key> <asciicircum> <KP_2>        : "²"   twosuperior # SUPERSCRIPT TWO
<dead_circumflex> <3>                   : "³"   threesuperior # SUPERSCRIPT THREE

XCOMM should give two different entries:
<Multi_key> <c> <s>                     : "š"   U0161 # LATIN SMALL LETTER S WITH CARON
<Multi_key> <c> <S>                     : "Š"   U0160 # LATIN CAPITAL LETTER S WITH CARON

XCOMM should support multiple chars:
<Multi_key> <A> <B> <C> <D> : "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

XCOMM we don't care about spaces:
<Multi_key> <A> <space> : "AA"
<Multi_key> <space> <B> : "BB"
<Multi_key> <space> <space> : "CC"

XCOMM leave backslash intact:
<Multi_key> <n> <b> <s> <p> : "\240"

"#;


const TEST_RESULT: &'static str = "\
¹       ^1
š       cs
Š       cS
ABCDEFGHIJKLMNOPQRSTUVWXYZ ABCD
\\240    nbsp
";

macro_rules! unwrap {
    ($e:expr) => { match $e {
        Ok(x) => x,
        Err(e) => panic!("Unwrap failure: {:?}", e),
    }}
}

#[test]
fn test_invocation() {
    // Specify the location of the EXE file.
    let mut exe_path = PathBuf::from(file!());
    exe_path.pop();
    exe_path.set_file_name("target");
    exe_path.push("debug");
    exe_path.push("parse_compose");

    // Run it.
    let mut process = unwrap!(Command::new(&exe_path)
                                        .stdin(Stdio::piped())
                                        .stdout(Stdio::piped())
                                        .spawn());

    // Feed the compose file.
    {
        let stdin = process.stdin.as_mut().unwrap();
        unwrap!(stdin.write_all(TEST_COMPOSE_FILE.as_bytes()));
    }

    // Read the output.
    let result = unwrap!(process.wait_with_output());
    assert!(result.status.success());

    // Check if the content is correct.
    assert_eq!(&*result.stdout, TEST_RESULT.as_bytes());
}


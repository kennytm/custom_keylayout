// custom_keylayout.rs: Generate OS X *.keylayout for compose key.
// Copyright (C) 2015  Kenny Chan
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// this program.  If not, see <http://www.gnu.org/licenses/>.

#![feature(path, fs, io, core, collections, std_misc)]
#![feature(old_io)]
#![feature(plugin)]
#![plugin(regex_macros, docopt_macros)]

extern crate docopt;
extern crate "rustc-serialize" as rustc_serialize;
extern crate handlebars;
extern crate regex;
extern crate rand;
#[macro_use] extern crate literator;

mod new_stdio;

use handlebars::{Handlebars, Context, Helper, RenderContext, RenderError};
use rustc_serialize::json::{Json, Object};
use rustc_serialize::hex::ToHex;
use rand::{Rng, thread_rng};
use regex::Captures;
use std::path::Path;
use std::fs::File;
use std::io::{Read, BufRead, BufReadExt};
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::num::from_str_radix;
use std::char;
use std::str::from_utf8;
use std::borrow::ToOwned;
use new_stdio::stdin;

docopt!{Args, "
Usage: custom_keylayout [-g GROUP] [-i ID] [-n NAME] [-t TEMPLATE]
       custom_keylayout --help

Generate OS X *.keylayout for compose key. It will read a definition file from
stdin and write the result to stdout.

The definition file format should be like:

á           'a
à           `a
¥           =y
¥           y=
# Start comments with number sign
ಠ_ಠ         disapprove  # multi-char output is allowed
\\x1f4a9     pileofpoo   # escape sequence of the form \\xABCD is supported


Options:
    -g GROUP, --group GROUP     Group ID [default: 126].
    -i ID, --id ID              Keyboard ID.
    -n NAME, --name NAME        Name of the keylayout [default: My New Keylayout].
    -t TEMPLATE, --template TEMPLATE
                                Template XML file to use [default: ./simple-qwerty-en-us.xml].
",
    flag_group: i32,
    flag_id: Option<i32>,
    flag_name: String,
    flag_template: String,
}

//{{{ Escape & Unescape

fn unescape(s: &str) -> String {
    let escape_regex = regex!(r#"\\(?:(?P<oct>[0-7]{1,7})|[xX](?P<hex>[0-9a-fA-F]{1,6})|(?P<special>[\\"nrt]))"#);
    escape_regex.replace_all(s, |captures: &Captures| {
        for group_name in &["oct", "hex", "special"] {
            if let Some(content) = captures.name(group_name) {
                let c = match *group_name {
                    "oct" => char::from_u32(from_str_radix(content, 8).unwrap()).expect("character code too large"),
                    "hex" => char::from_u32(from_str_radix(content, 16).unwrap()).expect("character code too large"),
                    "special" => match content.char_at(0) {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        c => c,
                    },
                    _ => unreachable!(),
                };
                return c.to_string();
            }
        }
        unreachable!();
    })
}

fn escape_html(s: &str) -> String {
    let escape_regex = regex!("[<>&\"]");
    escape_regex.replace_all(s, |captures: &Captures| {
        match captures.at(0).unwrap() {
            "<" => "&#x003c;",
            ">" => "&#x003e;",
            "&" => "&#x0026;",
            "\"" => "&#x0022;",
            _ => unreachable!(),
        }.to_owned()
    })
}

#[test]
fn test_unescape() {
    assert_eq!(unescape(r"\123\x4aBcg\n\rong\\??"), "\u{53}\u{4abc}g\n\rong\\??");
}

#[test]
fn test_escape_html() {
    assert_eq!(escape_html(r##"<a href="#">&copy;</a>"##),
               "&#x003c;a href=&#x0022;#&#x0022;&#x003e;&#x0026;copy;&#x003c;/a&#x003e;");
}

//}}}

//{{{ FSM

#[derive(Clone, PartialEq, Eq, Debug)]
enum Action {
    Next,
    Output(String),
    OutputGoto(String, Vec<u8>),
}

impl Action {
    fn is_output_goto(&self) -> bool {
        match *self {
            Action::OutputGoto(..) => true,
            _ => false,
        }
    }

    fn output(&self) -> Option<&String> {
        match *self {
            Action::Output(ref s) | Action::OutputGoto(ref s, _) => Some(s),
            Action::Next => None,
        }
    }
}


#[derive(Clone, PartialEq, Eq, Debug)]
struct Fsm {
    actions: Vec<HashMap<Vec<u8>, Action>>,
}

impl Fsm {
    fn new() -> Self {
        Fsm { actions: vec![HashMap::new(); 95] }
    }

    fn insert(&mut self, inputs: &[u8], act: Action) {
        let last_index = inputs.len() - 1;

        // Sort out the intermediate states.
        for i in 0 .. last_index {
            let index = (inputs[i] - 0x20) as usize;
            let source_state_name = inputs[..i].to_vec();
            let ins_result = self.actions[index].insert(source_state_name, Action::Next);
            match ins_result {
                Some(old_act @ Action::Output(_)) => {
                    self.actions[0].insert(inputs[..i+1].to_vec(), old_act);
                }
                Some(old_act @ Action::OutputGoto(..)) => {
                    panic!("Ambiguous state transition for {:?}", old_act);
                }
                _ => {},
            }
        }

        // Do the final output.
        let source_state_name = &inputs[..last_index];
        let index = (inputs[last_index] - 0x20) as usize;

        let (space_action, default_action) = self.actions.split_at_mut(index);
        let space_action = &mut space_action[0];
        let default_action = &mut default_action[0];

        match default_action.entry(source_state_name.to_vec()) {
            Entry::Vacant(vac) => {
                vac.insert(act);
            }
            Entry::Occupied(occ) => {
                match *occ.get() {
                    Action::Next if !act.is_output_goto() => {
                        space_action.insert(inputs.to_vec(), act);
                    }
                    _ => {
                        panic!("Duplicated entry: {:?}", from_utf8(inputs));
                    }
                }
            }
        }
    }

    /// List all intermediate states that have no associated outputs.
    // This should return an Iterator, but without rust-lang/rfcs#105 it is not
    // possible to write out the return type.
    fn intermediate_states(&self) -> HashSet<&[u8]> {
        self.actions[1..].iter().flat_map(|actions| {
            actions.keys().filter_map(|state| {
                if state.len() > 0 && !self.actions[0].contains_key(state) {
                    Some(&**state)
                } else {
                    None
                }
            })
        }).collect()
    }

    fn longest_output_len(&self) -> usize {
        self.actions.iter().flat_map(|actions| {
            actions.values().filter_map(|action| {
                action.output().map(|s| s.utf16_units().count())
            })
        }).max().unwrap_or(1)
    }
}

#[test]
fn test_fsm() {
    let mut fsm = Fsm::new();

    fsm.insert(b"^1", Action::Output("¹".to_owned()));
    fsm.insert(b"^2", Action::Output("²".to_owned()));
    fsm.insert(b"12", Action::Output("½".to_owned()));
    fsm.insert(b"gs", Action::Output("σ".to_owned()));
    fsm.insert(b"gsf", Action::Output("ς".to_owned()));
    fsm.insert(b"gsv", Action::Output("ς".to_owned()));

    let expected_actions_default = container![];
    let expected_actions_circum_and_g = container![
        vec![] => Action::Next,
    ];
    let expected_actions_1 = container![
        vec![] => Action::Next,
        vec![b'^'] => Action::Output("¹".to_owned()),
    ];
    let expected_actions_2 = container![
        vec![b'^'] => Action::Output("²".to_owned()),
        vec![b'1'] => Action::Output("½".to_owned()),
    ];
    let expected_actions_s = container![
        vec![b'g'] => Action::Next,
    ];
    let expected_actions_space = container![
        vec![b'g', b's'] => Action::Output("σ".to_owned()),
    ];
    let expected_actions_f_and_v = container![
        vec![b'g', b's'] => Action::Output("ς".to_owned()),
    ];

    for (i, actions) in fsm.actions.iter().enumerate() {
        let chr = (i + 0x20) as u8 as char;
        let expected_actions = match chr {
            '^' | 'g' => &expected_actions_circum_and_g,
            '1' => &expected_actions_1,
            '2' => &expected_actions_2,
            's' => &expected_actions_s,
            ' ' => &expected_actions_space,
            'f' | 'v' => &expected_actions_f_and_v,
            _ => &expected_actions_default,
        };
        assert!(actions == expected_actions,
                "actions differ at '{}': {:?} != {:?}",
                chr, actions, *expected_actions);
    }

    assert_eq!(fsm.intermediate_states(),
               vec![b"^", b"1", b"g"].into_iter().collect());
}

#[test]
#[should_fail]
fn test_fsm_duplicated() {
    let mut fsm = Fsm::new();
    fsm.insert(b"ab", Action::Output("c".to_owned()));
    fsm.insert(b"ab", Action::Output("d".to_owned()));
}

#[test]
fn test_fsm_longest_output_len() {
    let mut fsm = Fsm::new();
    fsm.insert(b"a", Action::Output("abababa".to_owned()));
    assert_eq!(fsm.longest_output_len(), 7);

    fsm.insert(b"bb", Action::OutputGoto("cdcdcdcd".to_owned(), vec![b'a']));
    assert_eq!(fsm.longest_output_len(), 8);

    fsm.insert(b"ab", Action::Output("lol".to_owned()));
    assert_eq!(fsm.longest_output_len(), 8);

    fsm.insert(b"xyxyxyxyxyxyxyxyxyxyxyxyxyx", Action::Next);
    assert_eq!(fsm.longest_output_len(), 8);

    fsm.insert(b"bc", Action::Output("ｕｔｆ１６🙀🙀".to_owned()));
    assert_eq!(fsm.longest_output_len(), 9);
}

#[test]
#[should_fail]
fn test_fsm_ambig_output_goto_fail() {
    let mut fsm = Fsm::new();
    fsm.insert(b"ab", Action::Output("c".to_owned()));
    fsm.insert(b"a", Action::OutputGoto("d".to_owned(), vec![b'a', b'b']));
}

#[test]
fn test_fsm_ambig_output_goto_ok() {
    let mut fsm = Fsm::new();
    fsm.insert(b"a", Action::Output("c".to_owned()));
    fsm.insert(b"ab", Action::OutputGoto("d".to_owned(), vec![b'a']));
}


//}}}


//{{{ Parse FSM from reader

impl Fsm {
    fn parse<R: BufRead>(reader: R) -> Fsm {
        // Note: Need to use "\t\n\r\v\f " instead of "\s" due to rust-lang/regex#28
        let line_regex = regex!(r"^\s*([^#\t\n\r\v\f ]\S*)\s+([!-~]+)(?:\s+->\s*([!-~]+))?");

        let mut fsm = Fsm::new();
        for line in reader.lines() {
            if let Ok(line) = line {
                if let Some(captures) = line_regex.captures(&line) {
                    let output = unescape(captures.at(1).unwrap());
                    let inputs = captures.at(2).unwrap();
                    let action = match captures.at(3) {
                        Some(s) => Action::OutputGoto(output, s.as_bytes().to_vec()),
                        None => Action::Output(output),
                    };
                    fsm.insert(inputs.as_bytes(), action);
                }
            }
        }

        fsm
    }
}

#[test]
fn test_parse_fsm() {
    let reader = r"

¹       ^1
²       ^2  # comments
# comments are ignored
     # also these
     #
#

ABCDEFGHIJKLMNOPQRSTUVWXYZ ABCD
Æ AB
ṵ   u~
\x2468  \o/
\x1a2Bc o_o
©   oc -> AB

    ".as_bytes();

    let fsm = Fsm::parse(reader);

    let mut expected = Fsm::new();
    expected.insert(b"AB", Action::Output("Æ".to_owned()));
    expected.insert(b"ABCD", Action::Output("ABCDEFGHIJKLMNOPQRSTUVWXYZ".to_owned()));
    expected.insert(b"^1", Action::Output("¹".to_owned()));
    expected.insert(b"^2", Action::Output("²".to_owned()));
    expected.insert(b"\\o/", Action::Output("\u{2468}".to_owned()));
    expected.insert(b"o_o", Action::Output("\u{1a2bc}".to_owned()));
    expected.insert(b"u~", Action::Output("ṵ".to_owned()));
    expected.insert(b"oc", Action::OutputGoto("©".to_owned(), b"AB".to_vec()));
    assert_eq!(fsm, expected);
}

//}}}

//{{{ Convert FSM to JSON for Handlebar.

fn to_json_state_string(state: &[u8]) -> String {
    format!("c-{}", state.to_hex())
}

impl Fsm {
    fn into_json_object(self) -> Object {
        let intermediates = self.intermediate_states().into_iter().map(|s| {
            let state = to_json_state_string(s);
            let output = Json::String(String::from_utf8(s.to_vec()).unwrap());
            (state, output)
        }).collect();

        let actions = self.actions.into_iter().enumerate().map(|(i, states)| {
            let next_action = format!("{:02x}", i + 0x20);

            Json::Object(states.into_iter().map(|entry| {
                let this_state = to_json_state_string(&entry.0);
                let mut result = Object::new();

                if let Some(output) = entry.1.output() {
                    result.insert("output".to_owned(), Json::String(output.clone()));
                }

                let next_state = match entry.1 {
                    Action::Next => Some(format!("{}{}", this_state, next_action)),
                    Action::OutputGoto(_, ref state) => Some(to_json_state_string(state)),
                    _ => None
                };
                if let Some(state) = next_state {
                    result.insert("state".to_owned(), Json::String(state));
                }

                (this_state, Json::Object(result))
            }).collect())
        }).collect();

        container![
            "actions".to_owned() => Json::Array(actions),
            "intermediates".to_owned() => Json::Object(intermediates),
        ]
    }
}

#[test]
fn test_to_json() {
    let fsm = Fsm::parse("
        ´ '
        ˝ ''
        ó 'o
        ő ''o
        Ó 'O
        Ő ''O
        ° oo
        ☺ lol
    ".as_bytes());

    let result = fsm.into_json_object();
    let expected = rustc_serialize::json::Builder::new(concat!(r#"
        {
            "actions": [
                {                           "#, /* space */ r#"
                    "c-27": {"output": "´"},
                    "c-2727": {"output": "˝"}
                },
                {},{},{},{},{},{},          "#, /* !"#$%& */ r#"
                {                           "#, /* ' */ r#"
                    "c-": {"state": "c-27"},
                    "c-27": {"state": "c-2727"}
                },
                {},{},{},{},{},{},{},{},    "#, /* ()*+,-./ */ r#"
                {},{},{},{},{},{},{},{},    "#, /* 01234567 */ r#"
                {},{},{},{},{},{},{},{},    "#, /* 89:;<=>? */ r#"
                {},{},{},{},{},{},{},{},    "#, /* @ABCDEFG */ r#"
                {},{},{},{},{},{},{},       "#, /* HIJKLMN */ r#"
                {                           "#, /* O */ r#"
                    "c-27": {"output": "Ó"},
                    "c-2727": {"output": "Ő"}
                },
                {},{},{},{},{},{},{},{},    "#, /* PQRSTUVW */ r#"
                {},{},{},{},{},{},{},{},    "#, /* XYZ[\]^_ */ r#"
                {},{},{},{},{},{},{},{},    "#, /* `abcdefg */ r#"
                {},{},{},{},                "#, /* hijk */ r#"
                {                           "#, /* l */ r#"
                    "c-": {"state": "c-6c"},
                    "c-6c6f": {"output": "☺"}
                },
                {},{},                      "#, /* mn */ r#"
                {                           "#, /* o */ r#"
                    "c-": {"state": "c-6f"},
                    "c-6f": {"output": "°"},
                    "c-27": {"output": "ó"},
                    "c-2727": {"output": "ő"},
                    "c-6c": {"state": "c-6c6f"}
                },
                {},{},{},{},{},{},{},{},    "#, /* pqrstuvw */ r#"
                {},{},{},{},{},{},{}        "#, /* xyz{|}~ */ r#"
            ],
            "intermediates": {
                "c-6f": "o",
                "c-6c": "l",
                "c-6c6f": "lo"
            }
        }
    "#).chars()).build().unwrap();

    assert_eq!(Json::Object(result), expected);
}

//}}}


fn fsm_format(_c: &Context,
              helper: &Helper,
              _t: &Handlebars,
              rc: &mut RenderContext) -> Result<String, RenderError> {
    let params = helper.params();
    let action = &params[0];
    let value = match *rc.get_local_var(&params[1]) {
        Json::I64(v) => (v + 0x20) as u8,
        Json::U64(v) => (v + 0x20) as u8,
        _ => return Err(RenderError { desc: "Integer expected" }),
    };

    let result = match &**action {
        "action" => format!("{:02x}", value),
        "key" => escape_html(from_utf8(&[value]).unwrap()),
        _ => return Err(RenderError { desc: "Unknown fsm_action" }),
    };

    Ok(result)
}

fn escape_helper(context: &Context,
                 helper: &Helper,
                 _t: &Handlebars,
                 rc: &mut RenderContext) -> Result<String, RenderError> {
    let param = &helper.params()[0];
    let input = match *context.navigate(rc.get_path(), param) {
        Json::String(ref s) => s,
        _ => return Err(RenderError { desc: "String expected" }),
    };
    Ok(escape_html(input))
}


pub fn main() {

    let args = Args::docopt().decode::<Args>().unwrap_or_else(|e| e.exit());

    // Parse the input table into FSM.
    let fsm = {
        let mut stdin = stdin();
        let lock = stdin.lock();
        Fsm::parse(lock)
    };

    let maxout = fsm.longest_output_len();

    // Convert to JSON and populate some extra arguments.
    let mut data = fsm.into_json_object();
    let id = args.flag_id.unwrap_or_else(|| thread_rng().gen_range(-32768, 0));
    data.insert("id".to_owned(), Json::I64(id as i64));
    data.insert("group".to_owned(), Json::I64(args.flag_group as i64));
    data.insert("name".to_owned(), Json::String(args.flag_name));
    data.insert("maxout".to_owned(), Json::U64(maxout as u64));
    let data = Json::Object(data);

    // Create the template.
    let template_string = {
        let mut result = String::with_capacity(1024);
        let path = Path::new(&args.flag_template);
        let mut template_file = File::open(&path).unwrap();
        template_file.read_to_string(&mut result).unwrap();
        result
    };

    let mut template = Handlebars::new();
    template.register_template_string("keylayout", template_string).unwrap();
    template.register_helper("fsm_format", Box::new(fsm_format));
    template.register_helper("escape", Box::new(escape_helper));

    // Render.
    let result = template.render("keylayout", &data).unwrap();
    print!("{}", result);

}



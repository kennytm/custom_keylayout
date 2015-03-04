// parse_compose.rs: Extract relevant mappings from Compose.pre.
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

#![feature(io)]
#![feature(plugin)]
#![plugin(regex_macros)]

extern crate regex;
extern crate "rustc-serialize" as rustc_serialize;


use std::io::{BufReadExt, stdin};
use std::collections::HashMap;
use std::borrow::ToOwned;

macro_rules! input_map {
    ($($name:ident = $chr:expr),*; $($chars:expr),*) => {{
        let mut map = HashMap::with_capacity(96);
        $(map.insert(stringify!($name), $chr);)*
        $(map.insert(concat!($chars), $chars);)*
        map
    }}
}


pub fn main() {
    let line_regex = regex!(r#"^<Multi_key>((?: <[^>]+>)+)\s*:\s*"([^"]*)""#); //"
    let input_regex = regex!("<([^>]+)>");

    let input_to_char = input_map![
        exclam = '!',
        at = '@',
        numbersign = '#',
        dollar = '$',
        percent = '%',
        asciicircum = '^',
        ampersand = '&',
        asterisk = '*',
        parenleft = '(',
        parenright = ')',
        grave = '`',
        asciitilde = '~',
        minus = '-',
        underscore = '_',
        equal = '=',
        plus = '+',
        bracketleft = '[',
        bracketright = ']',
        braceleft = '{',
        braceright = '}',
        backslash = '\\',
        bar = '|',
        semicolon = ';',
        colon = ':',
        apostrophe = '\'',
        quotedbl = '"',
        comma = ',',
        period = '.',
        slash = '/',
        less = '<',
        greater = '>',
        question = '?';
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p',
        'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l',
        'z', 'x', 'c', 'v', 'b', 'n', 'm',
        'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P',
        'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L',
        'Z', 'X', 'C', 'V', 'B', 'N', 'M'
    ];

    let stdin = stdin();
'read_lines:
    for line in stdin.lock().lines() {
        macro_rules! try_or_continue {
            ($e:expr) => {
                match $e {
                    Some(x) => x,
                    None => continue 'read_lines,
                }
            }
        }

        let line = line.unwrap();
        let captures = try_or_continue!(line_regex.captures(&line));
        let output = try_or_continue!(captures.at(2)).to_owned();
        let inputs = try_or_continue!(captures.at(1));
        let mut chars = String::with_capacity(4);
        for input in input_regex.captures_iter(inputs) {
            let chr = input.at(1).and_then(|k| input_to_char.get(k));
            let chr = *try_or_continue!(chr);
            chars.push(chr);
        }
        println!("{:<7} {}", output, chars);
    }
}


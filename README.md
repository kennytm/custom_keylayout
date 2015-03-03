Create Compose-Key-based Keyboard Layout for Mac OS X
=====================================================

Installation
------------

1. Get [Cargo](http://doc.crates.io/index.html), then build the executables

    ```bash
    $ cargo build --release
    ```

2. Generate the keylayout files

    ```bash
    $ ./target/release/custom_keylayout < sample.txt > sample.keylayout
    ```

3. Install the keylayout

    ```bash
    $ cp sample.keylayout "~/Library/Keyboard Layouts/"
    ```

4. Log out and log in again.

5. Enable the new keylayout from **System Preferences** → **Keyboard** → **Input Sources**.

Usage
-----

The Compose key is mapped to "F13" key by default. On keyboards without F13, you may use [Karabiner](https://pqrs.org/osx/karabiner/) to [remap](http://apple.stackexchange.com/questions/31487/add-compose-key-to-os-x) it to e.g. right-alt.

Entries in `*.txt` determine the compose sequence of each special character. For instance `sample.txt` contains

    æ   ae

Thus if you press <kbd>F13</kbd><kbd>a</kbd><kbd>e</kbd>, it should output “æ”.

Input format
------------

Input must be in UTF-8. If the first non-space character of a line is `#`, that line is ignored.

Each line is separated by spaces into several fields, e.g.:

```
½       12
ಠ_ಠ     disa
じゃ    jJA -> j
```

The first field (`½`, `ಠ_ಠ`, `じゃ`) specify the *output*. The output can be any non-space character sequence. You could also use the escape sequence `\x789ab` to represent a Unicode code-point U+789AB. This is necessary if a space is part of the output.

The second field (`12`, `disa`, `jJA`) specify the *input*. This means e.g. pressing the sequence <kbd>F13</kbd><kbd>j</kbd><kbd>J</kbd><kbd>A</kbd> will produce the string "じゃ".

Normally, after the input is finished we will return to the normal state. However, we could include an optional `->` field to keep in the composed state. There must be a space before the `->`. Here, the `-> j` means after typing `jJA`, the system will act as if you have pressed <kbd>F13</kbd><kbd>j</kbd> again.



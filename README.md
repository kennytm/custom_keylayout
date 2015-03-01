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


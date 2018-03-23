Ada Implementation of Bruce Schneier's Solitaire Encryption Algorithm
=====================================================================

This repository file contains an Ada (ISO/IEC 8652:1995) implementation of
the Solitaire encryption algorithm, designed by Bruce Schneier and described
in Neal Stephenson's novel, _Cryptonomicon_. See

  https://www.schneier.com/academic/solitaire/

These files are Copyright (C) 1999-2018 by Jeffrey R. Carter and Doug
Rogers. We grant permission for unlimited reproduction and distribution of
these files provided that all files are unchanged and this copyright notice
is included with all copies. Please send comments and error reports to
jrcarter@acm.org.

This implementation was originally tested with the GNAT 3.13p compiler on
Windows 98 and with GNAT 3.13p on Linux/Pentium. Most recently it was tested
using GNAT 4.9.3 on Linux Mint 18.1, Ubuntu 16.04, and Cygwin 2.10.0
x64.

There are no compiler-, OS-, or platform-dependent features in the code, so
this code should compile and run with any Ada compiler on any OS and
platform. File names use the GNAT default naming convention for systems with
long filenames.

Installing an Ada Compiler
--------------------------

GNAT is available from https://www.adacore.com/download, or use

```shell
    $ sudo apt-get install gnat
```
on Debian Linux distributions (Ubuntu, Mint, etc.) or
```shell
    $ sudo yum install gcc-gnat
```
on Redhat system (CentOS).

For Windows your options, among others, are to use AdaCore's x86 Windows
package mentioned above or to install cygwin and select the gnat gcc-ada
package in the installer downloaded from https://www.cygwin.com/install.html.

Source Code Design
------------------

The Solitaire algorithm is implemented by package
Solitaire_Operations. Package Solitaire_Operations.Text_Representation
provides text representations of cards and decks, and conversions from the
textual to the internal representations.

A few sample programs demonstrate the use of these packages:

Program Generate_Key generates a random deck in textual representation to
standard output.

Program Null_Key generates the standard deck ordering in textual
representation to standard output.

Get_Line is a function used by programs Encrypt, Decrypt, Key_Encrypt, and
Key_Decrypt.

Program Encrypt reads a line containing the textual representation of a deck
followed by a line containing plaintext from standard input. It removes
non-letter characters from the plaintext, converts the plaintext to upper
case, and pads the plain text with X's to a multiple of 5 characters in
length. It then encrypts the plaintext using the input deck and writes the
ciphertext to standard output.

Program Decrypt reads a line containing the textual representation of a deck
followed by a line containing ciphertext from standard input. It decrypts the
ciphertext using the input deck and writes the plaintext to standard output.

Program Key_Encrypt reads a line a passphrase followed by a line containing
plaintext from standard input. It keys the deck with the passphrase. It
removes non-letter characters from the plaintext, converts the plaintext to
upper case, and pads the plain text with X's to a multiple of 5 characters in
length. It then encrypts the plaintext using the keyed deck and writes the
ciphertext to standard output.

Program Key_Decrypt reads a line containing a passphrase followed by a line
containing ciphertext from standard input. It keys the deck with the
passphrase. It decrypts the ciphertext using the keyed deck and writes the
plaintext to standard output.

Building Solitaire
------------------

The easiest way to build the software is to run make, which simply invokes
gnatmake for the four executables. There are also 'clean' and 'test' targets
that may be used to tidy up the working directory or run the single test of
encrypt/decrypt.

```shell
    $ make clean
    rm -f decrypt encrypt key_decrypt key_encrypt *.exe
    rm -f *.o *.ali *~ *.bak *.out

    $ make
    gnatmake decrypt
    gcc-4.9 -c decrypt.adb
    gcc-4.9 -c get_line.adb
    gcc-4.9 -c solitaire_operations.adb
    solitaire_operations.adb:40:36: warning: "return" statement missing following this statement
    solitaire_operations.adb:40:36: warning: Program_Error may be raised at run time
    gcc-4.9 -c solitaire_operations-text_representation.adb
    gnatbind -x decrypt.ali
    gnatlink decrypt.ali
    gnatmake encrypt
    gcc-4.9 -c encrypt.adb
    mgnatbind -x encrypt.ali
    gnatlink encrypt.ali
    gnatmake key_decrypt
    gcc-4.9 -c key_decrypt.adb
    gnatbind -x key_decrypt.ali
    gnatlink key_decrypt.ali
    gnatmake key_encrypt
    gcc-4.9 -c key_encrypt.adb
    gnatbind -x key_encrypt.ali
    gnatlink key_encrypt.ali

    $ make test
    cat message.plain | ./encrypt > test.enc.out
    (head -1 message.plain; cat test.enc.out) | ./decrypt > test.dec.out
    echo YOURMOTHERWASAHAMPSTERANDYOURFATHERSMELTOFELDERBERRIESX | diff - test.dec.out
```

Running With Test Files
-----------------------

File message.cipher contains input for Decrypt. Running

```shell
    $ ./decrypt < message.cipher
```
should display a quotation from Monty Python.

File null_test.plain contains input for Encrypt of the 1st test vector supplied on
the web site (standard deck, plaintext of 15 A's). Running
```shell
    $ ./encrypt < null_test.plain
```
should display the ciphertext. File null_test.cipher contains input for Decrypt of this
ciphertext. Running

```shell
    $ ./decrypt < null_test.cipher
```
should display 15 A's.

File foo_key.plain contains input for Key_Encrypt of a test vector supplied on the
web site (passphrase of "FOO", plaintext of 15 A's). Running
```shell
    $ ./key_encrypt < foo_key.plain
```
should display the ciphertext. File foo_key.cipher contains input for Key_Decrypt of
this ciphertext. Running
```shell
    $ ./key_decrypt < foo_key.cipher
```
should display 15 A's.

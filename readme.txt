This ZIP file contains an Ada (ISO/IEC 8652:1995) implementation of the Solitaire
encryption algorithm, designed by Bruce Schneier and described in Neal Stephenson's
novel, _Cryptonomicon_, as well as at

http://www.counterpane.com/solitaire.html

These files are Copyright (C) 1999-2001 by Jeffrey R. Carter and Douglas Rogers. We grant
permission for unlimited reproduction and distribution of these files provided that all
files are unchanged and this copyright notice is included with all copies. Please
send comments and error reports to jrcarter@acm.org.

This implementation has been tested with the GNAT 3.13p compiler on Windows 98 and with
GNAT 3.13p on Linux/Pentium. There are no compiler-, OS-, or platform-dependent features
in the code, so this code should compile and run with any Ada compiler on any OS and
platform.

File names use the GNAT default naming convention for systems with long filenames.
GNAT is available from ftp://cs.nyu.edu/pub/gnat/.

The Solitaire algorithm is implemented by package Solitaire_Operations. Package
Solitaire_Operations.Text_Representation provides text representations of cards and
decks, and conversions from the textual to the internal representations.

A few sample programs demonstrate the use of these packages:

Program Generate_Key generates a random deck in textual representation to standard
output.

Program Null_Key generates the standard deck ordering in textual representation to
standard output.

Get_Line is a function used by programs Encrypt, Decrypt, Key_Encrypt, and Key_Decrypt.

Program Encrypt reads a line containing the textual representation of a deck
followed by a line containing plaintext from standard input. It removes non-letter
characters from the plaintext, converts the plaintext to upper case, and pads the
plain text with X's to a multiple of 5 characters in length. It then encrypts the
plaintext using the input deck and writes the ciphertext to standard output.

Program Decrypt reads a line containing the textual representation of a deck
followed by a line containing ciphertext from standard input. It decrypts the
ciphertext using the input deck and writes the plaintext to standard output.

Program Key_Encrypt reads a line a passphrase followed by a line containing plaintext
from standard input. It keys the deck with the passphrase. It removes non-letter
characters from the plaintext, converts the plaintext to upper case, and pads the
plain text with X's to a multiple of 5 characters in length. It then encrypts the
plaintext using the keyed deck and writes the ciphertext to standard output.

Program Key_Decrypt reads a line containing a passphrase followed by a line containing
ciphertext from standard input. It keys the deck with the passphrase. It decrypts the
ciphertext using the keyed deck and writes the plaintext to standard output.

File message.cipher contains input for Decrypt. Running

decrypt < message.cipher

should display a quotation from Monty Python.

File null_test.plain contains input for Encrypt of the 1st test vector supplied on
the web site (standard deck, plaintext of 15 A's). Running

encrypt < null_test.plain

should display the ciphertext. File null_test.cipher contains input for Decrypt of this
ciphertext. Running

decrypt < null_test.cipher

should display 15 A's.

File foo_key.plain contains input for Key_Encrypt of a test vector supplied on the
web site (passphrase of "FOO", plaintext of 15 A's). Running

key_encrypt < foo_key.plain

should display the ciphertext. File foo_key.cipher contains input for Key_Decrypt of
this ciphertext. Running

key_decrypt < foo_key.cipher

should display 15 A's.

with Get_Line;
with Solitaire_Operations.Text_Representation;
with Ada.Text_IO;

use Ada;
procedure Key_Decrypt is
   Passphrase : constant String := Get_Line;
   Crypto     : constant String := Get_Line;

   Deck  : Solitaire_Operations.Deck_List;
   Plain : String (Crypto'range);
begin -- Key_Decrypt
   Solitaire_Operations.Key (Deck => Deck, Passphrase => Passphrase);

   Solitaire_Operations.Decrypt (Deck => Deck, Plain => Plain, Crypto => Crypto);
   Text_IO.Put_Line (Item => Plain);
exception -- Key_Decrypt
when Solitaire_Operations.Text_Representation.Card_Not_Found =>
   Text_IO.Put_Line (Item => "Invalid deck");
end Key_Decrypt;
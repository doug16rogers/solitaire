with Get_Line;
with Solitaire_Operations.Text_Representation;
with Ada.Text_IO;

use Ada;
procedure Decrypt is
   Deck_Text : constant Solitaire_Operations.Text_Representation.Short_Card_Deck_String := Get_Line;
   Crypto    : constant String                                                          := Get_Line;

   Deck  : Solitaire_Operations.Deck_List;
   Plain : String (Crypto'range);
begin -- Decrypt
   Solitaire_Operations.Text_Representation.Set_Deck (Deck => Deck, To => Deck_Text);

   Solitaire_Operations.Decrypt (Deck => Deck, Plain => Plain, Crypto => Crypto);
   Text_IO.Put_Line (Item => Plain);
exception -- Decrypt
when Solitaire_Operations.Text_Representation.Card_Not_Found =>
   Text_IO.Put_Line (Item => "Invalid deck");
end Decrypt;
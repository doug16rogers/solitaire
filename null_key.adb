with Solitaire_Operations.Text_Representation;
with Ada.Text_IO;

use Ada;
procedure Null_Key is
   Deck : Solitaire_Operations.Deck_List := Solitaire_Operations.Standard_Deck;
begin -- Null_Key
   Output : for I in Deck'range loop
      Text_IO.Put (Item => Solitaire_Operations.Text_Representation.Short_Card_Name (Deck (I) ) );
   end loop Output;

   Text_IO.New_Line;
end Null_Key;
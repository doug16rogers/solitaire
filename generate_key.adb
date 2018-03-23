with Solitaire_Operations.Text_Representation;
with Ada.Text_IO;

use Ada;
procedure Generate_Key is
   Deck : Solitaire_Operations.Deck_List := Solitaire_Operations.Standard_Deck;
begin -- Generate_Key
   Solitaire_Operations.Shuffle (Deck => Deck);
   
   Output : for I in Deck'range loop
      Text_IO.Put (Item => Solitaire_Operations.Text_Representation.Short_Card_Name (Deck (I) ) );
   end loop Output;
   
   Text_IO.New_Line;
end Generate_Key;
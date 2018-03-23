with Get_Line;
with Solitaire_Operations.Text_Representation;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada;
use Ada.Characters;
use Ada.Strings.Unbounded;
procedure Encrypt is
   Deck_Text : constant Solitaire_Operations.Text_Representation.Short_Card_Deck_String := Get_Line;

   Deck  : Solitaire_Operations.Deck_List;
   Plain : Unbounded_String := To_Unbounded_String (Handling.To_Upper (Get_Line) );

   subtype Letter is Character range 'A' .. 'Z';
begin -- Encrypt
   Solitaire_Operations.Text_Representation.Set_Deck (Deck => Deck, To => Deck_Text);

   Compress : for I in reverse 1 .. Length (Plain) loop -- Remove non-letters from plaintext
      if Element (Plain, I) not in Letter then
         Delete (Source => Plain, From => I, Through => I);
      end if;
   end loop Compress;

   Expand : loop -- Make plaintext a multiple of 5 characters long by adding Xs to the end
      exit Expand when Length (Plain) rem 5 = 0;

      Append (Source => Plain, New_Item => 'X');
   end loop Expand;

   Create : declare
      Crypto : String (1 .. Length (Plain) );
   begin -- Create
      Solitaire_Operations.Encrypt (Deck => Deck, Plain => To_String (Plain), Crypto => Crypto);
      Text_IO.Put_Line (Item => Crypto);
   end Create;
exception -- Encrypt
when Solitaire_Operations.Text_Representation.Card_Not_Found =>
   Text_IO.Put_Line (Item => "Invalid deck");
end Encrypt;
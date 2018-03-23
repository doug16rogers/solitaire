with Ada.Characters.Handling;

use Ada.Characters;

package body Solitaire_Operations.Text_Representation is
   ------------------------------------------------------------------------
   function Card (Name : Short_Card) return Card_Value is
   --
   -- Returns the card for the given 2-character name.
   --
      Upper_Card : Short_Card := Handling.To_Upper (Name);
   begin -- Card
      Find_Card : for I in Short_Card_Name'Range loop
         if Upper_Card = Short_Card_Name (I) then
            return I;
         end if;
      end loop Find_Card;

      raise Card_Not_Found;
   end Card;

   ------------------------------------------------------------------------
   procedure Set_Deck (Deck : in out Deck_List;
                       To   : in     Short_Card_Deck_String) is
   --
   -- Sets the deck contents to the values provided in the compact list
   -- of 2-character names.
   --
      Card_Index : Positive;
   begin -- Set_Deck
      Copy: for I in Deck'Range loop
         Card_Index := To'First + 2 * (I - Deck'First);
         Deck (I) := Card (To (Card_Index .. Card_Index + 1));
      end loop Copy;
   end Set_Deck;
end Solitaire_Operations.Text_Representation;
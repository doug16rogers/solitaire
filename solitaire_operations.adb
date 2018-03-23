with Ada.Characters.Handling;
with Ada.Numerics.Discrete_Random;

use Ada;
use Ada.Characters;

package body Solitaire_Operations is
   function Value (Card : Card_Value) return Card_Value is
   begin -- Value
      if Card >= Card_Value'Last then
         return Card_Value'Last - 1;
      else
         return Card;
      end if;
   end Value;

   package Random is new Numerics.Discrete_Random (Card_Value);

   procedure Swap (Left : in out Card_Value; Right : in out Card_Value) is
      Temp : constant Card_Value := Left;
   begin -- Swap
      Left  := Right;
      Right := Temp;
   end Swap;
   pragma Inline (Swap);

   Gen : Random.Generator;

   procedure Shuffle (Deck : in out Deck_List) is
   begin -- Shuffle
      All_Cards : for I in Deck'range loop
         Swap (Deck (I), Deck (Random.Random (Gen) ) );
      end loop All_Cards;
   end Shuffle;

   function Find (Value : Card_Value; Deck : Deck_List) return Card_Value is
      -- Returns the index in Deck of Value
      -- Note: raises Program_Error if Value not in Deck
   begin -- Find
      Search : for I in Deck'range loop
         if Deck (I) = Value then
            return I;
         end if;
      end loop Search;
   end Find;
   
   procedure End_Joker_To_Front (Index : in out Card_Value; Deck : in out Deck_List) is
      -- If Index points to the last card of Deck, moves the last card of Deck to the front of Deck and adjusts Index
   begin -- End_Joker_To_Front
      if Index >= Deck'Last then
         Deck := Deck (Deck'Last) & Deck (Deck'First .. Deck'Last - 1);
         Index := Deck'First;
      end if;
   end End_Joker_To_Front;
   
   A_Joker : constant Card_Value := Card_Value'Last - 1;
   B_Joker : constant Card_Value := Card_Value'Last;
   
   procedure Move_A_Joker (Deck : in out Deck_List) is
      -- Moves the A Joker 1 card down
      
      Index : Card_Value := Find (A_Joker, Deck);
   begin -- Move_A_Joker
      End_Joker_To_Front (Index => Index, Deck => Deck); -- Make sure A Joker not last card of Deck
      Swap (Deck (Index), Deck (Index + 1) );
   end Move_A_Joker;

   procedure Move_B_Joker (Deck : in out Deck_List) is
      -- Moves the B Joker 2 cards down
      
      Index : Card_Value := Find (B_Joker, Deck);
   begin -- Move_B_Joker
      End_Joker_To_Front (Index => Index, Deck => Deck); -- Make sure B Joker not last card of Deck
      Swap (Deck (Index), Deck (Index + 1) );
      Index := Index + 1;
      End_Joker_To_Front (Index => Index, Deck => Deck); -- Make sure B Joker not last card of Deck
      Swap (Deck (Index), Deck (Index + 1) );
   end Move_B_Joker;
   
   procedure Triple_Cut (Deck : in out Deck_List) is
      -- Perform a triple cut on Deck
      
      A_Index : constant Card_Value := Find (A_Joker, Deck);
      B_Index : constant Card_Value := Find (B_Joker, Deck);
      T_Index : constant Card_Value := Card_Value'Min (A_Index, B_Index); -- Index of top Joker
      L_Index : constant Card_Value := Card_Value'Max (A_Index, B_Index); -- Index of lower (bottom) Joker
   begin -- Triple_Cut
      Deck := Deck (L_Index + 1 .. Deck'Last) & Deck (T_Index .. L_Index) & Deck (Deck'First .. T_Index - 1);
   end Triple_Cut;
   pragma Inline (Triple_Cut);
   
   procedure Counted_Cut (Deck : in out Deck_List; Count : in Card_Value) is
      -- Perform a counted cut on Deck moving Count cards from the top to before the last card
   begin -- Counted_Cut
      Deck := Deck (Count + 1 .. Deck'Last - 1) & Deck (Deck'First .. Count) & Deck (Deck'Last);
   end Counted_Cut;
   pragma Inline (Counted_Cut);

   function To_Value (Char : Character) return Character_Value is
   begin -- To_Value
      return Character'Pos (Handling.To_Upper (Char) ) - Character'Pos ('A') + 1;
   end To_Value;
   pragma Inline (To_Value);

   procedure Key (Deck : out Deck_List; Passphrase : in String) is
   begin -- Key
      Deck := Standard_Deck;
      
      All_Characters : for I in Passphrase'range loop
         if Handling.Is_Letter (Passphrase (I) ) then
            Move_A_Joker (Deck => Deck);
            Move_B_Joker (Deck => Deck);
            Triple_Cut (Deck => Deck);
            Counted_Cut (Deck => Deck, Count => Value (Deck (Deck'Last) ) );
            Counted_Cut (Deck => Deck, Count => Value (To_Value (Passphrase (I) ) ) );
         end if;
      end loop All_Characters;
   end Key;
   
   procedure Generate (Deck : in out Deck_List; Key : out Character_Value) is
      Count   : Card_Value;
      Result  : Card_Value;
   begin -- Generate
      Move_A_Joker (Deck => Deck);
      Move_B_Joker (Deck => Deck);
      Triple_Cut (Deck => Deck);
      Counted_Cut (Deck => Deck, Count => Value (Deck (Deck'Last) ) );

      -- Output card
      Count := Value (Deck (Deck'First) ) + 1;

      Result := Deck (Count);

      if Result in A_Joker .. B_Joker then -- Found a Joker; repeat
         Generate (Deck => Deck, Key => Key);
      else
         if Result > Character_Value'Last then
            Result := Result - Character_Value'Last;
         end if;

         Key := Result;
      end if;
   end Generate;

   function Add (Left : Character_Value; Right : Character_Value) return Character_Value is
      Result : Positive := Left + Right;
   begin -- Add
      Reduce : loop
         exit Reduce when Result in Character_Value;

         Result := Result - Character_Value'Last;
      end loop Reduce;

      return Result;
   end Add;

   function Sub (Left : Character_Value; Right : Character_Value) return Character_Value is
      Result : Integer := Left - Right;
   begin -- Sub
      Increase : loop
         exit Increase when Result in Character_Value;

         Result := Result + Character_Value'Last;
      end loop Increase;

      return Result;
   end Sub;

   function To_Character (Value : Character_Value) return Character is
   begin -- To_Character
      return Character'Val (Character'Pos ('A') + Value - 1);
   end To_Character;
   pragma Inline (To_Character);

   procedure Encrypt (Deck : in out Deck_List; Plain  : in String; Crypto : out String) is
      Key : Character_Value;
   begin -- Encrypt
      if Plain'Length /= Crypto'Length then
         raise Constraint_Error;
      end if;

      All_Chars : for I in Plain'range loop
         Generate (Deck => Deck, Key => Key);
         Crypto (I - Plain'First + Crypto'First) := To_Character (Add (To_Value (Plain (I) ), Key) );
      end loop All_Chars;
   end Encrypt;

   procedure Decrypt (Deck : in out Deck_List; Crypto : in String; Plain  : out String) is
      Key : Character_Value;
   begin -- Decrypt
      if Plain'Length /= Crypto'Length then
         raise Constraint_Error;
      end if;

      All_Chars : for I in Crypto'range loop
         Generate (Deck => Deck, Key => Key);
         Plain (I - Crypto'First + Plain'First) := To_Character (Sub (To_Value (Crypto (I) ), Key) );
      end loop All_Chars;
   end Decrypt;
begin -- Solitaire_Operations
   Random.Reset (Gen);
end Solitaire_Operations;
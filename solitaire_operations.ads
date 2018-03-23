package Solitaire_Operations is
   subtype Card_Value is Positive range 1 .. 54;

   function Value (Card : Card_Value) return Card_Value; -- B Joker (54) returns 53; all others return value

   type Deck_List is array (Card_Value) of Card_Value;
   -- Given "Deck : Deck_List;"
   -- Deck (Deck_First) is the top    card
   -- Deck (Deck_Last)  is the bottom card

   Standard_Deck : constant Deck_List := ( 1 =>  1,  2 =>  2,  3 =>  3,  4 =>  4,  5 =>  5,  6 =>  6,  7 =>  7,  8 =>  8,  9 =>  9,
                                          10 => 10, 11 => 11, 12 => 12, 13 => 13, 14 => 14, 15 => 15, 16 => 16, 17 => 17, 18 => 18,
                                          19 => 19, 20 => 20, 21 => 21, 22 => 22, 23 => 23, 24 => 24, 25 => 25, 26 => 26, 27 => 27,
                                          28 => 28, 29 => 29, 30 => 30, 31 => 31, 32 => 32, 33 => 33, 34 => 34, 35 => 35, 36 => 36,
                                          37 => 37, 38 => 38, 39 => 39, 40 => 40, 41 => 41, 42 => 42, 43 => 43, 44 => 44, 45 => 45,
                                          46 => 46, 47 => 47, 48 => 48, 49 => 49, 50 => 50, 51 => 51, 52 => 52, 53 => 53, 54 => 54);

   procedure Shuffle (Deck : in out Deck_List); -- Randomizes Deck

   subtype Character_Value is Positive range 1 .. 26;
   
   procedure Key (Deck : out Deck_List; Passphrase : in String);
   -- Keys the deck (creates an initial deck) from a passphrase (method 3 from the web page)
   -- Does not use the optional placement of jokers
   -- Passphrase should contain at least 80 letters for reasonable security

   procedure Generate (Deck : in out Deck_List; Key : out Character_Value); -- Generate next key value from Deck in Key

   function Add (Left : Character_Value; Right : Character_Value) return Character_Value;
   function Sub (Left : Character_Value; Right : Character_Value) return Character_Value;
   -- Modulo 26 addition and subtraction

   procedure Encrypt (Deck : in out Deck_List; Plain  : in String; Crypto : out String);
   procedure Decrypt (Deck : in out Deck_List; Crypto : in String; Plain  : out String);
   -- Encrypt and decrypt messages
   --
   -- Precondition: Plain'Length = Crypto'Length     raises Constraint_Error if violated
end Solitaire_Operations;
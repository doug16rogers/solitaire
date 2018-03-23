package Solitaire_Operations.Text_Representation is
   subtype Short_Card is String (1 .. 2);

   type Short_Card_Deck is array (Positive range <>) of Short_Card;

   Short_Card_Name : constant Short_Card_Deck (Card_Value) :=
            ( 1 => "AC",  14 => "AD",  27 => "AH",  40 => "AS",
              2 => "2C",  15 => "2D",  28 => "2H",  41 => "2S",
              3 => "3C",  16 => "3D",  29 => "3H",  42 => "3S",
              4 => "4C",  17 => "4D",  30 => "4H",  43 => "4S",
              5 => "5C",  18 => "5D",  31 => "5H",  44 => "5S",
              6 => "6C",  19 => "6D",  32 => "6H",  45 => "6S",
              7 => "7C",  20 => "7D",  33 => "7H",  46 => "7S",
              8 => "8C",  21 => "8D",  34 => "8H",  47 => "8S",
              9 => "9C",  22 => "9D",  35 => "9H",  48 => "9S",
             10 => "TC",  23 => "TD",  36 => "TH",  49 => "TS",
             11 => "JC",  24 => "JD",  37 => "JH",  50 => "JS",
             12 => "QC",  25 => "QD",  38 => "QH",  51 => "QS",
             13 => "KC",  26 => "KD",  39 => "KH",  52 => "KS",
             53 => "AJ",
             54 => "BJ");

   Card_Not_Found : exception;

   function Card (Name : Short_Card) return Card_Value;

   subtype Short_Card_Deck_String is String (1 .. Short_Card'Length * Deck_List'Length);

   procedure Set_Deck (Deck : in out Deck_List;
                       To   : in     Short_Card_Deck_String);
end Solitaire_Operations.Text_Representation;
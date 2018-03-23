with Ada.Text_IO;

use Ada;
function Get_Line return String is
   Line     : String (1 .. 256);
   Last     : Natural;
   I        : Positive := 1;
   Old_Last : Natural;

   function Is_Legal (C : in Character) return Boolean is
   begin -- Is_Legal
      return C in 'A' .. 'Z' or C in 'a' .. 'z' or C in '1' .. '9';
   end Is_Legal;
begin -- Get_Line
   Text_IO.Get_Line (Item => Line, Last => Last);
   Old_Last := Last;

   Remove_Illegal_Characters : loop
      exit Remove_Illegal_Characters when I > Last;

      if Is_Legal (Line (I) ) then
         I := I + 1;
      else
         Last := Last - 1;
         Line (I .. Last) := Line (I + 1 .. Last + 1);
      end if;
   end loop Remove_Illegal_Characters;

   if Old_Last < Line'Last then
      return Line (1 .. Last);
   else
      return Line (1 .. Last) & Get_Line;
   end if;
end Get_Line;

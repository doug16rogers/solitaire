with Ada.Text_IO;

use Ada;
function Get_Line return String is
   Line : String (1 .. 256);
   Last : Natural;
begin -- Get_Line
   Text_IO.Get_Line (Item => Line, Last => Last);

   if Last < Line'Last then
      return Line (1 .. Last);
   else
      return Line & Get_Line;
   end if;
end Get_Line;
------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                      Copyright (C) 2005-2012, AdaCore                    --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Templates_Parser.Debug;

procedure Test_Tag is
   use Ada;
   use Templates_Parser;

   T1, T2 : Tag;
begin
   T1 := +"1" & "2";
   T2 := T1;
   T1 := T1 & "3";
   Text_IO.Put_Line ("T1'Length = " & Natural'Image (Size (T1)));
   Text_IO.Put_Line ("T2'Length = " & Natural'Image (Size (T2)));

   Text_IO.Put ("T1 = ");
   Debug.Print (T1);
   Text_IO.New_Line;

   Text_IO.Put ("T2 = ");
   Debug.Print (T2);
   Text_IO.New_Line;
end Test_Tag;

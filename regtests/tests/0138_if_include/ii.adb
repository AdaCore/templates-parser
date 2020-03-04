------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Templates_Parser;

procedure Ii is

   use Ada;
   use Ada.Strings.Unbounded;
   use Templates_Parser;

   R : Unbounded_String;
   V : Vector_Tag;
   T : Translate_Table (1 .. 4);

begin
   T (1) := Assoc ("HEAD_FILE1", "/content/header.tmplt");
   T (2) := Assoc ("HEAD_FILE2", "header.tmplt");
   T (3) := Assoc ("HEAD_FILE3", "../content/header.tmplt");
   T (4) := Assoc ("HEAD_FILE4", "./content/header.tmplt");
   R := Parse ("content/ii.tmplt", T);

   Text_IO.Put_Line (To_String (R));
end Ii;

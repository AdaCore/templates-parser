------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

procedure Alo2 is

   use Ada;
   use Ada.Strings.Unbounded;
   use Templates_Parser;

   R : Unbounded_String;
   V : Vector_Tag;
   T : Translate_Table (1 .. 3);

begin
   V := +"a" & "result";
   T (1) := Assoc ("Param_Names", V);
   T (2) := Assoc ("Parent_Function", "sync");
   T (3) := Assoc ("Name", "fct");
   R := Parse ("alo2.tmplt", T);

   Text_IO.Put_Line (To_String (R));
end Alo2;

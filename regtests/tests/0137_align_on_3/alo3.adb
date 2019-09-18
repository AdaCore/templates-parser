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

procedure Alo3 is

   use Ada;
   use Ada.Strings.Unbounded;
   use Templates_Parser;

   R : Unbounded_String;
   V : Vector_Tag;
   T : Translate_Table (1 .. 3);

begin
   V := +"a : int;       b : float" & "r1 : boolean; r2 : character";
   T (1) := Assoc ("PARAMS", V);
   V := +"azerty : int; u : float"
        & "r : boolean; this_is_it : character";
   T (2) := Assoc ("PARAMS2", V);

   V := +"A : Natural := 0;"
        & "Is_That_True : constant boolean := False;"
        & "Value := 12.3;"
        & "No_Init : Natural;"
        & "Age : Positive := 12;";
   T (3) := Assoc ("DECL", V);
   R := Parse ("alo3.tmplt", T);

   Text_IO.Put_Line (To_String (R));
end Alo3;

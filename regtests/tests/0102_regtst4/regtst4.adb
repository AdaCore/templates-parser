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

with Templates_Parser;

with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Regtst4 is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Templates_Parser;

   V1     : Tag := +"v11" & "v12" & "v13";
   V2     : Tag := +"v21" & "v22";
   V3     : Tag := +"v31" & "v32" & "v33";

   Result : Unbounded_String;

begin
   Result := Parse
     ("regtst4.tmplt",
      (1 => Assoc ("V1",   V1, " - "),
       2 => Assoc ("V2", V2, " $ "),
       3 => Assoc ("V3", V3)),
      Cached => true);

   Put_Line (To_String (Result));
end Regtst4;

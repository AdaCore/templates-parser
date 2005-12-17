------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 2005                            --
--                                  AdaCore                                 --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
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

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

procedure Regtst3 is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Templates_Parser;

   V1     : Vector_Tag := +"v11" & "v12" & "v13";
   V2     : Vector_Tag := +"v21" & "v22";

   V3     : Vector_Tag := +"v31";
   V4     : Vector_Tag := +"v41";

   V5     : Vector_Tag;

   K      : Matrix_Tag := +V5;
   V      : Matrix_Tag := +V2;

   Result : Unbounded_String;

begin
   Result := Parse
     ("aws_status.thtml",
      (1 => Assoc ("KEYS_M",   K),
       2 => Assoc ("VALUES_M", V),
       3 => Assoc ("SESSIONS_V",  V1)),
      Cached => true);

   Put_Line ("Result : " & To_String (Result));
end Regtst3;

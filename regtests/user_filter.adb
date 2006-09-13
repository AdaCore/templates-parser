------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 2006                            --
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

with Ada.Text_IO;
with Templates_Parser;

procedure User_Filter is

   use Ada.Text_IO;
   use Templates_Parser;

   function F1
     (S : in String;
      T : in Translate_Set) return String;

   function F2
     (S : in String;
      P : in String;
      T : in Translate_Set) return String;

   function F1
     (S : in String;
      T : in Translate_Set) return String is
   begin
      return "[F1=" & S & "]";
   end F1;

   function F2
     (S : in String;
      P : in String;
      T : in Translate_Set) return String is
   begin
      return "[F2=" & S & "/" & P & "+" & Get (Get (T, "STAG")) & "]";
   end F2;

   T : Tag := +"v1" & "v2" & "v3";

begin
   Register_Filter ("F1", F1'Unrestricted_Access);
   Register_Filter ("F2", F2'Unrestricted_Access);

   Put_Line
     (Parse ("user_filter.tmplt",
             Translate_Table'
               (Assoc ("STAG", "tag_value"),
                Assoc ("VTAG", T))));
end User_Filter;

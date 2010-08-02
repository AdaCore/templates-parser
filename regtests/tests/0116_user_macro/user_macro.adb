------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                        Copyright (C) 2010, AdaCore                       --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Templates_Parser;

procedure User_Macro is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Templates_Parser;

   function UM (Name : String; Parameters : Parameter_Set) return String is
      P : Unbounded_String;
   begin
      for K in Parameters'Range loop
         Append (P, Parameters (K));
         if K /= Parameters'Last then
            Append (P, ",");
         end if;
      end loop;

      return '[' & Name & ']' & '(' & To_String (P) & ')';
   end UM;

begin
   Register_Macro_Handler (UM'Unrestricted_Access);

   Put_Line
     (Parse ("user_macro.tmplt",
             Translate_Table'
               (1 => Assoc ("STAG", "tag_value"))));
end User_Macro;

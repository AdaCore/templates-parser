------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                      Copyright (C) 2010-2012, AdaCore                    --
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

------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2006-2009, AdaCore                     --
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

   package LT is

      type Local_Tag is new Dynamic.Lazy_Tag with null record;

      procedure Value
        (Lazy_Tag     : not null access Local_Tag;
         Var_Name     : String;
         Translations : in out Translate_Set);

   end LT;

   Lazy : aliased LT.Local_Tag;

   function F1
     (S : String;
      C : Filter_Context) return String;

   function F2
     (S : String;
      P : String;
      C : Filter_Context) return String;

   function Selected
     (S : String;
      P : String;
      C : Filter_Context) return String;

   --------
   -- LT --
   --------

   package body LT is

      procedure Value
        (Lazy_Tag     : not null access Local_Tag;
         Var_Name     : String;
         Translations : in out Translate_Set) is
      begin
         Insert (Translations, Assoc (Var_Name, "//" & Var_Name & "\\"));
      end Value;

   end LT;

   --------
   -- F1 --
   --------

   function F1
     (S : String;
      C : Filter_Context) return String is
   begin
      return "[F1=" & S & "]";
   end F1;

   --------
   -- F2 --
   --------

   function F2
     (S : String;
      P : String;
      C : Filter_Context) return String is
   begin
      return "[F2=" & S & "/" & P & "+"
        & Get (Get (C.Translations, "STAG")) & "]";
   end F2;

   --------------
   -- Selected --
   --------------

   function Selected
     (S : String;
      P : String;
      C : Filter_Context) return String
   is
      T : Translate_Set;
   begin
      C.Lazy_Tag.Value (P, T);
      return S & " ." & P & ". = " & Get (Get (T, P));
   end Selected;

   T : Tag := +"v1" & "v2" & "v3";

begin
   Register_Filter ("F1", F1'Unrestricted_Access);
   Register_Filter ("F2", F2'Unrestricted_Access);
   Register_Filter ("SELECTED", Selected'Unrestricted_Access);

   Put_Line
     (Parse ("user_filter.tmplt",
             Translate_Table'
               (Assoc ("STAG", "tag_value"),
                Assoc ("VTAG", T)), Lazy_Tag => Lazy'Unchecked_Access));
end User_Filter;

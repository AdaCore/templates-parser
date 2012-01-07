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

with Ada.Text_IO;

package body Test_Callback is

   ---------------
   -- Dimension --
   ---------------

   overriding function Dimension
     (C   : not null access Cursor_Tag;
      Var : String) return Natural is
   begin
      if Var = "CURSOR" then
         return 2;
      elsif Var = "C_MAT3" then
         return 3;
      else
         return 0;
      end if;
   end Dimension;

   ------------
   -- Length --
   ------------

   overriding function Length
     (C    : not null access Cursor_Tag;
      Var  : String;
      Path : Templates_Parser.Dynamic.Path) return Natural is
   begin
      if Var = "CURSOR" then
         if Path'Length = 1 then
            return 3;
         elsif Path'Length /= 2 then
            raise Constraint_Error;
         else
            return Path (Path'Last);
         end if;

      elsif Var = "C_MAT3" then
         if Path'Length = 1 then
            return 3;
         elsif Path'Length = 2 then
            return 2;
         elsif Path'Length /= 3 then
            raise Constraint_Error;
         else
            return Path (Path'Last) + 2;
         end if;

      else
         return 0;
      end if;
   end Length;

   --------------
   -- Callback --
   --------------

   overriding procedure Value
     (L   : not null access Lazy_Tag;
      Var : String;
      S   : in out Templates_Parser.Translate_Set)
   is
      use Templates_Parser;
   begin
      if Var = "VAR1" then
         Insert (S, Assoc ("VAR1", "Callback value"));
      elsif Var = "DYNAMIC" then
         Insert (S, Assoc ("DYNAMIC", "This is a dynamic tag"));
      elsif Var = "N" then
         Insert (S, Assoc ("N", Integer'Image (L.N)));
         L.N := L.N + 1;
      elsif Var = "DYN_VECT" then
         Insert (S, Assoc ("DYN_VECT", +"12" & "89" & "90" & "2"));
      elsif Var = "TMPLT_NAME" then
         Insert (S, Assoc (Var, +"incl.tmplt"));
      end if;
   end Value;

   overriding procedure Value
     (L   : not null access Log_Context;
      Var : String;
      S   : in out Templates_Parser.Translate_Set)
   is
      pragma Unreferenced (L, S);
   begin
      Ada.Text_IO.Put_Line ("Tag " & Var & " missing.");
   end Value;

   overriding function Value
     (C    : not null access Cursor_Tag;
      Var  : String;
      Path : Templates_Parser.Dynamic.Path) return String is
   begin
      if Var = "CURSOR" then
         if Path'Length = 2 then
            return "CT" & Path (Path'First)'Img & Path (Path'First + 1)'Img;
         else
            raise Constraint_Error;
         end if;

      elsif Var = "C_MAT3" then
         if Path'Length = 3 then
            return "CMAT"
              & Path (Path'First)'Img
              & Path (Path'First + 1)'Img
              & Path (Path'First + 2)'Img;
         else
            raise Constraint_Error;
         end if;

      else
         raise Constraint_Error;
      end if;
   end Value;

end Test_Callback;

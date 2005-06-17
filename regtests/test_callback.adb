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

--  $Id$

with Ada.Text_IO;

package body Test_Callback is

   ---------------
   -- Dimension --
   ---------------

   function Dimension
     (C   : access Cursor_Tag;
      Var : in     String) return Natural is
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

   function Length
     (C    : access Cursor_Tag;
      Var  : in     String;
      Path : in     Templates_Parser.Dynamic.Path) return Natural is
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

   procedure Value
     (L   : access Lazy_Tag;
      Var : in     String;
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
      end if;
   end Value;

   procedure Value
     (L   : access Log_Context;
      Var : in     String;
      S   : in out Templates_Parser.Translate_Set)
   is
      pragma Unreferenced (L, S);
   begin
      Ada.Text_IO.Put_Line ("Tag " & Var & " missing.");
   end Value;

   function Value
     (C    : access Cursor_Tag;
      Var  : in     String;
      Path : in     Templates_Parser.Dynamic.Path) return String is
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

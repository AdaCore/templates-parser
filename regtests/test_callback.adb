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

   --------------
   -- Callback --
   --------------

   procedure Value
     (L   : in out Lazy_Tag;
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
     (L   : in out Log_Context;
      Var : in     String;
      S   : in out Templates_Parser.Translate_Set)
   is
      pragma Unreferenced (L, S);
   begin
      Ada.Text_IO.Put_Line ("Tag " & Var & " missing.");
   end Value;

end Test_Callback;

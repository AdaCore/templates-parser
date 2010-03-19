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

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Templates_Parser;

procedure Not_Initialized is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Templates_Parser;

   procedure Run1 is
      T1, T2 : Tag;
      R      : Unbounded_String;
      TS     : Translate_Set;
   begin
      T1 := T1 & T2;
      Insert (TS, Assoc ("VAR", T1));
      R := Parse ("not_initialized.tmplt", TS);
   exception
      when E : others =>
         Put_Line (Exception_Message (E));
   end Run1;

   procedure Run2 is
      T1, T2 : Tag;
      R      : Unbounded_String;
      TS     : Translate_Set;
   begin
      T1 := T1 & "toto" & "titi" & "toto";
      T1 := T1 & T2;
      Insert (TS, Assoc ("VAR", T1));
      R := Parse ("not_initialized.tmplt", TS);
   exception
      when E : others =>
         Put_Line (Exception_Message (E));
   end Run2;

begin
   Run1;
   Run2;
end Not_Initialized;

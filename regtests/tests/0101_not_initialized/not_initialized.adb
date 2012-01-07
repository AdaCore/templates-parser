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

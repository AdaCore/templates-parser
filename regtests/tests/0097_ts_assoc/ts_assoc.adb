------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with Templates_Parser.Query;

procedure TS_Assoc is

   use Ada.Text_IO;
   use Templates_Parser;

   TS : Translate_Set;

   procedure Display is

      procedure Output (A : Association; Quit : in out Boolean) is
      begin
         Put_Line (">" & Query.Variable (A));
      end Output;

      procedure Display_All_Assoc is new For_Every_Association (Output);

   begin
      Display_All_Assoc (TS);
   end Display;

   procedure Parse is
   begin
      Put_Line ("-----------------------------------");
      Put_Line (Parse ("ts_assoc.tmplt", TS, Keep_Unknown_Tags => True));
   end Parse;

   A : Association;

begin
   Parse;

   Insert (TS, Assoc ("NAME1", "one"));
   Insert (TS, Assoc ("NAME2", "two"));
   Display;
   Parse;

   A := Get (TS, "NAME2");
   Remove (TS, "NAME2");
   Parse;

   Insert (TS, A);
   Insert (TS, Assoc ("NAME1", "new one"));
   Parse;

   Remove (TS, "NAME1");
   Remove (TS, "NAME2");
   Parse;
end TS_Assoc;

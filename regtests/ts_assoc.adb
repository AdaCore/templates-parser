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

with Templates_Parser.Query;

procedure TS_Assoc is

   use Ada.Text_IO;
   use Templates_Parser;

   TS : Translate_Set;

   procedure Display is

      procedure Output (A : in Association; Quit : in out Boolean) is
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

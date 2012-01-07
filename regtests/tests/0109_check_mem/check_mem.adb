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

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_Io;

with Templates_Parser.XML;

procedure Check_Mem is

   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Templates_Parser;

   procedure Run is
      T1, T2, T, M : Tag;
      Translations : Translate_Table (1 .. 4);
      Result       : Unbounded_String;
      TS           : Translate_Set;
      TC           : Tag;
   begin
      TC := +"1" & "2" & "3" & "4" & "5" & "6" & "7" & "8" & "9";

      T1 := T1 & "first value";
      T1 := T1 & "second value";

      T2 := T2 & 1;
      T2 := T2 & 2;

      T := +T1;
      T := T & T2;

      M := +T & T & T & T & T & T;

      TC := M;
      TC := T1;

      Translations :=
        (1 => Assoc ("VAR", "Value"),
         2 => Assoc ("T1", T1),
         3 => Assoc ("T2", T2),
         4 => Assoc ("T", T));

      Result := Parse ("check_mem.tmplt", Translations);

      Put_Line ("=============================");
      Put_Line (To_String (Result));
      New_Line;

      XML.Save ("m.xml", To_Set (Translations));
      TS := XML.Load ("m.xml");
   end Run;

begin
   if Command_Line.Argument_Count = 1 then
      for K in 1 .. Positive'Value (Command_Line.Argument (1)) loop
         Put_Line ("Run" & Positive'Image (K));
         Run;
      end loop;
   else
      Put_Line ("Usage: check_mem <N>");
   end if;
end Check_Mem;

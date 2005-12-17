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
        (Assoc ("VAR", "Value"),
         Assoc ("T1", T1), Assoc ("T2", T2), Assoc ("T", T));

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

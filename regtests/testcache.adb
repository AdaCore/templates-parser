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

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Templates_Parser;

procedure Testcache is

   use Ada;
   use Ada.Strings.Unbounded;

   use type Calendar.Time;

   package TP renames Templates_Parser;

   use type TP.Vector_Tag;

   Translations : TP.Translate_Table
     := (TP.Assoc ("VAR1", "a value"),
         TP.Assoc ("VAR2",
                   +"a table" & "with" & "many" & "values" & "to" & "be"
                   & "displayed" & "one" & "by" & "one"),
         TP.Assoc ("VAR3",
                   +"one" & "two" & "three"),
         TP.Assoc ("VAR4", ""),
         TP.Assoc ("COND1", False),
         TP.Assoc ("COND2", True));

   Start, Stop : Calendar.Time;
   Elaps       : Duration;

   Right       : Unbounded_String;

   procedure Run (Filename : in String; Cached : in Boolean) is
      Current : Unbounded_String;
   begin
      Text_IO.Put_Line ("Cached " & Boolean'Image (Cached));

      for K in 1 .. 10 loop
         Start := Calendar.Clock;

         Current := To_Unbounded_String
           (TP.Parse (Filename, Translations, Cached));

         Stop := Calendar.Clock;

         if Right = Current then
            Text_IO.Put ("Ok  ");
         else
            Text_IO.Put ("NOk ");
         end if;

         Current := Null_Unbounded_String;

         Elaps := Stop - Start;
         Text_IO.Put_Line ("Elaps : " & Duration'Image (Elaps));
      end loop;
   end Run;

begin
   if Command_Line.Argument_Count = 0 then
      Text_IO.Put_Line ("Syntax: testcache <filename>");
      Text_IO.New_Line;
   else
      Right := To_Unbounded_String
        (TP.Parse (Command_Line.Argument (1), Translations, False));

      Run (Command_Line.Argument (1), False);
      Text_IO.New_Line;

      Run (Command_Line.Argument (1), True);
   end if;
end Testcache;

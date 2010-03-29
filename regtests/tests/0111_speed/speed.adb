------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                      Copyright (C) 2005-2010, AdaCore                    --
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

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Templates_Parser;

procedure Speed is

   use Ada;
   use Ada.Strings.Unbounded;
   use Templates_Parser;
   use type Calendar.Time;

   Str : constant String
     := "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      & "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
      & "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc";

   Start, Stop    : Calendar.Time;
   Elaps1, Elaps2 : Duration;

   ---------
   -- Job --
   ---------

   procedure Job (N : Positive) is
      V : Vector_Tag;
      T : Translate_Table (1 .. 1);
      R : Unbounded_String;
   begin
      for K in 1 .. N loop
         V := V & Str;
      end loop;

      T (1) := Assoc ("V", V);

      R := Parse ("speed.tmplt", T);
   end Job;

begin
   Start := Calendar.Clock;
   Job (4_000);
   Stop := Calendar.Clock;

   Elaps1 := Stop - Start;

   Start := Calendar.Clock;
   Job (40_000);
   Stop := Calendar.Clock;

   Elaps2 := Stop - Start;

   if Elaps2 < (Elaps1 * 15) then
      Text_Io.Put_Line ("OK");
   else
      Text_IO.Put_Line
        ("NOK: " & Duration'Image (Elaps1 * 15) & " < " & Elaps2'Img);
   end if;
end Speed;

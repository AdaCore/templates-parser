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

   if Elaps2 < (Elaps1 * 30) then
      Text_Io.Put_Line ("OK");
   else
      Text_IO.Put_Line
        ("NOK: " & Duration'Image (Elaps1 * 30) & " < " & Elaps2'Img);
   end if;
end Speed;

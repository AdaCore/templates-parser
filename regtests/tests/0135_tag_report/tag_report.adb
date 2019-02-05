------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Templates_Parser;
with Templates_Parser.Utils;

procedure Tag_Report is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Templates_Parser;

   procedure Report
     (Tag_Name : String;
      Filename : String := "";
      Line     : Natural := 0;
      Reason   : Reason_Kind) is
   begin
      Put_Line
        (Reason_Kind'Image (Reason)
         & "; T:" & Tag_Name
         & "; F:" & Filename & ':' & Utils.Image (Line));
   end Report;

   procedure Call
     (Template     : String;
      Translations : Translate_Table)
   is
      Result : constant String :=
                 Parse (Template, Translations, Report => Report'Access);
   begin
      null;
   end Call;

begin
   Call ("tag_report.tmplt",
          Translate_Table'
            (1 => Assoc ("TAG", "tag_value"),
             2 => Assoc ("NAME", "value")));
end Tag_Report;

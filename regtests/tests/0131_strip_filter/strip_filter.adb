------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with Templates_Parser; use Templates_Parser;
with Text_IO;          use Text_IO;

procedure Strip_Filter is
   Params : Tag;
   Set    : Translate_Set;
begin
   Params := Params & "Param1" & "Param2";
   Set    := +Assoc ("Parameters", Params) & Assoc ("Name", "Name1");
   declare
      Result1    : constant String := Parse ("signature.tmplt", Set);
      Interfaces : Tag;
      Set2       : Translate_Set;
   begin
      Interfaces := Interfaces & Result1;
      Set2 := +Assoc ("Provided_Interfaces", Interfaces);
      Put_Line (Parse ("header.tmplt", Set2));
   end;
end Strip_Filter;

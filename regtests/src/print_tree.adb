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

with Ada.Command_Line;
with Ada.Text_IO;

with Templates_Parser.Debug;

procedure Print_Tree is

   use Ada;
   use Templates_Parser;

begin
   if Command_Line.Argument_Count = 1 then
      Debug.Print_Tree (Command_Line.Argument (1));

   elsif Command_Line.Argument_Count = 2
     and then Command_Line.Argument (1) = "-M"
   then
      Debug.Print_Tree (Command_Line.Argument (2), Expand_Macro => True);

   elsif Command_Line.Argument_Count = 2
     and then Command_Line.Argument (1) = "-m"
   then
      Debug.Print_Tree (Command_Line.Argument (2));
      Debug.Print_Defined_Macros;

   else
      Text_IO.New_Line;
      Text_IO.Put_Line ("Usage: print_tree [-m] <filename>");
      Text_IO.New_Line;
      Text_IO.Put_Line ("       -m : print defined macros");
      Text_IO.New_Line;
   end if;
end Print_Tree;

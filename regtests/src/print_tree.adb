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

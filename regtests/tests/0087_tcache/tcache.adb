------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2005-2009, AdaCore                     --
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

with Ada.Text_IO;
with Templates_Parser;

procedure Tcache is

   use Ada.Text_IO;
   use Templates_Parser;

   procedure Set (Filename   : String;
                  C1, C2, C3 : String := "")
   is
      File : File_Type;
   begin
      Create (File, Out_File, Filename);
      Put_Line (File, C1);
      if C2 /= "" then
         Put_Line (File, C2);
      end if;
      if C3 /= "" then
         Put_Line (File, C3);
      end if;
      Close (File);
   end Set;

   procedure Run is
   begin
      Put_Line (Parse ("a.tmplt",      Cached => True));
      Put_Line (Parse ("b.tmplt",      Cached => True));
      Put_Line (Parse ("c.tmplt",      Cached => True));
      Put_Line (Parse ("main.tmplt",   Cached => True));
      Put_Line (Parse ("3level.tmplt", Cached => True));
   end Run;

begin
   for K in 1 .. 2 loop
      Release_Cache;
      Set ("a.tmplt", "@@INCLUDE@@ head.tmplt", "A", "@@INCLUDE@@ foot.tmplt");
      Set ("b.tmplt", "@@INCLUDE@@ head.tmplt", "B", "@@INCLUDE@@ foot.tmplt");
      Set ("c.tmplt", "@@INCLUDE@@ head.tmplt", "C", "@@INCLUDE@@ foot.tmplt");
      Set ("head.tmplt", "heading");
      Set ("foot.tmplt", "footing");
      Set ("main.tmplt", "@@INCLUDE@@ incl.tmplt", "main");
      Set ("incl.tmplt", "include file");
      Set ("3level.tmplt", "@@INCLUDE@@ 3l1.tmplt", "3level");
      Set ("3l1.tmplt", "@@INCLUDE@@ 3l2.tmplt", "3level-1");
      Set ("3l2.tmplt", "This is 3 level 2");
      Run;
      Put_Line ("--------------------------");

      delay 2.0;
      Set ("foot.tmplt", "footing 2");
      Set ("a.tmplt", "@@INCLUDE@@ head.tmplt", "New A",
           "@@INCLUDE@@ foot.tmplt");
      Set ("main.tmplt", "@@INCLUDE@@ incl.tmplt", "new main file");
      Set ("incl.tmplt", "new include file");
      Set ("3l2.tmplt", "This is 3 level 2 - v2");
      Run;
      Put_Line ("--------------------------");

      delay 2.0;
      Set ("incl.tmplt", "again a new include file");
      Set ("3l2.tmplt", "This is 3 level 2 - v3");
      Run;
      Put_Line ("--------------------------");
   end loop;
end Tcache;

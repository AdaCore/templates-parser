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

------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Templates_Parser.Debug is

   use Ada.Text_IO;
   use Ada.Strings.Fixed;

   -----------
   -- Print --
   -----------

   procedure Print (T : Tag) is

      procedure Print (T : Tag; K : Natural);
      --  Print tag T, K is the indent level

      -----------
      -- Print --
      -----------

      procedure Print (T : Tag; K : Natural) is
         Indent : constant String := K * ' ';
         N      : Tag_Node_Access := T.Data.Head;
      begin
         Put (Indent);
         Put_Line
           ("(N=" & Natural'Image (T.Data.Count)
            & ", Min=" & Natural'Image (T.Data.Min)
            & ", Max=" & Natural'Image (T.Data.Max)
            & ", Nested_Level=" & Natural'Image (T.Data.Nested_Level));

         while N /= null loop
            if N.Kind = Value then
               Put_Line (Indent & Indent & To_String (N.V));
            else
               Print (N.VS.all, K + 1);
            end if;

            N := N.Next;
         end loop;

         Put_Line (Indent & ")");
      end Print;

   begin
      Print (T, 1);
   end Print;

   --------------------------
   -- Print_Defined_Macros --
   --------------------------

   procedure Print_Defined_Macros is
   begin
      Templates_Parser.Print_Defined_Macros;
   end Print_Defined_Macros;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (Filename : String; Expand_Macro : Boolean := False) is
   begin
      Templates_Parser.Expand_Macro := Expand_Macro;
      Templates_Parser.Print_Tree (Filename);
   end Print_Tree;

end Templates_Parser.Debug;

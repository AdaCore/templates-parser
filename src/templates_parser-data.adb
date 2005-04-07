------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                        Copyright (C) 1999 - 2005                         --
--                                 AdaCore                                  --
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

with Ada.Text_IO;

separate (Templates_Parser)

package body Data is

   -----------
   -- Parse --
   -----------

   function Parse (Line : in String) return Tree is

      Begin_Tag : constant String
        := To_String (Templates_Parser.Begin_Tag);

      End_Tag : constant String
        := To_String (Templates_Parser.End_Tag);

      function Build (Line : in String) return Tree;
      --  Recursive function to build the tree

      -----------
      -- Build --
      -----------

      function Build (Line : in String) return Tree is
         Start, Stop : Natural;
      begin
         if Line = "" then
            return null;

         else
            Start := Strings.Fixed.Index (Line, Begin_Tag);

            if Start = 0 then
               --  No more tag
               return new Node'(Text,
                                null,
                                To_Unbounded_String (Line));
            else
               Stop := Strings.Fixed.Index (Line, End_Tag);

               if Stop = 0 then
                  Exceptions.Raise_Exception
                    (Internal_Error'Identity,
                     "Tag variable not terminated (missing " & End_Tag & ")");

               else
                  Stop := Stop + End_Tag'Length - 1;

                  if Start = Line'First then
                     --  The first token in Line is a variable
                     return new Node'
                       (Var,
                        Build (Line (Stop + 1 .. Line'Last)),
                        Build (Line (Start .. Stop)));

                  else
                     --  We have some text before the tag
                     return new Node'
                       (Text,
                        Build (Line (Start .. Line'Last)),
                        To_Unbounded_String (Line (Line'First .. Start - 1)));
                  end if;
               end if;
            end if;
         end if;
      end Build;

   begin
      return Build (Line);
   end Parse;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (D : in Tree) is
      N  : Tree := D;
      NL : Boolean := False;
   begin
      while N /= null loop
         case N.Kind is
            when Text =>
               declare
                  Value : constant String := To_String (N.Value);
               begin
                  Text_IO.Put (Value);
                  if Value'Length > 0 then
                     NL := Value (Value'Last) = ASCII.LF;
                  else
                     NL := False;
                  end if;
               end;
            when Var =>
               Text_IO.Put (Image (N.Var));
               NL := False;
         end case;
         N := N.Next;
      end loop;

      if not NL then
         Text_IO.New_Line;
      end if;
   end Print_Tree;

   -------------
   -- Release --
   -------------

   procedure Release (D : in out Tree) is

      procedure Free is new Ada.Unchecked_Deallocation (Node, Tree);

      P : Tree;
      T : Tree := D;

   begin
      while T /= null loop
         P := T;
         T := T.Next;

         if P.Kind = Var then
            Release (P.Var);
         end if;

         Free (P);
      end loop;
   end Release;

end Data;

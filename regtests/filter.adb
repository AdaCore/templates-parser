------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 2005                            --
--                                  AdaCore                                 --
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Filter is

   use Ada;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   procedure Set_Start (Str : in String);

   function Is_End (Str : in String) return Boolean;

   procedure Append (Str : in String);

   procedure Output;

   Line   : Unbounded_String;

   Buffer : String (1 .. 1_024);
   Last   : Natural;

   type Mode is (Simple_Tag, Composite_Tag, End_Tag);
   Current : Mode;

   ---------------
   -- Set_Start --
   ---------------

   procedure Set_Start (Str : in String) is
   begin
      if Index (Str, "SimpleTag") /= 0 then
         Current := Simple_Tag;
      elsif Index (Str, "CompositeTag") /= 0 then
         Current := Composite_Tag;
      else
         Current := End_Tag;
      end if;
   end Set_Start;

   ------------
   -- Is_End --
   ------------

   function Is_End (Str : in String) return Boolean is
   begin
      case Current is
         when Simple_Tag    => return Index (Str, "</SimpleTag>") /= 0;
         when Composite_Tag => return Index (Str, "</CompositeTag>") /= 0;
         when End_Tag       => return True;
      end case;
   end Is_End;

   ------------
   -- Append --
   ------------

   procedure Append (Str : in String) is
   begin
      for K in Str'Range loop
         if Str (K) /= ' ' then
            Append (Line, Str (K));
         end if;
      end loop;
   end Append;

   ------------
   -- Output --
   ------------

   procedure Output is
   begin
      Text_IO.Put_Line (To_String (Line));
      Line := Null_Unbounded_String;
   end Output;

begin
   --  Get headers

   Text_IO.Get_Line (Buffer, Last);
   Text_IO.Put_Line (Buffer (1 .. Last));

   Text_IO.Get_Line (Buffer, Last);
   Text_IO.Put_Line (Buffer (1 .. Last));

   while not Text_IO.End_Of_File loop
      --  Get tags

      Text_IO.Get_Line (Buffer, Last);
      Append (Buffer (1 .. Last));
      Set_Start (Buffer (1 .. Last));

      loop
         Text_IO.Get_Line (Buffer, Last);
         Append (Buffer (1 .. Last));
         exit when Is_End (Buffer (1 .. Last));
      end loop;

      Output;
   end loop;
end Filter;

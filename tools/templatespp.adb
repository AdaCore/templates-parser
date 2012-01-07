------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                      Copyright (C) 2008-2012, AdaCore                    --
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

--  This tool parses a file specified on the command line, and generates
--  another file. It can be used as a preprocessor.

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;

with Templates_Parser;

procedure TemplatesPP is

   use Ada;
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;
   use GNAT.Command_Line;

   procedure Help;
   --  Print help message

   procedure Process (In_File : String; Output : Text_IO.File_Type);
   --  Parses In_File, and print the result to Output

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      Text_IO.Put_Line ("Pre-processor based on the templates parser");
      Text_IO.Put_Line (Command_Name & " [-o output] file");
      Text_IO.Put_Line
         ("   Parses file and generate output file (or display on stdout)");
   end Help;

   -------------
   -- Process --
   -------------

   procedure Process (In_File : String; Output : Text_IO.File_Type) is
   begin
      Text_IO.Put_Line (Output, Templates_Parser.Parse (In_File));
   end Process;

   F           : Text_IO.File_Type;
   Output_File : Unbounded_String;

begin
   loop
      case Getopt ("o: h -help") is
         when 'h' =>
            Help;
            return;

         when '-' =>
            if Full_Switch = "-help" then
               Help;
               return;
            end if;

         when 'o' =>
            Output_File := To_Unbounded_String (Parameter);

         when others =>
            exit;
      end case;
   end loop;

   declare
      Input : constant String := Get_Argument;
   begin
      if Input = "" then
         Help;

      elsif Output_File = Null_Unbounded_String then
         Process (Input, Text_IO.Standard_Output);

      else
         Text_IO.Create (F, Text_IO.Out_File, To_String (Output_File));
         Process (Input, F);
         Text_IO.Close (F);
      end if;
   end;

exception
   when Text_IO.Name_Error =>
      Text_IO.Put_Line ("Input file not found");
end TemplatesPP;

------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                         Copyright (C) 2008, AdaCore                      --
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

--  This tool parses a file specified on the command line, and generates
--  another file.
--  It can be used as a preprocessor

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with AWS.Templates;         use AWS.Templates;
with GNAT.Command_Line;     use GNAT.Command_Line;

procedure TemplatesPP is
   procedure Help;
   --  Print help message

   procedure Process (In_File : String; Output : File_Type);
   --  Parses In_File, and print the result to Output

   procedure Help is
   begin
      Put_Line ("Pre-processor based on the AWS template parser");
      Put_Line (Command_Name & " [-o output] file");
      Put_Line
         ("   Parses file and generate output file (or display on stdin");
   end Help;

   procedure Process (In_File : String; Output : File_Type) is
   begin
      Put_Line (Output, Parse (In_File));
   end Process;

   F : File_Type;
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
         Process (Input, Standard_Output);
      else
         Create (F, Out_File, To_String (Output_File));
         Process (Input, F);
         Close (F);
      end if;
   end;

exception
   when Name_Error =>
      Put_Line ("Input file not found");
end TemplatesPP;

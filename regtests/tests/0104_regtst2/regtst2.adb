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

with Templates_Parser;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Task_Identification;

with Ada.Exceptions;

procedure Regtst2 is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Templates_Parser;

   task type Parser is
      pragma Storage_Size (100_000);
   end Parser;

   protected Start_Line is
      procedure Start;
      entry Wait;
   private
      State : Boolean := False;
   end Start_Line;

   protected body Start_Line is

      procedure Start is
      begin
         State := True;
      end Start;

      entry Wait when State is
      begin
         null;
      end Wait;

   end Start_Line;

   task body Parser is
      use Ada.Strings.Fixed;
      use Ada.Task_Identification;

      B                   : constant Boolean := False;
      Result              : Unbounded_String;
      Last_Error_Code     : constant Integer := 12345;
      Task_Image          : constant String  := Image (Current_Task);
      Short_Error_Message : String renames Task_Image;
      Error_Message       : constant String  :=
        140 * (Short_Error_Message & ASCII.LF);
   begin
      Start_Line.Wait;
      Result := Parse
        ("error.tmplt",
         (1 => Assoc ("CODE",    Last_Error_Code),
          2 => Assoc ("MESSAGE", Short_Error_Message),
          3 => Assoc ("DETAIL",  Error_Message)), Cached => True);
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Parser;

   Parsers : array (1 .. 20) of Parser;

begin
   delay 1.0;

   Start_Line.Start;
   Put_Line ("OK");
end Regtst2;

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

with Templates_Parser; use Templates_Parser;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Task_Identification;

with Ada.Exceptions;

procedure Regtst2 is

   use Ada.Strings.Unbounded;

   task type parser;

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
      end;

      entry Wait when State is
      begin
         null;
      end;

   end Start_Line;

   task body parser is
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;
      use Ada.Task_Identification;

      B                   : Boolean := False;
      Result              : Unbounded_String;
      Last_Error_Code     : Integer := 12345;
      Task_Image          : String := Image (Current_Task);
      Short_Error_Message : String renames Task_Image;
      Error_Message       : String := 140 * (Short_Error_Message & ASCII.LF);
   begin
      Start_Line.Wait;
      Result := Parse ("error.tmplt", (
                       1 => Assoc ("CODE",    Last_Error_Code),
                       2 => Assoc ("MESSAGE", Short_Error_Message),
                       3 => Assoc ("DETAIL",  Error_Message)
                     ), Cached => True);
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end;

   Parsers : array (1 .. 20) of Parser;

   Result : Unbounded_String;

begin
   delay 1.0;

   Result
     := Parse ("error.tmplt",
               (1 => Assoc ("CODE",    1),
                2 => Assoc ("MESSAGE", 2),
                3 => Assoc ("DETAIL",  4)), Cached => True);

   Start_Line.Start;
   Put_Line ("OK");
end Regtst2;

------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 1999                            --
--                               Pascal Obry                                --
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

with Ada.Strings.Unbounded;

package Templates_Parser is

   Template_Error : exception;

   Default_Begin_Tag : constant String    := "@@_";
   Default_End_Tag   : constant String    := "_@@";
   Default_Separator : constant Character := '|';

   type Association is private;

   type Translate_Table is array (Positive range <>) of Association;

   No_Translation : constant Translate_Table;

   function Assoc (Variable  : in String;
                   Value     : in String;
                   Is_Vector : in Boolean   := False;
                   Begin_Tag : in String    := Default_Begin_Tag;
                   End_Tag   : in String    := Default_End_Tag;
                   Separator : in Character := Default_Separator)
                   return Association;
   --  build an Association to be added to a Translate_Table. Is_Vector is
   --  set to true to build a vector variable. The Separator can be used to
   --  set the character used between values of a vector variable.

   function Assoc (Variable  : in String;
                   Value     : in Boolean;
                   Begin_Tag : in String    := Default_Begin_Tag;
                   End_Tag   : in String    := Default_End_Tag)
                   return Association;
   --  build an Association to be added to a Translate_Table. It set an assoc
   --  for variable to "TRUE" if value is true and "FALSE" otherwise.

   function Parse (Template_Filename : in String;
                   Translations      : in Translate_Table := No_Translation)
                   return String;
   --  parse the Template_File replacing variables' occurences by the
   --  corresponding values. See template file syntax above.

private

   use Ada.Strings.Unbounded;

   type Association is
      record
         Variable  : Unbounded_String;
         Value     : Unbounded_String;
         Separator : Character;
         Vector    : Boolean := False;
      end record;

   No_Translation : constant Translate_Table
     := (2 .. 1 => Association'(Null_Unbounded_String,
                                Null_Unbounded_String,
                                ASCII.Nul,
                                False));

end Templates_Parser;

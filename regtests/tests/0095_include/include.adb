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

with Ada.Exceptions;
with Ada.Text_IO;

with Templates_Parser;

procedure Include is

   use Ada;
   use Ada.Exceptions;

   Translations : Templates_Parser.Translate_Table :=
     (1 => Templates_Parser.Assoc ("NAME", "Ada"));

begin
   Text_IO.Put_Line (Templates_Parser.Parse ("include_1.html", Translations));
exception
   when E : others =>
      Text_IO.Put_Line
        ("raised " & Exception_Name (E) & " : " & Exception_Message (E));
end Include;

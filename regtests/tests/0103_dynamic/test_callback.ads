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

with Templates_Parser;

package Test_Callback is

   type Lazy_Tag is new Templates_Parser.Dynamic.Lazy_Tag with private;

   overriding procedure Value
     (L   : not null access Lazy_Tag;
      Var : String;
      S   : in out Templates_Parser.Translate_Set);

   type Log_Context is new Templates_Parser.Dynamic.Lazy_Tag with null record;

   overriding procedure Value
     (L   : not null access Log_Context;
      Var : String;
      S   : in out Templates_Parser.Translate_Set);

   type Cursor_Tag is new Templates_Parser.Dynamic.Cursor_Tag with null record;

   overriding function Dimension
     (C   : not null access Cursor_Tag;
      Var : String) return Natural;

   overriding function Length
     (C    : not null access Cursor_Tag;
      Var  : String;
      Path : Templates_Parser.Dynamic.Path) return Natural;

   overriding function Value
     (C    : not null access Cursor_Tag;
      Var  : String;
      Path : Templates_Parser.Dynamic.Path) return String;

private

   type Lazy_Tag is new Templates_Parser.Dynamic.Lazy_Tag with record
      N : Natural := 0;
   end record;

end Test_Callback;

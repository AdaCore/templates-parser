------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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

with Ada.Text_IO;

with Templates_Parser;

procedure Trans_Set is
   use Ada;
   use Templates_Parser;
   S1 : Translate_Set;
   S2 : Translate_Set;
begin
   Insert (S1, Assoc ("VAR1", 1));
   Insert (S1, Assoc ("VAR2", 2));

   Insert (S2, Assoc ("VAR3", 3));

   S1 := S1 & S2;

   Text_IO.Put_Line (Parse ("trans_set.tmplt", S1));
end Trans_Set;

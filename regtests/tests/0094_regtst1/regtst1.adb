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

with Templates_Parser; use Templates_Parser;
with Ada.Text_IO; use Ada.Text_IO;

procedure Regtst1 is
   Translate : Translate_Table  :=
     (1 => Assoc ("SESSIONS_V", +"session 1" & "session 2" & "session 3"),
      2 => Assoc ("KEYS_M", +(+"s1k1" & "s1k2" & "s1k3") & (+"s2k1" & "s2k2")),
      3 => Assoc ("VALUES_M", +(+"s1v1" & "s1v2" & "s1v3") & (+"s2v1")));
begin
   Put_Line (Parse ("regtst1.thtml", Translate));
end Regtst1;

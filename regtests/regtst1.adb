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

procedure Regtst1 is
   Translate : Translate_Table  :=
     (1 => Assoc ("SESSIONS_V", +"session 1" & "session 2" & "session 3"),
      2 => Assoc ("KEYS_M", +(+"s1k1" & "s1k2" & "s1k3") & (+"s2k1" & "s2k2")),
      3 => Assoc ("VALUES_M", +(+"s1v1" & "s1v2" & "s1v3") & (+"s2v1")));
begin
   Put_Line (Parse ("regtst1.thtml", Translate));
end Regtst1;

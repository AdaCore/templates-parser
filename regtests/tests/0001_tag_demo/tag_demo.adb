------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2005-2009, AdaCore                     --
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

with Ada.Text_IO;
with Templates_Parser;
with Templates_Parser.Debug;
with Templates_Parser.Utils;

procedure Tag_Demo is

   use Ada.Text_IO;
   use Templates_Parser;

   procedure Check_IV (Name : String; T : Tag);
   --  Check Image/Value convertion

   --------------
   -- Check_IV --
   --------------

   procedure Check_IV (Name : String; T : Tag) is
      Img : constant String := Utils.Image (T);
      NT  : constant Tag := Utils.Value (Img);
   begin
      Put_Line (Name & "=>" & Img);
      Put_Line (Name & "  " & Utils.Image (NT));

      if Img = Utils.Image (NT) then
         Put_Line ("   Ok");
      else
         Put_Line ("   NOk");
      end if;
   end Check_IV;

   T1 : Tag;
   T2 : Tag;
   T  : Tag;

   VT1 : Vector_Tag;
   VT2 : Vector_Tag;
   MT  : Matrix_Tag;

   I1  : constant Vector_Tag := +"1";
   I2  : constant Vector_Tag := "2" & I1;
   I3  : constant Vector_Tag := "3" & I2;

   M   : Tag;

   QT  : Tag;

begin
   T1 := T1 & "first" & "1_1_1";
   T2 := T2 & "second";

   T := T & T1 & T2;

   M := +T & T & T;

   Put_Line ("T1");
   Debug.Print (T1);

   Put_Line ("T2");
   Debug.Print (T2);

   Put_Line ("T");
   Debug.Print (T);

   Put_Line ("I3");
   Debug.Print (Tag (I3));

   Put_Line ("M");
   Debug.Print (M);

   Set_Separator (T, (1 => ASCII.LF));

   VT1 := VT1 & "first" & "1_1_1";
   VT2 := VT2 & "second";

   MT  := +VT1 & VT2;

   Put_Line
     (Parse ("gtag.tmplt",
             Translate_Table'
               (Assoc ("GTAG", T),
                Assoc ("VTAG", T1),
                Assoc ("MTAG", MT),
                Assoc ("OTAG", VT1))));

   Put_Line ("-----------------------");
   Check_IV ("T1", T1);
   Check_IV ("T2", T2);
   Check_IV ("T ", T);

   QT := QT & "un essai" & "with "" a quote" & "and ( and )";
   T := T & QT;
   Check_IV ("T", T);
end Tag_Demo;

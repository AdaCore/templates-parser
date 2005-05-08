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

--  Procedure to test the template_parser

with Ada.Text_IO;
with Ada.Command_Line;
with Templates_Parser;
with Test_Callback;

procedure Testme is

   use Ada;

   package TP renames Templates_Parser;

   use type TP.Vector_Tag;
   use type TP.Matrix_Tag;

   L_Tag : aliased Test_Callback.Lazy_Tag;

   KUT : Boolean := False;
   --  Keep Unknown Tags

   S1 : constant TP.Vector_Tag := +"single_value";

   V1 : constant TP.Vector_Tag := +"A1.1" & "A1.2" & "A1.3";
   V2 : constant TP.Vector_Tag := +"A2.1" & "A2.2" & "A2.3";
   V3 : constant TP.Vector_Tag := +"A3.1" & "A3.2" & "A3.3";
   V4 : constant TP.Vector_Tag := +"A4.1" & "A4.2" & "A4.3";

   M  : constant TP.Matrix_Tag := +V1 & V2 & V3 & V4;

   V5 : constant TP.Vector_Tag := +"one" & "two" & "three";
   V6 : constant TP.Vector_Tag := +"(one)" & "(two)";

   M2 : constant TP.Matrix_Tag := +V5 & V6;

   I1 : constant TP.Vector_Tag := +"1";
   I2 : constant TP.Vector_Tag := "2" & I1;
   I3 : constant TP.Vector_Tag := "3" & I2;

   VB0 : TP.Tag;
   VB1 : constant TP.Tag := +"B1.1" & "B1.2" & "B1.3";
   VB2 : constant TP.Tag := +"B2.1" & "B2.2" & "B2.3";
   VB3 : constant TP.Tag := +"B3.1" & "B3.2" & "B3.3";
   VB4 : constant TP.Tag := +"B4.1" & "B4.2" & "B4.3";

   MB  : constant TP.Tag := +VB1 & VB2 & VB3 & VB4;

   Nested_3 : constant TP.Tag := +M & MB;

   C  : constant TP.Tag := +"Class1" & "Class2";
   Mx : constant TP.Tag := +"Member1.1" & "Member1.2";
   My : constant TP.Tag := +"Member2.1" & "Member2.2" & "Member2.3";
   MF : constant TP.Tag := +Mx & My;

   Translations : TP.Translate_Table
     := (TP.Assoc ("VAR1", "a value"),
         TP.Assoc ("VAR2",
                   +"a table" & "with" & "many" & "values" & "to" & "be"
                   & "displayed" & "one" & "by" & "one"),
         TP.Assoc ("VAR3",
                   +"one" & "two" & "three"),
         TP.Assoc ("VAR4", ""),
         TP.Assoc ("VAR5", "T R uE"),
         TP.Assoc ("VAR6", "A"),
         TP.Assoc ("CLASS", C),
         TP.Assoc ("MEMBER", MF),
         TP.Assoc ("COND1", False),
         TP.Assoc ("COND2", True),
         TP.Assoc ("LABELS", +"lab1" & "lab2" & "lab3" & "lab4"),
         TP.Assoc ("MATDSIZE", M2),
         TP.Assoc ("SIZE0", VB0),
         TP.Assoc ("V5", V5),
         TP.Assoc ("V6", V6),
         TP.Assoc ("INV", I3),
         TP.Assoc ("MAT", M),
         TP.Assoc ("ONE", 1),
         TP.Assoc ("TWO", 2),
         TP.Assoc ("THREE", 3),
         TP.Assoc ("NUM0", 123456),
         TP.Assoc ("NUM1", "1234567.98765"),
         TP.Assoc ("NUM2", "12.98765"),
         TP.Assoc ("NUM3", "123.9"),
         TP.Assoc ("NUM4", "1234.98"),
         TP.Assoc ("NUM5", ".98"),
         TP.Assoc ("TEST_VAR", "Test"),
         TP.Assoc ("FILTER",
                   "   a text with < 6   words & good   92  chars like + > "),
         TP.Assoc ("TEXT",
                   "toto titi tata tata titi toto tata titi toto titi titi"),
         TP.Assoc ("REPL", "[_\1_]"),
         TP.Assoc ("WITHLF", "First line" & ASCII.LF & "Second line"),
         TP.Assoc ("WITHBR", "First line<br>Second line"),
         TP.Assoc ("VT", "vector_tag"),
         TP.Assoc ("DATE1", "1967-09-09"),
         TP.Assoc ("DATE2", "2003-12-12 13:34:12"),
         TP.Assoc ("URL1", "http://host:port/"),
         TP.Assoc ("URL2", "http://host/"),
         TP.Assoc ("URL3", "http://host/?"),
         TP.Assoc ("URL4", "http://host/?param1=un"),
         TP.Assoc ("URL5", "http://host:port/?param1=un&param2=deux"),
         TP.Assoc ("URL6", "http://host?param1=un&param2=deux&param3=trois"),
         TP.Assoc ("NESTED_3", Nested_3),
         TP.Assoc ("FILE", "/home/user"),
         TP.Assoc ("FILE2", "testme56.out"),
         TP.Assoc ("S1", S1),
         TP.Assoc ("QUOTE", """"""),
         TP.Assoc ("ACCENTS", "<été ça être paramètre à paraître> & """)
        );

begin
   if Command_Line.Argument_Count = 2
     and then Command_Line.Argument (2) = "kut"
   then
      KUT := True;
   end if;

   declare
      Result : constant String :=
        TP.Parse (Command_Line.Argument (1), Translations,
                  Keep_Unknown_Tags => KUT,
                  Lazy_Tag          => L_Tag'Unchecked_Access);
   begin
      Text_IO.Put (Result);
   end;
end Testme;

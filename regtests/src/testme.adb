------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

--  Procedure to test the template_parser

with Ada.Command_Line;
with Ada.Text_IO;

with Templates_Parser;
with Test_Callback;

procedure Testme is

   use Ada;

   package TP renames Templates_Parser;

   use type TP.Matrix_Tag;
   use type TP.Vector_Tag;

   L_Tag : aliased Test_Callback.Lazy_Tag;
   C_Tag : aliased Test_Callback.Cursor_Tag;

   KUT : Boolean := False;
   --  Keep Unknown Tags

   Cached : Boolean := False;
   --  Set to true if cache must be activated

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

   C   : constant TP.Tag := +"Class1" & "Class2";
   Mx  : constant TP.Tag := +"Member1.1" & "Member1.2";
   My  : constant TP.Tag := +"Member2.1" & "Member2.2" & "Member2.3";
   MF  : constant TP.Tag := +Mx & My;

   M11 : constant TP.Tag := +"M 1 1 1" & "M 1 1 2" & "M 1 1 3";
   M12 : constant TP.Tag := +"M 1 2 1" & "M 1 2 2" & "M 1 2 3" & "M 1 2 4";
   M21 : constant TP.Tag := +"M 2 1 1" & "M 2 1 2" & "M 2 1 3";
   M22 : constant TP.Tag := +"M 2 2 1" & "M 2 2 2" & "M 2 2 3" & "M 2 2 4";
   M31 : constant TP.Tag := +"M 3 1 1" & "M 3 1 2" & "M 3 1 3";
   M32 : constant TP.Tag := +"M 3 2 1" & "M 3 2 2" & "M 3 2 3" & "M 3 2 4";

   MC1 : constant TP.Tag := +M11 & M12;
   MC2 : constant TP.Tag := +M21 & M22;
   MC3 : constant TP.Tag := +M31 & M32;
   CM3 : constant TP.Tag := +MC1 & MC2 & MC3;

   FA  : constant TP.Tag := +"FA1" & "FA2" & "FA3";
   RT  : constant TP.Tag := +"RT1" & "RT2" & "RT3";
   RA  : constant TP.Tag := +"RA1" & "RA2" & "RA3";

   Translations : TP.Translate_Table
     := (TP.Assoc ("VAR1", "a value"),
         TP.Assoc ("VAR2",
                   +"a table" & "with" & "many" & "values" & "to" & "be"
                   & "displayed" & "one" & "by" & "one"),
         TP.Assoc ("VAR3", +"one" & "two" & "three"),
         TP.Assoc ("VAR4", ""),
         TP.Assoc ("VAR5", "T R uE"),
         TP.Assoc ("VAR6", "A"),
         TP.Assoc ("VAR7", +"ZZ" & "A3.4" & "A4.2" & "again!" & "A2.2" & "!"),
         TP.Assoc ("DECL", +"A : Natural := 0;"
                           & "Is_That_True : constant boolean := False;"
                           & "Value := 12.3;"
                           & "No_Init : Natural;"
                           & "Age : Positive := 12;"),
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
         TP.Assoc ("STMTS", "A := A + 1;" & ASCII.LF
                   & "A := B + 5;" & ASCII.LF
                   & "A := A / 2;"),
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
         TP.Assoc ("FILE2", "test.out"),
         TP.Assoc ("S1", S1),
         TP.Assoc ("QUOTE", """"""),
         TP.Assoc ("MAT3", CM3),
         TP.Assoc ("ACCENTS", "<été ça être paramètre à paraître> & """),
         TP.Assoc ("FOREIGN_ATTRS", FA),
         TP.Assoc ("REFERENCED_TABLES", RT),
         TP.Assoc ("REFERENCED_ATTRS", RA)
        );

begin
   if Command_Line.Argument_Count = 2 then
      if Command_Line.Argument (2) = "kut" then
         KUT := True;
      elsif Command_Line.Argument (2) = "cache" then
         Cached := True;
      end if;
   end if;

   declare
      Result : constant String :=
        TP.Parse (Command_Line.Argument (1), Translations,
                  Cached            => Cached,
                  Keep_Unknown_Tags => KUT,
                  Lazy_Tag          => L_Tag'Unchecked_Access,
                  Cursor_Tag        => C_Tag'Unchecked_Access);
   begin
      Text_IO.Put (Result);
   end;
end Testme;

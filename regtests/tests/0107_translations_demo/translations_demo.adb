------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Templates_Parser.XML;

procedure Translations_Demo is

   use Ada.Strings.Unbounded;
   use Templates_Parser;

   TS, TSR : Translate_Set;

   TA : Tag;
   TB : Tag;
   T  : Tag;

   L1, L2 : Tag;

begin
   TA := +"a1" & "a2" & "a3";
   TB := +"b1" & "b2" & "b3";
   T  := +TA & TB;

   L1 := +"les A" & "les B";
   L2 := +"2001" & "2002" & "2003";

   Insert (TS, Assoc ("ONE", 1));
   Insert (TS, Assoc ("TWO", 2));
   Insert (TS, Assoc ("VAR", "un essai"));
   Insert (TS, Assoc ("VAR_DESCRIPTION", "le label"));
   Insert (TS, Assoc ("TABLE", T));
   Insert (TS, Assoc ("TABLE_DESCRIPTION", "les a et les b"));
   Insert (TS, Assoc ("TABLE_DIM1_DESCRIPTION", "A/B"));
   Insert (TS, Assoc ("TABLE_DIM2_DESCRIPTION", "Année"));
   Insert (TS, Assoc ("TABLE_DIM1_LABELS", L1));
   Insert (TS, Assoc ("TABLE_DIM2_LABELS", L2));

   Ada.Text_IO.Put_Line (To_String (XML.Image (TS)));

   XML.Save ("ts.xml", TS);

   TSR := XML.Load ("ts.xml");

   XML.Save ("tsr.xml", TSR);

   Ada.Text_IO.Put_Line
     (Integer'Image (Size (TS)) & Integer'Image (Size (TSR)));

   TS := TSR & Assoc ("ONE", 1) &  Assoc ("TWO", 2) & Assoc ("THREE", 3);

   Ada.Text_IO.Put_Line
     (Integer'Image (Size (TS)) & Integer'Image (Size (TSR)));
end Translations_Demo;


-- $Id$

--  this package is used to parse an text template and produce a text file.
--  A template is a text file containing some specials variables. A
--  variable, in the text template must start with @@_ and end with _@@ by
--  default.
--
--  The parse command below takes a text template file and replace the
--  variables by the corresponding values contain in the Translate_Table.

with Ada.Strings.Bounded;

package Templates_Parser is

   Default_Begin_Tag : constant String := "@@_";
   Default_End_Tag   : constant String := "_@@";

   type Association is private;

   type Translate_Table is array (Positive range <>) of Association;

   No_Translation : constant Translate_Table;

   function Assoc (Variable  : in String;
                   Value     : in String;
                   Begin_Tag : in String := Default_Begin_Tag;
                   End_Tag   : in String := Default_End_Tag)
                   return Association;
   --  build an Association to be added to a Translate_Table.

   function Parse (Template_Filename : in String;
                   Translations      : in Translate_Table := No_Translation)
                   return String;
   --  parse the Template_File replacing variables' occurences by the
   --  corresponding values.

private

   package Strings_1024 is
     new Ada.Strings.Bounded.Generic_Bounded_Length (1_024);
   subtype B_String is Strings_1024.Bounded_String;

   type Association is
      record
         Variable : B_String;
         Value    : B_String;
      end record;

   No_Translation : constant Translate_Table
     := (2 .. 1 => Association'(Strings_1024.To_Bounded_String (""),
                                Strings_1024.To_Bounded_String ("")));

end Templates_Parser;

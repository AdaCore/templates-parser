
--  $Id$

--  This package is used to parse an text template and produce a text file.
--  A template is a text file containing some specials variables. A
--  variable, in the text template must start with @@_ and end with _@@ by
--  default.
--
--  The parse command below takes a text template file and replace the
--  variables by the corresponding values contained in the Translate_Table.
--
--  Special tags can be inserted in the template starting at column
--  1. Characters following the tag are discarded. This "free" space can be
--  used to put comments in the template.
--
--  These tags are :
--
--  @@TABLE@@
--  ---------
--
--  This tag is used to build a table. The lines between @@TABLE@@ and
--  @@END_TABLE@@ will be parsed for each values specified in the tag.
--
--  For example :
--
--  @@TABLE@@
--      @@_col1_@@     @@_col2_@@
--      ********       ********
--  @@END_TABLE@@
--
--  With col1 = "cell 1.1|cell 1.2" and col2 = "cell 2.1| cell 2.2"
--  (default separator is a pipe character)
--
--  will produce :
--      cell 1.1       cell 2.1
--      ********       ********
--      cell 1.2       cell 2.2
--      ********       ********
--
--  Note that the number of lines produced will be the equal to the smallest
--  number of values in the tags used in the table.
--
--  @@IF@@
--  ------
--
--  This tag is used to parse conditionaly part of the template.
--
--  For example :
--
--  @@IF@@ @@_T1_@@
--     ligne 1
--     ligne 2 avec un tag @@_T6_@@
--  @@ELSE@@
--     line 3
--  @@END_IF@@
--
--  The token just after @@IF@@ is a tag name. If this tag is defined
--  and the value is TRUE then following lines (until @@END_IF@@ or @@ELSE@@)
--  are parsed otherwise the else part is parsed if it is defined (i.e. the
--  else part is optional).
--
--  @@INCLUDE@@
--  -----------
--
--  This tag is used to include/inline another template.
--
--  For example :
--
--  @@INCLUDE@@ another_template.tmplt
--
--  This command will inline the file another_template.tmplt at the point of
--  this command.
--
--
--  Note that all commands can be mixed together as in :
--
--  @@IF@@ @@_OK_@@
--     well this is fine.
--  @@TABLE@@
--  @@_COL1_@@ @@_COL2_@@
--  @@END_TABLE@@
--  @@ELSE@@
--  @@INCLUDE@@ error_template.tmplt
--  @@ENDIF@@
--

with Ada.Strings.Unbounded;

package Templates_Parser is

   Template_Error : exception;

   Default_Begin_Tag : constant String    := "@@_";
   Default_End_Tag   : constant String    := "_@@";
   Default_Separator : constant Character := '|';

   type Association is private;

   type Translate_Table is array (Positive range <>) of Association;

   No_Translation : constant Translate_Table;

   function Assoc (Variable  : in String;
                   Value     : in String;
                   Begin_Tag : in String    := Default_Begin_Tag;
                   End_Tag   : in String    := Default_End_Tag;
                   Separator : in Character := Default_Separator)
                   return Association;
   --  build an Association to be added to a Translate_Table.

   function Assoc (Variable  : in String;
                   Value     : in Boolean;
                   Begin_Tag : in String    := Default_Begin_Tag;
                   End_Tag   : in String    := Default_End_Tag)
                   return Association;
   --  build an Association to be added to a Translate_Table. It set an assoc
   --  for variable to "TRUE" if value is true and "FALSE" otherwise.

   function Parse (Template_Filename : in String;
                   Translations      : in Translate_Table := No_Translation)
                   return String;
   --  parse the Template_File replacing variables' occurences by the
   --  corresponding values. See template file syntax above.

private

   use Ada.Strings.Unbounded;

   type Association is
      record
         Variable  : Unbounded_String;
         Value     : Unbounded_String;
         Separator : Character;
      end record;

   No_Translation : constant Translate_Table
     := (2 .. 1 => Association'(Null_Unbounded_String,
                                Null_Unbounded_String,
                                ASCII.Nul));

end Templates_Parser;

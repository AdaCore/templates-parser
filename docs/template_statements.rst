
.. _Template_statements:

.. highlight:: xml

*******************
Template statements
*******************

There are five different type statements. A tag statement is surrounded
by *@@*.

.. _Comments:

Comments
========

.. index:: Command, comments
.. index:: Command, @@--

Every line starting with *@@--* are comments and are completely
ignored by the parser. The resulting page will have the exact same
format and number of lines with or without the comments::

 @@-- This template is used to display the client's data
 @@-- It uses the following tags:
 @@--
 @@--    @_CID_@       Client ID
 @@--    @_ITEMS_V_@   List of items (vector tag)

 <P>Client @_CID_@

 ...

.. _INCLUDE_statement:

INCLUDE statement
=================

.. index:: Command, INCLUDE

This statement is used to include another template file. This is useful if you
have the same header and/or footer in all your HTML pages. For example::

 @@INCLUDE@@ header.tmplt

 <P>This is by Web page

 @@INCLUDE@@ footer.tmplt

It is also possible to pass arguments to the include file. These parameters
are given after the include file name. It is possible to reference these
parameters into the included file with the special variable names
*@_$<n>_@*, where *n* is the include's parameter index (0 is
the include file name, 1 the first parameter and so on)::

 @@INCLUDE@@ another.tmplt @_VAR_@ azerty

In file :file:`another.tmplt`:

**@_$0_@**
  is another.tmplt

**@_$1_@**
  is the variable @_VAR_@

**@_$2_@**
  is the string "azerty"

If an include variable references a non existing include parameter the
tag is kept as-is.

Note that it is possible to pass the include parameters using names,
a set of positional parameters can be pass first, so all following
include commands are identical::

 @@INCLUDE@@ another.tmplt one two three four "a text"
 @@INCLUDE@@ another.tmplt (one, two, 3 => three, 4 => four, 5 => "a text")
 @@INCLUDE@@ another.tmplt (one, 5 => "a text", 3 => three, 2 => two, 4 => four)

The file name can also be a tag. In this case the file loading is deferred at
the parsing time.

For security reasons the filename can't be a full pathname. If a full
pathname is passed then the leading directory separator is removed.

.. _IF_statement:

IF statement
============

.. index:: Command, IF

This is the conditional statement. The complete form is::

 @@IF@@ <expression1>
   part1
 @@ELSIF@@ <expression2
   part2
 @@ELSE@@
   part3
 @@END_IF@

<expression> is TRUE if it evaluates to one of "TRUE", "T" or
"1" and FALSE otherwise. Note that the test is not case sensitive.

The part1 one will be parsed if expression1 evaluate to TRUE, part2
will be parsed if expression2 evaluate to TRUE and the part3 will
be parse in any other case. The `ELSIF` and `ELSE` parts are
optional.

The expression here is composed of Boolean variables
and/or Boolean expression. Recognized operators are:

.. index:: Command, IF expression

**A = B**
  Returns TRUE if A equal B

**A /= B**
  Returns TRUE if A is not equal B

**A > B**
  Returns TRUE if A greater than B. If A and B are numbers it returns the
  the number comparison (5 > 003 = TRUE) otherwise it returns the string
  comparison ('5' > '003' = FALSE).

**A >= B**
  Returns TRUE if A greater than or equal to B. See above for rule about
  numbers.

**A < B**
  Returns TRUE if A lesser than B. See above for rule about numbers.

**A <= B**
  Returns TRUE if A lesser than or equal to B. See above for rule about
  numbers.

**A and B**
  Returns TRUE if A and B is TRUE and FALSE otherwise.

**A or B**
  Returns TRUE if A or B is TRUE and FALSE otherwise.

**A xor B**
  Returns TRUE if either A or B (but not both) is TRUE and FALSE otherwise.

**A in B**
  Returns TRUE if A is found into the composite tag B and FALSE
  otherwise. B must be a tag. If B contains a single value then this
  expression is equivalent to (A = B).

**not A**
  Returns TRUE if either A is FALSE and FALSE otherwise.

**A & B**
  Returns the catenation of A and B. A and B can be either strings or variables.

The default evaluation order is done from left to right, all operators
having the same precedence. To build an expression it is possible to
use parenthesis to change the evaluation order. A value with
spaces must be quoted as a string. So valid expressions could be::

 @@IF@@ (@_VAR1_@ > 3) or (@_COND1_@ and @_COND2_@)

 @@IF@@ not (@_VAR1_@ > 3) or (@_COND1_@ and @_COND2_@)

 @@IF@@ (@_VAR1_@ > 3) and not @_COND1_@

 @@IF@@ @_VAR1_@ = "a value"

 @@IF@@ "/" & @_FILE_@ = "/filename"

Note also that variables and values can be surrounded by quotes if needed.
Quotes are needed if a value contain spaces.

Let's see an example using an `IF` tag statement. With the following
template:

.. literalinclude:: src/user.tmplt
   :language: xml

The following program:

.. literalinclude:: src/user1.adb
   :language: ada

Will display:

.. literalinclude:: build/samples/user1.adb.res
   :language: xml

But the following program:

.. literalinclude:: src/user2.adb
   :language: ada

Will display:

.. literalinclude:: build/samples/user2.adb.res
   :language: xml

.. _TABLE_statement:

TABLE statement
===============

.. index:: Command, TABLE

Table tags are useful to generate `HTML` tables for example.
Basically the code between the *@@TABLE@@* and
*@@END_TABLE@@* will be repeated as many times as the vector
tag has values. If many vector tags are specified in a table
statement, the code between the table will be repeated a number of
times equal to the maximum length of all vector tags in the
`TABLE` tag statement.

A `TABLE` tag statement is a kind of implicit iterator. This is a very
important concept to build HTML tables. Using a composite tag variable in
a *@@TABLE@@* tag statement it is possible to build very
complex Web pages.

Syntax:

.. index:: Command, TERMINATE_SECTIONS
.. index:: Command, REVERSE
.. index:: Command, TERSE

::

 @@TABLE['REVERSE]['TERMINATE_SECTIONS]['TERSE]['ALIGN_ON("sep")]@@
 ...
 [@@BEGIN@@]
 ...
 [@@SECTION@@]
 ...
 [@@END@@]
 ...
 @@END_TABLE@

Let's have an example. With the following template:

.. literalinclude:: src/table.tmplt
   :language: xml

And the following program:

.. literalinclude:: src/table.adb
   :language: ada

The following output will be generated:

.. literalinclude:: build/samples/table.adb.res
   :language: xml

Note that we use vector tag variables here. A discrete variable tag in a table
will be replaced by the same (the only one) value for each row. A vector
tag outside a table will be displayed as a list of values, each value
being separated by a specified separator. The default is a comma and a
space ", ".

The complete prototype for the `Tag` Assoc function is:

.. code-block:: ada

 function Assoc
   (Variable  : in String;
    Value     : in Tag;
    Separator : in String := Default_Separator) return Association;
 --  Build an Association (Variable = Value) to be added to Translate_Table.
 --  This is a tag association. Separator will be used when outputting the
 --  a flat representation of the Tag (outside a table statement).

A table can contain many sections. The section to use will be selected
depending on the current line. For example, a table with two sections
will use different data on even and odd lines. This is useful when you
want to alternate the line background color for a better readability
when working on HTML pages.

A table with sections can have attributes:

**REVERSE**
  The items will be displayed in the reverse order.

**TERMINATE_SECTIONS**
  This ensure that the table output will end
  with the last section. If the number of data in the vector variable tag
  is not a multiple of the number of sections then the remaining section
  will be complete with empty tag value.

**TERSE**
  Empty lines won't be output. If the composite tag used into the table has
  an empty value then the corresponding line won't be output. This is
  especially important to avoid empty ending lines for table containing
  vector of different size.

**ALIGN_ON**
  The content of table will be aligned on the given separators.
  Multiple separators may be specified as coma separated strings, for
  example ALIGN_ON(":",":="). Each line will have the corresponding
  separator aligned in the specified order. That is, on the example
  above we first align on ":" and then ":=", if another ":" is found
  on the line it is not taken into account.

For the following template:

.. literalinclude:: src/table_section.tmplt
   :language: xml

And the following program:

.. literalinclude:: src/table_section.adb
   :language: ada

The following output will be generated:

.. literalinclude:: build/samples/table_section.adb.res
   :language: xml

It is important to note that it is possible to avoid code
duplication by using the *@@BEGIN@@* and *@@END@@*
block statements. In this case only the code inside the block is part
of the section, the code outside is common to all sections. Here is
an example to generate an HTML table with different colors for each line:

The template file above can be written this way:

.. literalinclude:: src/table_block.tmplt
   :language: xml

Another example to for the **ALIGN_ON** table attribute:

.. literalinclude:: src/table_align.tmplt
   :language: text

And the following program:

.. literalinclude:: src/table_align.adb
   :language: ada

The following output will be generated:

.. literalinclude:: build/samples/table_align.adb.res
   :language: ada

Into a table construct there are some additional variable tags available:

**@_UP_TABLE_LINE_@**
  .. index:: @_UP_TABLE_LINE_@

  This tag will be replaced by the table line number of the upper table
  statement. It will be set to 0 outside a table statement or inside a
  single table statement.

**@_TABLE_LINE_@**
  .. index:: @_TABLE_LINE_@

  This tag will be replaced by the current table line number. It will be
  replaced by 0 outside a table statement.

**@_NUMBER_LINE_@**
  .. index:: @_NUMBER_LINE_@

  This is the number of line displayed in the table. It will be replaced
  by 0 outside a table statement.

**@_TABLE_LEVEL_@**
  .. index:: @_TABLE_LEVEL_@

  This is the table level number. A table construct declared in a table
  has a level value of 2.  It will be replaced by 0 outside a table statement.

Let's have a look at a more complex example with mixed IF and TABLE
statements.

Here is the template:

.. literalinclude:: src/table_if.tmplt
   :language: xml

And the following program:

.. literalinclude:: src/table_if.adb
   :language: ada

The following output will be generated:

.. literalinclude:: build/samples/table_if.adb.res
   :language: xml

Table tag statements can also be used with matrix tag or more nested
tag variables. In this case, for a tag variable with N nested levels,
the Nth closest enclosing `TABLE` tag statement will be used for
the corresponding index. If there are not enough indexes, the last
axis are just streamed as a single text value.

Let's see what happens for a matrix tag:

* Inside a table of level 2 (a TABLE statement inside a TABLE
  statement).

  In this case the first `TABLE` iterates through the matrix lines.
  First iteration will use the first matrix's vector, second
  iteration will use the second matrix's vector and so on. And the second
  `TABLE` will be used to iterate through the vector's values.

* Inside a table of level 1.

  In this case the `TABLE` iterates through the matrix lines. First
  iteration will use the first matrix's vector, second iteration will
  use the second matrix's vector and so on. Each vector is then converted to
  a string by concatenating all values using the specified separator
  (see Assoc constructor for Tag or `Set_Separator` routine).

* Outside a table statement.

  In this case the matrix is converted to a string. Each line represents
  a vector converted to a string using the supplied separator (see point
  2 above), and each vector is separated by an ASCII.LF character. The
  separators to use for each level can be specified using `Set_Separator`.

Let's look at an example, with the following template:

.. literalinclude:: src/matrix.tmplt
   :language: xml

Using the program:

.. literalinclude:: src/matrix.adb
   :language: ada

We get the following result:

.. literalinclude:: build/samples/matrix.adb.res
   :language: xml

.. _SET_statement:

SET statement
=============

.. index:: Command, SET

The `SET` command tag can be used to define a constant or an
alias for an include file parameter. This is especially important in
the context of reusable template files. For example, instead of having
many references to the **red** color in an HTML document, it is better to
define a constant `COLOR` with the value **red** and use `COLOR`
everywhere. It is then easier to change the color afterward.

The first form, to define a simple constant that can be used as any
other variable in a template file, is::

 @@SET@@ <name> = <value>

The second form, to define an alias for a template file parameter, is::

 @@SET@@ <name> = $n [| <default_value>]

In this case <name> is an alias for the Nth include parameter. In this
form it is also possible to define a default value that would be used
if the Nth include parameter is not specified.

Some examples::

 @@SET@@ COLOR = red

 @@SET@@ SIZE = $1

 @@SET@@ COLOR = $4 | green

It is important to note that a variable is set global to a template
file. It means that constants set into an include file are visible
into the parent template. This is an important feature to be able to
have a "theme" like include template file for example.

.. _INLINE_statement:

INLINE statement
================

.. index:: Command, INLINE

The `INLINE` statement can be used to better control the
result's layout. For example it is not possible to have the results of
a vector tag on the same line, also it is not possible to have a
conditional output in the middle of a line. The `INLINE` block
tag statement can be used to achieve that.

Elements in an inlined block are separated by a single space by
default. It is possible to specify any string as the separator. The
text layout on an `INLINE` block has no meaning (the lines are
trimmed on both side). As part of the inline command it is possible to
specify texts to output before and after the block.

Syntax::

 @@INLINE[(<before>)(<separator>)(<after>)]@@
 ...
 @@END_INLINE@

There are three supported uses:

**@@INLINE@@**
  In this case there is no text before and after the block and the
  separator is a single space.


**@@INLINE(<separator>)@@**
  In this case there is no text before and after the block and the
  separator is the string given as parameter *<separator>*.


**@@INLINE(<before>)(<separator>)(<after>)@@**
  In this case all three values are explicitly given.

  *<before>*, *<separator>* and *<after>* may contain control characters:

    **\\n**
      To insert a new-line (CR+LF or LF depending on the Operation System)

    **\\r**
      To insert a line-feed

    **\\\\**
      To insert a single backslash

Let's look at an example, with the following template:

.. literalinclude:: src/table_inline.tmplt
   :language: xml

Using the program:

.. literalinclude:: src/table_inline.adb
   :language: ada

We get the following result:

.. literalinclude:: build/samples/table_inline.adb.res
   :language: xml

Another example with an `IF` tag statement:

.. literalinclude:: src/if_inline.tmplt
   :language: xml

Using the program:

.. literalinclude:: src/if_inline.adb
   :language: ada

We get the following result:

.. literalinclude:: build/samples/if_inline.adb.res
   :language: xml

.. _MACRO_statement:

MACRO statement
===============

.. index:: Command, MACRO

The `MACRO` statement is used to defined macros that can be used
in other places in the template. The macro statement takes a single
parameter which is the name of the macro.

Syntax::

 @@MACRO(NAME)@@
 ...
 @@END_MACRO@

The code inside the macro can be anything supported by the templates
engine. There is no restriction. The parameters inside the macro are
referenced as @_$N_@ (where N is a number and corresponds to the Nth
parameter passed to the macro). There is no maximum number of
parameters. A reference to a parameter that has no corresponding formal
parameter at the call point is ignored (the value will be the empty
string).

For example:

.. literalinclude:: src/macro.tmplt
   :language: xml

For using macros see :ref:`Macros`.

.. _EXTENDS_and_BLOCK_statements:

EXTENDS and BLOCK statements
============================

.. index:: Command, EXTENDS
.. index:: Command, BLOCK

The `EXTENDS` statement is similar to `INCLUDE`. However, it is
possible to replace parts of the included file. These parts are defined
with the `BLOCK` statement.

Syntax::

 @@EXTENDS@@ filename [variables]
   @@BLOCK(name1)@@
   ...
   @@END_BLOCK@@

   @@BLOCK(name2)@@
   ...
   @@END_BLOCK@@
 @@END_EXTENDS@

And in the included file (:file:`filename` in the above example)::

 ...

 @@BLOCK(name1)@@
   default contents1
 @@END_BLOCK@@

 @@BLOCK(name3)@@
 default contents3
 @@END_BLOCK@@
 ...

When parsing the first file, it will automatically include the
contents of the second file. However, the various `BLOCK`
will be replaced by the value given in the `EXTENDS` statement,
if a value is provided. If no value is provided, the default value
given in the included file will be used.

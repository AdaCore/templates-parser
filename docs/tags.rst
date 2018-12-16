.. _Tags:

****
Tags
****

.. _Tags_in_template_files:
.. highlight:: ada

Tags in template files
======================

A tag is a string found in the template page and surrounded by a specific set
of characters. The default is **@_** at the start and **_@** at the
end of the tag. This default can be changed using `Set_Tag_Separators`
routine, see :ref:`Templates_Parser_API_Reference`. Note that it must be
changed as the first API call and should not be changed after that.

The tag will be replaced by a value specified in the Ada code.
In this context, the role of the Ada code is therefore to prepare what is
known as a translation, and then pass it to the templates parser, along with
the name of the template file to parse. This results in an expanded version
of the templates file, where all tags have been replaced by the value given
in the Ada code.

Let's start with a simple example. Here is the contents of the file
:file:`demo.tmplt`, which is a very basic template file:

.. literalinclude:: src/demo.tmplt
   :language: xml

On its own, this template has little interest. However, it is used from some
Ada code similar to the following :file:`demo.adb` file:

.. literalinclude:: src/demo.adb
   :language: ada

Compile this program, link with the templates parser, and when you run it,
the output will be:

.. literalinclude:: build/samples/demo.adb.res

As you can imagine, this is a bare bone example. More complex structures are
of course possible. One thing to note, though, is that the template file
requires no Ada knowledge for editing, and is strongly related to your
application domain. One of the main usage for such templates is to generate
web pages. This can be done by a designer that knows nothing of how your
Ada code works. But you can use templates in other domains, including to
generate Ada code.

.. _Translations:

Translations
============

In your Ada code, you can associate one or more values with a name, and
then reference that name in the template file as we just saw above.

Associating the value(s) with the name is done through one of the `Assoc`
constructors, see :ref:`Templates_Parser_API_Reference`. Ada's overloading
resolution mechanism will take care of calling the appropriate constructor
automatically.

These associations are then grouped into a dictionary. This dictionary
is passed along with the name of the template file to the `Parse`
routine itself, which generates the final expanded representation of the
template. In fact, you will almost never have to manipulate an association
directly, since as soon as it is created you store it in the dictionary.

There are two types of dictionaries in the templates parser:

*Translate_Table*
  .. index:: Translate_Table

  This is an array of associations. If you know the exact number of
  associations when you write your code, this will generally provide
  a very readable code. The array can be initialized as soon as it is
  declared::

   declare
      T : constant Translate_Table :=
            (1 => Assoc ("NAME1", Value1),
             2 => Assoc ("NAME2", Value2));
   begin
      Put_Line (Parse ("demo.tmplt", T));
   end;

*Translate_Set*
  .. index:: Translate_Set

  If, on the other hand, you do not know statically the number of
  associations, it is generally a lot more flexible to use another type
  of dictionary, which isn't limited in size. It is also better to use
  this type of dictionary if you need extra code to compute the values::

   declare
      T : Translate_Set
   begin
      Insert (T, Assoc ("NAME1", Value1));
      Insert (T, Assoc ("NAME2", Value2));
   end;

Internally, the templates parser will always convert all dictionaries
to a `Translate_Set`, which is much more efficient when we need to
look values up.

.. _Discrete,_Boolean_and_Composite_values:

Discrete, Boolean and Composite values
======================================

As we just saw, the values by which a tag is replaced must be provided
by the Ada code. Such values can be provided in different formats,
depending on the intended use.

The three kinds of tags are **discrete**, **Boolean** and **composite**
tags. These are all ways to associate one or more value to a name, which
is the name used in the template file.

*discrete values*
  .. index:: Tag, discrete

  This represents a single value associated with a name. The types of value
  currently supported are String, Unbounded_String and Integer::

   Insert (T, Assoc ("NAME", 2));
   Insert (T, Assoc ("NAME", "VALUE"));

*Boolean values*
  .. index:: Tag, Boolean

  These are similar to discrete values. However, they are more convenient
  to manipulate within *@@IF@@* statements in the template. When
  outside an `IF` statement, such values are represented as TRUE or
  FALSE::

   Insert (T, Assoc ("NAME", True));

*composite values*
  .. index:: Tag, composite

  A composite tag is a variable which contains a set of values. In terms of
  programming languages, these would generally be called vectors. Since each
  value within that can itself be a composite tag, you can therefore build
  multi-dimensional arrays.

  These kind of variables will be used with the `TABLE` tag statement
  see :ref:`TABLE_statement`. Outside a table statement, the tag is
  replaced by all values concatenated with a specified separator. See
  `Set_Separator` routine. Such tag are variables declared in the
  Ada program a `Templates_Parser.Tag` type.

  There are many overloaded constructors to build a composite tags (see "+"
  operators).  The "+" operators are used to build a Tag item from
  standard types like String, Unbounded_String, Character, Integer and
  Boolean.

  To add items to a Tag many overloaded operators are provided (see
  "&" operators). The "&" operators add one item at the start or the end
  of the tag. It is possible to directly add String, Unbounded_String,
  Character, Integer and Boolean items using one of the overloaded operator.

  A tag composed of only Boolean values TRUE or FALSE is called a
  Boolean composite tag. This tag is to be used with a `IF` tag statement
  inside a `TABLE` tag statement.

  It is possible to build a composite tag having any number of nested
  level. A vector is a composite tag with only one level, a matrix is a
  composite tag with two level (a Tag with a set of vector tag).

  Two aliases exists for composite tags with one or two nested level,
  they are named `Vector_Tag` and `Matrix_Tag`. In the suite
  of the document, we call *vector tag* a tag with a single nested
  level and *matrix tag* a tag with two nested level::

   --  Building a composite tag
   --  Then add it into a translate set

   declare
      V : Tag;
      T : Translate_Set;
   begin
      for Index in 1 .. 10 loop
         V := V & I;
      end loop;
      Insert (T, Assoc ("VECTOR", V));
   end;

.. _Filters:

Filters
=======

.. index:: Filters

Within the template file, functions can be applied to tags. Such functions
are called `filters`. These filters might require one or more
parameters, see the documentation for each filter.

The syntax is::

 @_[[FILTER1_NAME[(parameter)]:]FILTER2_NAME[(parameter)]:]SOME_VAR_@

When multiple filters are associated to a tag, they are evaluated from right
to left. In the example above, `FILTER1_NAME` is applied to the result
of applying `FILTER2_NAME` to `SOME_VAR`.

Remember that one of the goals in using templates is to remove as much
hard-coded information from the actual Ada source, and move it into easily
editable external files. Using filters is a convenient way to give the template
designer the power to specify the exact output he wants, even without changing
the Ada code. For instance, imagine that one suddenly decides that some
names should be capitalized in a template. There are two solutions to such
a change in design:

* Modify the Ada code to capitalize strings before storing them in
  a tag variable. What if, in the template, we need the name once capitalized,
  and once with its original casing ? This means the Ada code would have to
  create two tags.

* Modify the template itself, and use a filter. A single tag is required
  on the Ada side, which doesn't even have to be changed in fact. The template
  would for instance become::

   @_CAPITALIZE:VAR_@ : constant String := "@_VAR_@";

The templates parser comes with a number of predefined filters, that can be
used in various situations. Some of these are highly specialized, but most of
them are fairly general. You can also define your own filters, adapted to
specific needs you might have.

Here are some more examples using the predefined filters,
If VAR is set to "*vector_tag*", ONE to "1" and TWO to "2" then::

 @_VAR_@                                         ->  vector_tag
 @_UPPER:VAR_@                                   ->  VECTOR_TAG
 @_CAPITALIZE:VAR_@                              ->  Vector_Tag
 @_EXIST:VAR_@                                   ->  TRUE
 @_UPPER:REVERSE:VAR_@                           ->  GAT_ROTCEV
 @_MATCH(VEC.*):UPPER:VAR_@                      ->  TRUE
 @_SLICE(1..6):VAR_@                             ->  vector
 @_REPLACE(([^_]+)):VAR_@                        ->  vector
 @_REPLACE(([a-z]+)_([a-z]+)/\\\\2_\\\\1):VAR_@  ->  tag_vector
 @_"+"(TWO):ONE_@                                ->  3
 @_"-"(TWO):ONE_@                                -> -1

.. _Predefined_filters:

Predefined filters
------------------

Here is the complete list of predefined filters that come with the templates
parser.

**"+"(N)** or **ADD(N)**
  .. index:: Filter, "+"

  Add N to variable and return the result. If the current variable value
  is not a number it returns the empty string. N must be a number or a
  discrete tag variable whose value is a number.

**"-"(N)** or **SUB(N)**
  .. index:: Filter, "-"

  Subtract N to variable and return the result. If the current variable value
  is not a number it returns the empty string. N must be a number or a
  discrete tag variable whose value is a number.

**"*"(N)** or **MULT(N)**
  .. index:: Filter, "*"

  Multiply N with variable and return the result. If the current variable value
  is not a number it returns the empty string. N must be a number or a
  discrete tag variable whose value is a number.

**"/"(N)** or **DIV(N)**
  .. index:: Filter, "/"

  Divide variable by N and return the result. If the current variable value
  is not a number it returns the empty string. N must be a number or a
  discrete tag variable whose value is a number.

**ABS**
  .. index:: Filter, ABS

  Returns the absolute value.

**ADD_PARAM(NAME[=VALUE])**
  .. index:: Filter, ADD_PARAM

  Add a parameter into an URL. This routine adds the '?' and '&'
  character if needed. *VALUE* can be a tag variable name.

**BR_2_EOL(EOL)**
  .. index:: Filter, BR_2_EOL

  Replaces all occurrences of the `<br>` HTML tag by a line terminator
  determined by EOL. EOL must be either CR (Carriage-Return), LF (Line-Feed),
  LFCR (Line-Feed followed by Carriage-Return) or CRLF (Carriage-Return
  followed by Line-Feed).

**BR_2_LF**
  .. index:: Filter, BR_2_LF

  Shortcut for BR_2_EOL(LF).

**CAPITALIZE**
  .. index:: Filter, CAPITALIZE

  Put all characters in the variable in lower case except characters after
  a space or an underscore which are set in upper-case.

**CLEAN_TEXT**
  .. index:: Filter, CLEAN_TEXT

  Keep only letters and digits all others characters are changed to
  spaces.

**COMA_2_POINT**
  .. index:: Filter, COMA_2_POINT

  Replaces all comas by points.

**CONTRACT**
  .. index:: Filter, CONTRACT

  Converts any suite of spaces by a single space character.

**DEL_PARAM(NAME)**
  .. index:: Filter, DEL_PARAM

  Delete parameter NAME from the URL. This routine removes the '?' and '&'
  character if needed. Returns the input string as-is if the parameter
  is not found.

**EXIST**
  .. index:: Filter, EXIST

  Returns **True** if variable is set and has a value different that the null
  string and **False** otherwise.

**FILE_EXISTS**
  .. index:: Filter, FILE_EXISTS

  Returns **True** if variable is set and has a value that corresponds
  to a file name present on the file system and **False** otherwise.

**FORMAT_DATE(FORMAT)**
  .. index:: Filter, FORMAT_DATE

  Returns the date with the given format. The date must be in the ISO
  format (YYYY-MM-DD) eventually followed by a space and the time with
  the format HH:MM:SS. If the date is not given in the right format it
  returns the date as-is. The format is using the GNU/date description
  patterns as also implemented in `GNAT.Calendar.Time_IO`.

  *Characters*:

    **%**: a literal %

    **n**: a newline

    **t**: a horizontal tab

  *Time fields*:

    **%H**: hour (00..23)

    **%I**: hour (01..12)

    **%k**: hour ( 0..23)

    **%l**: hour ( 1..12)

    **%M**: minute (00..59)

    **%p**: locale's AM or PM

    **%r**: time, 12-hour (hh:mm:ss [AP]M)

    **%s**: seconds  since 1970-01-01  00:00:00 UTC (a nonstandard extension)

    **%S**: second (00..59)

    **%T**: time, 24-hour (hh:mm:ss)

  *Date fields*:

    **%a**: locale's abbreviated weekday name (Sun..Sat)

    **%A**: locale's full weekday name, variable length (Sunday..Saturday)

    **%b**: locale's abbreviated month name (Jan..Dec)

    **%B**: locale's full month name, variable length (January..December)

    **%c**: locale's date and time (Sat Nov 04 12:02:33 EST 1989)

    **%d**: day of month (01..31)

    **%D**: date (mm/dd/yy)

    **%h**: same as %b

    **%j**: day of year (001..366)

    **%m**: month (01..12)

    **%U**: week number of year with  Sunday as first day of week (00..53)

    **%w**: day of week (0..6) with 0 corresponding to Sunday

    **%W**: week number of year with  Monday as first day of week (00..53)

    **%x**: locale's date representation (mm/dd/yy)

    **%y**: last two digits of year (00..99)

    **%Y**: year with four digits (1970...)

  *Padding*:
    By default,  date pads numeric fields with zeroes. GNU date recognizes
    the following nonstandard numeric modifiers:

    **-** (hyphen): do not pad the field

    **_** (underscore): pad the field with spaces

**FORMAT_NUMBER([DIGITSEP])**
  .. index:: Filter, FORMAT_NUMBER

  Returns the number with a separator added between each 3 digits
  blocks. The decimal part is not transformed. If the data is not a
  number nothing is done. The default separator is a space, although you can
  specify any separator (a single character) you wish. DIGITSEP can also be
  the name of another tag, whose value (or the first character of it) will be
  used as a separator.

**IS_EMPTY**
  .. index:: Filter, IS_EMPTY

  Returns **True** if variable is the empty string and **False** otherwise.

**LF_2_BR**
  .. index:: Filter, LF_2_BR

  Replaces all occurrences of the character LF (Line-Feed) by a
  `<br>` HTML tag.

**LOWER**
  .. index:: Filter, LOWER

  Put all characters in the variable in lower-case.

**MATCH(REGEXP)**
  .. index:: Filter, MATCH

  Returns **True** if variable match the regular expression passed as
  filter's parameter. The regular expression is using a format as
  found in :file:`gawk`, :file:`sed` or :file:`grep` tools.

**MAX(N)**
  .. index:: Filter, MAX

  Returns the maximum value between the variable and the parameter.

**MIN(N)**
  .. index:: Filter, MIN

  Returns the minimum value between the variable and the parameter.

**MOD(N)**
  .. index:: Filter, MOD

  Returns variable modulo N. If the current variable value is not a
  number it returns the empty string. N must be a number or a
  discrete tag variable whose value is a number.

**NEG**
  .. index:: Filter, NEG

  Change the sign of the value.

**NO_DYNAMIC**
  .. index:: Filter, NO_DYNAMIC

  This is a special command filter which indicates that the tag must not
  be searched in the dynamic tags. See :ref:`Lazy_Tag`. `NO_DYNAMIC` must
  be the first filter. This filter returns the value as-is.

**NO_DIGIT**
  .. index:: Filter, NO_DIGIT

  Replaces all digits by spaces.

**NO_LETTER**
  .. index:: Filter, NO_LETTER

  Replaces all letters by spaces.

**NO_SPACE**
  .. index:: Filter, NO_SPACE

  Removes all spaces in the variable.

**OUI_NON**
  .. index:: Filter, OUI_NON

  If variable value is **True** it returns **Oui**, if **False** it
  returns **Non**, otherwise does nothing. It keeps the way **True/False** is
  capitalized (all upper, all lower or first letter capital).

**POINT_2_COMA**
  .. index:: Filter, POINT_2_COMA

  Replaces all comas by points.

**REPEAT(N)**
  .. index:: Filter, REPEAT

  Returns *N* times the variable, *N* being passed as filter's parameter.
  *N* must be a number or a discrete tag variable whose value is a number.

**REPEAT(STR)**
  .. index:: Filter, REPEAT

  This is the second `REPEAT` form. In this case *STR* is repeated
  a number of time corresponding to the variable value which must be a number.

**REPLACE(REGEXP[/STR])**
  .. index:: Filter, REPLACE

  This filter replaces **\\n** (where *n* is a number) *STR*'s
  occurrences by the corresponding match from *REGEXP*. The first match
  in *REGEXP* will replace **\\1**, the second match **\\2** and so
  on. Each match in *REGEXP* must be parenthesized. It replaces only
  the first match. *STR* is an optional parameter, its default value
  is **\\1**. It is possible to escape characters in *STR* to avoid
  parsing confusions. This is required if you need to have **@_** or
  **_@** or a parenthesis in *STR* for example. *STR* can be a tag
  variable name. *STR* can contain the following escaped characters :
  **\\n** Carriage Return, **\\r** Line Feed and **\\t** for Horizontal
  Tabulation.

**REPLACE_ALL(REGEXP[/STR])**
  .. index:: Filter, REPLACE_ALL

  Idem as above but replaces all occurrences.

**REPLACE_PARAM(NAME[=VALUE])**
  .. index:: Filter, REPLACE_PARAM

  This is filter is equivalent to
  ADD_PARAM(*NAME[=VALUE]*):DEL_PARAM(*NAME*). *VALUE* can be a
  tag variable name.

**REVERSE**
  .. index:: Filter, REVERSE

  Reverse the string.

**SIZE**
  .. index:: Filter, SIZE

  Returns the size (number of characters) of the string value.

**SLICE(x .. y)**
  .. index:: Filter, SLICE

  Returns the sub-string starting from position x and ending to position
  y. Note that the string to slice always start from position 1.
  If x or y are negative, they are counted from the end of the string, so
  that 0 matches the last character of the string, -1 matches the character
  just before,...

**STRIP**
  .. index:: Filter, STRIP

  Removes leading and trailing spaces and special characters HT, VT, CR,
  LF, NUL, EOT, BS, FF.

**TRIM**
  .. index:: Filter, TRIM

  Removes leading and trailing spaces.

**UPPER**
  .. index:: Filter, UPPER

  Put all characters in the variable in upper-case.

**WEB_ENCODE**
  .. index:: Filter, WEB_ENCODE

  As WEB_ESCAPE and also encodes all non 7-bit characters and non
  printable characters using **&#xxx;** HTML encoding.

**WEB_ESCAPE**
  .. index:: Filter, WEB_ESCAPE

  Replaces characters '<', '>', '"' and '&' by corresponding HTML
  sequences: &lt; &gt; &quot; and &amp;

**WEB_NBSP**
  .. index:: Filter, WEB_NBSP

  Replaces all spaces by an HTML non breaking space.

**WRAP(N)**
  .. index:: Filter, WRAP

  Wraps lines having more N characters.

**YES_NO**
  .. index:: Filter, YES_NO

  If variable value is **True** it returns **Yes**, if **False** it
  returns **No**, otherwise does nothing. It keeps the way **True/False** is
  capitalized (all upper, all lower or first letter capital).

.. _User_defined_filters:

User defined filters
--------------------

It is also possible to define a new filter by registering a callback
routine associated with the filter name.

You can define three kinds of filters: filters that take no argument, and are
therefore simply used as in *@_FILTER:TAG_@*, filters that take one
or more arguments, used as in *@_FILTER(param1,param2):TAG_@*, and filters
that are implemented as tagged objects, and take the same form as the filters
with arguments described above.

The latter form of filters (using tagged types) provides slightly more
flexibility, as you can store your own user data in the filter when it is
registered. Among other things, this makes it possible to share filters
between various applications, when the filter needs to access some
application-specific variable as well.

The templates parser will not try to interpret the parameters for you, and
will simply return the string representation of the list of parameters,
for instance `"param1,param2"` in the example above. This provides
enhanced flexibility, since you are free to use any parameter-separator
you want, and to interpret parameters as integer, strings, references to
other tags,...

The templates parser doesn't support tag substitution within the parameter
list, but this is trivial to implement in your own code. For instance,
if the user has used *@_FILTER(REFTAG):TAG_@*, you are free to either
take `REFTAG` as a constant string, or as a reference to another tag,
to be looked up in a translation table. You should of course properly
document the behavior of your filter.

Here is the templates parser API for defining your own custom filters::

 type Filter_Context is record
    Translations : Translate_Set;
    Lazy_Tag     : Dynamic.Lazy_Tag_Access;
 end record;

 type Callback is access function
   (Value      : in String;
    Parameters : in String;
    Context    : in Filter_Context) return String;
 --  User's filter callback

 type Callback_No_Param is access function
   (Value   : in String;
    Context : in Filter_Context) return String;
 --  User's filter callback

 procedure Register_Filter
   (Name    : in String;
    Handler : in Callback);
 --  Register user's filter Name using the specified Handler

 procedure Register_Filter
   (Name    : in String;
    Handler : in Callback_No_Param);
 --  Register user's filter Name using the specified Handler

In the above calls, Value is the value of the tag on which the filter
applies. In the examples above, that would be the value of `TAG`
as looked up in the translation table. Context contains the the translation
table and the current lazy tag object you can use if you need to look
up other tags.

Here is a simple example of a custom filter, which can be used to
generate HTML forms. In such a form, it is common to have some `<input>`
tags that need a `selected='selected'` attribute if the toggle
button should be selected. This can be done without the use of a filter,
of course, using a simple *@@IF@@* statement, but that makes the
template less readable. The custom filter below behaves as such: it takes
one argument, and compares the value of the tag on which the filter is
applied to that argument. If they are equal, the string
`selected='selected'` will be substituted. As a special case, if the
argument to the filter starts with a `'@'` character, the argument
is interpreted as the name of a tag to look up first::

 function Custom_Select_Filter
   (Value      : in String;
    Parameters : in String;
    Context    : in Filter_Context) return String is
 begin
    if Parameters /= "" and then Parameters (Parameters'First) = '@' then
       if Get (Get (Context.Translations,
                    Parameters (Parameters'First + 1 .. Parameters'Last)))
         = Value
       then
          return "selected='selected'";
       end if;

    elsif Value = Parameters then
       return "selected='selected'";
    end if;

    return "";
 end Custom_Select_Filter;

 Register_Filter ("SELECTED", Custom_Select_Filter'Access);

and a template would look like::

 <option value="foo" @_SELECTED(@SELECTED_STATUS):STATUS_@ />

.. _Attributes:

Attributes
==========

In addition to filters, you can also apply attributes to tags.
Attributes are placed after the tag name and preceded with a simple quote.
*@_SOME_VAR['ATTRIBUTE_NAME]_@*. It is possible to use filters
and attributes together. In that case the attribute is first evaluated and
the result is passed-through the filters.

You cannot define your own attributes.

Current supported attributes are:

**V'length**
  .. index:: Attribute, 'Length

  Returns the number of item in the composite tag (can be applied only
  to a composite tag having a single nested level - a vector).

**V'Up_Level(n)**
  .. index:: Attribute, 'Up_Level

  Use index from the table command **n** level(s) upper so this attribute must
  be used in a nested table command tag. `'Up_Level` is equivalent
  to `'Up_Level(1)` (can be applied only to a composite tag having
  a single nested level - a vector).

**M'Line**
  .. index:: Attribute, 'Line

  Returns the number of line in the composite tag. This is identical to
  'Length but can be applied only to a composite tag having two nested
  level - a matrix).

**M'Min_Column**
  .. index:: Attribute, 'Min_Column

  Returns the size of smallest composite tag in M composite tag. This attribute
  can be applied only to a composite tag having two nested level - a matrix.

**M'Max_Column**
  .. index:: Attribute, 'Max_Column

  Returns the size of largest composite tag in M composite tag. This attribute
  can be applied only to a composite tag having two nested level - a matrix.

**M'Indent**
  .. index:: Attribute, 'Indent

  This attribute will indent (on the starting tag column) the tag
  value on new-line.

For example:

 If VEC is set to "*<1 , 2>*" and MAT to "*<a, b, c> ; <2, 3, 5, 7>*" then::

 @_VEC'Length_@              ->  2
 @_ADD(3):VEC'Length_@       ->  5
 @_MAT'Line_@                ->  2
 @_MAT'Min_Column_@          ->  3
 @_MAT'Max_Column_@          ->  4

.. _Predefined_tags:

Predefined tags
===============

There are some specific tags that can be used in any
templates. Here is an exhaustive list:

**NOW**
  .. index:: @_NOW_@

  Current date and time with format "YYYY-MM-DD HH:MM:SS".

**YEAR**
  .. index:: @_YEAR_@

  Current year number using 4 digits.

**MONTH**
  .. index:: @_MONTH_@

  Current month number using 2 digits.

**DAY**
  .. index:: @_DAY_@

  Current day number using 2 digits.

**HOUR**
  .. index:: @_HOUR_@

  Current hour using range 0 to 23 using 2 digits.

**MINUTE**
  .. index:: @_MINUTE_@

  Current minute using 2 digits.

**SECOND**
  .. index:: @_SECOND_@

  Current seconds using 2 digits.

**MONTH_NAME**
  .. index:: @_MONTH_NAME_@

  Current full month name (January .. December).

**DAY_NAME**
  .. index:: @_DAY_NAME_@

  Current full day name (Monday .. Sunday).

.. _Dynamic_tags:

Dynamic tags
============

.. index:: Dynamic tags

Dynamic tags are associations that are not created when `Parse` is called,
but only later on when they are actually needed.

Dynamic tags are handled through abstract interfaces and give the
opportunity to create tags dynamically while the template is being parsed.

.. _Lazy_Tag:

Lazy_Tag
--------

.. index:: Lazy_Tag

The `Lazy_Tag` object can be used to dynamically handle tags. Such
object can be passed to the `Parse` routines. If a template's tag
is not found in the translation dictionary, the `Lazy_Tag`'s Value
callback method is called by the parser. The default callback method
does nothing, it is up to the user to define it. The callback
procedure is defined as follow::

 procedure Value
   (Lazy_Tag     : access Dynamic.Lazy_Tag;
    Var_Name     : in     String;
    Translations : in out Translate_Set) is abstract;
 --  Value is called by the Parse routines below if a tag variable was not
 --  found in the set of translations. This routine must then add the
 --  association for variable Name. It is possible to add other
 --  associations in the translation table but a check is done to see if
 --  the variable Name as been set or not. The default implementation does
 --  nothing.

One common usage is to handle tag variables that can be shared by
many templates and are not always used (because a conditional is False for
example). If computing the corresponding value (or values for a ...)
is somewhat expensive it is better to delay building such tag at the
point it is needed. Using a `Lazy_Tag` object, it is possible to do
so. The `Value` procedure will be called if the tag value is
needed. At this point, one can just add the corresponding association
into the `Translate_Set`. Note that it is possible to add more
than one association. If the association for `Var_Name` is not
given, this tag has no value.

`Value` will be called only once per template and per tag. This is so
that if the value for the tag is expensive to compute, you only pay the
price once, and the value is then cached for the remaining of the template.
If the value should be recomputed every time, you should consider using
a `Cursor_Tag` instead (see :ref:`Cursor_Tag`).

.. _Cursor_Tag:

Cursor_Tag
----------

.. index:: Cursor_Tag

In some cases, data structure on the Ada side can be so complex that it is
difficult to map it into a variable tag. The `Cursor_Tag` object has
been designed to work around such problem. Using a `Cursor_Tag`
it is possible to create an iterator through a data structure without
mapping it into a variable tag. The data stays on the Ada side.
To create a `Cursor_Tag` it is necessary to implement the following
abstract routines::

 function Dimension
   (Cursor_Tag : access Dynamic.Cursor_Tag;
    Var_Name   : in     String) return Natural is abstract;
 --  Must return the number of dimensions for the given variable name. For
 --  a matrix this routine should return 2 for example.

 type Path is array (Positive range <>) of Natural;
 --  A Path gives the full position of a given element in the cursor tag

 function Length
   (Cursor_Tag : access Dynamic.Cursor_Tag;
    Var_Name   : in     String;
    Path       : in     Dynamic.Path) return Natural is abstract;
 --  Must return the number of item for the given path. The first
 --  dimension is given by the Path (1), for the second column the Path is
 --  (1, 2). Note that each dimension can have a different length. For
 --  example a Matrix is not necessary square.

 function Value
   (Cursor_Tag : access Dynamic.Cursor_Tag;
    Var_Name   : in     String;
    Path       : in     Dynamic.Path) return String is abstract;
 --  Must return the value for the variable at the given Path. Note that
 --  this routine will be called only for valid items as given by the
 --  dimension and Length above.

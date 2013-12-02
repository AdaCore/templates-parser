.. _Macros:

******
Macros
******

A macro usage is like a tag but with a set of parameters passed inside
parenthesis. Macros support all filters but attributes can't be
used. It is important to note that macros are expanded at the point of
their calls. This implementation maximizes speed but uses more memory
as the definition is not shared. If the code is large it may be better
to use an *@@INCLUDE@@* as the code is not expanded.

Syntax::

 @_[[FILTER[(parameter)]:]MACRO_NAME([PARAM1][,N=>PARAMN])_@

A macro call can have positional parameters (like `PARAM1` above)
or named (where the name is a number corresponding to the actual
parameter position) parameters (like `PARAMN` above).

With the following definition:

.. literalinclude:: src/macro.tmplt
   :language: xml

Using the program:

.. literalinclude:: src/macro.adb
   :language: ada

We get the following result:

.. literalinclude:: build/samples/macro.adb.res

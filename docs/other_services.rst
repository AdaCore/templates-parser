
.. _Other_services:

**************
Other Services
**************

.. _Tag_utils:

Tag utils
=========

.. index:: Tag utils
.. highlight:: ada

The child package `Utils`, see :ref:`Templates_Parser.Utils`
contains a routine to encode a Tag variable into a string and the
inverse routine that build a Tag given it's string
representation. This is useful for example, in the context of AWS to
store a Tag into a session variable. See the AWS project.

.. _XML_representation:

XML representation
==================

.. index:: XML

The child package `XML`, see :ref:`Templates_Parser.XML` contains
routines to save a `Translation_Set` into an XML document or to
create a `Translation_Set` by loading an XML document. The XML
document must conform to a specific `DTD` (see the Ada spec file).

.. _Templates2Ada:

Templates2Ada
=============

.. index:: templates2ada

`templates2ada` is a tool that will generate a set of Ada
packages from a templates file. These Ada packages can then be used in
your application to avoid hard-coded strings, and help maintain the
templates and the code synchronized.

One of its goal is to ensure that you are only setting tags that actually
exist in the template (and thus prevent, as much as possibly, typos in the
name of tags); also, when combined with other tools, to help ensure that all
tags needed by the template are properly set.

Templates2ada also has special knowledge about HTTP constructs
and will generate Ada constants for the HTTP parameters you might receive in
return. Once more the goal is to help avoid typos in the Ada code.

For instance, we will consider a simple template file, found in a local
file :file:`resources/block1.thtml`. This template contains the following simple
html code:

.. code-block:: xml

 <form>
   <input name="PARAM1" value="@_TAG1_@" />
   <input name="PARAM2" value="@_TAG2_@" />
 </form>}

When you run :file:`templates2ada` (as described in the following subsection),
the following Ada package will be generated. Note that this is only the
default output of :file:`templates2ada`, which can be fully tailored to your
needs::

 package Templates.Block1 is
    pragma Style_Checks (Off);

    Template : constant string := "resources/block1.thtml";

    Tag1 : constant String := "TAG1";
    Tag2 : constant String := "TAG2";

    package Http is
       Param1 : constant String := "PARAM1";
       Param2 : constant String := "PARAM2";
    end Http;
 end Templates.Block1;

`templates2ada` knows about special constructs in the template file.
Such templates are generally associated with html pages. It is possible to
specify within the template itself what the url associated with the template
is, so that it provides a convenient link between the two. Likewise, you
can also define explicitly what the possible HTTP parameters are when loading
that page. This is mostly useful when those parameters do not correspond to
some form fields within the page itself. The syntax for these two is the
following::

 --  HTTP_URL(the_url): any comment you want
 --  HTTP_GET(param1_name): description of the parameter
 --  HTTP_GET(param2_name): description of the parameter

and that results in the following constants in the generated Ada package::

 package Templates.Block1 is
    URL : constant String := "the_url";

    package Http is
       Param1_Name : constant String := "param1_name";
       Param2_Name : constant String := "param2_name";
    end Http;
 end Templates.Block1;

The templates parser API lets you define your own custom filters. It is
often useful for those filters to take parameters, just like the predefined
filters do. However, it is also useful for these parameters to be able to
check the value of other tags. One convention for doing this is to start the
name of the parameter with "@". See for example the example in
:ref:`User_defined_filters`. As a reminder, the template would look like:

.. code-block:: xml

 <option value="foo" @_SELECTED(@SELECTED_STATUS):STATUS_@ />

The `templates2ada` tool knows about this special convention, and would
generate the following Ada package from this example::

 package Templates.Block1 is
    Selected_Status : constant String := "SELECTED_STATUS";
    Status : constant String := "STATUS";
 end Templates.Block1;

Running templates2ada
---------------------

This tool parses all the template files found in a directory, and then
generate an output file from these, based on a template file
(a default example of which is provided as :file:`templates.tads`). The
latter contains in fact two examples, depending on whether one Ada
package should be generated per template, or whether a single package
should be build. In the former case, if you are using the GNAT compiler,
you should run `gnatchop` on the resulting file. Here is an example
to run this tool for the example we described above.

.. code-block:: sh

 $ rm -f src/templates/*.ads
 $ templates2ada -d resources/ -o src/templates/generated -r
 $ cd src/templates; gnatchop -w -q generated
 $ rm -f src/templates/generated}

If, in you Ada code, you no longer use hard-coded strings but only the
constants found in the output packages, this will ensure that you are
not trying to set tags that are never used in the template.

The other check that impacts the quality of your code is to ensure that
all tags that are used by the templates are properly set. This cannot be
ensured by the compiler only, but using an external tool it is relatively
to do.

For instance, if you are using GNAT, we recommend the following additional
targets in your :file:`Makefile`:

.. code-block:: make

  unset_tags:
    gnat xref -u main.adb | fgrep templates-

This checks for all unused entities in files called :file:`templates-*`,
which are the files generated by :file:`templates2ada`.

:file:`templates2ada` can be used in other situations as well. For instance,
one possible use is to generate, as output, a new template file that itself
contains a series of *@@SET@@* commands. This generated file can
then be included (*@@INCLUDE@@*) in your own templates. We have used it with
some success when implementation a web server: it is often the case that hyper
links refer to other pages in the same server. We have avoided hard-coding the
URLs and the names of their HTTP GET parameters, by fetching these names from
the generated file we were talking above.

The templates parser comes with an example file, called :file:`all_urls.thtml`,
which can be used with the `-t` switch to `templates2ada`, and will
generated a template file as output. You would use it as:

.. code-block:: xml

 @@INCLUDE@@ all_urls.html
 <a href="@_URI_BLOCK1_@?@_HTTP_BLOCK1__PARAM1_@=12" />

and this ensures the link is valid.

:file:`templates2ada` supports a number of command line switches:

* -d <dir>

  This switch specifies the directory in which the templates file are
  searched for.

* -o <file>

  This switch specifies the output file name

* -e <ext>

  This file specifies the file name extension for template files. All
  files in the directory that have this extension will be processed by
  templates2ada.

* -t <tmplt>

  This file specifies the template file to be used for the output file.
  The templates parser comes with an example for such a file, called
  :file:`templates.tads`, that you can adapt to your own needs.

* -r

  Sub directories of the one specified by `-d` will also be searched.

* -v

  Activate the verbose mode. This will output a warning when an http
  parameter has a name made only of template parser tags, since no matching
  entry can then be created for it in the output file.

Customizing templates2ada
-------------------------

As was mentioned before, the output of templates2ada is a single file that
results from parsing a template file. An example of such a file is provided
in the templates2ada distribution, as :file:`templates.tads`.

You are strongly encouraged to modify this file to adapt it to your needs,
and then use the `-t` switch to :file:`templates2ada` to make use of your
modified file.

This file contains extensive comments on how to make use, and customize, it.
This documentation is not duplicated here

.. _Templatespp:

Templatespp
===========

.. index:: templatespp

`templatespp` is a pre-processor based on the template parser. It is
generally used from scripts to process files and generate other files. One
of the possible uses, for instance, is to write the CSS (style-sheet) of a
web site as a template file (for instance :file:`mycss.tcss`), and use
template parser structures in there. This is a good way to share colors for
instance, or to name constants, as is often done in Ada code.

Here is a small example of such a CSS:

.. code-block:: css

 @@SET@@ COLOR1=blue
 @@SET@@ COLOR2=red
 @@SET@@ LENGTH1=10

 body {background:@_COLOR1_@}
 div  {background:@_COLOR2_@}
 ul.class {background:@_COLOR1_@}  /* same color as body */

 ul   {width:@_ADD(3):LENGTH1_@px} /* ul 3 pixels wider than li */
 li   {width:@_LENGTH1_@px}

Such a file would be processed with the following command line:

.. code-block:: sh

 $ templatespp -o mycss.css mycss.tcss

Debug
=====

.. index:: Debug

A set of routines to help to debug the `Templates_Parser` engine,
see :ref:`Templates_Parser.Debug`. For example, `Debug.Print_Tree`
will display, to the standard output, a representation of the internal
semantic tree for a template file.

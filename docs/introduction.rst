.. _Introduction:

************
Introduction
************

The templates parser package has been designed to parse files and to
replace some specific tags into these files by some specified values.

The main goal was to ease the development of Web servers. In CGI
(*Common Gateway Interface*) mode you have to write the HTML page in
the program (in Ada or whatever other languages) by using some specific
libraries or by using only basic output functions like Ada `Put_Line` for
example. This is of course not mandatory but by lack of a good library
every Web development end up doing just that.

The main problems with this approach are:


* It is painful to have to recompile the program each time you have
  a slight change to do in the design (center an image, change the border
  width of a table...)

* You have the design and the program merged together. It means that
  to change the design you must know the Ada language. And to change the
  Ada program you need to understand what is going on with all these
  inline HTML command.

* You can't use the nice tools to generate your HTML.


With the templates parser package these problems are gone. The code and
the design is **completely** separated. This is a very important
point. PHP or JSP have tried this but most of the time you have the
script embedded into the Web template. And worst you need to use another
language just for your Web development.


* The HTML page is separated from the program code. Then you can
  change the design without changing the code. Moreover when you fix the
  code you don't have to handle all the specific HTML output. And you do
  not risk to break the design.

* It is easier to work on the design and the program at the same time
  using the right people for the job.

* It reduces the number of *edit/build/test* cycles. Writing HTML
  code from a program is error prone.

* It is possible to use standard tools to produce the HTML.

* You don't have to learn a new language.

* The script is Ada, so here you have the benefit of all the Ada power.


In fact, the Ada program now simply computes some values, gets some data
from a database or whatever and then calls the templates parser to output
a page with the data displayed. To the templates parser you just pass
the template file name and an associative table.

It is even more convenient to have different displays with the same set
of data. You just have to provide as many templates as you like.


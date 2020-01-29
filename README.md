Templates_Parser
================

This repository hosts the `Templates_Parser` library and the associated
`templates2ada` program. These are tools to create templated text streams, such
as dynamic HTML documents.

To learn more about them, you can either read our Sphinx documentation [in this
repository](docs/) or read it from [AdaCore's live
docs](https://docs.adacore.com/live/wave/aws/html/template_parser/index.html).

Build
-----

Some make variables can be adjusted to change the default setup:

```text
DEFAULT_LIBRARY_TYPE=[static|relocatable]
    (default is static)

prefix=<install directory>
    (default to compiler root directory)

ENABLE_STATIC=[true|false]
    (default true)

ENABLE_SHARED=[true|false]
    (default yes on platforms supporting shared libraries)

DEBUG=[true|false]
    (default false)

PROCESSORS=N
    Number of parallel compilations
    (default 2)
```

To build both the static and shared version (if supported) using the default
setup:

```sh
$ make
```

To setup the default library as relocatable and change the installation
directory:

```sh
$ make DEFAULT_LIBRARY_TYPE=relocatable prefix=/opt/templates_parser setup
$ make
```

To install:

```sh
$ make install
```

Note that the installation will be done into your current GNAT root directory
by default. It is possible to change this default by setting the prefix make
variable, for example:

```
$ make prefix=/opt/templates_parser install
```

or using the setup step:

```
$ make prefix=/opt/templates_parser setup
$ make && make install
```

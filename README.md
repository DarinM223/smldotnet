SML.NET
=======

SML.NET   is  a   compiler   for  the   functional
programming language Standard  ML that targets the
.NET  Common Language  Runtime and  which supports
language interoperability features for easy access
to .NET libraries.

Status
------

This project has been updated to compile
and build the demos with modern 64-bit SML/NJ
(> 2025.1, currently on the master branch only).
In order to use the legacy 32-bit SML/NJ versions,
use the `legacy` branch instead.

This distribution only supports  the 2.0 and lower
versions  of  the  Microsoft  .NET  Framework  and
Microsoft Visual Studio .NET 2005. SML.NET remains
compatible with the initial 1.0 and 1.1 releases.

Although SML.NET fully  supports SML polymorphism,
it does not yet produce or consume .NET generics

SML.NET also runs on Linux with Mono. To install
and configure Mono on Ubuntu for SML.NET, run the
following:

```
sudo apt-get install mono-complete
cd /usr/lib/mono
sudo ln -s 2.0-api v2.0-api
cd v2.0-api
sudo ln -s /usr/bin/mono-csc ./csc.exe
sudo ln -s /usr/bin/ilasm ./ilasm.exe
```

SML.NET requires the version name to start with
a `v` so a symbolic links are created for it and
the `csc.exe` and `ilasm.exe` executables.

If you want to have access to posix libraries, go to
[Mono.Posix.NETStandard](https://www.nuget.org/packages/Mono.Posix.NETStandard/5.20.1-preview)
and go to `Open in NuGet Package Explorer`, then in the `runtimes/` folder there are multiple
targets. For x86-64, go into `linux-x64/lib/netstandard2.0/Mono.Posix.NETStandard.dll` and
double click it to download it. Then move that dll file into the `demos/posix` folder and
then you can compile the posix example.

Finally, you need to set the `SMLNETPATH`
environment variable to the project's root
directory and these environment variables:

```
export FrameworkDir=/usr/lib/mono
export FrameworkVersion=v2.0-api
```

Warning: if the SML.NET compiled versions of
`ml-lex` and `ml-yacc` only work on files without
Windows style line endings. So you should run
`dos2unix` on `.lex` and `.grm` files first.

To build SML.NET on Linux, first run
`bld/buildsmlnet.sh`. Then go into the demos and
either run it with a smlnet file like
`bin/smlnet.sh @File` where there is a
`File.smlnet` file in the directory, or
`bin/smlnet.sh File` where there is a `File.sml`
file in the directory. This should create an
`.exe` file that can be ran with
`mono program.exe`. In some demos, you may need
to run `buildclient.sh` after building the
smlnet file in order to produce an `.exe` file.

Features
--------

* Support all of Standard ML

SML.NET compiles  all of  SML '97 (with  some very
minor discrepancies).

* Support for the Basis library

Almost  all of  the Standard  ML Basis  Library is
implemented.

* Seamless  interoperability with  other languages

SML.NET extends the SML  language to support safe,
convenient use of the .NET Framework libraries and
code written in other  languages for the CLR, such
as C# or VB. SML.NET  can both consume and produce
.NET classes, interfaces, delegates etc.

* Command-line compilation

SML.NET supports traditional  compilation from the
command-line. Interactive  compilation environment
Alternatively, you  can control the  compiler from
an  interactive  environment.  This lets  you  set
and  query options  incrementally and  to see  the
signatures  of   compiled  and   imported  SML.NET
modules.

* Automatic dependency analysis

In  either  mode   of  compilation,  the  compiler
requires  only the  names  of root  modules and  a
place  to  look  for  source code.  It  then  does
dependency analysis  to determine which  files are
required and which need recompilation.

* Produces verifiable CLR IL

The  output of  the  compiler  is verifiable  MSIL
(Microsoft Intermediate Language) for the CLR.

* Whole program optimization

SML.NET performs optimizations  on a whole program
(or library)  at once.  It usually  produces small
executables with fairly good performance.

* Integration with Visual Studio .NET

A   separate  binary   distribution  includes   an
experimental package  for Microsoft  Visual Studio
.NET  2002, 2003  and &  2005 that  allows you  to
edit, build and debug SML.NET projects from within
the development environment.

Limitations
-----------

* No interactive evaluation

The interactive environment  is for compilation of
stand-alone  applications or  libraries only.  SML
expressions can not be evaluated interactively and
the  use command  is not  available. For  programs
that make no use of  the language extensions it is
possible to develop and test them using a compiler
such as Moscow ML or Standard ML of New Jersey and
then to use SML.NET to produce final executables.

* Whole program optimization

Top-level   SML    modules   are    not   compiled
individually  to .NET  object code.  Instead, some
compilation takes place  on separate modules (type
checking,  translation   to  the   compiler's  own
intermediate  form,  and some  optimizations)  but
most is deferred until  after the linking together
of top-level modules. This improves performance of
the  generated code,  but significantly  increases
(re)compilation times.

* Only CLR types at boundaries of compiled code

The  exposed interfaces  of  applications or  DLLs
compiled by  SML.NET may  only refer to  CLR types
(classes,   interfaces,  delegates,   etc.).  They
may  not  expose  SML-specific  types  (functions,
datatypes,  records,  etc.). In  particular,  this
restriction  means  that  one  cannot  compile  an
arbitrary SML  module into  a DLL  for consumption
even by  other SML.NET  programs: the  module must
be  either  linked  into  the  client  program  at
compile-time  or   use  only  CLR  types   at  its
interface.


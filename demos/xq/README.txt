SML.NET XML Query demo
======================

Overview
--------

This demo is an interpreter for an untyped, vaguely XQuery-like, language
for querying XML documents. The interpreter is written in SML (using a
lexer and parser generated by ml-lex and ml-yacc) and uses the .NET
libraries to read and write XML files. The user-interface of the demo
is an ASP.NET page (with code in C#) which calls into the SML.NET code
to perform query evaluation.

The Query Language
------------------

The syntax of the query language is based on that presented in:
M Fernandez and J Simeon and P Wadler. A Semistructured Monad for
Semistructured Data. ICDT 2001. 

It should, however, be pointed at that the XML type system (which is
the most interesting part of the paper) is not implemented in this
demo. Furthermore, although this language is semantically equivalent
to the interesting parts of XQuery, its actual syntax is rather
different. There is also no support for attributes, which makes the
system of rather limited utility for real XML processing tasks. (But
since I had the code lying around, it seemed worth giving it a .NET
wrapper and distributing it anyway, in the hope that someone else
might feel inspired to do the job properly...)

The Interface
-------------

Should be pretty self-explanatory. You can browse the three sample XML
documents by selecting from the drop-down menu at the top. You can
load sample queries into the second text box by selecting from the
second drop-down menu, or you can just type your own query
directly. Pressing the "Evaluate!" button submits the query and returns
the results in the third text box. There's no error handling, but if
you make a mistake, ASP.NET will produce an HTML-formatted stack trace
and you can just click "Back" to try again.

Building The Demo
-----------------

There are several stages in the compilation of the demo:
1) The files xquery.lex and xquery.grm are processed by ml-lex and
ml-yacc to produce xquery.lex.sml and xquery.grm.sml.
2) The lexer, parser, associated support libraries and the code for
the interpreter proper are compiled by sml.net to produce xqlib.dll.
3) The C# code for the web page "business logic" (xq.aspx.cs)
codebehind is compiled against xqlib.dll to produce xq.aspx.dll.
4) IIS is configured to publish the asp.net code for the web page
"presentation layer" (xq.aspx). (This is then
munged, compiled, linked against xq.aspx.dll and executed
automagically by the ASP.NET infrastructure at browse-time.)

Running build.bat will perform the first three stages, *assuming* that
you have first built ml-lex and ml-yacc in their own directories
(..\ml-lex\ and ..\ml-yacc\).

For the final stage, you first need to have installed and started IIS
and registered the ASP.NET file extensions (see below).  Then go to
Control Panel - Administrative Tools - Internet Information Services,
expand the treeview and select Default Web Site. Then Select Action -
New - Virtual Directory from the menu to start the appropriate
wizard. Type an alias name (e.g. "xq") and then the path to this
directory (e.g. "c:\smlnet\demos\xq"). The default permissions (read
and run scripts) are sufficient. Finish the wizard and you should then
be able to run the demo by pointing your browser at, for example,
http://localhost/xq/xq.aspx

NOTE: Potential difficulties with IIS and ASP.NET. If you installed
the .NET Framework without IIS, then you must not only install IIS
(use Control Panel - Add/Remove Programs - Windows Components), but
then also register the ASP.NET file types with it (this is done
automatically if IIS is installed BEFORE the .NET framework). See
http://support.microsoft.com/default.aspx?scid=kb;en-us;Q306005 for
instructions on how to do this.

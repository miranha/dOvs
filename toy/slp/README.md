# Demo files from Compilation lecture on September 11, 2015

- Author:  [Aslan Askarov](http://askarov.net/)


## Notes


- For purpose of the exercise the included grammar here  contains 1
  shift/reduce conflict related to the "dangling else".

- Parse trees are generated using "dot" command. See
  [Graphviz](http://www.graphviz.org/) page for more info.

- To generate a parse tree, assuming all system tools are installed, and the
  command is running on a unix and has a write access to /tmp/, open SML REPL,
  compile the project with CM.make "sources.cm". Test examples  can be
  visualized using function Examples.parseAndShow "tests/test1.cmd"

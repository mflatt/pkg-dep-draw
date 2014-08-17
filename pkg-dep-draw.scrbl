#lang scribble/manual
@(require scribble/bnf
          (for-label pkg-dep-draw))

@title{Package Dependency Graph Visualization}

@defmodule[pkg-dep-draw]{The @racketmodname[pkg-dep-draw] module
provides a @racket[pkg-dep-draw] function for drawing a
package-dependency graph, and it also supports command-line options
when run as a program.}

To run from the command line, use

@commandline{racket -l- pkg-dep-draw @kleenestar{@nonterm{option}} @kleenestar{@nonterm{root-pkg}}}

which shows packages reachable from the @nonterm{root-pkg}s (or all
packages if no @nonterm{root-pkg}s are specified). The visualization
shows packages that are either installed or available from the
catalogs/directories specified by @DFlag{catalog}/@DFlag{dir}
options. By default, the graph is shown in a GUI window, but use the
@Flag{o} flag to direct the output to a PNG or PDF file.

Packages are sorted in the visualization so that a package's
dependencies appear below it or at the same vertical layer (in the
case that packages are mutually dependent). Build dependencies are
shown with purple lines, while run-time dependencies are shown with
blue lines. A blue or purple square connects packages that are
mutually dependent, depending on whether they have a mutual run-time
dependence (blue square) or merely a mutual build-time dependence
(purple square). In GUI mode, click on a package name to pin its
dependency lines (and click in an empty area to unpin lines).

As a program, @racketmodname[pkg-dep-draw] accepts the following
command-line @nonterm{option}s:

@itemlist[

 @item{@DFlag{select} @nonterm{pkg} --- Draw dependencies starting
       from @nonterm{pkg}, along with any other selected package
       (i.e., this flag can be specified multiple times to select
       multiple packages).

       By default, all dependency lines are show. Note that supplying
       a @nonterm{root-pkg} as a command-line argument limits the
       packages that are listed in the graph overall, while
       @DFlag{select} limits only which dependency lines are shown.}

 @item{@DFlag{catalog} @nonterm{catalog} --- Read package information
       from @nonterm{catalog}, along with any other specified catalogs
       and directories (i.e., this flag can be specified multiple
       times), instead of using the set of installed packages.

       If multiple catalogs and directories are provided, information
       in a later catalog/directory takes precedence over information
       in an earlier one.}

  @item{@DFlag{dir} @nonterm{dir} --- Read package information from
        @nonterm{dir} (i.e., this flag can be specified multiple
        times), along with any other specified catalogs and
        directories, instead of using the set of installed packages.

        Each directory within @nonterm{dir} that contains an
        @filepath{info.rkt} file is treated as a package.}

 @item{@DFlag{no-build} --- Ignore build dependencies (as specified by
       @racket[build-deps] in a package's @filepath{info.rkt} file).}

 @item{@DFlag{no-build-lines} --- Suppress lines in the visualization
        that represent build dependencies, even when build
        dependencies are otherwise used to compute the graph.

        In GUI mode, this option selects the initial state of the GUI,
        but it can be changed interactively.}

 @item{@DFlag{no-trans-lines} --- Suppress lines in the visualization
        that represent transitive dependencies.

        In GUI mode, this option selects the initial state of the GUI,
        but it can be changed interactively.}

 @item{@DFlag{reverse} --- Draw dependency lines backwards from the
       selected packages.

        In GUI mode, this option selects the initial state of the GUI,
        but it can be changed interactively.}

 @item{@DFlag{scale} @nonterm{scale} : Use @nonterm{scale} as the
       initial drawing scale.

       Note that the width of lines used to show dependencies is
       insensitive to the scale. In GUI mode, this option selects the
       initial state of the GUI, but it can be changed interactively.}

 @item{@Flag{o} @nonterm{file} --- Write the visualization to
       @nonterm{file}, which must have an extension that is either
       @filepath{.png} or @filepath{.pdf}, and disable the default GUI
       mode.}

 @item{@DFlag{quiet} or @Flag{q} --- Suppress hints and notes.}

]

@defproc[(pkg-dep-draw [#:root-pkgs root-pkgs (listof string?) null]
                       [#:select-pkgs select-pkgs (listof string?) null]
                       [#:srcs srcs (listof (cons/c (or/c 'dir 'catalog) string?)) null]
                       [#:no-build? no-build? any/c #f]
                       [#:no-build-lines? no-build-lines? any/c #f]
                       [#:no-trans-lines? no-trans-lines? any/c #f]
                       [#:invert? invert? any/c #f]
                       [#:quiet quiet? any/c #t]
                       [#:dest-file dest-file (or/c #f path-string?) #f]
                       [#:dest-format dest-format (or/c 'png 'pdf) #f]
                       [#:scale scale real? 1])
          void?]{

Draws a dependency graph the same as running @racketmodname[pkg-dep-draw] as
a program with command-line arguments (with hopefully obvious
representations and treatments of the arguments).}

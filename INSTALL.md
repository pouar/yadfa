To compile, you need a decent CL implementation, a recent version of ASDF (newer versions fix a few bugs that affect my game) and Quicklisp (for dependency management)

If you're on Windows, you probably want Clozure CL

https://ccl.clozure.com

The rest of the world will probably want Steel Bank Common Lisp

http://sbcl.org

Also, if you're on Windows, you need to install an X Server. VcXsrv is a pretty good one

https://sourceforge.net/projects/vcxsrv

Note, there is a bug in CCL on Windows that prevents the game from starting, a workaround is listed here

https://trac.clozure.com/ccl/ticket/1370

I think the minimum version of ASDF required for this game is 3.3.2.4. You can download the latest version here

https://gitlab.common-lisp.net/asdf/asdf/tags

create a directory called `~/common-lisp`. (In Windows, the equivelant of `~` is `C:\Users\$User`) extract ASDF in there.

You will also need to install Quicklisp, which can be downloaded from here

https://www.quicklisp.org

After which, either place this repo in either `~/common-lisp`, `~/quicklisp/local-projects`, or create a text file called `$XDG_CONFIG_HOME/common-lisp/source-registry.conf.d/50-yadfa.conf` with the following in it

```
(:tree "$PATH_TO_THIS_REPOSITORY")
```

In Windows, `$XDG_CONFIG_HOME` is set to `C:/Users/$User/AppData/Local/`. Also, when setting the pathname, you need to use what the rest of the world uses as the directory separator, which is `/`, Not what Windows uses, which is `\`.

After which, either run `sbcl --script build.lisp mods` or `ccl -l build.lisp -- mods` to build

To generate the docs, run
```
sbcl --script run.lisp mods texi;makeinfo --css-include=style-common.css --css-include=style-single.css --html --no-split yadfa.texi;makeinfo --pdf --no-split yadfa.texi;makeinfo --no-split yadfa.texi
```
The game uses Declt for generating the docs, which only works in SBCL so far

To build with SLIME/SWANK support, run `sbcl --script build.lisp mods swank` or `ccl -l build.lisp -- mods swank`, then run `./yadfa swank` to launch the game

To build with Sly/Slynk support, run `sbcl --script build.lisp mods slynk` or `ccl -l build.lisp -- mods slynk`, then run `./yadfa slynk` to launch the game
;;;; -*- mode: Common-Lisp; sly-buffer-package: "net.didierverna.declt"; coding: utf-8-unix; -*-
(in-package :net.didierverna.declt)
(defun render-docstring (item)
  "Render ITEM's documentation string.
Rendering is done on *standard-output*."
  (when-let ((docstring (docstring item)))
            (write-string docstring *standard-output*)))
(defun render-header (library-name tagline version contact-names contact-emails
                      copyright-years license
                      texi-name info-name declt-notice
                      current-time-string)
  "Render the header of the Texinfo file."
  (format t "\\input texinfo~2%@c ~A.texi --- Reference manual~2%" texi-name)

  (when copyright-years
    (mapc (lambda (name)
            (format t "@c Copyright (C) ~A ~A~%" copyright-years name))
          ;; #### NOTE: we already removed the duplicates in the original contact
          ;; list, but there may still be duplicates in the names, for instance if
          ;; somebody used his name several times, with a different email
          ;; address.
          (remove-duplicates (remove-if #'null contact-names)
                             :from-end t :test #'string=))
    (terpri))

  (format t "@c This file is part of ~A.~2%" library-name)

  (when license
    (with-input-from-string (str (caddr license))
      (loop :for line := (read-line str nil str)
            :until (eq line str)
            :do (format t "@c ~A~%" line))))

  (terpri)
  (terpri)

  (format t "@c Commentary:~2%~
             @c Generated automatically by Declt version ~A~%~
             @c on ~A.~3%"
          (version :long) current-time-string)

  (format t "~
@c ====================================================================
@c Header
@c ====================================================================
@c %**start of header
@setfilename ~A.info
@settitle The ~A Reference Manual
@afourpaper
@documentencoding UTF-8
@c %**end of header~4%"
          (escape info-name) (escape library-name))

  (format t "~
@c ====================================================================
@c Format Specific Tweaks
@c ====================================================================
@tex
%% Declt uses several Unicode characters to \"reveal\" blanks. This
%% works fine in HTML or Info output, but TeX will have problems with
%% these. The code below translates those characters to something that
%% TeX can handle.

%% U+23B5 (Bottom Square Bracket), used to reveal white spaces, is
%% translated to its Computer Modern teletype version.
\\DeclareUnicodeCharacter{23B5}{{\\tt\\char'040}}

%% U+21B5 (Downwards Arrow With Corner Leftwards), used to reveal
%% carriage returns, is translated to \\hookleftarrow in math mode.
\\DeclareUnicodeCharacter{21B5}{\\ensuremath\\hookleftarrow}

%% U+21E5 (Rightwards Arrow To Bar), used to reveal tabs, is
%% translated to something that looks similar, based on a rightarrow
%% and a vertical bar from the math extension font.
\\DeclareUnicodeCharacter{21E5}{%
  \\ensuremath{\\rightarrow\\kern-.5em\\mathchar\\\"130C}}


%% Declt uses several Unicode characters to replace \"fragile\" ones in
%% anchor names and references. These characters are chosen to resemble
%% the original ones, without interfering with Info syntax. In TeX
%% however, we can switch them back to the original versions, because
%% cross-references are done differently. In theory, I think we could do
%% something similar for HTML output (again, only the Info syntax poses
%% problems), but I don't know how to do something similar to what's
%% below.

%% U+2024 (One Dot Leader) replaces periods.
\\DeclareUnicodeCharacter{2024}{.}

%% U+2236 (Ratio) replaces colons.
\\DeclareUnicodeCharacter{2236}{:}

%% U+2768 (Medium Left Parenthesis Ornament) replaces left parenthesis.
\\DeclareUnicodeCharacter{2768}{(}

%% U+2769 (Medium Right Parenthesis Ornament) replaces right parenthesis.
\\DeclareUnicodeCharacter{2769}{)}

%% U+214B (Turned Ampersand) replaces ampersands.
\\DeclareUnicodeCharacter{214B}{&}

%% U+2216 (Set Minus) replaces backslashes.
\\DeclareUnicodeCharacter{2216}{\\char\"5C}

%% The following ones are already defined in texinfo.tex so we have nothing
%% more to do:
%% U+201A (Single Low-9 Quotation Mark) replaces commas.
%% U+2205 (Empty Set) replaces empty symbol names.

@end tex~4%")

  (format t "~
@c ====================================================================
@c Settings
@c ====================================================================
@setchapternewpage odd
@documentdescription
The ~A Reference Manual~@[, version ~A~].
@end documentdescription~4%"
          (escape library-name) (escape version))

  (format t "~
@c ====================================================================
@c New Commands
@c ====================================================================

@c ---------------
@c Indexing macros
@c ---------------

@c Packages
@macro packageindex{name}
@tpindex \\name\\
@tpindex @r{Package, }\\name\\
@end macro

@c Systems
@macro systemindex{name}
@tpindex \\name\\
@tpindex @r{System, }\\name\\
@end macro

@c Modules
@macro moduleindex{name}
@cindex @t{\\name\\}
@cindex Module, @t{\\name\\}
@end macro

@c Other files
@macro otherfileindex{name}
@cindex @t{\\name\\}
@cindex Other File, @t{\\name\\}
@cindex File, other, @t{\\name\\}
@end macro

@c Lisp files
@macro lispfileindex{name}
@cindex @t{\\name\\}
@cindex Lisp File, @t{\\name\\}
@cindex File, Lisp, @t{\\name\\}
@end macro

@c C files
@macro cfileindex{name}
@cindex @t{\\name\\}
@cindex C File, @t{\\name\\}
@cindex File, C, @t{\\name\\}
@end macro

@c Java files
@macro javafileindex{name}
@cindex @t{\\name\\}
@cindex Java File, @t{\\name\\}
@cindex File, Java, @t{\\name\\}
@end macro

@c Static files
@macro staticfileindex{name}
@cindex @t{\\name\\}
@cindex Static File, @t{\\name\\}
@cindex File, static, @t{\\name\\}
@end macro

@c Doc files
@macro docfileindex{name}
@cindex @t{\\name\\}
@cindex Doc File, @t{\\name\\}
@cindex File, doc, @t{\\name\\}
@end macro

@c HTML files
@macro htmlfileindex{name}
@cindex @t{\\name\\}
@cindex HTML File, @t{\\name\\}
@cindex File, html, @t{\\name\\}
@end macro

@c The following macros are meant to be used within @defxxx environments.
@c Texinfo performs half the indexing job and we do the other half.

@c Constants
@macro constantsubindex{name}
@vindex @r{Constant, }\\name\\
@end macro

@c Special variables
@macro specialsubindex{name}
@vindex @r{Special Variable, }\\name\\
@end macro

@c Symbol macros
@macro symbolmacrosubindex{name}
@vindex @r{Symbol Macro, }\\name\\
@end macro

@c Slots
@macro slotsubindex{name}
@vindex @r{Slot, }\\name\\
@end macro

@c Macros
@macro macrosubindex{name}
@findex @r{Macro, }\\name\\
@end macro

@c Compiler Macros
@macro compilermacrosubindex{name}
@findex @r{Compiler Macro, }\\name\\
@end macro

@c Functions
@macro functionsubindex{name}
@findex @r{Function, }\\name\\
@end macro

@c Methods
@macro methodsubindex{name}
@findex @r{Method, }\\name\\
@end macro

@c Generic Functions
@macro genericsubindex{name}
@findex @r{Generic Function, }\\name\\
@end macro

@c Setf Expanders
@macro setfexpandersubindex{name}
@findex @r{Setf Expander, }\\name\\
@end macro

@c Method Combinations
@macro shortcombinationsubindex{name}
@tpindex @r{Short Method Combination, }\\name\\
@tpindex @r{Method Combination, Short, }\\name\\
@end macro

@macro longcombinationsubindex{name}
@tpindex @r{Long Method Combination, }\\name\\
@tpindex @r{Method Combination, Long, }\\name\\
@end macro

@c Conditions
@macro conditionsubindex{name}
@tpindex @r{Condition, }\\name\\
@end macro

@c Structures
@macro structuresubindex{name}
@tpindex @r{Structure, }\\name\\
@end macro

@c Types
@macro typesubindex{name}
@tpindex @r{Type, }\\name\\
@end macro

@c Classes
@macro classsubindex{name}
@tpindex @r{Class, }\\name\\
@end macro

@c Pouar's Custom Stuff
@html
<script defer>
MathJax = {
  tex: {inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],
    macros: {
      RR: \"{\\\\bf R}\",
      bold: [\"{\\\\bf #1}\", 1]
    }}
};

    MathJax.Hub.Config({ TeX: { extensions: [] }});
</script>
<!-- Source is apparently here https://github.com/mathjax/MathJax-src -->
<script id=\"MathJax-script\" defer src=\"https://cdn.jsdelivr.net/npm/mathjax@@3/es5/tex-chtml-full.js\"></script>
@end html
@macro mathjax{math}
@tex
$$\\math\\$$
@end tex
@html
$$\\math\\$$
@end html
@ifnothtml
@ifnottex
@center @backslashchar{}[@math{\\math\\}@backslashchar{}]
@end ifnottex
@end ifnothtml
@end macro
@macro imathjax{math}
@inlineraw{html,$\\math\\$}
@inlineraw{tex,$\\math\\$}
@inlinefmtifelse{tex,,@inlinefmtifelse{html,,@backslashchar{}(@math{\\math\\}@backslashchar{})}}
@end macro~4%")

  (format t "~
@c ====================================================================
@c Info Category and Directory
@c ====================================================================
@dircategory Common Lisp
@direntry
* ~A Reference: (~A). The ~A Reference Manual.
@end direntry~4%"
          (escape library-name) (escape info-name) (escape library-name))

  (when license
    (format t "~
@c ====================================================================
@c Copying
@c ====================================================================
@copying
@quotation~%")

    (when copyright-years
      (mapc (lambda (name)
              (format t "Copyright @copyright{} ~A ~A~%"
                      (escape copyright-years) (escape name)))
            ;; #### NOTE: we already removed the duplicates in the original
            ;; contact list, but there may still be duplicates in the names, for
            ;; instance if somebody used his name several times, with a different
            ;; email address.
            (remove-duplicates (remove-if #'null contact-names)
                               :from-end t :test #'string=))
      (terpri))
    (format t "~
Permission is granted to make and distribute verbatim copies of this
manual provided the copyright notice and this permission notice are
preserved on all copies.

@ignore
Permission is granted to process this file through TeX and print the
results, provided the printed document carries a copying permission
notice identical to this one except for the removal of this paragraph
(this paragraph not being relevant to the printed manual).

@end ignore
Permission is granted to copy and distribute modified versions of this
manual under the conditions for verbatim copying, provided also that the
section entitled ``Copying'' is included exactly as in the original.

Permission is granted to copy and distribute translations of this manual
into another language, under the above conditions for modified versions,
except that this permission notice may be translated as well.
@end quotation
@end copying~4%"))

  (format t "~
@c ====================================================================
@c Title Page
@c ====================================================================
@titlepage
@title The ~A Reference Manual
~A~%"
          (escape library-name)
          (if (or tagline version)
              (format nil "@subtitle ~@[~A~]~:[~;, ~]~@[version ~A~]~%"
                      (escape tagline) (and tagline version) (escape version))
              ""))
  (mapc (lambda (name email)
          (format t "@author ~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
                  (escape name) email (escape email)))
        contact-names contact-emails)
  (terpri)
  (when (or declt-notice license)
    (format t "@page~%"))

  (when declt-notice
    (format t "~
@quotation
This manual was generated automatically by Declt ~A~@[ on ~A~].
@end quotation~%"
            (escape (version declt-notice))
            (when (eq declt-notice :long) (escape current-time-string))))

  (when license
    (format t "@vskip 0pt plus 1filll~%@insertcopying~%"))

  (format t "@end titlepage~4%")

  (format t "~
@c ====================================================================
@c Table of Contents
@c ====================================================================
@contents~%"))

#lang scribble/doc

@title{DrFuzzy file finder}

This is the documentation for the DrFuzzy tool. 


@section{Installation}

Run this inside DrScheme

@(require scribble/manual
          scribble/eval
          (for-label scheme 
                     (planet "fuzzy-tool.scm"
                             ("ebellani" "dr-fuzzy.plt" 1 1))))
Wait for the indication that the process is over. A new instance of
DrScheme will be open. In this instance a menu called "DrFuzzy file finder" will
now be available under the "File" menu, right below the "Install .plt file". You
can start using this DrScheme instance or just close all instances and open a
new one.

@section{Usage}

DrFuzzy loads every file path available in every directory starting with the
directory DrScheme first was started. For now it is not possible to change that
initial directory pick, so, if you are planning to use DrFuzzy in a project,
open DrScheme in the root directory of that project. There is also a 10000 files
limit, and DrFuzzy by default ignores files that end with "~" and files and
directories that begin with ".".

By clicking in the "DrFuzzy file finder" a popup dialog will appear. Any
character typed in the "Search" input field will activate the search. The list
field allows for multiple file selections, so you can CTRL-click to open several
files.

An overall description is that hits in the filename are worth more to DrFuzzy
than in the directory names. To explictly search for something inside
directories, you have 2 ways. Either you type the directory name, or its first
letter followed by a separator.

For example, imagine that you have those files in an UNIX system:
@verbatim{
/|
 |
 |- dir|
       |-file1
       |-pic1
 |
 |- anotherdir|
              |
              |dir2|
                   |scheme-file.scm
}


To find the scheme-file.scm you could type ".scm" any combination of the chars
in the file name "scheme-file.scm". 

To match exactly to that file you would type 
"anotherdir/d/scheme-file.scm" or "a/d/scheme-file.scm" or
"a/dir2/scheme-file.scm".

To check the contents of dir you could type "d/" or "dir/".


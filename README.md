Haskell Tagger
==============

A super simple rule-based parts-of-speech tagger, based on the work of Eric Brill. This was written primarily as an exercise for learning Haskell.

Compiling
---------

This program can be easily compiled and installed using Cabal, the Haskell package system. With cabal (>=1.8) installed, run the following:

    cabal install --prefix=/usr/local

(Or use an installation prefix of your preference.)

Compilation requires the PCRE library and headers to be installed on your system. For Mac OS X, the easiest way to install PCRE is via Homebrew.

    brew install pcre

Linux-based systems should be able to install PCRE via the preferred package manager (yum, apt-get, etc.)

Use
---

Tag any quoted string like so:

    tagger "Time flies like an arrow. Fruit flies like a banana."

The above input will result in output similar to the following:

> Time/NNP flies/VBZ like/IN a/DT arrow/NN Fruit/NNP flies/VBZ like/IN a/DT banana/NN

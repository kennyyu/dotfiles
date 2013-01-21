#!/bin/bash

# base directory
dir=`pwd`

emacsrepo=$dir/emacs
emacslink=~/.emacs.d
ln -s $emacsrepo $emacslink
echo Symlinking $emacsrepo to $emacslink

gitrepo=$dir/gitconfig
gitlink=~/.gitconfig
ln -s $gitrepo $gitlink
echo Symlinking $gitrepo to $gitlink

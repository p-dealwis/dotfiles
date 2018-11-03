#!/usr/bin/env bash

# For testing
if [ "$1" == "--nopull" -o "$1" == "-n" ]; then
    git pull origin master;
fi

# Declare an associative array
declare -A files

# Define the files. The key is the file and the value is the destination.
files=( \
    [".vimrc"]="$HOME" \
    [".zshrc"]="$HOME" \
    [".tmux.conf"]="$HOME" \
    ["i3/config"]="$HOME/.config/i3" \
    ["nvim/init.vim"]="$HOME/.config/nvim" \
)

function linktocustom () {
    ln -s $PWD/$1 $2 \
}

function run (){
    for file in "${!files[@]}"
        do linktocustom $file ${files[$file]}
    done
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
    run;
else
	read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1;
	echo "";
	if [[ $REPLY =~ ^[Yy]$ ]]; then
        run
	fi;
fi;

unset run;

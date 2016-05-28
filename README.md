## Installation

You will need cabal :

    cabal sandbox init
    cabal install -j4
The binary will in the sandbox (`.cabal/bin/mng`).

## Usage

mng --help will give you the options.

You can add a movie or a comic with the titel and status

    mng add movie "The avengers" towatch
    mng add comic "Batman" read

You can also set the serie (for comics), rating or year :

    mng add movie "The avengers" watched -y 2012
    mng add comics "Arkham Asylum: Living Hell" read -s Batman

## Technical details

Data is stored in $HOME/.mng/movies.csv, $HOME/.mng/comics.csv

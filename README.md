![Build status](https://travis-ci.org/alexDarcy/mng.svg?branch=travis)

## Installation

Install stack then run:

    stack build
    stack install

## Usage

`mng --help` will give you the options.

You can add a movie or a comic with the title and status

    mng add movie "The avengers" towatch
    mng add comic "Batman" read

You can also define the serie (for comics), rating or year :

    mng add movie "The avengers" watched -y 2012
    mng add comics "Arkham Asylum: Living Hell" read -s Batman

If the entry already exists, it will not be updated. Otherwise, if there is a
previous entry (the first with the same title), it will be updated.

You can also export the list to HTML :

    mng export movie

The output will be by default in the `html` directory

## Technical details

Data is stored in `$HOME/.mng/TYPE.csv` where `TYPE=movie, comics`.

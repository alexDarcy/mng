![Build status](https://travis-ci.org/alexDarcy/mng.svg?branch=travis)

## Installation

Install stack then run:

    stack build
    stack install

## Usage

mng --help will give you the options.

You can add a movie or a comic with the titel and status

    mng add movie "The avengers" towatch
    mng add comic "Batman" read

You can also set the serie (for comics), rating or year :

    mng add movie "The avengers" watched -y 2012
    mng add comics "Arkham Asylum: Living Hell" read -s Batman

## Technical details

Data is stored in `$HOME/.mng/TYPE.csv` where `TYPE=movie, comics`.

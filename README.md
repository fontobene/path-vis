# FontoBene PathVis

A tool to interactively visualize FontoBene path expressions / polylines,
written in [Elm](http://elm-lang.org/).

You can find a hosted version here: https://fontobene.dbrgn.ch/pathvis/#init=1,1;2,7;7,7,6;8,1

## Requirements

You need npm and Elm 0.19: https://guide.elm-lang.org/install.html

If you aren't familiar with Elm yet, you should probably read [the
tutorial](https://guide.elm-lang.org/).

To install all dependencies:

    make setup

## Development

To start the dev server:

    make run

Now visit [localhost:8000](http://localhost:8000/) in your browser
to see the application.

## Production

To build the application, just type

    make dist

The output will be written to the `dist/` directory.

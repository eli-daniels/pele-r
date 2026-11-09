## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Test environments

* Local: Fedora 44, R 4.6.1

## Bundled JavaScript

The package bundles the 'pele' JavaScript library, by kepano, in
`inst/js/pele.global.js`. It is the minified bundle published on npm. The
unminified TypeScript source is at <https://github.com/obsidianmd/pele>, and
`inst/COPYRIGHTS` links to it and keeps the MIT and ISC license texts of the
bundled code. kepano and the Lucide contributors are listed as copyright
holders in `Authors@R`.

## Package name

The package is named after the library it runs. It is an R interface to
'pele', and all the rendering is done by the bundled library.

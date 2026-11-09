# Vendors the pele browser bundle into inst/js/. Run from the package root:
#   Rscript data-raw/update_pele.R

version <- "1.13.0"
url <- sprintf("https://cdn.jsdelivr.net/npm/pele@%s/dist/pele.global.js", version)

tmp <- tempfile(fileext = ".js")
utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
js <- readChar(tmp, file.size(tmp), useBytes = TRUE)
Encoding(js) <- "UTF-8"

# The V8 package's engine has no ICU, so it rejects Unicode property escapes and the
# whole bundle fails to load. pele uses them once, in the regex that validates font
# family names (src/svg/theme.ts). A wider range is still safe there: the characters
# that could end a CSS declaration are all ASCII and stay excluded.
escape <- "\\p{L}\\p{N}"
n <- lengths(regmatches(js, gregexpr(escape, js, fixed = TRUE)))
if (n != 2L) stop("Expected 2 Unicode escapes in pele ", version, ", found ", n, ". Review the patch.")
js <- gsub(escape, "A-Za-z0-9\\u00C0-\\uFFFF", js, fixed = TRUE)
if (grepl("\\p{", js, fixed = TRUE)) stop("Unicode property escapes remain after patching.")

con <- file("inst/js/pele.global.js", open = "wb")
writeChar(js, con, eos = NULL, useBytes = TRUE)
close(con)
message("Vendored pele ", version, " into inst/js/pele.global.js")

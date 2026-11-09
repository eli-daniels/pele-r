# pele_theme ----

#' Colours for pele diagrams
#'
#' @description
#' A theme sets the colours 'pele' draws with, and they are written into the
#' SVG. Colours not in the theme take the 'pele' defaults, not the page's.
#'
#' Without a theme, a diagram in HTML keeps the 'pele' CSS variables, so the
#' page sets its colours with CSS on `.pele`, which can follow a dark mode:
#'
#' ```css
#' .quarto-dark .pele { --pele-fg: #e6e4d9; --pele-surface: #282726; }
#' ```
#'
#' A theme can hand single colours back to the page:
#'
#' - `"inherit"` takes the page's value of that 'pele' variable, as set by
#'   CSS on `.pele`, or the 'pele' default if the page sets none.
#' - A `var()` of another variable with a fallback, such as
#'   `fg = "var(--bs-body-color, #1f1f1f)"`, follows that page variable.
#'
#' In files and non-HTML documents there is no page, so `"inherit"` gives the
#' 'pele' default and a `var()` gives its fallback.
#'
#' Set a theme for every diagram with `options(pele.theme = pele_theme(...))`.
#'
#' @param ... Named CSS values, or `"inherit"`. The names are the 'pele'
#'   variables without their `--pele-` prefix, with `_` for `-`:
#'   - `bg`, `fg`: background and text
#'   - `fg_on_light`, `fg_on_dark`: text on fills the diagram text sets with
#'     `style`, `classDef`, or a sequence `rect` or `box`. 'pele' picks the
#'     one that contrasts more with the fill.
#'   - `muted`: secondary text
#'   - `line`: edges and arrows
#'   - `surface`, `surface_alt`: node fills
#'   - `border`: node outlines
#'   - `accent`: highlights
#'   - `critical`: what a diagram marks as critical
#'   - `series_1` to `series_8`: chart series. A palette the diagram text
#'     sets in `themeVariables`, such as `pie1`, takes their place.
#'   - `radius`: corner radius, such as `"6px"`
#'
#' @return A `pele_theme`: a named character vector of CSS variables.
#' @export
#' @examples
#' flexoki <- pele_theme(bg = "#fffcf0", fg = "#100f0f", accent = "#205ea6")
#' d <- pele("flowchart LR\n  A --> B", theme = flexoki)
#'
#' # Fixed colours on the page's background
#' on_page <- pele_theme(fg = "#100f0f", accent = "#205ea6", bg = "inherit")
pele_theme <- function(...) {
  values <- list(...)
  nms <- names(values) %||% rep("", length(values))

  unnamed <- which(!nzchar(nms))
  if (length(unnamed)) {
    pele_abort(c(
      "All values in {.arg ...} must be named.",
      x = "Value{?s} {unnamed} {?is/are} unnamed.",
      i = "Name each value after a theme variable, such as {.code fg = \"#100f0f\"}."
    ))
  }

  unknown <- setdiff(nms, theme_names)
  if (length(unknown)) {
    pele_abort(c(
      "Can't use unknown theme variable{?s} {.val {unknown}}.",
      bullets(did_you_mean(unknown, theme_names), "i"),
      i = "See {.help pele::pele_theme} for the variables."
    ))
  }

  bad <- nms[!vapply(values, function(v) is.character(v) && length(v) == 1L && !is.na(v), logical(1))]
  if (length(bad)) {
    pele_abort(c(
      "Theme values must be single strings.",
      bullets(vapply(bad, function(n) cli::format_inline("{.arg {n}} is {.obj_type_friendly {values[[n]]}}."), ""), "x")
    ))
  }

  css <- as.character(unlist(values))
  # A value that could end its declaration would let a theme inject other CSS
  bad <- nms[grepl("[;{}<>\"]", css)]
  if (length(bad)) {
    pele_abort(c(
      "Theme values can't contain semicolons, braces, angle brackets or double quotes.",
      bullets(vapply(bad, function(n) cli::format_inline("{.arg {n}} is {.val {values[[n]]}}."), ""), "x"),
      i = "Each value is one CSS value, such as the colour {.val #205ea6} or the length {.val 6px}."
    ))
  }
  structure(css, names = paste0("--pele-", gsub("_", "-", nms)), class = "pele_theme")
}

#' "Did you mean" hints for names that are a small edit away from a known one
#' @noRd
did_you_mean <- function(x, known) {
  hints <- vapply(x, function(name) {
    d <- utils::adist(name, known)[1, ]
    if (min(d) > 2) return(NA_character_)
    cli::format_inline("Did you mean {.val {known[which.min(d)]}} for {.val {name}}?")
  }, character(1))
  hints[!is.na(hints)]
}

theme_names <- c(
  "bg", "fg", "fg_on_light", "fg_on_dark", "muted", "line", "surface", "surface_alt",
  "border", "accent", "critical",
  paste0("series_", 1:8), "radius"
)

#' @export
print.pele_theme <- function(x, ...) {
  cat("<pele_theme>\n")
  if (length(x)) cat(paste0("  ", names(x), ": ", unclass(x), "\n"), sep = "")
  invisible(x)
}

# resolve_vars ----

#' Replace the `var()`s in an SVG with their values
#'
#' librsvg ignores CSS custom property definitions (`--x: ...`) and only reads
#' the fallback of a `var()`, so 'pele' output rasterises black. A themed
#' diagram in HTML needs its colours fixed too. This writes the values in:
#' from the definitions in the SVG first, then from `theme`, then the
#' fallback.
#'
#' In a file every `var()` with a value or a fallback is resolved. On a web
#' page (`page = TRUE`), two kinds stay live for the page's CSS to set:
#' variables that are not pele's, such as `var(--bs-body-color, #222)` in a
#' theme value, and pele variables the theme sets to `"inherit"`. A `var()`
#' with no value and no fallback always stays; `unresolved_vars()` lists them.
#'
#' @param svg An SVG string from [pele()].
#' @param theme A [pele_theme()] or `NULL`.
#' @param page Whether the SVG goes on a web page, rather than into a file.
#' @param call The call to report if the variables refer to each other in a
#'   cycle.
#' @return The SVG string.
#' @keywords internal
resolve_vars <- function(svg, theme = NULL, page = FALSE, call = rlang::caller_env()) {
  # A definition runs to the next `;` or `"`, but `&quot;` in a font name is part of it
  defs <- regmatches(svg, gregexpr('--[A-Za-z0-9_-]+:(?:&[a-z]+;|[^;"&])+', svg, perl = TRUE))[[1]]
  defs <- structure(sub("^[^:]+:", "", defs), names = sub(":.*$", "", defs))
  # An "inherit" value is not a value: leaving it out of `defs` sends its var() to the
  # pele default in a file, and leaves it live on a page
  inherited <- names(theme)[theme == "inherit"]
  # The SVG's own definitions come first: a palette set in the diagram text, such as
  # `pie1`, reaches the SVG as `--pele-series-1` and is fixed, like other diagram styles
  defs <- c(defs, unclass(theme)[!names(theme) %in% inherited])

  # Innermost var() first: its fallback holds no parentheses
  re <- "var\\((--[A-Za-z0-9_-]+)(,[^()]*)?\\)"
  # Each pass removes one level of nesting; pele nests three deep
  for (pass in 1:10) {
    hits <- unique(regmatches(svg, gregexpr(re, svg, perl = TRUE))[[1]])
    names <- sub(re, "\\1", hits, perl = TRUE)
    fallbacks <- sub(re, "\\2", hits, perl = TRUE)
    # On a page, a pele variable the theme leaves out still takes the pele default, so the
    # page can't mix its colours into a themed diagram. Only other variables stay live.
    use_fallback <- nzchar(fallbacks) &
      (!page | (startsWith(names, "--pele-") & !names %in% inherited))
    resolve <- names %in% names(defs) | use_fallback
    if (!any(resolve)) return(svg)
    for (i in which(resolve)) {
      # The fallback was captured with its leading comma
      value <- if (names[i] %in% names(defs)) defs[[names[i]]] else substring(fallbacks[i], 2)
      svg <- gsub(hits[i], value, svg, fixed = TRUE)
    }
  }
  pele_abort(
    c(
      "Can't resolve the CSS variables of the diagram.",
      x = "They refer to each other in a cycle.",
      i = "To take the page's value of a pele variable, set it to {.val inherit}, not to its own {.code var()}."
    ),
    call = call
  )
}

#' Names of the CSS variables [resolve_vars()] left in an SVG
#' @noRd
unresolved_vars <- function(svg) {
  unique(regmatches(svg, gregexpr("(?<=var\\()--[A-Za-z0-9_-]+", svg, perl = TRUE))[[1]])
}

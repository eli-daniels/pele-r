# pele_save ----

#' Save a diagram to a file
#'
#' @description
#' Writes an SVG, PNG or PDF file, chosen by the extension of `file`. CSS
#' variables are replaced with their values first, so the file looks the same
#' in any viewer and needs no stylesheet.
#'
#' @param x A diagram from [pele()].
#' @param file Path ending in `.svg`, `.png` or `.pdf`.
#' @param theme A [pele_theme()] or `NULL`. Defaults to the diagram's theme.
#'   With a theme the file has the theme's background colour. Without one the
#'   background is transparent.
#' @param scale Size multiplier. For a PNG, `2` gives a sharp image on high
#'   density screens.
#'
#' @return `file`, invisibly.
#' @export
#' @examples
#' d <- pele("flowchart LR\n  A --> B")
#' pele_save(d, tempfile(fileext = ".png"), scale = 2)
pele_save <- function(x, file, theme = x$theme, scale = 1) {
  check_pele(x)
  check_string(file)
  check_theme(theme)
  check_number(scale)
  ext <- tolower(sub("^.*\\.", "", basename(file)))
  if (!ext %in% c("svg", "png", "pdf")) {
    pele_abort(c(
      "{.arg file} must end in {.file .svg}, {.file .png} or {.file .pdf}.",
      x = "{.file {file}} ends in {.file .{ext}}."
    ))
  }
  # rsvg fails on a missing folder too, but only says "cannot open the connection"
  if (!dir.exists(dirname(file))) {
    pele_abort(c(
      "Can't write {.file {file}}.",
      x = "The folder {.path {dirname(file)}} doesn't exist."
    ))
  }

  svg <- resolve_vars(x$svg, theme)
  if (!is.null(theme)) svg <- paint_bg(svg)
  unresolved <- unresolved_vars(svg)
  if (length(unresolved)) {
    cli::cli_warn(c(
      "Can't resolve CSS variable{?s} {.code {unresolved}} outside a web page.",
      i = "{cli::qty(length(unresolved))}{?It is/They are} drawn as if unset, which is usually black.",
      i = "Give theme values that use {.code var()} a fallback, such as {.code var(--my-fg, #1f1f1f)}."
    ))
  }
  if (ext == "svg") {
    writeLines(svg, file, useBytes = TRUE)
    return(invisible(file))
  }
  width <- round(x$width * scale)
  height <- round(x$height * scale)
  # rsvg takes a string as a path or URL, never as SVG text, so the SVG goes in as raw bytes
  data <- charToRaw(enc2utf8(svg))
  switch(ext,
    png = rsvg::rsvg_png(data, file, width = width, height = height),
    pdf = rsvg::rsvg_pdf(data, file, width = width, height = height)
  )
  invisible(file)
}

# HTML ----

# A themed diagram has its colours written in, and its background painted, so the page
# cannot mix its own into them, except where the theme hands a colour back with "inherit"
# or a var() of a page variable. Without a theme the CSS variables stay, and page CSS on
# `.pele` sets the colours.
#' @importFrom htmltools as.tags
#' @exportS3Method htmltools::as.tags
as.tags.pele <- function(x, ...) {
  svg <- x$svg
  if (!is.null(x$theme)) svg <- paint_bg(resolve_vars(svg, x$theme, page = TRUE))
  htmltools::div(class = "pele-r", htmltools::HTML(svg))
}

#' Paint the background of a resolved SVG
#'
#' pele leaves the background transparent. A themed diagram gets a rect of its
#' background colour behind everything else. A rect and not a CSS
#' `background`, because librsvg ignores CSS backgrounds.
#'
#' The colour is read from the resolved SVG, not the theme, so a theme without
#' `bg` gets pele's default, and `bg = "inherit"` on a page gets a live
#' `var(--pele-bg, ...)`.
#' @noRd
paint_bg <- function(svg) {
  bg <- regmatches(svg, regexpr("(?<=--_bg:)[^;\"]+", svg, perl = TRUE))
  box <- strsplit(regmatches(svg, regexpr('(?<=viewBox=")[^"]+', svg, perl = TRUE)), " ")[[1]]
  rect <- sprintf(
    '<rect class="pele-r-bg" x="%s" y="%s" width="%s" height="%s" fill="%s"/>',
    box[1], box[2], box[3], box[4], bg
  )
  sub("(<svg[^>]*>)", paste0("\\1", rect), svg)
}

#' @export
print.pele <- function(x, ...) {
  print(htmltools::browsable(as.tags(x)))
  invisible(x)
}

# knitr ----

#' Print a diagram in a knitr document
#'
#' Inline SVG in HTML output. Elsewhere a figure file at the diagram's CSS
#' size: PDF for LaTeX, where it stays vector, and PNG at twice the
#' resolution for other formats.
#'
#' @param x A diagram from [pele()].
#' @param options The chunk options.
#' @param ... Unused.
#' @return Output for knitr.
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.pele <- function(x, options = knitr::opts_current$get(), ...) {
  if (knitr::is_html_output()) {
    return(knitr::knit_print(as.tags(x), options = options))
  }
  latex <- knitr::is_latex_output()
  # Numbered by diagram id, so several diagrams in one chunk get a file each. Two with the
  # same text share an id, and a file, which then holds the same picture.
  path <- knitr::fig_path(if (latex) ".pdf" else ".png", options, number = x$id)
  # knitr creates the figure folder only for the plots it records itself
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # A PDF is sized in points, and a CSS pixel is 0.75 of one. A PNG is drawn twice as
  # large for sharpness, and its 192 dpi brings it back to the CSS size.
  pele_save(x, path, scale = if (latex) 0.75 else 2)
  knitr::knit_print(knitr::include_graphics(path, dpi = if (!latex) 192), options = options)
}

#' knitr engine for `pele` chunks
#'
#' Renders the chunk text with [pele()]. The diagram id comes from the chunk
#' label, which knitr keeps unique in a document, so two chunks with the same
#' text still get different ids.
#'
#' @param options The chunk options.
#' @return Output for knitr.
#' @keywords internal
eng_pele <- function(options) {
  if (!isTRUE(options$eval)) {
    return(knitr::engine_output(options, options$code, ""))
  }
  d <- pele(options$code, id = paste0("pele-", options$label))
  # engine_output() takes a list when the output is not plain text. An object of class
  # `source` is how knitr represents echoed code in that list.
  source <- if (!isFALSE(options$echo)) list(structure(list(src = options$code), class = "source"))
  knitr::engine_output(options, out = c(source, list(knitr::knit_print(d, options = options))))
}

.onLoad <- function(libname, pkgname) {
  knitr::knit_engines$set(pele = eng_pele)
}

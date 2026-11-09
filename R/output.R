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

  svg <- standalone_svg(x$svg, theme)
  if (ext == "svg") {
    writeLines(svg, file, useBytes = TRUE)
    return(invisible(file))
  }
  width <- round(x$width * scale)
  height <- round(x$height * scale)
  data <- svg_bytes(svg)
  switch(ext,
    png = rsvg::rsvg_png(data, file, width = width, height = height),
    pdf = rsvg::rsvg_pdf(data, file, width = width, height = height)
  )
  invisible(file)
}

# pele_as_grob ----

#' Convert a diagram to a grob
#'
#' @description
#' Turns a diagram into a 'grid' grob, so it can be drawn with
#' [grid::grid.draw()] and laid out next to plots by packages such as
#' 'patchwork', which takes a grob added to a plot with `+` or wrapped in
#' `wrap_elements()`.
#'
#' The grob keeps the diagram's aspect ratio and is as large as its
#' viewport allows.
#'
#' By default the grob is made of vector shapes, like a ggplot, so it stays
#' sharp at any size and in PDF output. Text is drawn as glyph outlines in
#' the diagram's font, as installed on this computer. This needs the
#' 'grImport2' package. Sankey links lose their gradients and take the
#' colour of their source node.
#'
#' With `raster = TRUE` the grob is a bitmap. It needs no other package and
#' keeps sankey gradients, but blurs when drawn larger than its pixel size.
#'
#' @param x A diagram from [pele()].
#' @param theme A [pele_theme()] or `NULL`. Defaults to the diagram's theme.
#'   With a theme the grob has the theme's background colour. Without one the
#'   background is transparent.
#' @param raster Whether to draw the diagram as a bitmap rather than as
#'   vector shapes.
#' @param scale Size multiplier of the bitmap, used when `raster = TRUE`. The
#'   bitmap has the diagram's CSS size in pixels times `scale`.
#'
#' @return A grob.
#' @export
#' @examples
#' d <- pele("flowchart LR\n  A --> B")
#' grid::grid.newpage()
#' grid::grid.draw(pele_as_grob(d, raster = TRUE))
#'
#' if (requireNamespace("grImport2", quietly = TRUE)) {
#'   grid::grid.newpage()
#'   grid::grid.draw(pele_as_grob(d))
#' }
pele_as_grob <- function(x, theme = x$theme, raster = FALSE, scale = 2) {
  check_pele(x)
  check_theme(theme)
  check_bool(raster)
  check_number(scale)
  svg <- standalone_svg(x$svg, theme)

  if (raster) {
    width <- round(x$width * scale)
    height <- round(x$height * scale)
    bitmap <- rsvg::rsvg_nativeraster(svg_bytes(svg), width = width, height = height)
    return(grid::rasterGrob(bitmap, interpolate = TRUE))
  }

  rlang::check_installed(
    "grImport2",
    reason = "to draw a diagram as vector shapes. Use `raster = TRUE` for a bitmap."
  )
  # grImport2 draws gradients only through gridSVG, and paints them black elsewhere. pele
  # follows each gradient with a plain colour, its fallback, which is used instead.
  svg <- gsub('fill="url\\(#[^)]+\\) ', 'fill="', svg)
  # grImport2 reads SVG written by Cairo, as rsvg does, with text as glyph outlines
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  rsvg::rsvg_svg(svg_bytes(svg), file)
  # readPicture() recognises Cairo output by a top-level `<g id="surface...">` that recent
  # Cairo versions no longer write, so it would warn about a valid file
  picture <- grImport2::readPicture(file, warn = FALSE)
  # Delayed, the picture is fitted to the viewport it is drawn in, not to the device open
  # when the grob is made
  grImport2::pictureGrob(picture, expansion = 0, delayContent = TRUE)
}

#' The SVG of a diagram that looks the same outside a web page
#'
#' Writes in the values of the CSS variables and, with a theme, paints the
#' background. Warns about variables only a web page could define.
#' @noRd
standalone_svg <- function(svg, theme) {
  svg <- resolve_vars(svg, theme)
  if (!is.null(theme)) svg <- paint_bg(svg)
  unresolved <- unresolved_vars(svg)
  if (length(unresolved)) {
    cli::cli_warn(c(
      "Can't resolve CSS variable{?s} {.code {unresolved}} outside a web page.",
      i = "{cli::qty(length(unresolved))}{?It is/They are} drawn as if unset, which is usually black.",
      i = "Give theme values that use {.code var()} a fallback, such as {.code var(--my-fg, #1f1f1f)}."
    ))
  }
  svg
}

# rsvg takes a string as a path or URL, never as SVG text, so the SVG goes in as raw bytes
svg_bytes <- function(svg) charToRaw(enc2utf8(svg))

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

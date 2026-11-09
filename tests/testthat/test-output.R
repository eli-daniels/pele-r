d <- pele("flowchart LR\n  A[Start] --> B[End]", id = "out")

test_that("pele_save() rasterises in the theme colours, not black", {
  file <- withr::local_tempfile(fileext = ".png")
  pele_save(d, file, theme = pele_theme(surface = "#00ff00"))
  img <- png::readPNG(file)
  colours <- unique(grDevices::rgb(img[, , 1], img[, , 2], img[, , 3]))
  expect_true("#00FF00" %in% colours)
})

test_that("pele_save() paints a theme's background, and leaves none without a theme", {
  corner_alpha <- function(theme) {
    file <- withr::local_tempfile(fileext = ".png")
    pele_save(d, file, theme = theme)
    img <- png::readPNG(file)
    # An opaque PNG has no alpha channel
    if (dim(img)[3] == 4) img[1, 1, 4] else 1
  }
  expect_equal(corner_alpha(pele_theme(bg = "#000000")), 1)
  expect_equal(corner_alpha(NULL), 0)
})

test_that("pele_save() writes SVG and PDF, and scales", {
  svg <- withr::local_tempfile(fileext = ".svg")
  pele_save(d, svg)
  expect_no_match(paste(readLines(svg), collapse = ""), "var(", fixed = TRUE)

  pdf <- withr::local_tempfile(fileext = ".pdf")
  pele_save(d, pdf)
  expect_equal(readBin(pdf, "raw", 4), charToRaw("%PDF"))

  png <- withr::local_tempfile(fileext = ".png")
  pele_save(d, png, scale = 2)
  expect_equal(dim(png::readPNG(png))[2], round(d$width * 2))
})

# The pixels of a grob drawn on a white 300 x 300 square, as rows, columns and RGB. The grob
# is made while the whole square is the viewport, then drawn in its left half, as a
# patchwork cell would.
draw_pixels <- function(grob) {
  file <- withr::local_tempfile(fileext = ".png")
  grDevices::png(file, width = 300, height = 300, bg = "white")
  force(grob)
  grid::pushViewport(grid::viewport(x = 0.25, width = 0.5))
  grid::grid.draw(grob)
  grDevices::dev.off()
  png::readPNG(file)[, , 1:3]
}

test_that("pele_as_grob() fits the diagram in its viewport and keeps its aspect ratio", {
  black <- pele_theme(bg = "#000000")
  fit <- function(grob) {
    px <- draw_pixels(grob)
    is_bg <- px[, , 1] + px[, , 2] + px[, , 3] == 0
    c(width = sum(colSums(is_bg) > 0), height = sum(rowSums(is_bg) > 0))
  }
  expected_height <- 150 * d$height / d$width
  for (raster in c(TRUE, if (requireNamespace("grImport2", quietly = TRUE)) FALSE)) {
    size <- fit(pele_as_grob(d, theme = black, raster = raster))
    expect_equal(size[["width"]], 150, tolerance = 0.02)
    expect_equal(size[["height"]], expected_height, tolerance = 0.05)
  }
})

test_that("a raster grob has the diagram's size times scale", {
  g <- pele_as_grob(d, raster = TRUE, scale = 3)
  expect_equal(dim(g$raster), round(c(d$height, d$width) * 3))
})

test_that("a vector grob draws sankey links in their source colours, not black", {
  skip_if_not_installed("grImport2")
  sankey <- pele("sankey-beta\nA,B,10\nA,C,5")
  px <- draw_pixels(pele_as_grob(sankey))
  expect_false(any(px[, , 1] + px[, , 2] + px[, , 3] == 0))
})

test_that("pele_save() explains bad arguments", {
  expect_snapshot(error = TRUE, {
    pele_save("flowchart LR", "x.png")
    pele_save(d, "x.jpg")
    pele_save(d, "no-such-folder/x.png")
    pele_save(d, "x.png", scale = 0)
    pele_save(d, "x.png", scale = "2")
  })
})

test_that("pele_save() warns about CSS variables only a web page could define", {
  file <- withr::local_tempfile(fileext = ".svg")
  expect_snapshot(pele_save(d, file, theme = pele_theme(fg = "var(--page-fg)")))
})

test_that("as.tags() keeps CSS variables, unless a theme fixes the colours", {
  plain <- as.character(htmltools::as.tags(pele("flowchart LR\n  A --> B")))
  expect_match(plain, '<div class="pele-r"><svg ', fixed = TRUE)
  expect_match(plain, "var(--pele-fg", fixed = TRUE)

  th <- pele_theme(fg = "#100f0f", bg = "#fffcf0")
  themed <- as.character(htmltools::as.tags(pele("flowchart LR\n  A --> B", theme = th)))
  expect_no_match(themed, "var(", fixed = TRUE)
  expect_match(themed, "#100f0f", fixed = TRUE)
  expect_match(themed, '<rect class="pele-r-bg" [^>]*fill="#fffcf0"/>')
})

# Knits a pele chunk to markdown as if for the given pandoc format
knit_chunk <- function(to) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  rmd <- file.path(dir, "doc.Rmd")
  writeLines(c("```{pele flow, echo = FALSE}", "flowchart LR", "  A --> B", "```"), rmd)
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::opts_knit$set(rmarkdown.pandoc.to = to)
  withr::defer(knitr::opts_knit$set(rmarkdown.pandoc.to = old))
  withr::local_dir(dir)
  md <- knitr::knit(rmd, quiet = TRUE, envir = new.env())
  list(md = paste(readLines(md), collapse = "\n"), dir = dir)
}

test_that("the pele engine inlines SVG in HTML output", {
  out <- knit_chunk("html")
  expect_match(out$md, "<svg ", fixed = TRUE)
})

test_that("the pele engine includes a PDF figure in LaTeX output", {
  out <- knit_chunk("latex")
  expect_no_match(out$md, "<svg", fixed = TRUE)
  figs <- list.files(out$dir, pattern = "\\.pdf$", recursive = TRUE)
  expect_length(figs, 1)
  expect_match(out$md, basename(figs), fixed = TRUE)
})

test_that("pele_theme() maps names to pele CSS variables", {
  th <- pele_theme(fg = "#100f0f", surface_alt = "#e6e4d9")
  expect_equal(unclass(th), c(`--pele-fg` = "#100f0f", `--pele-surface-alt` = "#e6e4d9"))
})

test_that("pele_theme() explains bad names and values", {
  expect_snapshot(error = TRUE, {
    pele_theme("red", fg = "blue")
    pele_theme(surfce = "red", colour = "blue")
    pele_theme(fg = 1, bg = c("a", "b"))
    pele_theme(fg = "red;background:url(x)")
  })
})

test_that("resolve_vars() leaves no var() and takes the theme over the defaults", {
  d <- pele("flowchart LR\n  A --> B", font_family = '"Fira Sans", sans-serif')
  out <- resolve_vars(d$svg, pele_theme(surface = "#00ff00"))
  expect_no_match(out, "var(", fixed = TRUE)
  expect_match(out, 'fill="#00ff00"', fixed = TRUE)
  # A quoted font name survives: its &quot; is not the end of the declaration
  expect_match(out, "font-family:&quot;Fira Sans&quot;, sans-serif", fixed = TRUE)
})

test_that("resolve_vars() keeps a var() it has no value for, for the page to define", {
  out <- resolve_vars(pele("flowchart LR\n  A --> B")$svg, pele_theme(fg = "var(--page-fg)"))
  expect_equal(unresolved_vars(out), "--page-fg")
})

test_that("on a page, a theme hands back only the colours it says to", {
  svg <- pele("flowchart LR\n  A --> B")$svg
  th <- pele_theme(fg = "var(--bs-body-color, #222222)", bg = "inherit", surface = "#00ff00")

  page <- resolve_vars(svg, th, page = TRUE)
  expect_setequal(unresolved_vars(page), c("--bs-body-color", "--pele-bg"))
  expect_match(page, "var(--bs-body-color, #222222)", fixed = TRUE)
  expect_match(page, "var(--pele-bg,#fff)", fixed = TRUE)
  # A colour the theme leaves out takes the pele default, not the page's
  expect_no_match(page, "var(--pele-border", fixed = TRUE)
  expect_match(page, 'fill="#00ff00"', fixed = TRUE)

  # A file has no page: the var() takes its fallback, and "inherit" the pele default
  file <- resolve_vars(svg, th)
  expect_length(unresolved_vars(file), 0)
  expect_match(file, "--_fg: #222222", fixed = TRUE)
  expect_match(file, "--_bg:#fff", fixed = TRUE)
})

test_that("a palette in the diagram text takes the place of the theme's series colours", {
  pie <- c(
    "---", "config:", "  themeVariables:", "    pie1: '#ff0000'", "---",
    "pie", '  "A" : 1', '  "B" : 2'
  )
  th <- pele_theme(series_1 = "#00ff00", series_2 = "#0000ff")
  for (page in c(FALSE, TRUE)) {
    out <- resolve_vars(pele(pie)$svg, th, page = page)
    expect_match(out, 'fill="#ff0000"', fixed = TRUE)
    expect_no_match(out, 'fill="#00ff00"', fixed = TRUE)
    # A series the palette leaves out keeps the theme's colour
    expect_match(out, 'fill="#0000ff"', fixed = TRUE)
  }
})

test_that("as.tags() paints an inherited background with the page's value", {
  d <- pele("flowchart LR\n  A --> B", theme = pele_theme(bg = "inherit"))
  html <- as.character(htmltools::as.tags(d))
  expect_match(html, '<rect class="pele-r-bg" x="0" y="0" width="[0-9.]+" height="[0-9.]+" fill="var\\(--pele-bg,#fff\\)"/>')
})

test_that("resolve_vars() stops on a cycle", {
  svg <- '<svg style="--a:var(--b);--b:var(--a)" fill="var(--a)"></svg>'
  expect_snapshot(resolve_vars(svg), error = TRUE)
})

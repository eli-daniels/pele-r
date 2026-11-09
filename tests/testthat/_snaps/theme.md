# pele_theme() explains bad names and values

    Code
      pele_theme("red", fg = "blue")
    Condition
      Error in `pele_theme()`:
      ! All values in `...` must be named.
      x Value 1 is unnamed.
      i Name each value after a theme variable, such as `fg = "#100f0f"`.
    Code
      pele_theme(surfce = "red", colour = "blue")
    Condition
      Error in `pele_theme()`:
      ! Can't use unknown theme variables "surfce" and "colour".
      i Did you mean "surface" for "surfce"?
      i See `?pele::pele_theme()` for the variables.
    Code
      pele_theme(fg = 1, bg = c("a", "b"))
    Condition
      Error in `pele_theme()`:
      ! Theme values must be single strings.
      x `fg` is a number.
      x `bg` is a character vector.
    Code
      pele_theme(fg = "red;background:url(x)")
    Condition
      Error in `pele_theme()`:
      ! Theme values can't contain semicolons, braces, angle brackets or double quotes.
      x `fg` is "red;background:url(x)".
      i Each value is one CSS value, such as the colour "#205ea6" or the length "6px".

# resolve_vars() stops on a cycle

    Code
      resolve_vars(svg)
    Condition
      Error:
      ! Can't resolve the CSS variables of the diagram.
      x They refer to each other in a cycle.
      i To take the page's value of a pele variable, set it to "inherit", not to its own `var()`.


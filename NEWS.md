# pele 0.2.1

* DESCRIPTION gains `URL` and `BugReports` fields.

# pele 0.2.0

* The package is renamed from peler to pele, and its repository to
  [eli-daniels/pele-r](https://github.com/eli-daniels/pele-r). Install it
  with `pak::pak("eli-daniels/pele-r")` and load it with `library(pele)`.
  These names change with it:
  * The `peler.theme` option is now `pele.theme`.
  * Errors have class `pele_error` and `pele_error_<code>`, not
    `peler_error` and `peler_error_<code>`.
  * In HTML, the diagram's `<div>` has class `pele-r`, not `peler`, and a
    theme's background `<rect>` has class `pele-r-bg`, not `peler-bg`.
* Bundles pele 1.13.0, up from 1.12.0. See the
  [pele releases](https://github.com/obsidianmd/pele/releases).
* A palette the diagram text sets in `themeVariables`, such as `pie1` or
  `cScale0`, now colours its series, and takes the place of the theme's
  `series_1` to `series_8` in themed diagrams and saved files.

# peler 0.1.1

* Bundles pele 1.12.0, up from 1.6.0. See the
  [pele releases](https://github.com/obsidianmd/pele/releases).
* `pele()` gains `monospace` and `font_family_mono`.
* `pele_theme()` gains `fg_on_light` and `fg_on_dark`.

# peler 0.0.0.9000

* First version, bundling pele 1.6.0.

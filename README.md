# daisyr.studio

Shiny studio for the **daisyr** package: set up Daisy projects, edit
parameters and `.dai` files, run simulations, define objectives, and run
calibration, sensitivity analysis, and space-filling designs from a
browser.

Requires a working **daisyr** install and, for simulation, a Daisy
executable (see the daisyr README).

## Launch

```r
library(daisyr.studio)
run_daisyr_studio()
```

Sessions are stored in `~/daisyr-studio-sessions` (your user home).
Project-folder copies are still found if you point the app at a folder.

## Status

Pages for project, setup (YAML and raw `.dai`), parameters, simulate,
plots, observe, calibrate, sensitivity, and design.

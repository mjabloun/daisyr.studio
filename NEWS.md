# daisyr.studio 0.0.0.9000

* Renamed the package from `daisyr.shiny` to `daisyr.studio`.
* Initial package: Shiny studio for daisyr (project setup, parameters, simulate,
  plots, objective function, sensitivity, calibration, and design).
* Setup page has YAML and raw `.dai` editor tabs (load / edit / save without
  converting through YAML).
* Session files live in `~/daisyr-studio-sessions` (user home) and are listed
  even before a project folder is set; project-folder copies are still found.
* Parameter YAML is `parameters.yaml`. The R API uses `param_config`
  (`read_param_config()`, class `daisyr_param_config`). Older `registry.yaml`
  files still load.

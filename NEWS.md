# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres
to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]

### Changed

- Add test on the frequency of the input time series to avoid misleading error (#165).

## [0.2.8] - 2024-12-12

### Changed

- Improvement of warning message importing the model when no data in a SaItem (#61).

- `get_model`, `get_jmodel` and `get_jspec.sa_item` have now an argument `type` to define which specification to import (domain, estimation or point).
By default the domain specification is extracted (as before).

### Fixed

- Correction of bug of `add_sa_item()` of models created by `jtramoseats()` with external variables.

- Correction of bug of `get_jmodel()` when model contains user-defined regressors (calendar or regressors) with fixed coefficients (#157).

- Correction of bug for annual data for `regarima()` functions. 


## [0.2.7] - 2024-10-09

### Changed

- URL to github repository updated (github.com/jdemetra replaced by github.com/rjdverse).

- results of `user_defined_variables()` updated.

- .jars updated.

### Fixed

- README correction.

### Added

- benchmarking option added to `x13_spec()` and `tramoseats_spec()` and in output of `x13()` and `tramoseats()`.


## [0.2.6] - 2024-03-20

### Added

- possibility to export last msr for monthly data (issue #122).

- possibility to export X-11 some components: `y_cmp`, `y_cmp_f`, `t_cmp`, `t_cmp_f`, `sa_cmp`, `s_cmp`, `s_cmp_f`, and `i_cmp`.

### Fixed

- correction when importing models containing ramp regressors when the frequency is not 12 (monthly).

- correction of `get_jmodel()` with empty multiprocessings.


## [0.2.5] - 2024-02-04

### Changed

- `proc_data()` update to export more data from Java object.

### Fixed

- seasonality Kurskal-Wallis test corrected (issue #128).

- `vcov()` correction when matrix NULL and new parameter `component`.

- some corrections in `print()` methods.


## [0.2.4] - 2023-10-18

### Added

- New function `get_all_names()` and `get_position()`.

### Fixed

- correction of `save_workspace()` and  `load_workspace()` when using relative path.


## [0.2.3] - 2023-06-04

### Fixed

- Typo in printed arima coefficients : BPhi and Theta were inverted.


## [0.2.2] - 2023-04-27

### Changed

- update documentation.

- SystemRequirements update for CRAN policies.

### Removed

- removes extra argument print tramo.


## [0.2.1] - 2022-11-15

### Fixed

- Fix `complete_dictionary.SA()` to avoid warning.


## [0.2.0] - 2022-11-15

### Changed

- Java 17 compatibility.

- Change of javanamespace to avoid name clashes with jd+ 3.0.

- Default parameters of the span specification `d0` and `d1` as NA for clarity (issue #102).

### Fixed

- Bug with x11 introduced in RJDemetra 0.1.9 (issue #99).

- `easter.enabled` not working since RJDemetra 0.1.9.


## [0.1.9] - 2021-11-17

### Changed

- SystemRequirement update: only Java JRE is needed.

### Fixed

- Easter specification was not working.

- Some typos in the documentation.


## [0.1.8] - 2021-11-17

### Fixed

- Java version restriction: JDemetra+ and RJDemetra are not compatible with Java 16 and higher. The compatibility with those versions of Java will be possible from next release of JDemetra+.

### Added

- `java_ncore` option added to limit the number of cores used in Java to two to be sure to respect CRAN policies (to remove the option, use `options(java_ncore = NULL)`). However, it should not be necessary since RJDemetra shouldn't use multithread (issue #89).


## [0.1.7] - 2021-06-07

### Fixed

- fixed coefficients with user-defined calendar regressors can now correctly be used (issue #87).

- there was a bug in `add_sa_item` (more precisely in `complete_dictionary.SA`) when a userdefined variable was already in the workspace but with a different suffix.


## [0.1.6] - 2020-08-11

### Fixed

- `x11.seasonalma` argument wasn't taken into account (issue #78).

- `ipi_c_eu` updated: the previous data were calendar adjusted data, they are now unadjusted (neither seasonally adjusted nor calendar adjusted data).

- the print result of the combined test was incorrect.


## [0.1.5] - 2020-03-12

### Fixed

- Henderson filter was not correctly selected (issue #73).  
- Bug in the selection of TD with tramoseats (issue #71).

### Added

- Function `get_jspec`, to get the Java object that contains the specification, is now exported.


## [0.1.4] - 2020-01-10

### Fixed

- Error while loading a workspace with metadata (issue #53). 
- The degree of freedom were wrong with the `summary` functions.
- `jregarima` function is now exported.  
- The message was not complete when there was an error importing a model with `get_model`.
- The trading-days specification was not correctly specified with TRAMO-SEATS.

### Added

- Parameter `seats.predictionLength` added to `tramoseats_spec`.  
- Parameters `x11.calendarSigma` and `x11.sigmaVector` added to `x13_spec`.  
- Parameter `ylim` added to `plot` functions (issue #60).  
- New generic functions: `logLik`, `vcov`, `df.residual`, `nobs`, `residuals` (linked to issue #56).
- New pre-specified specification "X11" (no pre-processing; linked to issue #59).  
- Ramp effects are now imported.
- `add_sa_item` now compatible with `jSA` object.
- When a model is added to a workspace with `add_sa_item`, the external regressors are renamed only if they don't already exist in the workspace.
- Warning added when `tradingdays.option = "UserDefined"` and `tradingdays.autoadjust`, `tradingdays.leapyear` or `tradingdays.stocktdtradingdays` are defined in a new specification (because they are currently ignored). (issue #67)


## [0.1.3] - 2019-06-26

### Fixed

- When the multiprocessing is empty, `get_model` no longer produces an error.  
- Correction of the option `setOutliers` in TRAMO-SEATS.  
- jaxb.jar files added: `save_workspace` now works on Java 9 and higher (jaxb is no longer provided by default since Java 9).  
- Deprecated functions removed.

### Changed

- `jSA` documentation improved.

### Security

- Error added while loading the package if you don't have Java 8 or higher.


## [0.1.2] - 2019-04-18

### Changed

- All _def functions are now deprecated and replaced by the functions with the same name but without _def. Use: `x13_spec` instead of `x13_spec_def`, `x13` instead of `x13_def`, `tramoseats_spec` instead of `tramoseats_spec_def`, `tramoseats` instead of `tramoseats_def`, `regarima_spec_tramoseats` instead of `regarima_spec_def_tramoseats`, `regarima_tramoseats` instead of `regarima_def_tramoseats`, `regarima_x13` instead of `regarima_def_x13` and `regarima_spec_x13` instead of `regarima_spec_def_x13`.  
- `object` argument renamed by `spec` in `x13_spec`, `tramoseats_spec`, `regarima_spec_x13` and `regarima_spec_tramoseats`.

### Added

- Parameter `preliminary.check` added to the specifications functions (`regarima_spec_tramoseats`, `tramoseats_spec`, `regarima_spec_x13` and `x13_spec`). By default (`preliminary.check = TRUE`), JDemetra+ checks the quality of the input series and exclude highly problematic ones: e.g. these with a number of identical observations and/or missing values above pre-specified threshold values. When `preliminary.check = FALSE`, the thresholds are ignored and process is performed, when possible. (issue #39)  
- Error message returned when the seasonal adjustment fails due to the preliminary.check.  
- Possibility to use user-defined calendar regressors. To do it use `tradingdays.option = "UserDefined` and add new regressors variables (`usrdef.varEnabled = TRUE` to enable user-defined regressors and `usrdef.var` to define the regressors) using `usrdef.varType = "Calendar"`.  
- `usrdef.varType` argument is recycled with the number of variables defined in the `usrdef.var` parameter.  
- News functions to only get the Java object from a seasonal adjustment or a pre-adjustment method: `jx13`, `jtramoseats`, `jregarima`, `jregarima_x13`, `jregarima_tramoseats` and `get_jmodel`. Therefore, there is no formatting and the computation is faster than the non 'j' functions (`x13`, `tramoseats`, `regarima`, `regarima_x13`, `regarima_tramoseats` and `get_model`). To manipulate these objects, there are three functions: `get_dictionary` to get the indicators that can be extracted, `get_indicators` to extract these indicators and `jSA2R` to get the formatted R model.


### Fixed

-  `x11.fcast` can now be set to 0 or 1 (issue #42).

## [0.1.1] - 2019-03-22


[Unreleased]: https://github.com/rjdverse/rjdemetra/compare/v0.2.8...HEAD
[0.2.8]: https://github.com/rjdverse/rjdemetra/compare/v0.2.7...v0.2.8
[0.2.7]: https://github.com/rjdverse/rjdemetra/compare/v0.2.6...v0.2.7
[0.2.6]: https://github.com/rjdverse/rjdemetra/compare/v0.2.5...v0.2.6
[0.2.5]: https://github.com/rjdverse/rjdemetra/compare/v0.2.4...v0.2.5
[0.2.4]: https://github.com/rjdverse/rjdemetra/compare/v0.2.3...v0.2.4
[0.2.3]: https://github.com/rjdverse/rjdemetra/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/rjdverse/rjdemetra/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/rjdverse/rjdemetra/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/rjdverse/rjdemetra/compare/v0.1.9...v0.2.0
[0.1.9]: https://github.com/rjdverse/rjdemetra/compare/v0.1.7...v0.1.9
[0.1.8]: https://github.com/rjdverse/rjdemetra/blob/develop/NEWS.md#rjdemetra-018
[0.1.7]: https://github.com/rjdverse/rjdemetra/compare/v0.1.6...v0.1.7
[0.1.6]: https://github.com/rjdverse/rjdemetra/compare/v0.1.5...v0.1.6
[0.1.5]: https://github.com/rjdverse/rjdemetra/compare/v0.1.4...v0.1.5
[0.1.4]: https://github.com/rjdverse/rjdemetra/compare/v0.1.3...v0.1.4
[0.1.3]: https://github.com/rjdverse/rjdemetra/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/rjdverse/rjdemetra/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/rjdverse/rjdemetra/releases/tag/v0.1.1

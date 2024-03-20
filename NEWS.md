# RJDemetra 0.2.6

- possibility to export last msr for monthly data (issue #122).

- possibility to export X-11 some components: `y_cmp`, `y_cmp_f`, `t_cmp`, `t_cmp_f`, `sa_cmp`, `s_cmp`, `s_cmp_f`, and `i_cmp`.

- correction when importing models containing ramp regressors when the frequency is not 12 (monthly).

- correction of `get_jmodel()` with empty multiprocessings.


# RJDemetra 0.2.5

- `proc_data()` update to export more data from Java object.

- seasonality Kurskal-Wallis test corrected (issue #128).

- `vcov()` correction when matrix NULL and new parameter `component`.

- some corrections in `print()` methods.

# RJDemetra 0.2.4

- New function `get_all_names()` and `get_position()`.

- correction of `save_workspace()` and  `load_workspace()` when using relative path.

# RJDemetra 0.2.3

- Typo in printed arima coefficients : BPhi and Theta were inverted.


# RJDemetra 0.2.2

- update documentation.

- removes extra argument print tramo.

- SystemRequirements update for CRAN policies.


# RJDemetra 0.2.1

- Fix `complete_dictionary.SA()` to avoid warning.


# RJDemetra 0.2.0

- Bug with x11 introduced in RJDemetra 0.1.9 (issue #99).

- Java 17 compatibility.

- Change of javanamespace to avoid name clashes with jd+ 3.0.

- `easter.enabled` not working since RJDemetra 0.1.9.

- Default parameters of the span specification `d0` and `d1` as NA for clarity (issue #102).


# RJDemetra 0.1.9

- Easter specification was not working.

- Some typos in the documentation.

- SystemRequirement update: only Java JRE is needed.


# RJDemetra 0.1.8

- Java version restriction: JDemetra+ and RJDemetra are not compatible with Java 16 and higher. The compatibility with those versions of Java will be possible from next release of JDemetra+.

- `java_ncore` option added to limit the number of cores used in Java to two to be sure to respect CRAN policies (to remove the option, use `options(java_ncore = NULL)`). However, it should not be necessary since RJDemetra shouldn't use multithread (issue #89).


# RJDemetra 0.1.7

Data updated until December 2020.

## Bug fixed

- fixed coefficients with user-defined calendar regressors can now correctly be used (issue #87).

- there was a bug in `add_sa_item` (more precisely in `complete_dictionary.SA`) when a userdefined variable was already in the workspace but with a different suffix.


# RJDemetra 0.1.6

## Bug fixed

- `x11.seasonalma` argument wasn't taken into account (issue #78).

- `ipi_c_eu` updated: the previous data were calendar adjusted data, they are now unadjusted (neither seasonally adjusted nor calendar adjusted data).

- the print result of the combined test was incorrect.


# RJDemetra 0.1.5

## Bug fixed

- Henderson filter was not correctly selected (issue #73).  
- Bug in the selection of TD with tramoseats (issue #71).

## New functionalities

- Function `get_jspec`, to get the Java object that contains the specification, is now exported.


# RJDemetra 0.1.4

## Bug fixed

- Error while loading a workspace with metadata (issue #53). 
- The degree of freedom were wrong with the `summary` functions.
- `jregarima` function is now exported.  
- The message was not complete when there was an error importing a model with `get_model`.
- The trading-days specification was not correctly specified with TRAMO-SEATS.

## New functionalities

- Parameter `seats.predictionLength` added to `tramoseats_spec`.  
- Parameters `x11.calendarSigma` and `x11.sigmaVector` added to `x13_spec`.  
- Parameter `ylim` added to `plot` functions (issue #60).  
- New generic functions: `logLik`, `vcov`, `df.residual`, `nobs`, `residuals` (linked to issue #56).
- New pre-specified specification "X11" (no pre-processing; linked to issue #59).  
- Ramp effects are now imported.
- `add_sa_item` now compatible with `jSA` object.
- When a model is added to a workspace with `add_sa_item`, the external regressors are renamed only if they don't already exist in the workspace.
- Warning added when `tradingdays.option = "UserDefined"` and `tradingdays.autoadjust`, `tradingdays.leapyear` or `tradingdays.stocktdtradingdays` are defined in a new specification (because they are currently ignored). (issue #67)


# RJDemetra 0.1.3

## Bug fixed

- When the multiprocessing is empty, `get_model` no longer produces an error.  
- Correction of the option `setOutliers` in TRAMO-SEATS.  
- jaxb.jar files added: `save_workspace` now works on Java 9 and higher (jaxb is no longer provided by default since Java 9).  
- Deprecated functions removed.

## Documentation

- `jSA` documentation improved.

## Other

- Error added while loading the package if you don't have Java 8 or higher.


# RJDemetra 0.1.2
## Major changes

- All _def functions are now deprecated and replaced by the functions with the same name but without _def. Use: `x13_spec` instead of `x13_spec_def`, `x13` instead of `x13_def`, `tramoseats_spec` instead of `tramoseats_spec_def`, `tramoseats` instead of `tramoseats_def`, `regarima_spec_tramoseats` instead of `regarima_spec_def_tramoseats`, `regarima_tramoseats` instead of `regarima_def_tramoseats`, `regarima_x13` instead of `regarima_def_x13` and `regarima_spec_x13` instead of `regarima_spec_def_x13`.  
- `object` argument renamed by `spec` in `x13_spec`, `tramoseats_spec`, `regarima_spec_x13` and `regarima_spec_tramoseats`.

## New functionalities

- Parameter `preliminary.check` added to the specifications functions (`regarima_spec_tramoseats`, `tramoseats_spec`, `regarima_spec_x13` and `x13_spec`). By default (`preliminary.check = TRUE`), JDemetra+ checks the quality of the input series and exclude highly problematic ones: e.g. these with a number of identical observations and/or missing values above pre-specified threshold values. When `preliminary.check = FALSE`, the thresholds are ignored and process is performed, when possible. (issue #39)  
- Error message returned when the seasonal adjustment fails due to the preliminary.check.  
- Possibility to use user-defined calendar regressors. To do it use `tradingdays.option = "UserDefined` and add new regressors variables (`usrdef.varEnabled = TRUE` to enable user-defined regressors and `usrdef.var` to define the regressors) using `usrdef.varType = "Calendar"`.  
- `usrdef.varType` argument is recycled with the number of variables defined in the `usrdef.var` parameter.  
- News functions to only get the Java object from a seasonal adjustment or a pre-adjustment method: `jx13`, `jtramoseats`, `jregarima`, `jregarima_x13`, `jregarima_tramoseats` and `get_jmodel`. Therefore, there is no formatting and the computation is faster than the non 'j' functions (`x13`, `tramoseats`, `regarima`, `regarima_x13`, `regarima_tramoseats` and `get_model`). To manipulate these objects, there are three functions: `get_dictionary` to get the indicators that can be extracted, `get_indicators` to extract these indicators and `jSA2R` to get the formatted R model.


## Bug fixed

-  `x11.fcast` can now be set to 0 or 1 (issue #42).

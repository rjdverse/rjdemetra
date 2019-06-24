# RJDemetra 0.1.3

## Bug fixed

-  When the multiprocessing is empty, `get_model` no longer produces an error.  
- Correction of the option setOutliers in TRAMO-SEATS.  
- jaxb.jar files added: `save_workspace` now works on Java 9 and higher (jaxb is no longer provided by default since Java 9).  
- deprecated functions removed.

## Documentation

- `jSA` documentation improved.  
- Installation manual


# RJDemetra 0.1.2
## Major changes

- All _def functions are now deprecated and replaced by the functions with the same name but without _def. Use: `x13_spec` instead of `x13_spec_def`, `x13` instead of `x13_def`, `tramoseats_spec` instead of `tramoseats_spec_def`, `tramoseats` instead of `tramoseats_def`, `regarima_spec_tramoseats` instead of `regarima_spec_def_tramoseats`, `regarima_tramoseats` instead of `regarima_def_tramoseats`, `regarima_x13` instead of `regarima_def_x13` and `regarima_spec_x13` instead of `regarima_spec_def_x13`.  
- `object` argument renamed by `spec` in `x13_spec`, `tramoseats_spec`, `regarima_spec_x13` and `regarima_spec_tramoseats`.

## New functionalities

- Parameter `preliminary.check` added to the specifications functions (`regarima_spec_tramoseats`, `tramoseats_spec`, `regarima_spec_x13` and `x13_spec`). By default (`preliminary.check = TRUE`), JDemetra+ checks the quality of the input series and exclude highly problematic ones: e.g. these with a number of identical observations and/or missing values above pre-specified threshold values. When `preliminary.check = FALSE`, the thresholds are ignored and process is performed, when possible. (issue #39)  
- Error message returned when the seasonal adjustment fails due to the preliminary.check.  
- Possibility to use user-defined calendar regressors. To do it use `tradingdays.option = "UserDefined` and add new regressors variables (`usrdef.varEnabled = TRUE` to enable user-defined regressors and `usrdef.var` to define the regressors) using `usrdef.varType = "Calendar"`.  
- `usrdef.varType` argument is recycled with the number of variables defined in the `usrdef.var` parameter.  
- News functions to only get the Java object from a seasonal adjustment or a pre-ajustment method: `jx13`, `jtramoseats`, `jregarima`, `jregarima_x13`, `jregarima_tramoseats` and `get_jmodel`. Therefore, there is no formatting and the computation is faster than the non 'j' functions (`x13`, `tramoseats`, `regarima`, `regarima_x13`, `regarima_tramoseats` and `get_model`). To manipulate these objects, there are three functions: `get_dictionary` to get the indicators that can be extracted, `get_indicators` to extract these indicators and `jSA2R` to get the formatted R model.



## Bug fixed

-  `x11.fcast` can now be set to 0 or 1 (issue #42).

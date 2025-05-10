# A script to approximate wet bulb globe temperature.
# Available for current climates and as daily averages:
# WBGT, WBT (?), T, based on the differences between temper

fun_compute_wbt <- function(x, var) {
  x[var == "hurs"] * 0.075 + 0.75 * x[var == variant_var_name]
}



wbgt_function <- function(x, var) {
  0.7*x[var == "tw"] + 0.2*x[var == "ts"] + 0.1*x[var == variant_var_name]
}
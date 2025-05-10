# Functions for scenario calculation for future GVI
fct_slope_a <- function(slope){
  slopefancy = .5 - 1 / (1 + exp(slope*10))
  return(slopefancy)
}
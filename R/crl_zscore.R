

crl_zscore = function(ga_days, crl){
  constant1 = -50.6562
  constant2 = 0.815118
  constant3 = 0.00535302
  constant4 = -2.21626
  constant5 = 0.0984894

  estimated_mean = constant1 + (constant2*ga_days) + (constant3*(ga_days^2))
  estimated_sd = constant4 + (constant5*ga_days)

  crlz = round((crl - estimated_mean)/estimated_sd, 1)

  message("Z-score calculation based on the Excel calculator developed by The INTERGROWTH-21ˢᵗ Consortium.
          https://intergrowth21.tghn.org/intergrowth-21st-applications/")

  return(crlz)
}

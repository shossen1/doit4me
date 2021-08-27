

crl_zscore = function(ga_days, crl){
  constant1 = -50.6562
  constant2 = 0.815118
  constant3 = 0.00535302
  constant4 = -2.21626
  constant5 = 0.0984894

  crlz = round((crl - ((constant1 +constant2*ga)+(constant3*ga^2)))/(constant4+ (constant5*ga)), 1)

  message("Crown-Ramp Length (CRL) Z-score based on the Excel calculator developed by The INTERGROWTH-21ˢᵗ Consortium.
          https://intergrowth21.tghn.org/intergrowth-21st-applications/")

  return(crlz)
}

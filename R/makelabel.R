

makelabel = function(var1, var2, var3, multiply, rounding, divider){
  paste0(sprintf(paste0("%3.", rounding, "f"),var1*multiply),
         " (", sprintf("%3.1f",var2*100), divider,
         sprintf("%3.1f",var3*100), ")")
}

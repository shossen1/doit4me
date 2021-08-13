

cat_or_cont = function(datain, var){
  datain$var <- datain[[deparse(substitute(var))]]
  xx = length(unique(datain$var))/length(datain$var)
  if(xx <= 0.05){return("Categorical")} else {return("Continuous")}
}

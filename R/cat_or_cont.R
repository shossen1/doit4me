


cat_or_cont = function(var){
  xx = length(unique(var))/length(na.omit(var))
  if(xx <= 0.05 | length(unique(var))>= 10){return("Categorical")} else {return("Continuous")}
}

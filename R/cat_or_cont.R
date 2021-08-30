

cat_or_cont = function(var){
  xx = length(unique(var))/length(var)
  if(xx <= 0.05){return("Categorical")} else {return("Continuous")}
}

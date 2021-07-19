

missmatch = function(a,b){
  print("Present in dataset A but absent in dataset B")
 a[!(a %in% b)]

  print("Present in dataset B but absent in dataset A")
  b[!(b %in% a)]
}

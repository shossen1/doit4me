
# This function compares two datasets and return the mismatched variables

missmatch_var = function(a,b){
  print("Present in dataset A but absent in dataset B")
  names(a)[!(names(a) %in% names(b))]

  print("Present in dataset B but absent in dataset A")
  names(b)[!(names(b) %in% names(a))]
}

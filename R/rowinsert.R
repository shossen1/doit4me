
rowinsert <- function(locations = c(1, 4, 6, 8, 11),
                      rownm = c("Municipio", "User type", "Main stove", "Secondary stove", "Fuel used"),
                      tab = tab1,
                      rowdata = NULL){
  for(l in locations){
    if(l == 1){
      if(is.null(rowdata) == "TRUE"){
        a <- rep("", ncol(tab))
        tab <- rbind(a, tab)
        rownames(tab)[1] <- rownm[1]
      }else{
        tab <- rbind(unlist(rowdata[which(l == locations)]), tab)
        rownames(tab)[1] <- rownm[1]
      }
    }else{
      if(is.null(rowdata) == "TRUE"){
        b <- rep("", ncol(tab))
        dd <- which(l == locations)
        tab <- rbind(tab[c(1:(l-2+dd)),],
                     b,
                     tab[c(l-1+dd):nrow(tab),])
        rownames(tab)[l-1+dd] <- rownm[which(l == locations)]
      }else{
        b <- unlist(rowdata[which(l == locations)])
        dd <- which(l == locations)
        tab <- rbind(tab[c(1:(l-2+dd)),],
                     b,
                     tab[c(l-1+dd):nrow(tab),])
        rownames(tab)[l-1+dd] <- rownm[which(l == locations)]
      }
    }
  }
  return(tab)
}


# tab1 <-
#   rowinsert(locations = c(1, 3, 5, 8, 12, 15, 23, 34, 39, 45, 51),
#             rownm = c("User type", "Main stove", "Secondary stove", "Fuel used", "Used gas before",
#                       "Weekly wood collection", "Appliances", "Gas connection", "Number of burner",
#                       "Buy tortilla", "Working family member"),
#             tab = tab1,
#             rowdata = NULL)


table1 = function(datain, varlist, group){

 if("janitor" %in% rownames(installed.packages())){
   library(janitor)
 }else{message("Package janitor is required. Please install it.")}

  if("broom" %in% rownames(installed.packages())){
    library(broom)
  }else{message("Package broom is required. Please install it.")}

if(any(grepl("package:plyr", search()))) detach("package:plyr")

  tab1 = NULL
  for(i in varlist){
    datain$var = datain[[i]]
    datain$group = datain[[group]]

    if(cat_or_cont(datain$var) == "Continuous"){
      xx = data.frame(datain %>%
                        dplyr::group_by(group) %>%
                        summarise(mean = mean(as.numeric(var), na.rm = TRUE),
                                  sd = sd(as.numeric(var), na.rm = TRUE)) %>%
                        mutate(meansd = paste0(sprintf("%3.1f", mean), " (", sprintf("%3.1f", sd), ")")) %>%
                        dplyr::select(meansd) %>%
                        t())
      rownames(xx) = paste0(i, " mean(SD)")
      colnames(xx) = names(table(datain$group))

      xx$Overall = paste0(sprintf("%3.1f", mean(datain$var, na.rm = TRUE)), " (",
                          sprintf("%3.1f", sd(datain$var, na.rm = TRUE)), ")")

      xx$p = substr(format(tidy(aov(var ~ group, data = datain))$p.value[1], scientific = F), 1, 6)
      #print(names(xx))
    }

    if(cat_or_cont(datain$var) == "Categorical"){
      xx = data.frame(datain %>%
                        tabyl(var, group) %>%
                        adorn_totals("col") %>%
                        adorn_percentages("col") %>%
                        adorn_pct_formatting(digits = 1) %>%
                        adorn_ns(position = "front") %>%
                        rename(Overall = Total))

      p = substr(format(chisq.test(datain$var, datain$group)$p.value , scientific = F), 1, 6)
      xx$var = paste0(i , " (", xx$var, ")")

      rownames(xx) = xx$var
      xx$p = p
      xx$var = NULL
      #print(names(xx))
    }
    names(xx) = c(names(table(datain$group)), "Overall","p")
    tab1 = rbind(tab1, xx)
  }
  return(tab1)
}

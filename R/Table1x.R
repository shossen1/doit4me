table1x = function(datain, varlist, group = NULL){

  library(janitor)
  library(broom)

  if(is.null(group) == "TRUE"){

    tab1 = NULL
    for(i in varlist){

      datain$var = datain[[gsub("[()]", "", gsub('factor', '', i,useBytes = TRUE), useBytes = TRUE)]]

      if(grepl("factor", i) == "FALSE"){
        xx = data.frame(datain %>%
                          dplyr::summarise(mean = mean(as.numeric(var), na.rm = TRUE),
                                           sd = sd(as.numeric(var), na.rm = TRUE)) %>%
                          dplyr::mutate(meansd = paste0(sprintf("%3.1f", round(mean, 1)), " (",
                                                        sprintf("%3.1f", round(sd, 1)), ")")) %>%
                          dplyr::select(meansd) %>%
                          t())
        rownames(xx) = paste0(i, " mean(SD)")
        colnames(xx) = ""
      }

      if(grepl("factor", i) == "TRUE"){
        xx = data.frame(datain %>%
                          tabyl(var) %>%
                          adorn_pct_formatting(digits = 1) %>%
                          dplyr::mutate(n = paste0(n, " (", percent, ")")) %>%
                          dplyr::select(var, n))

        xx$var = paste0(i , " (", xx$var, ")")

        rownames(xx) = xx$var
        xx$var = NULL
      }
      names(xx) = ""
      tab1 = rbind(tab1, xx)
    }

  }else{

    tab1 = NULL
    for(i in varlist){
      datain$var = datain[[gsub("[()]", "", gsub('factor', '', i,useBytes = TRUE), useBytes = TRUE)]]
      datain$group = datain[[gsub("[()]", "", gsub('factor', '', group, useBytes = TRUE), useBytes = TRUE)]]

      if(grepl("factor", i) == "FALSE"){
        xx = data.frame(datain %>%
                          dplyr::group_by(group) %>%
                          dplyr::summarise(mean = mean(as.numeric(var), na.rm = TRUE),
                                           sd = sd(as.numeric(var), na.rm = TRUE)) %>%
                          dplyr::mutate(meansd = paste0(sprintf("%3.1f", round(mean, 1)), " (",
                                                        sprintf("%3.1f", round(sd, 1)), ")")) %>%
                          dplyr::select(meansd) %>%
                          t())
        rownames(xx) = paste0(i, " mean(SD)")
        colnames(xx) = names(table(datain$group))

        xx$Overall = paste0(sprintf("%3.1f", round(mean(datain$var, na.rm = TRUE), 1)), " (",
                            sprintf("%3.1f", round(sd(datain$var, na.rm = TRUE), 1)), ")")

        xx$p = substr(format(tidy(aov(var ~ group, data = datain))$p.value[1], scientific = F), 1, 6)
        #print(names(xx))
      }

      if(grepl("factor", i) == "TRUE"){
        xx = data.frame(datain %>%
                          tabyl(var, group) %>%
                          adorn_totals("col") %>%
                          adorn_percentages("col") %>%
                          adorn_pct_formatting(digits = 1) %>%
                          adorn_ns(position = "front") %>%
                          dplyr::rename(Overall = Total))


        chisqtry <- try(chisq.test(datain$var, datain$group))

        if(class(chisqtry)[1] == "try-error"){
          p = substr(format(fisher.test(table(datain$var, datain$group))$p.value , scientific = F), 1, 6)
          message(paste0("Using Fisher's exact test for", var))
        }
        if(class(chisqtry)[1] != "try-error"){
          p = substr(format(chisq.test(datain$var, datain$group)$p.value , scientific = F), 1, 6)
        }

        xx$var = paste0(i , " (", xx$var, ")")

        rownames(xx) = xx$var
        xx$p = p
        xx$var = NULL
        #print(names(xx))
      }
      names(xx) = c(names(table(datain$group)), "Overall","p")
      tab1 = rbind(tab1, xx)
    }
  }
  return(tab1)
}

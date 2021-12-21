# forestplot.coxph = function(data = df,
#                       adjustment.status = unadjusted,
#                       outcome = "y",
#                       varlist = c("factor(group2)" ,"age_admit_iqr", "factor(ma0fe1)", "bmi_iqr", "factor(race2)", "factor(ethnicity2)",
#                                   "sofa_calc", "charlson_mv", "pafi_mv0_iqr"),
#                       y.lab = y.lab,
#                       round = 2,
#                       sectionhead = NA,
#                       title = "title",
#                       errorbar.width = 0,
#                       errorbar.size = 1,
#                       x.min = -50,
#                       x.max = 150,
#                       x.break = 10,
#                       xcoord.min = -50,
#                       xcoord.max = 140,
#                       x.title = "",
#                       y.title = "",
#                       point.size = 6,
#                       min.point.size = 5,
#                       max.point.size = 10,
#                       relative.size = TRUE,
#                       lab.size = 16,
#                       lab.hjust = 1.1,
#                       corner.title = TRUE,
#                       vline.x = 0,
#                       vline.type = "dashed",
#                       vline.size = 1,
#                       slash.gap = 2,
#                       n.lab = TRUE,
#                       shade.var = c(1, 5, 7)){
#   frm <- as.formula(frm)
#   var = word(as.character(frm[3]), 1)
#   
#   if(deparse(substitute(adjustment.status)) == "unadjusted"){
# 
#   tab = NULL
#   for(i in varlist){
#     formula = as.formula(paste0(y, "~", i))
#     xx = summary(coxph(formula, data = datain1))$conf.int
#     message(i)
#     # print(summary(coxph(formula, data = datain1)))
#     
#     aa = paste0(sprintf("%3.2f", xx[1]), " (", sprintf("%3.2f",xx[3]), "-", sprintf("%3.2f",xx[4]), ")")
#     tabx = rbind(tabx, c(i, aa))
#   }
#   tab[, c(1,2)] = tabx
#   }
#   
# #   # adjusted
# #   x = "factor(group2) + age_admit_iqr + factor(ma0fe1) + bmi_iqr + factor(race2) + factor(ethnicity2) + sofa_calc + charlson_mv + pafi_mv0_iqr"
# #   formula2 = as.formula(paste0(y, "~", x))
# #   yy = summary(coxph(formula2, data = datain1))$conf.int
# #   tab[, 3] = paste0(sprintf("%3.2f", yy[, 1]), " (", sprintf("%3.2f", yy[, 3]), "-", sprintf("%3.2f", yy[, 4]), ")")
# #   
# #   # Propensity matched
# #   formula3 = as.formula(paste0(y , "~", "group2"))
# #   zz = summary(coxph(formula3, data = datain2, robust = TRUE, weights = weights, cluster = subclass))$conf.int
# #   tab[1,4] = paste0(sprintf("%3.2f", zz[1]), " (", sprintf("%3.2f",zz[3]), "-", sprintf("%3.2f",zz[4]), ")")
# #   
# #   print(summary(coxph(formula3, data = datain2, robust = TRUE, weights = weights, cluster = subclass)))
# #   
# #   tab[c(2:9), 4] = ""
# #   return(tab)
# # }  
# 
#   tab2 = NULL
#   for(i in c(1:length(y.lab))){
#     if(y.lab[i] %in% sectionhead){
#       tab2 = rbind(tab2, rep(NA, ncol(tab)))
#       colnames(tab2) = colnames(tab)
#     }
#     if(!y.lab[i] %in% sectionhead){
#       tab2 = rbind(tab2, tab[i - sum(is.na(tab2[[4]])),])
#     }
#   }
#   tab2$y = y.lab
#   tab2$serial = nrow(tab2):1
#   tab2$serial = factor(tab2$serial, levels = tab2$serial[order(tab2$serial)])
#   tab2$face = ifelse(tab2$y %in% sectionhead, "bold", "plain")
#   tab2$lab = replace(tab2$lab, is.na(tab2$lab), "")
#   
#   if(n.lab == "TRUE" | n.lab == "T"){
#     zz = paste0(tab2$y, " (n = ", tab2[[4]], ")")
#     zz = gsub("\\(n = NA)", "", zz)
#     tab2$y <- zz
#   }
#   
#   p =
#     ggplot(data = tab2)
#   
#   if(relative.size == "TRUE" | relative.size == "T"){
#     p = p + geom_point(aes(x = tab2[[1]], y = tab2[[7]], size = tab2[[4]]),  shape=23, fill="black")
#   }
#   
#   if(relative.size != "TRUE" & relative.size != "T"){
#     p = p + geom_point(aes(x = tab2[[1]], y = tab2[[7]]), shape=23, fill="black", size = point.size)
#   }
#   
#   p = p +
#     geom_errorbar(aes(xmin = tab2[[2]], xmax = tab2[[3]], y = tab2[[7]]), width = errorbar.width, size = errorbar.size) +
#     scale_x_continuous(x.title, breaks = seq(x.min, x.max, x.break), expand = c(0,0)) +
#     scale_y_discrete(y.title, label = rev(tab2[[6]])) +
#     coord_cartesian(xlim=c(xcoord.min, xcoord.max)) +
#     scale_size(range = c(min.point.size, max.point.size)) +
#     geom_vline(xintercept = vline.x, linetype = vline.type, size = vline.size) +
#     theme_bw()+
#     ggtitle(title)+
#     annotate("text", x = Inf, y = tab2[[7]], label =tab2[[5]], hjust = lab.hjust, size = lab.size)
#   
#   for(i in shade.var){
#     p = p +
#       annotate("rect", xmin = -Inf, xmax = Inf, ymin = (nrow(tab2)-i+ 0.5), ymax = (nrow(tab2)-i+ 1.4), alpha = 0.2)
#   }
#   p = p +
#     theme(axis.ticks = element_blank(),
#           panel.grid = element_blank(),
#           plot.title = element_text(hjust=0.5,size=27),
#           axis.text = element_text(size=17),
#           axis.title = element_text(size=22),
#           axis.text.y = element_text(face = rev(tab2$face)),
#           legend.position = "none")
#   if(corner.title == "T" | corner.title == "TRUE"){
#     p = p +
#       theme(plot.title = element_text(hjust = 0.04, vjust = -8, size=27))
#   }
#   
#   
#   for(i in tab2$serial){
#     tab2$xstart[tab2$serial == i] = ifelse(tab2[[2]][tab2$serial == i] < x.min, x.min+4, 0)
#     tab2$xend[tab2$serial == i] = ifelse(tab2[[2]][tab2$serial == i] < x.min, x.min+1, 0)
#     tab2$ystart[tab2$serial == i] = ifelse(tab2[[2]][tab2$serial == i] < x.min, as.numeric(i)+0.25, 0)
#     tab2$yend[tab2$serial == i] = ifelse(tab2[[2]][tab2$serial == i] < x.min, as.numeric(i)-0.25, 0)
#   }
#   
#   p = p +
#     geom_segment(aes(x = tab2$xstart, xend = tab2$xend , y = tab2$ystart, yend = tab2$yend),color="steelblue") +
#     geom_segment(aes(x = tab2$xstart + slash.gap, xend = tab2$xend + slash.gap , y = tab2$ystart, yend = tab2$yend),color="steelblue")
#   
#   return(list(tab2, p))
# }

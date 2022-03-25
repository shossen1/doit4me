
fun.ecdf = function(datain, group, x, x.lab, x.limits, x.breaks, y, fct = NULL){
  if(group == "sitee"){lab = c("Lima", "Tumbes", "Urban Puno", "Rural Puno")} else {lab = c("Alive", "Dead")}
  
  p1 = 
    ggplot(data = datain, aes(x = datain[[x]])) +
    stat_ecdf(aes(color = factor(datain[[group]])), geom = "step") +
    scale_colour_manual("", values = c("cornflowerblue", "tomato", "orange", "grey"),
                        labels = lab)+
    scale_x_continuous(x.lab, limits = x.limits, breaks = x.breaks)+
    scale_y_continuous("Cumulative fraction \n")
  
  p1 = p1 +
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.title = element_blank(),
          strip.text = element_blank(),
          legend.position  = "bottom")+
    guides(colour = guide_legend(override.aes = list(size=5)))
  
  p2 = 
    ggplot(data=datain,
           aes(x = reorder(datain[[group]], -datain[[y]], FUN = median), 
               y = datain[[y]]))+
    geom_boxplot(aes(colour=factor(datain[[group]])),size=1)+
    scale_colour_manual("", values = c("cornflowerblue", "tomato", "orange", "grey"))+
    coord_flip()+
    theme_bw() 
  
  p2 = p2 +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank())+
    scale_x_discrete("")+
    scale_y_continuous("", limits = x.limits)
  
  if(!is.null(fct) == "TRUE"){
    datain$fct = datain[[fct]]
    p1 = p1 + facet_wrap(fct, ncol = 4)
    p2 = p2 + facet_wrap(fct, ncol = 4)
  }
  
  if(is.null(fct) == "TRUE"){
    p1 = p1 + theme(strip.text = element_blank())
    p2 = p2 + theme(strip.text = element_blank())
  }
  
  g1 = ggplotGrob(p1)
  g2 = ggplotGrob(p2)
  g2$widths <- g1$widths
  p = grid.arrange(g2, g1, nrow = 2, heights=c(1,5))
}
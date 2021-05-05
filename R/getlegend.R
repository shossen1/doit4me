

getlegend = function(plot, position, textsize, cm.height){
library(ggplot2)

g <- ggplotGrob(plot + theme(legend.position = position,
                           legend.text = element_text(size = textsize)))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

legend$heights[2] = unit(c(cm.height), "cm")
}

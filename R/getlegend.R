

getlegend = function(plot, position = "bottom", textsize = 18, cm.height = 1.5){
library(ggplot2)

g <- ggplotGrob(plot + theme(legend.position = position,
                           legend.text = element_text(size = textsize)))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

legend$heights[2] = unit(c(cm.height), "cm")
}

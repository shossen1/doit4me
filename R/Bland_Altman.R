# ##############################
# ###         BA plot       ####
# ##############################
#
#
# ba_rr <- bland.altman.stats(rad$radgrr1, rad$radgrr2)
# ba_mm <- bland.altman.stats(manual$manualrr1, manual$manualrr2)
# ba_means<- bland.altman.stats(bothrr$meanmanual, bothrr$meanrr)
#
#
# BA_SKR<-function(v1, v2,title='t',bias='b',test='bb',test2='nn', xname = 'v',yname = 'z',...){
#   mean <- (v1+v2) / 2
#   diff <- v1-v2
#   xnamestr=xname
#   ynamestr=yname
#   titlestr=title
#   staticx=mean/10
#   biasstr=bias
#   teststr=test
#
#   ggplot(df, aes(x=mean,
#                  y=diff)) +
#     geom_point(shape=3,colour="grey") +
#     geom_hline(yintercept = mean(diff), colour = "black", size = 0.8,linetype="dashed") +
#     geom_hline(yintercept = mean(diff) - (1.96 * sd(diff)), colour = "black", size = 0.8,linetype="dashed") +
#     geom_hline(yintercept = mean(diff) + (1.96 * sd(diff)), colour = "black", size = 0.8,linetype="dashed") +
#     geom_hline(yintercept =0, colour="black",size=0.8, linetype="solid")+
#     xlab(xnamestr) +
#     ylab(ynamestr)+
#     theme_bw()+
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           #panel.background = element_rect(fill = "transparent"),
#           #plot.background = element_rect(fill = "transparent", color = NA),
#           axis.text = element_text(size=15),
#           axis.title = element_text(size=18))+
#     ylim(-20,60)+
#     xlim(20,80)+
#     geom_text(x=22, y=57, label=titlestr,size=10)+
#     geom_text(x=30,y=-18,label=biasstr,size=6)+
#     coord_fixed(ratio=.5)
# }
#
#
# # ba1<-
# #   BA_SKR(rad,
# #          rad$radgrr1,
# #          rad$radgrr2,
# #          title='A',
# #          bias='Bias = -0.58',
# #          xname='Mean of 1ˢᵗ and 2ⁿᵈ Rad-97 respiratory rates (breaths/min)',
# #          yname='Difference of 1ˢᵗ and 2ⁿᵈ Rad-97 \n respiratory rates (breaths/min)')
# # ba1<-ba1+ theme(plot.margin=unit(c(1,1,-2,1), "cm"))

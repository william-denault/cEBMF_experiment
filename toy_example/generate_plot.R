load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/Fig_1_data/fit_plot_Neurips.RData")
x= file_pc$x
y= file_pc$y

L= file_pc$L
res= file_pc$fit_custom
fit_default=file_pc$fit_default
LIBD=file_pc$LIBD



df <- data.frame(x=x,y=y, L=  L[,1])
P01 <- ggplot(df, aes ( x,y, col = L ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab("Ground Truth ")+xlab(" ")+ ggtitle("factor 1")+
  theme_minimal()+theme( axis.text.y=element_blank() ,

                         axis.ticks.y=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank(),
                         plot.title = element_text(size = 20), # Set plot title size
                         axis.title.x = element_text(size = 20), # Set X axis title size
                         axis.title.y = element_text(size = 20))
df <- data.frame(x=x,y=y, L=  L[,2])
P02 <- ggplot(df, aes ( x,y, col = L ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab(" ")+xlab(" ")+ ggtitle("factor 2")+
  theme_minimal()+theme( axis.text.y=element_blank(),

                         axis.ticks.y=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank(),
                         plot.title = element_text(size = 20), # Set plot title size
                         axis.title.x = element_text(size = 20), # Set X axis title size
                         axis.title.y = element_text(size = 20))

df <- data.frame(x=x,y=y, L=  L[,3])
P03 <- ggplot(df, aes ( x,y, col = L ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab(" ")+xlab(" ")+ ggtitle("factor 3")+
  theme_minimal()+theme( axis.text.y=element_blank(),

                         axis.ticks.y=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank(),
                         plot.title = element_text(size = 20), # Set plot title size
                         axis.title.x = element_text(size = 20), # Set X axis title size
                         axis.title.y = element_text(size = 20))


df <- data.frame(x=x,y=y, L= res$loading[,1])
P11 <- ggplot(df, aes ( x,y, col = L ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab(" cEBMF")+xlab(" ")+
  ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank() ,

                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank(),
                                     plot.title = element_text(size = 20), # Set plot title size
                                     axis.title.x = element_text(size = 20), # Set X axis title size
                                     axis.title.y = element_text(size = 20))


df <- data.frame(x=x,y=y, L= fit_default$L_pm[,1])
P12 <- ggplot(df, aes ( x,y, col = L ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab(" EBMF")+xlab(" ")+
  ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),

                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank(),
                                     plot.title = element_text(size = 20), # Set plot title size
                                     axis.title.x = element_text(size = 20), # Set X axis title size
                                     axis.title.y = element_text(size = 20))


df <- data.frame(x=x,y=y, L= LIBD@SpatialPCs[1,] )
P13 <- ggplot(df, aes ( x,y, col = L ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab("Spatial PCA")+xlab(" ")+
  ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),

                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank(),
                                     plot.title = element_text(size = 20), # Set plot title size
                                     axis.title.x = element_text(size = 20), # Set X axis title size
                                     axis.title.y = element_text(size = 20))


df <- data.frame(x=x,y=y, L= res$loading[,2])

P21 <-ggplot(df, aes ( x,y, col =  L  ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab("  ")+xlab(" ")+
  ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),

                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank())



df <- data.frame(x=x,y=y, L= fit_default$L_pm[,2])
P22 <-ggplot(df, aes ( x,y, col = L ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ggtitle(" ")+theme_minimal()+
  xlab(" ")+ylab(" ")+
  theme( axis.text.y=element_blank(),

         axis.ticks.y=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank())



df <- data.frame(x=x,y=y, L=LIBD@SpatialPCs[2,] )
P23 <- ggplot(df, aes ( x,y, col = L ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab("  ")+xlab(" ")+
  ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),

                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank())


df <- data.frame(x=x,y=y, L= res$loading[,3])
P31 <-ggplot(df, aes ( x,y, col = abs( L)  ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab("  ")+xlab(" ")+
  ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank())


df <- data.frame(x=x,y=y, L= 0*fit_default$L_pm[,2])
P32 <-ggplot(df, aes ( x,y, col = L ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  xlab(" ")+ylab(" ")+
  ggtitle(" ")+theme_minimal()+theme( axis.text.y=element_blank(),

                                      axis.ticks.y=element_blank(),
                                      axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank())


df <- data.frame(x=x,y=y, L=LIBD@SpatialPCs[3,] )
P33 <- ggplot(df, aes ( x,y, col =  L  ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0  ) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab("  ")+xlab(" ")+
  ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank() ,

                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank(),
                                     plot.title = element_text(size = 20), # Set plot title size
                                     axis.title.x = element_text(size = 20), # Set X axis title size
                                     axis.title.y = element_text(size = 20))


library(cowplot)
library(grid)
#legend <- get_legend(
#  P13+
#    guides(color = guide_legend(nrow = 1)) #+
# theme(legend.position = "bottom")
# )
# legend1 <- get_legend(
#  P1
#)



fit_factor = ggdraw() +
  draw_plot(P01 + theme(legend.position = "none" ), x = 0   , y = 0.75, width= 0.25, height= 0.25) +
  draw_plot(P02 + theme(legend.position = "none"), x = 0.3 , y = 0.75, width= 0.25, height= 0.25) +
  draw_plot(P03 + theme(legend.position = "none"), x = 0.6 , y = 0.75, width= 0.25, height= 0.25) +
  draw_plot(P11 + theme(legend.position = "none"), x = 0   , y = 0.5, width= 0.25, height= 0.25) +
  draw_plot(P21 + theme(legend.position = "none"), x = 0.3 , y = 0.5, width= 0.25, height= 0.25) +
  draw_plot(P31 + theme(legend.position = "none"), x = 0.6 , y = 0.5, width= 0.25, height= 0.25) +

  draw_plot(P12 + theme(legend.position = "none"), x = 0   , y = 0.25, width= 0.25, height= 0.25) +
  draw_plot(P22 + theme(legend.position = "none"), x = 0.3 , y = 0.25, width= 0.25, height= 0.25) +
  draw_plot(P32 + theme(legend.position = "none"), x = 0.6 , y = 0.25, width= 0.25, height= 0.25) +

  draw_plot(P13 + theme(legend.position = "none"), x = 0   , y = 0.0 , width= 0.25, height= 0.25) +
  draw_plot(P23 + theme(legend.position = "none"), x = 0.3 , y = 0.0 , width= 0.25, height =0.25) +
  draw_plot(P33 + theme(legend.position = "none"), x = 0.6 , y = 0.0 , width= 0.25, height= 0.25)
fit_factor



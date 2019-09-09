library(HistData)

data <- Prostitutes
# setup plot layout
layout(matrix(c(1, 1, 1, 1, 1, 2), ncol=6, byrow=T))
###################################################################################
#                           Hexagon Heatmap                                       #
###################################################################################

# function to draw hexagons
draw_hexmap <- function(df_row, color, unit=1){
  # dislocation on the columns to fit hexagons
  # when mon is even number, dislocate the x coordination by unit/2 to right
  if (df_row$mon %% 2 == 0){ 
    loc <- df_row$Year + unit/2
  } else {
    loc <- df_row$Year
  }
  # x coordinations, left(0), middle(1), right(2)
  x_0 <- loc - unit/2; x_1 <- loc; x_2 <- loc + unit/2
  # y coordinations, lowest(0), lower(1), higher(2), highest(3)
  y_0 <- df_row$mon - unit*0.625; y_1 <- df_row$mon - unit*0.375
  y_2 <- df_row$mon + unit*0.375; y_3 <- df_row$mon + unit*0.625
  polygon(c(x_0, x_0, x_1, x_2, x_2, x_1), c(y_1, y_2, y_3, y_2, y_1, y_0), col=color, border=T)
}

# setup color palette for heatmap
colPal <- colorRampPalette(c('gold1', 'firebrick3'))
colMap <- colPal(255)[as.numeric(cut(data$count, breaks=255))]

###### plot heatmap ######
par(mar=c(2.5, 4, 2, 0))
plot(1820, 5, xlim=c(data$Year[1], data$Year[length(data$Year)]), ylim=c(0.5, 12.5), 
     type='n', axes=F, ann=F)
for (idx in 1:nrow(data)){
  draw_hexmap(data[idx, ], color=colMap[idx])
}
# add axis-ticks and axis-labels
axis(side=1, labels=T, at=c(1812, seq(1815, 1852, 5), 1854), 
     lwd=0, lwd.ticks=2, col.axis='black', cex.axis=1, line=0)
axis(side=2, labels=F, at=1:12, tck=.03,
     lwd=0, lwd.ticks=2, col.axis='black' ,cex.axis=.8, las=1)
mtext(side=2, text=paste(month.abb, ' '), at=1:12, las=1, cex=.9)
# setup title and subtitles
title(main='Hexagon Heatmap', col='black', cex.main=2.5, adj=0.5)

###### plot heatmap legends ######
par(mar=c(1, 2, 2, 1))
colMap_legend <- as.raster(matrix(rev(colPal(255)), ncol=1))
plot(c(0,2),c(0,1),type='n', axes=F,xlab="", ylab="", main='Count Gradients')
rasterImage(colMap_legend, 0.5, 0, 1.25, 1)
text(x=1.5, y=seq(0, 1,l=8), labels=seq(1000, 4500,l=8), cex=0.9)
for (y_h in seq(0, 1,l=8)){segments(0.5, y_h, 1.25, y_h) }


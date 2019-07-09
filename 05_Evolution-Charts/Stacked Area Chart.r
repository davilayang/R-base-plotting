library(HistData)

data <- Prostitutes

###################################################################################
#                                Stacked Area Chart                               #
###################################################################################

# prepare data for each season by each year
seasons_gp <- list(c(3, 4, 5), c(6, 7, 8), c(9, 10, 11), c(12, 1, 2))
seasons <- list('spring', 'summer', 'autumn', 'winter')
df <- NULL
for (y in unique(data$Year)){
  tmp <- data[data$Year==y, ]
  for (idx in 1:4){
    m <- round(mean(tmp[tmp$mon %in% seasons_gp[[idx]], ]$count))
    df <- rbind(df, c(y, seasons[[idx]], m))
  }
}
df <- as.data.frame(df, stringsAsFactors=F); names(df) <- c('Year', 'season', 'count')
df[, c('Year', 'count')] <- sapply(df[, c('Year', 'count')], as.integer)
df[, 'season'] <- sapply(df[, 'season'], as.factor)
# str(df)

# setup variables for plotting
pstns <- seq(0.95*pi, 0.15*pi, length.out=nrow(df)/4)
Xps <- cos(pstns); Yps <- sin(pstns)
Yps_base <- 0.5*Yps # initial y-base
Yps_height <- 0.8*Yps # initial y-height
colors <- list('#41b6c4', '#d7191c', '#fdae61', '#2b83ba')
year_arws <- c(1822, 1827, 1838, 1847, 1854) # some years to plot arrows

###### plot stacked area chart ######
par(mar=c(0.5, 2, 2, 2))
plot(Xps, Yps_base, ylim=c(0,5), type='n', axes=F, xlab="", ylab="")
mtx_arws <- matrix(nrow=4, ncol=0) # matrix to store coords for arrows

for (idx in 1:4){  # iteration through each season
  sn <- seasons[[idx]] # season
  sn_clr <- colors[[idx]] # color for that season
  tmp <- df[df$season == sn, ] 
  tmp$scaled <- (tmp$count - min(tmp$count)) / (max(tmp$count) - min(tmp$count))
  Yps_height <- Yps_height + tmp$scaled # compute bar height of each season
  
  for (jdx in 2:nrow(tmp)){  # iteration through each data
    polygon(c(Xps[jdx-1], Xps[jdx-1], Xps[jdx], Xps[jdx]),
            c(Yps_base[jdx-1], Yps_height[jdx-1], Yps_height[jdx], Yps_base[jdx] ),
            col=sn_clr , border=F)
    
    if (unique(data$Year)[jdx] %in% year_arws){ # save coords to plot arrows later
      mtx_arws <- cbind(mtx_arws, c(Xps[jdx], Yps_base[jdx], Xps[jdx], Yps_height[jdx]))
    }
  }
  Yps_base <- Yps_height # update y-base to previous y-height
}

# add arrows and numbers on some years
for (y_idx in 1:length(year_arws)){
  year <- year_arws[y_idx]
  s_idx <- 1 # season index
  for (col_gap in seq(0, 15, 5)){
    c_idx <- y_idx + col_gap # col index for mtx_arws at each year
    val_label <- df[df$Year==year & df$season==seasons[[s_idx]], ]$count # count value
    
    arrows(mtx_arws[1, c_idx], mtx_arws[2, c_idx], mtx_arws[3, c_idx], mtx_arws[4, c_idx], 
           code=3, angle=40, lwd=1, length=0.08)
    text(mtx_arws[1, c_idx]+0.035, (mtx_arws[2, c_idx] + mtx_arws[4, c_idx])/2, 
         labels=val_label, cex=0.8)
    s_idx <- s_idx + 1 # move to plot next season
  }
}
# add 5-year separation lines
for (jdx in 2:nrow(tmp)){
  if (unique(data$Year)[jdx] %% 5==0){
    segments(Xps[jdx], 0.5*Yps[jdx], Xps[jdx], Yps_height[jdx], lwd=2, lty='dashed', col="grey40")
    if (unique(data$Year)[jdx] > 1825){
      text(Xps[jdx], 0.5*Yps[jdx], labels=unique(data$Year)[jdx], pos=1, col="grey40", 
           cex=1, las=2)
    }
  }
}
# add ticks
segments(Xps, 0.5*Yps, Xps, 0.85*0.5*Yps, lwd=2, col="grey40")
# add start and end years
text(Xps[1], 0.5*Yps[1], labels="1812", pos=1, col="black", cex=1.2)
text(Xps[nrow(df)/4], 0.5*Yps[nrow(df)/4], labels="1854", pos=1, col="black", cex=1.2)
# add legend
legend(-1.0, 5, title="Average Count of Each Season", 
       legend=c("Spring", "Summer", "Autumn", "Winter"), fill=unlist(colors), horiz=TRUE, cex=1)
# add main and sub titles
title(main="Seasonal Change of Counts", col='black', cex.main=2.5, adj=0.5)


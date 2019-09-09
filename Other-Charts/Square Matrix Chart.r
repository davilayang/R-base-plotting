library(HistData)

data <- Prostitutes
layout(matrix(c(1, 1, 1, 2, 2, 2, 
                3, 3, 3, 4, 4, 4), ncol=6, byrow=T))

###################################################################################
#                         Part1: Square Matrix Chart                             #
###################################################################################
# showing the difference of the four years: 1812, 1827, 1842, 1854

# function to draw each symbol in dot matrix chart
draw_symbol <- function(x_s, y_s, size=1, color='skyblue', w=5, h=5){
  # x_s and y_s for the bottom-left position; w and h for width and height
  # size should be between 0 and 1, <1 for smaller size
  x_coords <- c(x_s, x_s, x_s+w*size, x_s+w*size)
  y_coords <- c(y_s, y_s+h, y_s+h, y_s)
  polygon(x=x_coords, y=y_coords, col=color, border=F)
}

# function to draw all symbols of a year
draw_year_symbols <- function(value, per=20, n_cols=15, start_coords=c(0, 95), color='#41b6c4'){
  # per for the representating value of each symbol; ncol for the number of columns for all symbols
  # start_coords for the (x, y) coordinations to start drawing
  n_symbols <- value / per # total number of symbols to draw
  n_rows <- (n_symbols %/% n_cols) + 1 # number of rows for all symbols
  counter <- 0
  for (i in seq(0, n_rows-1)){
    y_start <- start_coords[2] - i*6 # 6 for symbol width(5) + gap between(1)
    for (j in seq(0, n_cols-1)){
      x_start <- start_coords[1] + j*6 # 6 for symbol height(5) + gap between(1)
      if (counter < floor(n_symbols)){
        # draw regular size symbol
        draw_symbol(x_start, y_start, color=color) 
        counter <- counter + 1
      } else {
        # draw reduced size symbol and end loop
        draw_symbol(x_start, y_start, size=n_symbols - floor(n_symbols), color=color)
        break
      }
    }
  }
}

# prepare average data for 1812, 1827, 1842, 1854
year_avg <- matrix(c(1812, 1827, 1842, 1854, NA, NA, NA, NA), ncol=2)
idx <- 1
for (year in year_avg[, 1]){
  year_avg[idx, 2] <- round(sum(data[data$Year==year, ]$count) / 12)
  idx <- idx + 1
}

###### plot dot-matrix chart ######
par(mar=c(1, 1, 1, 1), bg=rgb(230, 230, 230, maxColorValue=255), xpd=T)

# plot the first year: 1812
plot(100, 100, type='n', xlim=c(0, 100), ylim=c(0, 100), axes=F, xlab="", ylab="")
draw_year_symbols(year_avg[1, 2], start_coords=c(0, 45))
polygon(c(0, 0, 5, 5), c(45, 50, 50, 45), border=T, lwd=2.5)
# add the curved arrow
iArrows <- igraph:::igraph.Arrows 
iArrows(15, 35, 2.5, 45, curve=1, h.lwd=2, sh.lwd=2, size=0.4, width=2, sh.col='gray10')
# add year text and messages
text(15, 35, "20 People", font=4, cex=1.2, col="gray10", adj=0)
text(70, 0, "1812", font=4, cex=5, col='gray30')
message <- "In year 1812, the average \nmonthly count \nwas about 1294."
text(25, 0, message, cex=1.2, col='gray30')

# plot the second year: 1827
plot(100, 100, type='n', xlim=c(0, 100), ylim=c(0, 100), axes=F, xlab="", ylab="")
draw_year_symbols(year_avg[2, 2], start_coords=c(0, 45))
draw_year_symbols(year_avg[1, 2], color='gray',  start_coords=c(0, 45)) # overlap data from 1812
# add year text and messages
text(70, 0, "1827", font=4, cex=5, col='gray30')
message <- "In year 1827, the count \ndoubled to about 2472."
text(25, 0, message, cex=1.2, col='gray30')

# plot the third year: 1842
plot(100, 100, type='n', xlim=c(0, 100), ylim=c(0, 100), axes=F, xlab="", ylab="")
draw_year_symbols(year_avg[3, 2])
draw_year_symbols(year_avg[2, 2], color='gray') # overlap data from 1842
# add year text and messages
text(70, 80, "1842", font=4, cex=5, col='gray30')
message <- "In year 1842, the count \nreached about 3841."
text(25, 80, message, cex=1.2, col='gray30')

# plot the last year: 1854
plot(100, 100, type='n', xlim=c(0, 100), ylim=c(0, 100), axes=F, xlab="", ylab="")
draw_year_symbols(year_avg[4, 2])
draw_year_symbols(year_avg[3, 2], color='gray') # overlap data from 1854
# add year text and messages
text(70, 80, "1854", font=4, cex=5, col='gray30')
message <- "In year 1854, the count \nwas about 4232."
text(25, 80, message, cex=1.2, col='gray30')

# add main title
mtext("Square Matrix Chart", side=3, line=-5, outer=T, font=2, cex=2)
mtext("Comparing the counts from 1812 to 1854", side=3, line=-7, outer=T, col='gray40', adj=0.5)


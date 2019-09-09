# http://www.r-graph-gallery.com/97-correlation-ellipses/

data <- cor(mtcars)[1:3, 1:3]
data

#cannot use points, b.c. each circle has its own color
#polygon()


par(pty="s") # set to square size plot

pnts <- seq(0, 2*pi, length.out=128) #points for a circle
plot(0, 0, xlim=c(-1, 1), ylim=c(-1, 1), type='n', axes=T, ann=T)
polygon(sin(pnts), cos(pnts), col='navy', border=T)

abline(a=0, b=1, col='red')
# basically, move the points around the 45 degree line?


# pattern of the ellipses
# when cor=0, a complete circle
# when cor=+1, a 45 degree line from top-right to bottom-left
# when cor=-1, a 45 degree line from top-left to bottom-right
# when 1>cor>0, an ellipse with reduced size and tilt from 0 to 45 degree
# when 0>cor-1, an ellipse with reduced size and tilt from 0 to 45 degree


par(mfrow=c(1, 1))
par(mar=c(1, 1, 1, 1))

temps <- mtcars

pstns <-  seq(0.95*pi, 0.25*pi, length.out=nrow(temps))
pstns <-  seq(0, 2*pi, length.out=nrow(temps))
Xps <- cos(pstns); Yps <- sin(pstns)

# min-max scale on the data values
temps$data_scaled <- (temps$disp - min(temps$disp)) / (max(temps$disp) - min(temps$disp))

Yps_base <- Yps
Yps_height <- Yps + temps$data_scaled
# Yps_height <- Yps_base + temps$data_scaled # alternative way to get height
plot(Xps, Yps_base,  xlim=c(-2, 2), ylim=c(-2,2), axes=T)
points(Xps, Yps_height, ylim=c(0,2), pch=16)
polygon(Xps, Yps_height)

# https://stackoverflow.com/questions/29193499/how-to-plot-polar-coordinates-in-r
t <- seq(0,10, len=100)  # the parametric index
# Then convert ( sqrt(t), 2*pi*t ) to rectilinear coordinates
x = sqrt(t)* cos(2*pi*t) 
y = sqrt(t)* sin(2*pi*t)

plot(x,y)
plot(x,y, type="b")


# https://stackoverflow.com/questions/2190498/how-to-plot-in-circle-instead-of-straight-line-axis-in-matlab


# https://socratic.org/questions/what-is-a-polar-plot
t <- seq(0,360, len=180)
x <- 0.75 / (1 - 0.5 * cos(t))
# y <- 0.75 / (1 - 0.5 * sin(t))

# https://stackoverflow.com/questions/41820683/how-to-plot-ellipse-given-a-general-equation-in-r
par 


xc <- 1 # center x_c or h
yc <- 2 # y_c or k

t <- seq(0, 2*pi, length.out=120) 
a <- 3 # major axis length
b <- 2 # minor axis length
phi <- pi/3 # angle of major axis with x axis phi or tau



a <- -10; b <- 2; phi <- pi/3
x <- a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
y <- a*cos(t)*cos(phi) + b*sin(t)*cos(phi)
plot(x, y)

par(pty="s") # set to square size plot
plot(0, 0, xlim=c(-3, 3), ylim=c(-3, 3), type='n', axes=T, ann=T)
polygon(x,y, col='blue')

# https://stat.ethz.ch/pipermail/r-help/2006-October/114652.html

a <- -1; b <- 2
x <- a * cos(t)
y <- b * sin(t)
plot(x, y, type = "l")

degree = 45# rotating degree
x <- a*cos(t)*cos(degree) - b*sin(t)*sin(degree)
y <- a*cos(t)*cos(degree) + b*sin(t)*cos(degree)
plot(x, y)


alpha = 45 # rotating degree
x <- a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
y <- a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
plot(x, y, type = "l")

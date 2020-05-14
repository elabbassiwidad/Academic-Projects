install.packages('spatstat')
install.packages('GmAMisc')
data(malta_polyg)

#load the data
load(url("http://github.com/mgimond/Spatial/raw/master/Data/ppa.RData"))
str(starbucks)

#bind ma boundries
Window(starbucks) <- ma 
plot(starbucks, main=NULL, cols=rgb(0,0,0,.4), pch=19)

plot(distmap(starbucks))
miplot(starbucks.km)

#rescale the data
starbucks.km <- rescale(starbucks, 1000, "km")
ma.km <- rescale(ma, 1000, "km")
pop.km    <- rescale(pop, 1000, "km")
pop.lg.km <- rescale(pop.lg, 1000, "km")

summary(starbucks.km)

#Intesity Analysis
intensity(starbucks.km)
Q<- quadratcount(starbucks.km, nx= 4, ny=5)
quadrat.test(Q)
plot(starbucks.km, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE)  # Add quadrat grid

#the standard error
lam <- intensity(starbucks.km)
sqrt(lam/area(Window(starbucks.km)))

#Quadrat counting tests for CSR (quadrat.test)
quadrat.test(starbucks)
plot(starbucks.km, main = NULL)
plot(quadrat.test(starbucks.km), add = TRUE)

#Kernel Density 

d <- density(starbucks.km, edge = TRUE,sigma=50, kernel="gaussian")
plot(d, main = NULL, las=1 )
contour(d, add=TRUE)

K2 <- density(starbucks.km, sigma=50, kernel="epanechnikov")
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)

#Correlation
fryplot(starbucks.km, main = NULL)

K <- Kest(starbucks.km)
plot(K, main=NULL, las=1, legendargs=list(cex=0.7, xpd=TRUE, inset=c(0.5,0.1) ))

L <- Lest(starbucks.km, main=NULL)
plot(L, main=NULL, las=1, legendargs=list(cex=0.7, xpd=TRUE, inset=c(0.5, 0.1)))

g  <- pcf(starbucks.km)
plot(g, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(0.5, 0.1)))

a <- capture.output(plot(envelope(starbucks.km,Kest),main = NULL))
a <- capture.output(plot(envelope(starbucks.km, Lest, nsim = 19, global = TRUE), main = NULL, legendargs=list(cex=0.7, xpd=TRUE, inset=c(0.5, 0.1))))

#all
all <- allstats(starbucks.km,dataname="starbucks stores")
plot(all,legend=FALSE)

#Spacing
Fc <- Fest(starbucks.km)
plot(Fc, main = NULL, legendargs=list(cex=0., xpd=TRUE, inset=c(0.7, 0.5)))
a <- capture.output(plot(envelope(starbucks.km, Fest, nsim = 39), main = NULL,legend = FALSE))

plot(Gest(starbucks.km), legendargs=list(cex=0.5, xpd=TRUE, inset=c(0.5, 0.1)) ,main = NULL)


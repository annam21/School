#Some packages to install
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("maps")
#install.packages("ggmap")

library(ggplot2)

names(iris) #Data we'll be using
names(iris) <- c("sl","sw","pl","pw","spe") #Rename columns for ease of use

#qplot is in ggplot. it is a point-based plotting system similar 
#   to base plot function
qplot(sl, sw, data=iris)
qplot(sl, sw, data=iris, color=spe)
qplot(sl, sw, data=iris, size=spe) #can also be used for continuous variable
qplot(sl, sw, data=iris, shape=spe)

# It will use data from local environment
iris<-iris   #create dataframe from attached iris data set
sl<-iris$sl
sw<-iris$sw
spe<-iris$spe
qplot(sl, sw)
qplot(sl, sw, color=spe, shape=spe)

#Can also do some strange things
qplot(1:5, letters[1:5], size=c(1,3,4,5,2) )

# qplot will attempt to guess what geom you want depending on the input
# both x and y supplied = scatterplot
qplot(sl, sw, data = iris)

# just x supplied = histogram
qplot(sl, data = iris)

# just y supplied = scatterplot, with x = seq_along(y) - a quick way for looking at
#   potential outliers in your data 
qplot(y = sw, data = iris, color = spe)

#use geom = to choose plot type - easy to see changes with time with out having to 
#   include time in the plot if data already entered in temporal order
qplot(sl, sw, data = iris, geom="line", group=spe, color=spe)

#or create a box plot
qplot(spe, sw, data = iris, geom="boxplot", color=spe)

#Also visualize raw data (overlay on boxplot)
qplot(spe, sw, data = iris, geom=c("boxplot", "point"), color=spe)

qplot(factor(spe), sw, data = iris, geom=c("boxplot", "jitter"), color=spe) #jitter x/y amount can be set

#For more customization
#using the ggplot() function
# aes  = aesthetics (the x and y values)
#ggplot(data = , aes(x=, y=)) + geom_XXX()

ggplot(data= iris, aes(x=spe,y=pl)) + geom_boxplot()
ggplot(data= iris, aes(x=spe,y=pl)) + geom_boxplot() + geom_jitter(width = 0.2, height = 0)

#geom_histogram() default binwidth is range/30
ggplot(data= iris, aes(x=pl,group=spe)) + geom_histogram() ## group doesn't work to separate them out, so use:
ggplot(data= iris, aes(x=pl,fill=spe)) + geom_histogram() 

#change binwidth
ggplot(data= iris, aes(x=pl,fill=spe)) + geom_histogram(binwidth=.5)

#Or you decide to get creative with violin plots
# To look at distribution of data
ggplot(data= iris, aes(x=spe,y=pl)) + geom_violin()

#you can also flip the plot on an axis using +coord_flip()
ggplot(data= iris, aes(x=spe,y=pl,fill=spe)) + geom_violin() + coord_flip()

#geom_density()
# Smoothed distribution of data
ggplot(data= iris, aes(x=pl,group=spe)) + geom_density()
ggplot(data= iris, aes(x=pl)) + geom_density() # not grouping by species

#add color or fill to aes()
# color="red", color=SomeFactor, works with lines and points
# fill="red", fill=SomeFactor, works with shapes
ggplot(data= iris, aes(x=pl,group=spe,color=spe)) + geom_density()
ggplot(data= iris, aes(x=pl,group=spe,fill=spe)) + geom_density()
ggplot(data= iris, aes(x=pl,group=spe,fill=spe,color=spe)) + geom_density()

#Lets get serious now...how about a quick analysis
ggplot(data= iris, aes(x=pl, y=pw)) + geom_point()

#geom_smooth() by default adds a smoothed conditional mean for all data  -  but can change method..method="lm"
ggplot(data= iris, aes(x=pl, y=pw)) + geom_point() + geom_smooth(method="loess") # loess, lm, etc

#define groups and method of line fit
ggplot(data= iris, aes(x=pl, y=pw, group=spe)) + geom_point() + geom_smooth(method="lm")

#make groups stand out by color - change aes()
# alpha value changes transparency. If you make smooth alpha = 0, 
#   it gets rid of SE, or give it se = F
ggplot(data= iris, aes(x=pl, y=pw,group=spe, color=spe)) + geom_point() + 
  geom_smooth(method="lm", alpha = 0.5)

# #Summary Function great for ggplot and bar plots
# thegist <- function(data=NULL, vars, groups=NULL, na.rm=FALSE,
# conf.interval=.95, .drop=TRUE) { require(plyr)
#     length2 <- function (x, na.rm=FALSE) { if (na.rm) sum(!is.na(x)) 				else length(x)}
#     gistdat <- ddply(data, groups, .drop=.drop, .fun = function(xx, col) 			{ c(N = length2(xx[[col]], na.rm=na.rm),
#     		mean =mean(xx[[col]], na.rm=na.rm),
#     		sd = sd(xx[[col]], na.rm=na.rm))},vars) 
#     gistdat <- rename(gistdat, c("mean" = vars))
#     gistdat$se <- gistdat$sd/sqrt(gistdat$N)
#     ciMulti <- qt(conf.interval/2 + .5, gistdat$N-1)
#     gistdat$ci <- gistdat$se * ciMulti
#     return(gistdat)}

#Summary Function great for ggplot and bar plots
thegist <- function(data=NULL, vars, groups=NULL, na.rm=FALSE,conf.interval=.95, .drop=TRUE) { 
  require(plyr)
   length2 <- function (x, na.rm=FALSE) { 
     if (na.rm) {
       sum(!is.na(x)) 
       } else {
         length(x)
       }
   gistdat <- ddply(data, groups, .drop=.drop, .fun = function(xx, col){ 
     c(N = length2(xx[[col]], na.rm=na.rm),
       mean =mean(xx[[col]], na.rm=na.rm),
       sd = sd(xx[[col]], na.rm=na.rm))
     },
     vars) 
   gistdat <- rename(gistdat, c("mean" = vars))
   gistdat$se <- gistdat$sd/sqrt(gistdat$N)
   ciMulti <- qt(conf.interval/2 + .5, gistdat$N-1)
   gistdat$ci <- gistdat$se * ciMulti
   return(gistdat)
   }

#form of summary function: thegist(data=, vars="",groups="")
#vars = variables to summarize
#groups = groups to summarize vars over

plsum<-thegist(iris, vars="pl",groups="spe")
pwsum<-thegist(iris, vars="pw",groups="spe")

plsum
pwsum

#bar plot
ggplot(data=plsum, aes(x=spe,y=pl)) + geom_bar(stat="identity")
#stat="bin" warning message

#add fill
ggplot(data=plsum, aes(x=spe,y=pl,fill=spe)) + geom_bar(stat="identity")

#add errorbars
ggplot(data=plsum, aes(x=spe,y=pl,fill=spe)) + geom_bar(stat="identity") + geom_errorbar(data=plsum,aes(ymax=pl+se,ymin=pl-se),width=.5)

#can also predefine error bar limits
# can make errorbar = confidence interval too 
limits<-aes(ymax=pl+se,ymin=pl-se)
plot1<-ggplot(data=plsum, aes(x=spe,y=pl,fill=spe)) + geom_bar(stat="identity") + 
  geom_errorbar(limits,width=0.5) 
#typically errorbar width of 0.5 looks good

#Changing scales
plot1 + ylim(0,8)
plot1 + scale_y_continuous(limits=c(0,6), breaks=0:6) 

plot1 <- plot1 + scale_y_continuous(limits=c(0,6), breaks=c(0:6), expand=c(0,0)) #expand collapses bars to axis

#for factors can use discrete scales to determine order
plot1 + scale_x_discrete(limits=c("virginica","versicolor","setosa"))

plot1 <- plot1 + scale_x_discrete(limits=c("virginica","versicolor","setosa"),labels=c("Virginica","Versicolor","Setosa"))

plot1

#Now we can manipulate the theme of the plot
#change theme of plot - background/axis/legend

#fastest way to standard style plot..theme_bw()
plot1 + theme_bw()
plot1 + theme_bw(base_size = 20)

#but also several others...
plot1 + theme_classic()
plot1 + theme_void()

? theme

plot1 <- plot1 + theme_bw()

#More customization

plot1 + theme(axis.line.y = element_line(color = "red",size=1))
#can change specific axes using axis.line.y or axis.line.x

plot1 + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
#remove the grid lines to have just a white background

plot1 + theme( axis.text.x = element_text(colour = "black",size=12), axis.text.y = element_text(colour = "black",size=8))
#change the size of text along axes

plot1 + labs(y="Petal Length (mm)",x="Genera",title="Figure 1")
#axis labels and plot title

plot1 + theme(axis.title = element_text(size=20))
#change style of axis labels

plot1 + theme(axis.ticks=element_line(colour="red",size=3))
#customize tick marks along axes

plot1 + theme(legend.position="top", legend.text=element_text(size=20))
#customize legend position etc.

plot1 + scale_fill_manual(values=c("#999999","#000000","#666666"),name="Species")
#manually change the fill or color and can then easily adjust the legend title
#name = "Title of Legend"

plot1 + scale_fill_brewer(palette = "Set1",name="Species")
plot1 + scale_fill_brewer(palette = "Greens",name="Species")
#alternative color scheme
#note that if you are plotting points or lines then subsitute "color" where you find the word "fill" above Ex: scale_color_manual()

plot1 + geom_vline(xintercept=c(2.5))
#add lines to distinguish between groups etc.

#string together theme elements to build plot up 
plot1 + theme(
axis.line = element_line(colour = "black",size=0.5),
panel.background=element_blank(), 
panel.grid.minor.x=element_blank(),
panel.grid.minor.y=element_blank(), 
axis.text.x = element_text(color = c("blue","green","red"), size=12),
axis.text.y = element_text(color = "red", size=12)) + 
labs(y="Petal\nLength (mm)",x="") +
scale_y_continuous(expand=c(0,0))

#using \n between words on an axis will put whatever follows on a second line

#use the expand= command in the scale_y_continuous call to remove space between x-axis and bars

#plotting multiple figures
library(gridExtra)

plot1
p2<-qplot(pl,pw, data = iris)
p3<-qplot(sl,sw, data = iris)

grid.arrange(plot1,p2,p3,ncol=3)
grid.arrange(plot1,p2,p3,ncol=1,nrow=3)

#arrangeGrob() with in plot command to adjust layout within a column or row
grid.arrange(plot1,arrangeGrob(p2,p3,nrow=2),ncol=2)

#Lots of other cool things - worth spending the time to explore

#Making quick maps
library(maps)
map.dat <- map_data("world")
map.dat <- map_data("county","montana")
ggplot(map.dat, aes(long,lat, group=group, fill=group)) + geom_polygon() + theme_bw()

library(ggmap)

mis <- get_map(location = "Missoula", source = "osm", maptype = "roadmap")
ggmap(mis)

mis2 <- get_map(location = "Missoula", source = "google", maptype = "satellite")
ggmap(mis2)

lats <- c(46.8,47,47)
lons <- c(-114.2,-114.2,-114)
pts <- data.frame(cbind(lons,lats))

ggmap(mis2) + geom_point(data=pts, aes(lons,lats), color = "red", size = 10, shape = c("A","B","C"))


#heatmap plot
v <- ggplot(faithfuld) +
  geom_tile(aes(waiting, eruptions, fill = density))
v

v + scale_fill_distiller(palette = "Spectral")

v + scale_fill_distiller(palette = "Set1")

v + scale_fill_distiller(palette = "Greens")

v + scale_fill_distiller(palette = "Oranges")

v + scale_fill_distiller(palette = "Blues")
#notice the capitalization in the palette commands

#There are plenty of fun color packages
install.packages("wesanderson")
library(wesanderson)

ggplot(iris, aes(sl, sw, color = spe)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = wes_palette("GrandBudapest"))

#

pal <- wes_palette(21, name = "Zissou", type = "continuous")
image(volcano, col = pal)


#####
#my go to resource
# http://docs.ggplot2.org/current/
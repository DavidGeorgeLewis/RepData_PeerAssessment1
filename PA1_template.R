
## Load data

data <- read.csv("activity.csv")

##mean

mean <- 24*12*mean(data$steps,na.rm=T)

##plot total number of steps per day

data$date <- unclass(as.Date(data$date,format="%Y-%m-%d"))  ## Change Date to datetime format
id=15614:15674
a <- numeric()
for(i in id){
        dataseg <- data[,2]==i
        dataseg1 <- data[dataseg,]
        a1 <- sum(dataseg1$steps,na.rm=T)
        a <- append(a,a1)
        }
##median

median <- median(a)
y <- seq(as.Date("2012/10/01"), as.Date("2012/11/30"), by="day")
hist <- data.frame(cbind(y,a))
plot(y,a,type="h",xlab="Date",ylab="Steps",main="Number of Steps by Day",col="red",lwd=8)

##plot average number of steps by time interval and max val

data <- read.csv("activity.csv")
id=seq(from=0,to=2355,by=5)
x <- numeric()
x2 <- numeric()
for(i in id){
        dataseg <- data[,3]==i
        dataseg1 <- data[dataseg,]
        x1 <- mean(dataseg1$steps,na.rm=T)
        x3 <- sum(dataseg1$steps,na.rm=T)
        x <- append(x,x1)
        x2 <- append(x2,x3)
}
p <- is.nan(x)
x <- x[!p]
u <- 0:(length(x)-1)
hist <- data.frame(cbind(u,x))
plot(u,x,type="l",xlab="Time Interval (hours)",ylab="Steps",main="Average Number of Steps by Time Interval",col="blue",lwd=2)


maxsegments <- hist[,2]==max(x)
maxsegments1 <- hist[maxsegments,]
maximumInterval <- maxsegments1[[1]]

##Calculating number of missing values

data <- read.csv("activity.csv")
complete <- complete.cases(data)
NAcases <- data[!complete,]
numberNA <- nrow(NAcases)

##Replace missing values

data <- read.csv("activity.csv")
data$date <- unclass(as.Date(data$date,format="%Y-%m-%d"))

id=15614:15674
adjdata <- data.frame(steps=numeric(),date=as.Date(character()),interval=numeric())
for(i in id){
        dataseg <- data[,2]==i
        dataseg1 <- data[dataseg,]
        t <- i-15613
        b <- a[[t]]/288
        dataseg1[is.na(dataseg1)] <- b
        adjdata <- rbind(adjdata,dataseg1)             
        
}
adjdata$date <- as.Date(adjdata$date,origin="1970-01-01")
Daysweek <- weekdays(adjdata$date)
Daysweek1 <- cbind(adjdata,Daysweek)

##plot the total number of steps per day and report new mean and median
newmean <- 24*12*mean(Daysweek1$steps,na.rm=T)
Daysweek1$date <- unclass(as.Date(Daysweek1$date,format="%Y-%m-%d"))  ## Change Date to datetime format
id=15614:15674
v <- numeric()
for(i in id){
        Daysweek1seg <- Daysweek1[,2]==i
        Daysweek1seg1 <- Daysweek1[Daysweek1seg,]
        v1 <- sum(Daysweek1seg1$steps,na.rm=T)
        v <- append(v,v1)
}
newmedian <- median(v)
w <- seq(as.Date("2012/10/01"), as.Date("2012/11/30"), by="day")
hist <- data.frame(cbind(w,v))
plot(w,v,type="h",xlab="Date",ylab="Steps",main="Number of Steps by Day (Adjusted)",col="black",lwd=8)

##Number of steps by time interval split weekday, weekend

Daysweek1seg <- Daysweek1[,4]=="Saturday"
Daysweek1seg1 <- Daysweek1[Daysweek1seg,]
Daysweek2seg <- Daysweek1[,4]=="Sunday"
Daysweek2seg1 <- Daysweek1[Daysweek2seg,]
Weekends <- rbind(Daysweek1seg1,Daysweek2seg1)

Daysweek3seg1 <- Daysweek1[!Daysweek1seg,]
Weekdays <- Daysweek3seg1[!Daysweek2seg,]



library(lattice)

#This is the First Project on the course: "Introduction in Probability and Statistics With R" 
#The comments with double "#" are the questions of the project

#I insert the data in R (https://drive.google.com/file/d/16Wg93j2JOtow9XPjHuiwA5pc0sEVJxlj/view?usp=sharing)
datadf <- read.csv("ABNYC2019.csv", header = TRUE, sep = "", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

##Calculate descriptive measures for the price of accommodation per area

nbhd <- as.vector(unique(datadf$neighbourhood_group))

desc.stat <- list(0)
for (i in 1:5) { 
  desc.stat[[i]] <- summary(datadf[datadf[,"neighbourhood_group"]==nbhd[i],"price"])
}
names(desc.stat) <- nbhd
print(desc.stat)

##Make a boxplot for accommodation availability per region. Each neighbourhood group should be displayed in a different color and the title of the graph should be "Availability for booking".

boxplot(availability_365~neighbourhood_group, data=datadf, main="Availability For Booking", xlab="Neighbourhood Group", ylab="Availability per Year (365 days)", col=c("brown4","firebrick3","darkorange2","gray","gray100"))

##Create a frequency table to combine room_type and neighourhood_group variables. Then make a barplot for the variable room_type / the levels of the variable neighourhood_group. The bars should be a different color for each level of the variable neighourhood_group, the main title of the graph should be "Bar Plot of room type by location" and the title of the horizontal axis "Location".

freq <- table(datadf$neighbourhood_group,datadf$room_type)
print(freq)

barplot(freq, main="Bar Plot Of Room Type By Location",xlab="Room Type", ylab="Location", col=c("brown4","firebrick3","darkorange2","gray","gray100"),legend=rownames(freq))


##Based on the quantiles of the variable price, create a new variable named price_grp. Give the following labels to the levels of the new variable: "cheap", "average", "expensive", "super expensive". Also create a two-price variable called availability_6m depending on whether the availability of accommodation is> 6 months or ≤6 months.

bin <- quantile(datadf$price)
price_grp <- cut(datadf$price, bin, labels = c("cheap", "average", "expensive", "super expensive"),right = TRUE)
availability_6m <- cut(datadf$availability_365, breaks=c(0,180,365), labels = c(">6 months","=<6 months"), include.lowest=TRUE)

##Create a table of frequencies for each of the new variables of question (4) but also for their combination and show it with appropriate graphs

freq_price_grp <- table(price_grp)
freq_price_grp

barplot(freq_price_grp,xlab="Price Range", ylab="No. of Rooms", col=c("brown4","firebrick3","darkorange2","gray","gray100"),legend=rownames(freq_price_grp))

freq_availability_6m <- table(availability_6m)
freq_availability_6m

combination <- data.frame(price_grp,availability_6m)
freq_comb <- table(combination)

barplot(freq_availability_6m ,xlab="Availability", ylab="No. of Rooms", col=c("firebrick3","gray100"),legend=rownames(freq_availability_6m))
plot(price_grp,availability_6m,main="Availability For Booking per Price Group", xlab="Price Group", ylab="Availability", col=c("firebrick3","gray100"))   

##For each group defined by the combination of levels of the new variables in question (4), find the mean value per night.

names_pr_grp <- c("cheap", "average", "expensive", "super expensive")
names_av_6m <- c(">6 months","=<6 months")
match <- 0
values <- 0 
result <- matrix(0,4,2)
for (i in 1:4) {
  for (j in 1:2) {
    match <- names_pr_grp[i]==price_grp & names_av_6m[j]==availability_6m
    values <- datadf$price[match==TRUE]
    result [i,j] <- sum(values, na.rm=TRUE)/length(values)
  }	
}
colnames (result) <- names_av_6m
rownames (result) <- names_pr_grp


##Create a new data set that includes data only for the Bronx and Staten Island areas.

ds <- datadf[datadf["neighbourhood_group"]==c("Bronx","Staten Island"),]

##Create a chart that shows the availability of accommodation throughout New York and the availability in the above two areas.

par(mfrow=c(1,2))

plot(datadf$availability_365, ylab="Rooms Available", col=c("brown4","firebrick3","darkorange2","gray","gray100"))

plot(ds$availability_365, ylab="Rooms Available", col=c("firebrick3","gray100"))


## From the data set you created in question (7), remove the 150 accommodations with the highest price per night. Calculate the average value of the price per night before and after the subtraction


#the 150th value of the vector sort(ds$price, decreasing=TRUE) will be the 150th higher price per night.

ds2 <- ds[ds$price < sort(ds$price, decreasing=TRUE)[150], ]

#Before the substraction
mean(ds$price)
#After the substraction
mean(ds2$price)

##Calculate the asymmetry and curvature coefficients for the price variable in the original data set and in what you created in question (7).

(1/length(datadf$price))*sum((datadf$price-mean(datadf$price))^3)/(sqrt(var(datadf$price)))^3

(1/length(ds$price))*sum((ds$price-mean(ds$price))^3)/(sqrt(var(ds$price)))^3

(1/length(datadf$price))*sum((datadf$price-mean(datadf$price))^4)/(sqrt(var(datadf$price)))^4

(1/length(ds$price))*sum((ds$price-mean(ds$price))^4)/(sqrt(var(ds$price)))^4

##Randomly select 150 accommodations from all the accommodations in the Bronx and Staten Island areas and find the average price per night. Repeat the process 50 times and calculate the same amount for each repetition. Then build a histogram with these values.

random_price <- function(x=150){
random <- 0 
for (i in 1:x) {
  random <- sample(1:x, x, replace=FALSE)
  random [i] <- ds$price[random[i]]
}
  rndm <- return(random) 
}


mean_random <- 0 
for (i in 1:50){
mean_random [i] <- mean(random_price(150))
}

dev.off()

hist(mean_random,
     prob=TRUE,col="white",border="orange",
     xlab="Mean of Random Prices")
lines(density(mean_random,na.rm=T),col="red",lwd=1)

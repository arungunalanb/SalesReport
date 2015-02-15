setwd("F:/xangars/ShoppingCart")
data <- read.csv("F:/xangars/ShoppingCart/ALL_DC_Datewise_data.csv",stringsAsFactors=FALSE)
library(dplyr)
library(TraMineR)
library(reshape2)
library(googleVis)
names(data)

data1 <- select(data, DC_City,Date_1, Class_Name, Sub_Class, Sub_Class_Name, Net.Sales)
data1 <- filter(data1, DC_City =="Chennai")
#data1$Date <- as.Date(data1$Date_1, format = "%d-%m-%y")
data2<- data1 %>% group_by(Class_Name, Sub_Class_Name, Date_1) %>% summarise(Net_sales = sum(Net.Sales))
data3<- data2
data3<- ungroup(data3)
data1.new$Date <- filter(data.new1, Date!="") #take out the Netsales 1 value with date as blank

data4 <- dcast(data3, Class_Name + Date_1 ~ Sub_Class_Name, value.var="Net_sales")
data5 <- dcast(data3, Class_Name + Sub_Class_Name ~ Date_1, value.var="Net_sales")

#write.csv(data4,"result2.csv")
#-------------------------The pie chart-----------------------------
data.new<- read.csv("result3.csv")
data.new <- melt(data.new,id.vars=c("Class_Name","Sub_Class_Name") , variable.name="Date",
     value.name="NetSales")

data.new<- data.new %>% group_by(Class_Name, Sub_Class_Name, Date) %>% summarise(NetSales = sum(NetSales))
ungroup(data.new)
data.new$Date <- sub("X", "", data.new$Date)
data.new <- filter(data.new, Date!="") #take out the Netsales 1 value with date as blank

 View(data.new)
write.csv(data.new, "cleanedData.csv")


# which subclass in class_name had more sales ?
data.new1<- data.new %>% group_by(Class_Name, Sub_Class_Name, Date) %>% summarise(NetSales = sum(NetSales))
ungroup(data.new1)
data.new2 <- data.new %>% group_by(Class_Name, Sub_Class_Name) %>% summarise(NetSales = sum(NetSales))
data.new2 <- arrange(data.new2, NetSales) # arrange the data by date small to max
data.new2 <- data.new2[with(data.new2, order(NetSales)), ] # arrange the data by date small to max

# Pie Chart with Percentages
sales <- c(267856.7, 386117.4,1001431.0,1270432.1, 1395539.0,1923298.3,  3270767.7, 5305178.3, 8273664.6, 8597839.6)
lbls <-  c("Besan (Chana Flour)"," Wheat","Raw Peanuts","Chana Dal","Other Dals","Moong Dal","Tur Dal","Urad Dal","Boiled Rice", "Raw Rice")
pct <- round(sales/sum(sales)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(sales,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")

total <- sum(data.new2$NetSales)
df.data <- data.frame()
for (i in 1:nrow(data.new2)){
  df.class<- data.new2$Sub_Class_Name[i]
  df.cache <- data.new2$NetSales[i]
  pct <- round(df.cache / total *100)
  lbls <- paste(df.class, pct) # add percents to labels
  lbls <- paste( lbls, "%",sep="") # ad % to labels 
  df.pie <- cbind(df.cache, lbls)
  df.data<- rbind(df.data, df.pie)
}

#write.csv(data.new, "PieChartData.csv")
x <- as.vector(df.data$df.cache)
s<- as.numeric(x)
y <- as.vector(df.data$lbls)

# Simple Pie Chart
slices <- s
lbls <- y
pie(slices, labels = lbls, main="Pie Chart  Countries", cex=0.60)


#-----------------------------------------

data.new <- filter(data.new, Date != "X")
data6 <- select(data5, - Class_Name)

#df.form <- seqformat(data3, begin='Date_1', process=FALSE)

df.seq <- seqdef(data6, left='DEL', right='DEL', xtstep=10)
seqdplot(df.seq, border=NA, withlegend='right')


data3 <- select(data3, - Class_Name)
class<- unique(data2$Class_Name)

library(sm)

# create value labels 
class <- factor(data3$Sub_Class_Name, levels= c("Besan (Chana Flour)"," Wheat","Raw Peanuts","Chana Dal","Other Dals","Moong Dal","Tur Dal","Urad Dal","Boiled Rice", "Raw Rice"),
                labels = c("Boiled Rice","Raw Rice","Wheat","Besan (Chana Flour)","Chana Dal","Moong Dal","Other Dals","Raw Peanuts","Tur Dal", "Urad Dal")) 

# plot densities 
sm.density.compare(class, data3$Net_sales, xlab="sales")
title(main="MPG Distribution by Car Cylinders")

# Simple Bar Plot 
counts <- table(data3$Sub_Class_Name)
barplot(counts, main="Car Distribution", 
        xlab="Number of Gears")

df.seq2 <- seqdef(data3, left='DEL', right='DEL', xtstep=10)
seqmtplot(df.seq, border=NA, withlegend='right')

seqstatd(data4)
df.form <- seqformat(data2, id='Class_Name', begin='orderdate', end='to', status='cart',
                     from='SPELL', to='STS', process=FALSE)



barplot( c(267856.7, 386117.4,1001431.0,1270432.1, 1395539.0,1923298.3,  3270767.7, 5305178.3, 8273664.6, 8597839.6), main="Cars", xlab="Days",  
        ylab="Total", names.arg=c("Besan (Chana Flour)"," Wheat","Raw Peanuts","Chana Dal","Other Dals","Moong Dal","Tur Dal","Urad Dal","Boiled Rice", "Raw Rice"), 
        border="blue")

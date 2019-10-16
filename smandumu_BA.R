#*************---Business Analytics---************#
# Part A Descriptive Statistics & Normal Distributions
# Question 1 A
pnorm(700,494,100,lower.tail = FALSE)
#**********#

# Question 1 B
(pnorm(450,494,100,lower.tail = TRUE) - pnorm(350,494,100,lower.tail = TRUE)) *100
#**********#

# Question 2
bus<-qnorm(0.8665)
avg<- 449-(bus*36)
avg # average per diem cost in Buenos Aires
#**********#

# Question 3
kent <- c(59, 68, 78, 60) 
los_angeles <- c(90, 82, 78, 75) 
cor(kent,los_angeles) # Direct way to finnd Correlation
# Calucation Step - by - Step
N <- sum((kent-mean(kent)) * (los_angeles-mean(los_angeles)) )
N
D <- sqrt(sum(((kent-mean(kent))^2) * sum((los_angeles-mean(los_angeles))^2)))
D
N / D
#**********#

# Part B Data Wrangling
# Question 4 
ort <- read.csv("Online_ort(2).csv") # Reading the CSV file 
ort
library(plyr) # Calling and using library for Splitting, Applying and Combining Data
count1 <- count(ort,"Country") # Counting the Country 
per <-prop.table(table(ort$Country))*100 #C calculating Percentage
rdper <- round(per,digits = 3) # Rounding to 3 Digits
rdpercent <- paste0(rdper,sep = "%") # Adding % Symbol 
df1<-as.data.frame(per) # Converting 
tbl1 <- cbind(count1,df1[2],rdpercent) # Binding all values in proper Table Format
names(tbl1)<-c("Country","Count","Percentage","Round %") # Assigning Names 
tbl1 # Final table with required 
per[per>1] # countries accounting for more than 1% 
#**********#

# Question 5
ort$TransactionValue <- ort$Quantity * ort$UnitPrice #New Variable with 'Quantity' and 'UnitPrice' 
View(ort) # Viewing the Updated table with values 
#*********#

# Question 6
ctxn <- tapply(ort$TransactionValue,ort$Country,sum) # Summing values by combination
df2 <- as.data.frame(ctxn) # Converting
df2[,]
tbl2 <- cbind(count1,df1[2],rdpercent,df2[,]) #  Binding table Format
names(tbl2)<-c("Country","Count","Percentage","Round %","TransactionValue") # Assigning Lables
tbl2 # Final table with required
ctxn[ctxn>130000] # Total transaction exceeding 130,000 
#********#

# Question 8
cht <- hist(ort$TransactionValue[ort$Country=="Germany"],breaks = 10,xlim = c(-1000,1000),ylim = c(0,10000),main  = "Germany Transaction Values",xlab = "Values") # Creating Histogram
cht
text(cht$mids,cht$counts,labels = cht$counts,adj = c(0.5,-0.5)) # Displaying the Text 
#********#

# Question 9 a
htxn<-tapply(ort$TransactionValue,ort$CustomerID,length) # Summing values by combination
which.max(htxn) # Finding Max value with index number of customer  
htxn[4043] # 4043 Customer with highest txns as 17841
htxn
#*********#
# Question 9 b
hvl<-tapply(ort$TransactionValue,ort$CustomerID,sum)
which.max(hvl)
hvl[1704]
hvl
#***********#

# Question 10
colMeans(is.na(ort))*100
#***********#

# Question 11
fn1<-function(x){
k<-sum(is.na(x))
return(k)}
tapply(ort$CustomerID,ort$Country,fn1)
#***********#

#  Question 13
NROW(ort$Quantity[ort$Quantity<0 & ort$Country=="France"])/NROW(ort)*100 # return rate for the French customers
#***********#


# Question 14
hrev<-tapply(ort$TransactionValue,ort$Description,sum) # Selecting Product with Highest Revenue
which.max(hrev) # Picking the item with value index 1140
hrev[1140] # Displaying the item with the highest total sum of TransactionValue
hrev
#***********#

# Question 15
length(unique(ort$CustomerID)) # Unique Customers
#**********#

# Golden Questions
# Question 7
Temp=strptime(ort$InvoiceDate,format='%m/%d/%Y %H:%M',tz='GMT')
ort$New_Invoice_Date <- as.Date(Temp)
ort$New_Invoice_Date[20000]- ort$New_Invoice_Date[10]
ort$Invoice_Day_Week= weekdays(ort$New_Invoice_Date)
ort$New_Invoice_Hour = as.numeric(format(Temp, "%H"))
ort$New_Invoice_Month = as.numeric(format(Temp, "%m"))

#Question 7a
tapply(ort$TransactionValue,ort$Invoice_Day_Week,NROW)
txnper<-tapply(ort$TransactionValue,ort$Invoice_Day_Week,NROW)/NROW(ort$TransactionValue)*100 # percentage of transactions numbers
txnper
#********#

#Question 7b
tapply(ort$TransactionValue,ort$Invoice_Day_Week,sum)
txnpervol<-tapply(ort$TransactionValue,ort$Invoice_Day_Week,sum)/sum(ort$TransactionValue)*100 # Transaction  Percentages by volume
txnpervol
#*********#

#Question 7c
tapply(ort$TransactionValue,ort$New_Invoice_Month,sum)
txnvolmnt<-tapply(ort$TransactionValue,ort$New_Invoice_Month,sum)/sum(ort$TransactionValue)*100 # # Transaction  Percentages by volume per month
txnvolmnt
#**********#

#Question 7d
aus<- max(ort$TransactionValue[ort$Country == "Australia"])
aus
#*********#

#Question 7e
conse<-table(ort$New_Invoice_Hour)
abs(diff(conse))
# Need to try some things with rollapply or find which min
#**********#

# Question 12 
library(lubridate)#using Lubriate library
diff.Date()
diff.Date(date(ort$InvoiceDate), date(lag(ort$InvoiceDate, 1)))
DaysSinceLastPurchase = diff.Date(day("2011-12-10 00:00:00"), date(max(ort$InvoiceDate, na.rm = TRUE)))
TenureDays = diff.Date(day("2011-12-10 00:00:00"), date(min(ort$InvoiceDate, na.rm = TRUE)))
#************#



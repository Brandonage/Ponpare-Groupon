######## Analysis of the Ponpare Groupon Data #########

library(data.table)
library(plyr)
library(scales)

#### the purchase log of users buying coupons . It has a unique value that is purchaseID_hash
cpdetailpath <- '/Users/alvarobrandon/GitHub/AuxFiles/Ponpare Groupon/coupon_detail_train.csv' 
#### the master list of coupons aka the features of the coupon 
cplistpath <- '/Users/alvarobrandon/GitHub/AuxFiles/Ponpare Groupon/coupon_list_train_translated.csv' 
#### details of visits of users to coupons
cpvisitspath <- '/Users/alvarobrandon/GitHub/AuxFiles/Ponpare Groupon/coupon_visit_train.csv'
#### The master list of users
cpuserspath <- '/Users/alvarobrandon/GitHub/AuxFiles/Ponpare Groupon/user_list.csv'

##### I read the data into data.tables
cpdetail <- read.csv(cpdetailpath,header=TRUE)
cpdetail <- data.table(cpdetail,key='PURCHASEID_hash')

cplist <- read.csv(cplistpath,header=TRUE)
cplist <- data.table(cplist,key='COUPON_ID_hash')

cpvisits <- read.csv(cpvisitspath,header=TRUE)
cpvisits <- data.table(cpvisits)

cpusers <- read.csv(cpuserspath,header=TRUE)
cpusers <- data.table(cpusers,key='USER_ID_hash')


### I set the key of cpdetail to join it with users
setkeyv(cpdetail, key(cpusers))
### And I join
tmp <- cpusers[cpdetail]

### now i want to join tmp with the description of the coupons 
setkeyv(tmp,key(cplist))
### And we join them
tmp2 <- cplist[tmp]

### We want to keep only the columns that are of interest

purchases <- tmp2[,.(USER_ID_hash, COUPON_ID_hash,
CAPSULE_TEXT, GENRE_NAME, PRICE_RATE, CATALOG_PRICE, 
VALIDPERIOD, USABLE_DATE_MON, USABLE_DATE_TUE,
USABLE_DATE_WED, USABLE_DATE_THU, USABLE_DATE_FRI, 
USABLE_DATE_SAT, USABLE_DATE_SUN, USABLE_DATE_HOLIDAY,
USABLE_DATE_BEFORE_HOLIDAY, SEX_ID, AGE, ITEM_COUNT )]

## We free space
rm(tmp)
rm(tmp2)


##### Feature engineering
## NA imputation
purchases[is.na(purchases)] <- 1
## Changing the age into different buckets
purchases$AGE <- cut(purchases$AGE, breaks=c(0,21,25,30,45,55,65,Inf),labels=c('Teen','20-25','25-30','30-45','45-55','Senior','Retired'))
## We change the long string depicting a hotel into a smaller one.
purchases[GENRE_NAME=='Hotel and Japanese hotel',GENRE_NAME := 'Hotel']




#### DATA VISUALISATION ####

### What's the distribution of the ages between users?
m <- ggplot(cpusers, aes(x=AGE))
m + geom_histogram(bindwidth = 5,breaks=seq(15,80,by=5),aes(fill=..count..)) + 
  scale_x_continuous(breaks=seq(15,80,by=5)) + 
  scale_y_continuous(breaks=seq(0,4000,by=500))

### What's the distribution of the ages for users that actually bought coupons
m <- ggplot(purchases, aes(x=AGE))
m + geom_histogram(bindwidth = 5,breaks=seq(15,80,by=5),aes(fill=..count..)) + 
  scale_x_continuous(breaks=seq(15,80,by=5)) + 
  scale_y_continuous(breaks=seq(0,30000,by=5000))


### What age ranges are the top buyers for each category?

m <- ggplot(purchases, aes(x=reorder(GENRE_NAME,GENRE_NAME,function(x)-length(x))))
m + geom_freqpoly(aes(color=AGE,group=AGE)) + theme_minimal() + 
  labs(x='Coupon Type',title='Coupon types for each Age category') +
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90))

### What are the distribution of Sexes inside each category?

m <- ggplot(purchases, aes(x=reorder(GENRE_NAME,GENRE_NAME,function(x)-length(x))))
m + geom_bar(aes(fill=SEX_ID)) + theme_minimal() +
  labs(x='Coupon Type',title='Sex and type of coupon') +
theme(text = element_text(size=16),axis.text.x = element_text(angle=90))


#### BROWSING HISTORY   #####

setkey(cpvisits,VIEW_COUPON_ID_hash)
history <- cpvisits[cplist]
## We change the long string depicting a hotel into a smaller one.
history[GENRE_NAME=='Hotel and Japanese hotel',GENRE_NAME := 'Hotel']

### We are going to plot the ratio of coupons purchased/browsed by coupon type

### It is difficult to do transformations of the data through ggplot, so we are 
### going to make that separately by using grouping by with data.table
ans <- history[,.(.N),by=.(GENRE_NAME,PURCHASE_FLG)]
### We divide the data frame by GENRE_NAME and we operate with each piece. In this case we
### transform the data frame adding a new percent column
ans = ddply(ans,.(GENRE_NAME),transform,percent=N/sum(N)*100)
### Same here but with the position
ans = ddply(ans,.(GENRE_NAME),transform, pos=cumsum(N) - 0.5*N)
### Another new column with the label for the barchart
ans$label = paste0(sprintf("%.0f", ans$percent), "%")
### We take out the row that has NA's values 
ans <- ans[-11,]
### We add an offset to the ones that have been purchased. This is so because in some bars 
### they are too close together so we push them forward in the chart
ans[ans$PURCHASE_FLG==1,]$pos <- ans[ans$PURCHASE_FLG==1,]$pos + 25000


### We do the plot with the different parameters
ggplot(ans,aes(x=reorder(GENRE_NAME,N) , y = N, fill=factor(PURCHASE_FLG))) + 
  geom_bar(stat="identity") + geom_text(aes(y=pos,label=label)) +
  scale_y_continuous(name="Number of Visits",labels=comma) + labs(x='Coupon Type')  + coord_flip() +
  scale_fill_discrete(name="Purchase Flag",labels=c("Not Purchased","Purchased")) + theme_minimal() +
  theme(text = element_text(size=16))










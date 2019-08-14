library("tidyverse")
library(ggplot2)

amazon2 <- read.csv("amazon_kenya_data.csv", stringsAsFactors = FALSE)
View(amazon2)

#exploratory data analysis(E.D.A)----

#compare the performance of the different branches
#Best performing category
#find the largest transaction and what was bought
#compare sales within the different days of the week
#best performing brand
#find the mean transaction and quantity
#the most loyal customer

#question1-compare the performance of the different branches

amazon2 <- amazon2%>%mutate(sales=ITEM.PRICE*QUANTITY.PURCHASED)
View(amazon2)
names(amazon2)
branch_performance <- amazon2%>%
  group_by(NAIVAS.BRANCH)%>% 
  summarise(total_sales=sum(sales),number_of_products=sum(QUANTITY.PURCHASED), customer = n_distinct(CUSTOMER.ID))

View(branch_performance)

branch_performance <- amazon2%>%
  group_by(NAIVAS.BRANCH)%>% 
  summarise(total_sales=sum(sales),number_of_products=sum(QUANTITY.PURCHASED), customer = n_distinct(CUSTOMER.ID),Total_transaction=n_distinct(RECEIPT.ID))

View(branch_performance)

#best performing category

Category_performance <- amazon2%>%
  group_by(NAIVAS.BRANCH,CATEGORY,DEPARTMENT)%>% 
  summarise(TOTAL_REVENUE=sum(sales),NUMBER_OF_TRANSACTION=n_distinct(RECEIPT.ID))

View(Category_performance)

#the best performing category as far as total_revenue is concerned is beverages under snacks and beverages department at Naivas westlands with a total revenue of 9,131,598


#mean of sales and quantity purchased

data1 <- amazon2%>%group_by(NAIVAS.BRANCH)%>%
  summarise(Mean_Sales=mean(sales),Mean_No_of_Products=mean(QUANTITY.PURCHASED))

View(data1)

#Most loyal customer

loyal_customer <- amazon2%>%group_by(NAIVAS.BRANCH,CUSTOMER.NAME)%>%
  summarise(count=n())

View(loyal_customer)

#the most loyal customer is Victoria Namatosi Simiyu. she does her shopping at Naivas Nyali branch



#comparing sales within the different days of the week
library(lubridate)
amazon2 <- amazon2%>%mutate(weekday=wday(transaction_date, label=TRUE))
amazon2

compare_sales <- amazon2%>%group_by(weekday,NAIVAS.BRANCH)%>%
  summarise(Total_sales=sum(sales))
View(compare_sales)  


#largest transaction and what was bought

max(amazon2$sales,amazon2$ITEM.DESCRIPTION)


#in summary
 
amazon3 <- amazon2%>%group_by(NAIVAS.BRANCH,weekday,ITEM.DESCRIPTION,FINELINE)%>%
  summarise(Total_sales=sum(sales))
View(amazon3)











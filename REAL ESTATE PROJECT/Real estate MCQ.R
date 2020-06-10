options(scipen = 999, stringsAsFactors = F)



library(dplyr)

h_train = read.csv('C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE PROJECT\\REAL ESTATE PROJECT\\housing_train.csv', stringsAsFactors = FALSE)

glimpse(h_train)

h_test = read.csv('C:\\Users\\Mr. Jarvis\\Desktop\\DATA SCIENCE PROJECT\\REAL ESTATE PROJECT\\housing_test.csv', stringsAsFactors = FALSE)

glimpse(h_test)



setdiff(names(h_train),names(h_test))

View(h_train)

View(h_test)



#q1: Find the variance of the target variable 'Price'.

var(h_train$Price)

##### ans = 432958829215


#q2: Find out how many observations have missing values for variable 'YearBuilt'?

sum(is.na(h_train$YearBuilt))

sum(is.na(h_train$YearBuilt[1:10]))

sum(is.na(h_test$YearBuilt))


##### ans = 3717



library(data.table)


#q3: What is the difference in average price between house type h and t?

h_train = as.data.table(h_train)

h_train


glimpse(h)



g = h_train[Type=='h'& Type=='t',Type,Price]

g


f = h_train[Type=='t',Type,Price]

f


mean(g[,Price])-mean(f[,Price])

str(h)


z = h_train %>% filter(Type=='h')
zz = mean(z$Price)
z1 = h_train %>% filter(Type=='t')
zz1 = mean(z1$Price)

zz2 = zz-zz1
zz2


##### ans = 392384.2


#q4: How many unique values variable postcode takes?

length(unique(h$Postcode))

##### ans = 94



#q5: how should you treat post code . As a categorical variable or numeric variable ( write "categorical" or "numeric" as your answer)

#Note: Answers are not case sensitive . 

#Ans: Categorical


#q6: Does distance follow a normal distribution?

install.packages("nortest")

options(scipen=999)

library(nortest)

ad.test(h$Distance)

h$Distance

qqnorm(h$Distance)

##### Ans = No


##  Normal distributions are symmetric, unimodal, and asymptotic, 
##  and the mean, median, and mode are all equal. 
##  A normal distribution is perfectly symmetrical around its center.




#q7: Which seller has maximum value transactions? [ sum of price ]

glimpse(h)

length(unique(h$SellerG))

View(h)

sum_of_price = unique(h$SellerG) 


unique(h_train$SellerG)


library(dplyr)


sum_of_price <- h_train %>% 
  group_by(SellerG) %>% 
  summarise(total = as.numeric(sum(Price))) %>% 
  arrange(desc(total))

sum_of_price


###### ans =   SellerG            total
#                <chr>              <dbl>
#               1 Jellis        1062676265



#q8: Which CouncilArea has maximum average price?

library(dplyr)

avg_of_price <- h_train %>% 
  group_by(CouncilArea) %>% 
  summarise(total = as.numeric(mean(Price))) %>% 
  arrange(desc(total))

avg_of_price


### ans =   CouncilArea      total
#            <chr>            <dbl>
#            1 Bayside       1692174.
  




#q9: which CouncilArea has maximum variance in the price?


View(h_train$Price)

library(dplyr)

maximum_var = h_train %>% 
  group_by(CouncilArea) %>% 
  summarise(varr = as.numeric(var(Price))) %>% 
  arrange(desc(varr))

maximum_var


##### ans =   CouncilArea            varr
#                <chr>                 <dbl>
#             1 Stonnington   994218615783.






#q10: Should we use Address as is in the modeling process?

h_train$Address

#Ans: No

##### about this sir already explain in the class



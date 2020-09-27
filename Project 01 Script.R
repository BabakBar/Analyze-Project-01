> library("readr")
> library("tidyverse")
> library("dplyr")
> myData <- read.csv("myData.csv",header = TRUE, sep = ",", dec = ".")

> dim(myData)
[1] 150   5
> str(myData)
'data.frame':	150 obs. of  5 variables:
  $ Order.ProdA : num  511 491 471 461 500 ...
$ Return.ProdA: num  35.7 30.7 32.3 31.9 36.5 39 34.9 34.9 29.5 32 ...
$ Order.ProdB : num  14.6 14.4 13.6 15.3 14.9 17.6 14.8 15.7 15 15.6 ...
$ Return.ProdB: num  2.9 2.8 2.1 2.2 2.9 4.8 3.7 2.2 2.7 1.2 ...
$ Customer    : chr  "A" "A" "A" "A" ...

> unique(myData$Customer)
[1] "A"  "B"  "C "

> CustomersB <- filter(myData, Customer == "B")
> dim(CustomersB)
[1] 50  5
> str(CustomersB)
'data.frame':	50 obs. of  5 variables:
  $ Order.ProdA : num  701 641 690 550 651 ...
$ Return.ProdA: num  32.4 32.7 31.5 23.5 28.3 28.6 33.9 24.3 29.1 27 ...
$ Order.ProdB : num  47.3 45.7 49.9 40.6 46.3 45.4 47.3 33.5 46.6 39.9 ...
$ Return.ProdB: num  15 15.4 15.6 13.9 15 13.9 16.3 10 13 15 ...
$ Customer    : chr  "B" "B" "B" "B" ...

> mean(CustomersB$Order.ProdB)
[1] 43.068

> NCustomersB <- filter(myData, Customer != "B")
> dim(NCustomersB)
[1] 100   5
> str(NCustomersB)
'data.frame':	100 obs. of  5 variables:
  $ Order.ProdA : num  511 491 471 461 500 ...
$ Return.ProdA: num  35.7 30.7 32.3 31.9 36.5 39 34.9 34.9 29.5 32 ...
$ Order.ProdB : num  14.6 14.4 13.6 15.3 14.9 17.6 14.8 15.7 15 15.6 ...
$ Return.ProdB: num  2.9 2.8 2.1 2.2 2.9 4.8 3.7 2.2 2.7 1.2 ...
$ Customer    : chr  "A" "A" "A" "A" ...

> mean(NCustomersB$Order.ProdB)
[1] 35.554

> CustomersA <- filter(myData, Customer == "A")
> head(select(arrange(CustomersA, desc(Order.ProdA)),Order.ProdA), 5)
Order.ProdA
1       580.6
2       570.5
3       570.1
4       550.8
5       550.6

CustomersC <- filter(myData, Customer != "A" , Customer != "B")
> tail(select(arrange(CustomersC, desc(Order.ProdA)),Order.ProdA), 4)
Order.ProdA
47       580.4
48       570.7
49       560.1
50       490.1

myData$Profit <- (myData$Order.ProdA - (2 * (myData$Return.ProdA + myData$Return.ProdB))) * 3
> CustomersA <- filter(myData, Customer == "A")
> mean(CustomersA$Profit)
[1] 1276.758
> CustomersB <- filter(myData, Customer == "B")
> mean(CustomersB$Profit)
[1] 1530.156
> CustomersC <- filter(myData, Customer != "A" , Customer != "B")
> mean(CustomersC$Profit)
[1] 1672.062

> CustomersA <- filter(myData, Customer == "A")
> CustomersB <- filter(myData, Customer == "B")
> CustomersC <- filter(myData, Customer != "A" , Customer != "B")

> summary(CustomersA)
Order.ProdA     Return.ProdA    Order.ProdB     Return.ProdB        Profit
Min.   :430.2      Min.   :23.70       Min.   :10.60       Min.   :1.200            Min.   :1103
1st Qu.:480.5   1st Qu.:32.15       1st Qu.:14.12    1st Qu.:2.325          1st Qu.:1225
Median :500.7   Median :34.80   Median :15.05   Median :2.700      Median :1276
Mean   :501.1    Mean   :34.74     Mean   :15.12    Mean   :3.014          Mean   :1277
3rd Qu.:520.4   3rd Qu.:37.02    3rd Qu.:16.15    3rd Qu.:3.500          3rd Qu.:1328
Max.   :580.6    Max.   :44.30       Max.   :19.80      Max.   :6.400              Max.   :1484
> summary(CustomersB)
Order.ProdA     Return.ProdA    Order.ProdB     Return.ProdB     Profit    
Min.   :491.0       Min.   :20.00       Min.   :30.80       Min.   :10.00         Min.   :1267  
1st Qu.:560.4     1st Qu.:26.10     1st Qu.:40.45     1st Qu.:12.65       1st Qu.:1435  
Median :590.2   Median :28.40   Median :43.70   Median :13.90      Median :1501  
Mean   :594.1     Mean   :28.22     Mean   :43.07    Mean   :13.81        Mean   :1530  
3rd Qu.:630.3   3rd Qu.:30.50     3rd Qu.:46.52    3rd Qu.:15.20        3rd Qu.:1634  
Max.   :700.9     Max.   :34.90       Max.   :51.10      Max.   :19.00           Max.   :1818  

> summary(CustomersC)
Order.ProdA     Return.ProdA    Order.ProdB     Return.ProdB                 Profit    
Min.   :490.1        Min.   :22.50      Min.   :45.80       Min.   :14.30                Min.   :1213  
1st Qu.:623.1      1st Qu.:28.23    1st Qu.:51.55     1st Qu.:18.73              1st Qu.:1576  
Median :650.2   Median :30.40   Median :55.90   Median :20.60             Median :1636  
Mean   :659.3     Mean   :30.21     Mean   :55.99    Mean   :20.79               Mean   :1672  
3rd Qu.:690.4    3rd Qu.:32.12   3rd Qu.:59.08      3rd Qu.:23.27              3rd Qu.:1751  
Max.   :790.1      Max.   :38.80      Max.   :69.20        Max.   :25.30                Max.   :2014


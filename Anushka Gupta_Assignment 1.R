install.packages("imager")
install.packages("stringr")

library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)





############question 1
html1 <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
M.cap <- html1 %>% html_elements("ml-2") %>% html_text()
CMP <-  html1 %>% html1_elements(".ratings.ratingfreedm") %>% html_text() %>% as.numeric()
price_change <- html1%>% html1_elements("text-green") %>% html_text() %>% as.numeric()
Market_Cap_CR <- html1 %>% html1_elements("text-green") %>% html_text() %>% as.numeric()
Week_High <-html1 %>% html1_elements("text-secondary font-10") %>% html_text() %>% as.numeric()
Week_Low <- html1 %>% html1_elements("text-secondary font-10") %>% html_text() %>% as.numeric()
ROE <-html1 %>% html1_elements("text-secondary font-10") %>% html_text() %>% as.numeric()
P/E<-html1 %>% html1_elements("text-secondary font-10") %>% html_text() %>% as.numeric()
P/BV<-html1 %>% html1_elements("text-secondary font-10") %>% html_text() %>% as.numeric()
EV/EBITDA<-html1 %>% html1_elements("text-secondary font-10") %>% html_text() %>% as.numeric()
YSales_Gr <- html1 %>% html1_elements("text-secondary font-10") %>% html_text() %>% as.numeric()
YProfit_Gr <- html1 %>% html1_elements("text-secondary font-10") %>% html_text() %>% as.numeric()




##############question 2

for (i in 1:5)
Sales[i] <- html2 %>% html_elements("fw-700 text-left stick")
YoY_Gr_Rtfw[i]  <- html2 %>% html_elements("fw-700 text-left stick")
Adj_EPS[i] <- html2 %>% html_elements("fw-700 text-left stick")
YoY_Gr_Rt[i] <- html2 %>% html_elements("fw-700 text-left stick")
BVPShtml2[i] <- html2 %>% html_elements("fw-700 text-left stick")
Adj_Net_Profithtml2[i]  <- html2 %>% html_elements("fw-700 text-left stick")
Cash_Flow_from_Ops[i]<- html2 %>% html_elements("fw-700 text-left stick")
Debt/CF_from_Ops[i]<- html2 %>% html_elements("fw-700 text-left stick")
 Return_on_Equity[i] <- html2 %>% html_elements("fw-700 text-left stick")
 Op_Profit_Mgn[i]<- html2 %>% html_elements("fw-700 text-left stick")
 Net_Profit_Mgn[i]<- html2 %>% html_elements("fw-700 text-left stick")
 Debt_to_Equity[i]<- html2 %>% html_elements("fw-700 text-left stick")
 Working_Cap_Days[i]<- html2 %>% html_elements("fw-700 text-left stick")
 Cash_Conv_Cycle[i]  <- html2 %>% html_elements("fw-700 text-left stick")
 
 
 
 
 ######################question 3
 tennis<- function(p)
 {
   A<-0
   B<-0
   for (i in 1:5)
   {
     other_game <- sample(0:1, size=1, prob=c(p,1-p))
     
     if (other_game ==1){ 
       A <- A+1}
   
   if (other_game ==0){
     B<-B+1
   }
   
   if( B==3 || A==3)
   {
     x <- i
     break
   }
   }
return (x)}
 
 
 
 
 matches <- numeric(length=1000)
 for(i in 1:1000)
 {
   matches[i] <- tennis(0.7)
 }
 ans <- mean(matches)
 ans
 
 
 ##################question 4
 
 doors <- c(1,2,3)
 corr_door <- sample(d=doors,size=1)
 first_door <-1
 if (corr_door== first_door)
     {
        
    open_door <- sample (d=doors[-c(first_door)],size=1)
   
     }
 else {
   open_door<- doors[-c(first_door,coor_door)]
 }
 last_door<- doors[-c(first_door, open_door)]
 last_door<- last_door
 if (last_door== corr_door)
    {
       print("1")
     }
 else
   {print("0")
   }
 
 
 
 
 win<-0
 for(i in 1:1000){
   corr_door<- sample(doors,1)
   first_door<-1
   if (corr_door== first_door)
   {
     open_door <- sample (d=doors[-c(first_door)],size=1)
     
   }
   else {
     open_door<- doors[-c(first_door,coor_door)]
   }
   last_door<- doors[-c(first_door, open_door)]
   last_door<- last_door
   if (last_door== corr_door)
   {
      win<-win+1
   }
 }
 print (round(win/1000, 2))
 
 
 
 
 
 
 
 ########################question 5
 library(rvest)
 
 html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-rightnow/")
 
all_data <- html %>% html_table()
 
length(all_data)

all_data[[1]]
all_data[[2]]
all_data[[3]]
all_data[[4]]

   

 
 
 
 
 
 
 
   


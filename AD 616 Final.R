library(readxl)
library(tidyverse)
library(fastDummies)
library(e1071)
n<-10000
df <- read_excel("616data.xlsx")
str(df)
df$Outlook<-as.factor(df$Outlook)
str(df)
df$ExpectedSales<-as.numeric(df$ExpectedSales)
df[27,2]<-61969.04
str(df)
regression <- lm(RealizedSales~ExpectedSales+Outlook, data=df)
summary(regression)

df1 <- data.frame(
  Outlook=rdiscrete(n,probs=c(12/32, 9/32, 11/32), values=c(-1,0,1))
) %>%
  mutate(
    Actual_sales= case_when(
      Outlook=='-1'~65153.54*0.9277-6217.2255,
      Outlook=='0'~65153.54*0.9277-6217.2255+12352.2688,
      Outlook=='1'~65153.54*0.9277-6217.2255+19872.5661
    ),
    S_Error=rnorm(n, 0, 5178),
    Precise_sales= Actual_sales+S_Error,
    Present_sales= Precise_sales/(1+0.06)^3,
    Land_cost_Oct=rep(4375/(1+0.06)^0.25,n),
    Land_cost=rep(500,n),
    Construction_2007=rep(20000/(1+0.06)^0.25,n),
    Construction_2008=rep(9000/(1+0.06)^1,n),
    Construction_2009=rep(10200/(1+0.06)^2,n),
    Sales_and_marketing_cost=rep(2300/(1+0.06)^2,n),
    Interest=rep(8636.03/(1+0.06)^3,n),
    principle=rep(38375/(1+0.06)^3,n),
    Profit=Present_sales-Land_cost_Oct-Land_cost-Construction_2007-Construction_2008-Construction_2009
    -Sales_and_marketing_cost-Interest-principle+38375,
    Interest_and_loan_=rep(38375+8636.03,n),
    Affordable=pmin(Interest_and_loan_,Precise_sales),
    Profit_bank_NPV=Affordable/(1+0.06)^3-38375
  )
mean(df1$Precise_sales)
mean(df1$Profit)

ggplot(df1)+geom_histogram(mapping=aes(x=Present_sales),
                            bins = 45,
                            fill="blue",
                            color="dark blue")+scale_y_continuous(expand=c(0,0))+theme_classic()

ggplot(df1)+geom_histogram(mapping=aes(x=Profit),
                           bins = 45,
                           fill="blue",
                           color="dark blue")+scale_y_continuous(expand=c(0,0))+theme_classic()

ggplot(df1)+geom_histogram(mapping=aes(x=Profit_bank_NPV),
                           bins = 45,
                           fill="blue",
                           color="dark blue")+scale_y_continuous(expand=c(0,0))+theme_classic()

library(readxl)
library(tidyverse)
credit <- read_excel("C:/Users/Hafid Pradipta/OneDrive/ITEC 621/credit card default/creditdefault.xls")
credit2 <- credit
credit <- credit2
attach(credit)
hist(AGE)
hist(LIMIT_BAL)

ifelse(credit$SEX==1,"male","female")



credit <- credit %>% 
  filter(MARRIAGE %in% c(1,2))

credit <- credit %>% 
  mutate(limbal = LIMIT_BAL/1000)
credit <- credit %>% 
  mutate(limbalusd =LIMIT_BAL*0.034)
credit <- credit %>% 
  mutate(flimbalusd = cut(limbalusd, c(0, 5000, 15000, 20000 ,25000,30000, Inf), right = FALSE,
                           labels = c("cat1","cat2","cat3","cat4","cat5","cat6")))

names(credit)
credit %>% View()
credit %>% 
  group_by(flimbalusd) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x =flimbalusd, y = count), label = count)+
  geom_bar(stat="identity")+
  geom_text(aes(label = count))


credit[25]
flimbalusd


#EDA
default <- credit %>% 
  filter(credit[25]==1)

def <- credit %>% gather(paste0("BILL_AMT",1:6), key = "billperiod", value ="billamount")
def %>% View()
abc <- credit %>% gather(PAY_0,paste0("PAY_",2:6), key = "payperiod", value = "payvalue")


longcc <- def[,c(1:6,19,23:24)]
longdata <- bind_cols(longcc, abc[,c(23:24)])
longdata

default %>% group_by(SEX) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x =SEX, y = count), label = count, fill =SEX)+
  geom_bar(stat="identity")+
  geom_text(aes(label = count))

default %>% group_by(EDUCATION) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x =EDUCATION, y = count), label = count, fill =SEX)+
  geom_bar(stat="identity")+
  geom_text(aes(label = count))

default %>% group_by(MARRIAGE) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x =MARRIAGE, y = count), label = count, fill =SEX)+
  geom_bar(stat="identity")+
  geom_text(aes(label = count))

default


names(abc)
names(credit)
credit[,7:24] %>% colMeans()
tail(def)
View(def)
names(def)

longcc <- def[,c(1:6,19,23:24)]
longdata <- bind_cols(longcc, abc[,c(23:24)])

longdefault <- longdata %>% 
  filter(longdata[7]==1)

longnondef <- longdata %>% 
  filter(longdata[7]==0)


ppdef <- longdefault %>% 
  group_by(payperiod) %>% 
  summarise(average = mean(payvalue))
ppdef %>% 
  ggplot(aes(x= payperiod, y = average))+
  geom_point(color = "blue")+
  geom_point(aes(x = payperiod, y = avnd), data = ppnd, color = "red")

ppnd <- longnondef %>%
  group_by(payperiod) %>% 
  summarise(avnd = mean(payvalue))
ppnd %>% 
  ggplot(aes(x= payperiod, y = avnd))+
  geom_point()

diffpp <- ppdef %>% left_join(ppnd, by = "payperiod")
diffpp  %>% gather(average, avnd, key= "type", value = "amount") %>% 
  ggplot(aes(x= type, y = amount))+
  geom_point()
#########################################################################
longdata %>% 
  ggplot(aes( x = payperiod, y =payvalue))+
  geom_smooth()

longdata %>% 
  group_by(payperiod) %>% 
  summarise(average = mean(payvalue)) %>% 
  ggplot(aes(x= payperiod, y = average))+
  geom_point()


longdata %>% 
  group_by(payperiod) %>% 
  summarise(average = mean(payvalue)) %>% 
  ggplot(aes(x= payperiod, y = average))+
  geom_point()+
  geom_text(aes(label = round(average,2)))

longdata$ID <- as.factor(longdata$ID)

longdata %>% 
  arrange(ID) %>%
  slice(1:100) %>% 
  ggplot(aes(x = ID, y= payvalue, color = payperiod))+
  geom_point()

test1[1:12,] %>% 
  ggplot(aes(x = payperiod, y= payvalue, col = ID))+
  geom_point()

  ggplot(aes(x= billperiod, y = average))+
  geom_point()+
  geom_text(aes(label = round(average,2)))

longdata

cor(credit)

plot(credit$PAY_2, color = credit$ID)

longdata %>% 
  ggplot(aes(x = payperiod, y= payvalue, color ))+
  geom_point()

longdata %>% 
  group_by(payperiod) %>% 
  summarise(count = n())

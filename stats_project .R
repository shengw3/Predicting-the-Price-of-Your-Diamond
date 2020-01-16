###############################################
#install package 
pkgs <-c("ggplot2","car","tidyr","broom","sjPlot")
install.packages(pkgs)
sapply(pkgs,require,character.only = T)
library(ggplot2)
library(MASS)
library(car)
library(dplyr)
library(tidyr)
###############################################
#import data 
data <- read.csv("clean_diamond_data.csv")
dim(data)
str(data)
###############################################
#######################
#split data into training and testing 
set.seed(1)
sample <- sample(1:nrow(data), size = 0.3*nrow(data))
test <- data[sample,]
dim(test)
train <- data[-sample,]
dim(train)
###############################################
#set up a raw model 
#check correlation 
cor(data$carat,data$price)#0.75 
raw_mod <-lm(price~., data = train) 
summary(raw_mod)
par(fmrow=c(2,2))
plot(raw_mod)
###############################################
#check vif
vif(raw_mod)

#boxcox 
bc_res <- boxcox(price~.,data = train) 
best_lambda <- bc_res$x[bc_res$y == max(bc_res$y)]
train$transformed_y <- (train$price^best_lambda - 1)/best_lambda
raw_fin_mod <- lm(transformed_y~ cut + clarity + carat + color, data = train)
summary(raw_fin_mod)# r-square 92.8%
par(mfrow=c(2,3))
plot(raw_fin_mod)

#plot out the price, highly skewed 
ggplot(aes(x = price),data = data) + geom_histogram(fill = "royalblue4")
ggplot(aes(x = log(price)),data = data) + geom_histogram()
#so we did log transformation to our response variables, the skewness get better 
###############################################
#data exploratory 
# price vs. cut
ggplot(data,aes(factor(cut),log(price),fill=cut))+
  xlab("Cutting Type ")+
  ylab("Price")+
  geom_boxplot()#+ggtitle('Boxplot of price by cut')
#the better the cut, the higher the price, several outliters 
ggplot(data)+
  geom_histogram(aes(x=log(price), fill=cut))

ggplot(data)+
  geom_bar(aes(x=factor(1), fill=cut))+
  coord_polar(theta="y")
###############################################

# price vs. color
ggplot(data,aes(factor(color),log(price),fill=color))+
  xlab(" Color ")+
  ylab("Price")+
  geom_boxplot()#+ggtitle('Boxplot of price by color')
ggplot(data)+
  geom_histogram(aes(x=log(price), fill=color))
###############################################

# price vs. clarity
ggplot(data,aes(factor(clarity),log(price),fill=clarity))+
  xlab("Clarity Type ")+
  ylab("Price")+
  geom_boxplot()#+
ggplot(data)+
  geom_histogram(aes(x=log(price), fill=clarity))


###############################################

## price vs. carat
ggplot(aes(x = carat,y = log(price)),data = data)+
  xlab(" Carat ")+
  ylab("Log(Price)")+
  geom_point()+
  scale_y_log10()+stat_smooth()+
  ggtitle('Price(Log 10) vs. Carat')


#square
ggplot(aes(x = carat^2,y = log(price)),data = data)+
  xlab(" Carat ")+
  ylab("Price")+
  geom_point()+
  ggtitle('Price(Log 10) vs. Carat(square)')

#not good 
#
#square root
ggplot(aes(x = carat^(1/2),y = log(price)),data = data)+
  xlab(" Carat ")+
  ylab("Price")+
  geom_point()+
  scale_y_log10()+stat_smooth()+
  ggtitle('Price(Log 10) vs. Carat(square root)')


#cube root 
ggplot(aes(x = carat^(1/3),y = log(price)),data = data)+
  xlab(" Carat ")+
  ylab("Price")+
  geom_point()+
  scale_y_log10()+stat_smooth()+
  ggtitle('Price(Log 10) vs. Carat(cube root) + Carat')

#log 
ggplot(aes(x = log(carat),y = log(price)),data = data)+
  xlab(" Carat ")+
  ylab("Price")+
  geom_point()+
  scale_y_log10()+stat_smooth()+
  ggtitle('Price(Log 10) vs. Carat(cube root )')
#seems the best transformation 
############heat map
new= 
  data %>%
  group_by(color, cut) %>%
  summarise(n=n()) %>%
  group_by(cut) %>%
  mutate(sum.n= sum(n)) %>%
  ungroup() %>%
  mutate(n2= n/sum.n) %>%
  select(color, cut, n2)

new %>% spread(cut,n2)

new %>%
  ggplot(aes(color, cut)) +
  geom_tile(aes(fill=n2*100), colour = "white") +
  scale_fill_gradient(low="white",high="blue") +
  labs(fill = "Density")

###############################################
######################################

#Building models 
#mod2
mod2<-lm(log(price)~cut + clarity + color + I(carat^(1/2)), data = train)
summary(mod2)#r-square 93%
par(mfrow=c(2,3))
plot(mod2)

#mod3
mod3<-lm(log(price)~cut + clarity + color + I(carat^(1/3)), data = train)
summary(mod3)#r-square 96% 
par(mfrow=c(2,3))
plot(mod3)
tab_model(raw_fin_mod,show.ci = FALSE)

#mod4
mod4<-lm(log(price)~cut + clarity + color + I(carat^(1/3)) + carat, data = train)
summary(mod4)#r-square 98.12% 
par(mfrow=c(2,3))
plot(mod4)
extern_mod4 <- studres(mod4)
qqnorm(extern_mod4)
qqline(extern_mod4)
plot(extern_mod4)

#mod6
mod6 <- lm(log(price) ~ log(carat) + cut + clarity + color, data = train)
summary(mod6) #r-square 98.14%
par(mfrow=c(2,3))
plot(mod6)
extern_s_resids6 <- studres(mod6)
qqnorm(extern_s_resids6)
qqline(extern_s_resids6)
plot(extern_s_resids6)

test_pred2 <- predict(mod2, newdata = test) 
test_pred3 <- predict(mod3, newdata = test) 
test_pred4 <- predict(mod4, newdata = test) 

mse_mod2 <-mean(test_pred2-log(test$price)^2)#-50.87594
mse_mod3 <-mean(test_pred3-log(test$price)^2)#-50.87607
mse_mod4 <-mean(test_pred4-log(test$price)^2)# -50.87608

#model 6 has a better normality 

#print out the summary table nicely 
tab_model(raw_mod,show.ci = FALSE)
tab_model(raw_fin_mod,show.ci = FALSE)
tab_model(mod2,show.ci = FALSE)
tab_model(mod3,show.ci = FALSE)
tab_model(mod4,show.ci = FALSE)
tab_model(mod6,show.ci = FALSE)
################################################################
###################################################################
#try adding dummy variable 
quantile(data$price,0.75)
l<-lm(log(price) ~ log(carat) + cut + clarity + color +I(price>4235):color + I(carat>1.4):clarity:color, data = train)
par(mfrow=c(2,3))
plot(l)
anova(l)
summary(l)#summary table is still not good 
top25<-data[data$price>4250,]
bottom30 <-data[data$price<2000,]
#top 25% data 
ggplot(top25,aes(factor(clarity),log(price),fill=clarity))+
  xlab("Clarity Type ")+
  ylab("Price")+
  geom_boxplot()
ggplot(top25,aes(factor(color),log(price),fill=color))+
  xlab(" Color ")+
  ylab("Price")+
  geom_boxplot()

ggplot(top25,aes(factor(cut),log(price),fill=cut))+
  xlab("Cutting Type ")+
  ylab("Price")+
  geom_boxplot()
#bottom 30% data
ggplot(bottom30,aes(factor(clarity),log(price),fill=clarity))+
  xlab("Clarity Type ")+
  ylab("Price")+
  geom_boxplot()
ggplot(bottom30,aes(factor(color),log(price),fill=color))+
  xlab(" Color ")+
  ylab("Price")+
  geom_boxplot()

ggplot(bottom30,aes(factor(cut),log(price),fill=cut))+
  xlab("Cutting Type ")+
  ylab("Price")+
  geom_boxplot()

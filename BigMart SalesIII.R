library(dplyr)
library(ggplot2)
library(reshape)
library(plyr)
library(stringr)
library(randomForest)

setwd("D:/Users/1015624/GE Internal/My Docs/Work/Bigmart")

train <- read.csv("BigMartSales_Train.csv",stringsAsFactors = FALSE)
test <- read.csv("BigMartSales_Test.csv",stringsAsFactors = FALSE)

#looking at the summary stats of the data
summary(train)

colnames(train)
dim(train)

#combining training and test dataset for data preparation
test$Item_Outlet_Sales <- 0
dat <- rbind(train,test)

##Data Preparation##

#Item_Fat_Content has different naming conventions. LF is same as Low Fat and reg is same as Regular
dat$Item_Fat_Content <- revalue(dat$Item_Fat_Content, c("LF" = "Low Fat", "reg" = "Regular", "low fat" = "Low Fat" ))

#Non-food items in Item_Type cannot have fat content. Creating new level for such Item_types
dat[dat$Item_Type == 'Others', ]$Item_Fat_Content <- "None"
dat[dat$Item_Type == 'Household', ]$Item_Fat_Content <- "None"
dat[dat$Item_Type == 'Health and Hygiene', ]$Item_Fat_Content <- "None"

##Missing Values Imputation##

#checking missing values. This gives missing values for numeric types.
colSums(is.na(dat))
#Item_Weight is missing also Outlet_Size has NA's which was seen in summary(dat)

#Imputing missing values for Item_Weight. Exploring weight based on other columns.
#Boxplot of weights vs Item type
ggplot(dat, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")


ggplot(dat, aes(dat$Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")


#creating dataframe with mean values based on Item_identifier which could be an unique item
weightsByItem <- as.data.frame( ddply(na.omit(dat), 
                                      ~Item_Identifier, 
                                      summarise, 
                                      mean=mean(Item_Weight), 
                                      sd=sd(Item_Weight)))

#we can fill the missing weights based on above data
dat$Item_Weight <- ifelse(is.na(dat$Item_Weight), 
                          weightsByItem$mean[match(dat$Item_Identifier, weightsByItem$Item_Identifier)], 
                          dat$Item_Weight)

#we can see that the Outlet_size is missing for identifiers out010,out017,out045
aggregate(dat$Outlet_Identifier, by=list(Category=dat$Outlet_Identifier, Category=dat$Outlet_Size), FUN=length)

#To get an idea on outlet_size we can check the sales based on location_type and outlet_type
ddply(train, .(Outlet_Location_Type, Outlet_Size, Outlet_Type), summarise, mean = mean(Item_Outlet_Sales))

#based on the mean values we can categorise the missing values for outlet_size as "small"
table(dat$Outlet_Size)
dat[dat$Outlet_Identifier %in% c("OUT010","OUT017","OUT045"),]$Outlet_Size <- "Small"

#Item_type has many levels. we shall try to categorise the items based on item_identifier
dat$Item_Category <- ifelse(strtrim(dat$Item_Identifier,2) == "FD", "Food",
                            ifelse(strtrim(dat$Item_Identifier,2) == "DR", "Drinks", "Non-Consumables"))

#It is observed that item_visibility has 0's in the data which looks like missing.
#we can fill these values with the mean of each item type.
dat$Item_Visibility[dat$Item_Visibility == 0] <- NA
itemvisibility <- as.data.frame(ddply(na.omit(dat),
                                      ~Item_Type, 
                                      summarise, 
                                      mean=mean(Item_Visibility)))

dat$Item_Visibility <- ifelse(is.na(dat$Item_Visibility),
                              itemvisibility$mean[match(dat$Item_Type, itemvisibility$Item_Type)],
                              dat$Item_Visibility)

#we have outlet established year. we can generate no. of years since the outlet is operating.
#as the base date given in problem statement is 2013
dat$Outlet_Age <- 2013 - dat$Outlet_Establishment_Year
dat$Outlet_Establishment_Year <- NULL

#creating dummy variables for categorical attributes
#before creating dummy variables we need to convert char attributes to factor

#Filter(is.character, dat)
#a <- select_if(dat,is.character)
#dat[,sapply(dat,is.numeric)] 
#unlist(lapply(dat, is.character))

# col <- colnames(Filter(is.character,dat))
# dat[col] <- lapply(dat[col], factor)
# str(dat)
# 
# dat <- model.matrix(~Item_Fat_Content +Item_Type +Outlet_Identifier +Outlet_Size +Outlet_Location_Type +Outlet_Type +Item_Category+0, data = dat)
# 
# dat_copy <- dat
# for (i in col[-1]){
# for(level in unique(dat_copy[i])){
#   dat_copy[paste("dummy", level, sep = "_")] <- ifelse(dat_copy$strcol == level, 1, 0)
# }
# }
# 
# for (i in col) {f = unique(dat$i)}

# new_train = as.data.frame(model.matrix( ~ . + 0, data = new_train))
# new_test = as.data.frame(model.matrix( ~ . + 0, data = new_test))

##detaching train and test sets before creating the model
train_dat <- dat[1:nrow(train),]
test_dat <- dat[-(1:nrow(train)),]

test_dat$Item_Outlet_Sales <- NULL

##building the model
#Linear regression model
lr_model <- lm(Item_Outlet_Sales ~ Item_Visibility+Item_MRP+Outlet_Size+Outlet_Age+Outlet_Location_Type, data = train_dat)
summary(lr_model)

outlet_sales <- predict(lr_model, newdata = test_dat)

rmse <- sqrt(mean((lr_model$residuals)^2))

test_dat$Item_Outlet_Sales <- outlet_sales

#Random forest model
rf_model <- randomForest()

result <- test_dat[, c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_Sales")]

write.csv(result, "result.csv", row.names = FALSE)



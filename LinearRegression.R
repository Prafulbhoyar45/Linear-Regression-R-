# Linear Regression Model

# Load The Dataset  
df <- read.csv("C:/Users/abc/Desktop/Linnear Model on R/Data Files/Linear Regression Dataset/House_Price.csv")
View(df)

# Write The Inference about Data
str(df) # Structure of Data Set
summary(df) #EDD <Extendade Data Dictionary>

# Inference about Datset (House Price)
# 1) Here airport,waterbody and bus_ter are the catagorical veriable
# 2) Price is our Target veriabel 
# 3) as we see crime_rate column have much difference in 3rd quartile and maximum value mense it have outliers
# 4) n_hos_bed contains 8 null values
# 5) n_hot_room contains otliers because of 3rd quartile range and maxima contains major difference
# 6) Rainfall contain outliers because of differe between minima and 1st quartile is more
# 7) There is 4 Distances are given in the Database

## Missing Value Tretmant
mean(df$n_hos_beds,na.rm = TRUE) # Mean of the n_hos_bed
which(is.na(df$n_hos_beds)) # at which possition we have null value
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds) # Replace that null values with mean

## Univariate Analysis
# Box Plot
boxplot(df$n_hot_rooms)

#Scatter Plot
pairs(~df$Sold+df$rainfall)

# Barplot
barplot(table(df$airport)) 
barplot(table(df$bus_ter)) # All Cities have Bus Terminal with no unque values so in our analyis it wont helpfull

df2 <- df[,-13] # Remove the Bus Terminal column
df <- df2
View(df)

### Outlier treatment
## We have two veriabels with outliers
# 1) n_hot_rooms
quantile(df$n_hot_rooms,0.99) # Find quantile at 99 % data in frame
ul = 3* quantile(df$n_hot_rooms,0.99) # Multiply it with standerie of 3 (for upper outlier is 3)
df$n_hot_rooms[df$n_hot_rooms > ul] <- ul # Replace it with that maximum value

summary(df) # Check the inference in summary

# 2) rainfall
quantile(df$rainfall,0.01) # Find quantile at 0.01 data in frame
ll = 0.3*quantile(df$rainfall,0.01) # Multiply it with standered of 0.3 (for lower outlier is 0.3)
df$rainfall[df$rainfall < ll] <- ll # replace it with minimum value

summary(df) # Check the changes in data

## Describe relation price and crime rate
pairs(~crime_rate+price,data = df) # There is some relation in between them
plot(df$price,df$crime_rate)

df$crime_rate = log(1+df$crime_rate) # Here we do log transformation on data to sprade the data 

plot(df$price,df$crime_rate) # Here is the data after the transformation and this data is look near linear in nature

# Label encoding to make data more inferential
install.packages("Dummies") # Install Dummies Package
df <- dummy.data.frame(df) # Create dummies for all Catagorical variables
View(df)
df <- df[,-9] # Its a rule of n-1 so remove unwanted Column
df <- df[,-13]

## Check its there any multicolinearity will present because it affect to our final model
cor(df)
round(cor(df),2) # By using Corelation we can easily find other then target variable is there any other relative column 

df <- df[,-16] # To avoide such corelation just remove the column

# Draw a simple model with single indepandant variable
simple_model <- lm(price~room_num,data = df) # Here room_num is indepandant variable
simple_model
summary(simple_model)

## Simple_Model Inference
# Yes Room_num variable 99.99 % impacting on target variable(Price) indecated by that 3 stars his notations you see in the output of above equation
# bita value indicate that if you increase the room_mun 1 unit the room price increase by 9

# Try to show that relationship by graphical way
plot(df$room_num,df$price)
# As wee see by the graph there is relation between these two variables
abline(simple_model)

## Multiple Linear Regression
multiple_model <- lm(price~.,data = df) # Here we take all indepandant veriable
multiple_model
summary(multiple_model)

# Multiple linear regression inference
# These are the variables with * are imacting to our target variable
# F stats give us by how much percentage it would do changes ar say affected to the target veriable
# Bita values(Estimated) are taill us about by how many poins we do changes in that variable by that values price (Target veriable ) wlii move
# * indicate the confidance interval *-95%,**-99%,***-99.99
# P value indicate the probability of sucess rate

### Train Test Split ###
# Training Error- perdormance of model on previously seen data
# Testing Error- performance of model on unseen Data
# There are three technique tospit the data
# 1) Vallidation set aproch -random division of data into two parts 
#                            -Usal split is 80:20 (training,testing)
#                           - In case of large numbers of observation we have we go for this approch
# 2) Leave one out cross validation- Leaving one observation every time from training set
# 3) K-Fold Validation - Devide the data into k set
#                      - we will keep one testing and k-1 for training
install.packages("caTools")
set.seed(0)
split = sample.split(df,SplitRatio = 0.8)
training_set = subset(df,split  == TRUE)

test_set = subset(df,split == FALSE)

lm_a = lm(price~.,data = training_set)
summary(lm_a)

train_a = predict(lm_a,training_set)
test_a = predict(lm_a,test_set)

mean((training_set$price - train_a)**2,na.rm = TRUE)
mean((test_set$price - test_a)**2,na.rm = TRUE) # As the difference is major that means our model works worstly on Test (Unseen Data)

#### OLS (Ordinary List Square)
# Here we study two methods that are
# 1) Subset Selection (some predictor of the subsets)
# 2) Shrinkage Method (Beta ko 0 Karne ki try karenge)

install.packages("leaps") 
lm_best = regsubsets(price~.,data = df,nvmax = 15)
summary(lm_best)

summary(lm_best)$adjr2
which.max(summary(lm_best)$adjr2)

coef(lm_best,8)


lm_forward = regsubsets(price~.,data = df,nvmax = 15,method = "forward")
summary(lm_forward)

summary(lm_forward)$adjr2
which.max(summary(lm_forward)$adjr2)


lm_reverse = regsubsets(price~.,data = df,nvmax = 15,method = "reverse")
summary(lm_reverse)

summary(lm_reverse)$adjr2
which.max(summary(lm_reverse)$adjr2) 

# There is two shrinkage method 
# 1) Ridge
# 2) Lasso

x = model.matrix(price~.,data=df)[,-1]
y = df$price
grid = 10^seq(10,-2, length =100)

grid

lm_ridge = glmnet(x,y,alpha = 0, lambda =grid)
summary(lm_ridge)
cv_fit = cv.glmnet(x,y,alpha = 0, lambda = grid )
plot(cv_fit)

opt_lambda = cv_fit$lambda.min
tss = sum((y-mean(y))^2)

y_a = predict(lm_ridge,s=opt_lambda , newx = x)

rss = sum((y_a-y)^2)

rsq = 1 - rss/tss

lm_lasso = glmnet(x,y,alpha = 1, lambda =grid)
summary(lm_ridge)
cv_fit = cv.glmnet(x,y,alpha = 1, lambda = grid )
plot(cv_fit)

opt_lambda = cv_fit$lambda.min
tss = sum((y-mean(y))^2)

y_a = predict(lm_ridge,s=opt_lambda , newx = x)

rss = sum((y_a-y)^2)

rsq = 1 - rss/tss





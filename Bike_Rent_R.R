######clear the environment and set the directory


rm(list = ls())
setwd("C:/Users/Spathak/Desktop/Bike_Renting/Bike_Rent_R")


### check the required library is installed or not install if not and then load all the lib


x = c("ggplot2", "corrgram", "purrr", "caret", "randomForest", "tidyr","rpart", 'sampling', 'corrplot','GGally')
lib = x[! x %in% installed.packages()[,'Package']]
if (length(lib))
  install.packages(lib, dependencies = TRUE)
lapply(x, require, character.only = TRUE)
rm(x,lib)


###load the data into a df


bike_data = read.csv('day.csv')

## removing instatnt and dteday because they are always unique
bike_data = subset(bike_data, select = -c(instant,dteday))

##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(bike_data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(bike_data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
#there are no miising value in the dataframe so we don't have to go for missing data analysis
#lets convert column into thier proper datatype
bike_data$season=factor(bike_data$season)
bike_data$yr = factor(bike_data$yr)
bike_data$mnth = factor(bike_data$mnth)
bike_data$holiday =factor(bike_data$holiday)
bike_data$weekday = factor(bike_data$weekday)
bike_data$workingday = factor(bike_data$workingday)
bike_data$weathersit = factor(bike_data$weathersit)


##################################plotting various graph to get idea about data###############################################
#lets plot the distribution of numeric variable to get idea about data distribution
bike_data_num = bike_data %>% keep(is.numeric)

bike_data_num %>% gather() %>% ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +                     # In separate panels
  geom_density()

ggpairs(bike_data_num)

p1 = ggplot(data = bike_data, aes(x=weekday, y= cnt))+
  geom_bar(stat="identity")

p2 = ggplot(data = bike_data, aes(x=season , y= cnt))+
  geom_bar(stat="identity")

p3 = ggplot(data = bike_data, aes(x=yr , y= cnt))+
  geom_bar(stat="identity")

p4 = ggplot(data = bike_data, aes(x=mnth , y= cnt))+
  geom_bar(stat="identity")

p5 =  ggplot(data = bike_data, aes(x=holiday , y= cnt))+
  geom_bar(stat="identity")

p6 = ggplot(data = bike_data, aes(x=workingday , y= cnt))+
  geom_bar(stat="identity")

p7 = ggplot(data = bike_data, aes(x=weathersit , y= cnt))+
  geom_bar(stat="identity")

grid.arrange(p1,p2,p3,p4,p5,p6,p7)


##################################Outliers Analysis###############################################
bike_data_num %>% gather()  %>% ggplot(aes(x="", y=value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE) +
  theme(legend.position="bottom")


# removing outliers from the boxlot method
#casual
val = boxplot.stats(bike_data_num$casual)$out
bike_data = bike_data[which(!bike_data$casual %in% val),]
#hum
val = boxplot.stats(bike_data_num$hum)$out
bike_data = bike_data[which(!bike_data$hum %in% val),]
#windspeed
val = boxplot.stats(bike_data_num$windspeed)$out
bike_data = bike_data[which(!bike_data$windspeed %in% val),]

##################################feature selection###############################################
cor_bike_data = cor(bike_data_num)
corrplot(cor_bike_data, method="circle")

ggplot(bike_data, aes(x= casual,y=cnt)) +
  geom_point()

ggplot(bike_data, aes(x = hum, y=cnt)) +
  geom_point()

ggplot(bike_data, aes(x = windspeed , y=cnt)) +
  geom_point()

ggplot(bike_data, aes(x = temp , y=cnt)) +
  geom_point()

ggplot(bike_data, aes(x = atemp, y=cnt)) +
  geom_point()

ggplot(bike_data, aes(x = registered , y=cnt)) +
  geom_point()

#by the scatterplot for all numeric variable we can see windspeed and hum is very weakly related to cnt we will drop these column
#similarly corelaton between temp and atemp is very high so we will remove atemp as well
#there are no use of instant and dteday column  because they are always unique don't add any value so removing these two column
bike_data = subset(bike_data, select = -c(hum, windspeed ,atemp))



##################################feature scaling###############################################
cnames = c('registered','casual')
for(i in cnames){
  print(i)
  bike_data[,i] = (bike_data[,i] - min(bike_data[,i]))/
    (max(bike_data[,i] - min(bike_data[,i])))
}

###################################Model Development#######################################
#Clean the environment
rmExcept("bike_data")
#Divide data into train and test
set.seed(123)
index = sample(1:nrow(bike_data), 0.8*nrow(bike_data))
train = bike_data[index, ]
test = bike_data[-index, ]

MAPE = function(y, yper){
  mean(abs((y - yper)/y))
}


lm_model = lm(cnt ~., data = train)
summary(lm_model)
predictions_LR = predict(lm_model, test[,1:10])
#Calculate MAPE
errorlr = MAPE(test[,11], predictions_LR)
#error rate in LR model = 5.405056e-16 almost 100% accuracy


dr_model = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(dr_model, test[,-11])
errordt = MAPE(test[,11],predictions_DT)
# error rate in dt model = 0.1410895 that means 99.85891% accuracy

RF_model = randomForest(cnt ~ ., data = train)
predictions_RF = predict(RF_model, test[,1:10])
errorrf = MAPE(test[,11], predictions_RF)
# error rate in rf model = 0.06399467 that means 99.93601% accuracy
modelval=test
modelval$lr_predict = predictions_LR
modelval$dt_predict = predictions_DT
modelval$RF_predict = predictions_RF

rmExcept(c("bike_data",'dr_model','lm_model','RF_model','errordt','errorlr','errorrf','modelval'))

write.csv(modelval,'OT_TEST_R.csv')





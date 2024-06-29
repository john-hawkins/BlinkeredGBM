library(h2o)
h2o.init()

df 		<- h2o.importFile( path=normalizePath("data/YearPredictionMSD/YearPredictionMSD.txt") )
feat 		<- 1:90
featnames 	<- paste("feat", feat, sep="")

colnames(df) 	<- c('Year', featnames )

feature.list 	<- featnames 
target 		<- "Year"
 
train.df 	<- df[1:450000,]
valid.df 	<- df[450001:463715,]
test.df		<- df[463716:nrow(df),]

mod.rf 		<- h2o.randomForest(x=feature.list, y=target, train.df, validation_frame=valid.df)
mod.gbm   	<- h2o.gbm(x=feature.list, y=target, train.df, validation_frame=valid.df)
mod.xgb   	<- h2o.xgboost(x=feature.list, y=target, train.df, validation_frame=valid.df)


rf.pred		<- h2o.predict(object=mod.rf, newdata=test.df)
gbm.pred	<- h2o.predict(object=mod.gbm, newdata=test.df)
xgb.pred        <- h2o.predict(object=mod.xgb, newdata=test.df)
 
mean(abs(rf.pred - test.df$Rings))
mean((rf.pred - test.df$Rings)^2)
mean(abs(gbm.pred - test.df$Rings))
mean((gbm.pred - test.df$Rings)^2)
mean(abs(xgb.pred - test.df$Rings))
mean((xgb.pred - test.df$Rings)^2)


mod.gbm         <- h2o.gbm(x=feature.list, y=target, train.df)
rf.pred         <- h2o.predict(object=mod.rf, newdata=test.df)
gbm.pred        <- h2o.predict(object=mod.gbm, newdata=test.df)
rf.pred         <- h2o.predict(object=mod.rf, newdata=test.df)
mean(abs(rf.pred - test.df$Rings))
#[1] 1.550467 
mean(abs(gbm.pred - test.df$Rings))
#[1] 1.515933


source("BlinkeredModel.R")
bmod <- BlinkeredModel(feature.list, target, train.df, valid.df, learning_rate=0.5, epochs=15)

bmod.pred	<- forecast(bmod, test.df)
mean(abs(bmod.pred - test.df$Rings))


source("BlinkeredTreeModel.R")
mod.bgbt <- BlinkeredGBTreeModel(feature.list, target, train.df, valid.df, learning_rate=0.3, epochs=15)

bgbt.pred       <- forecast(mod.bgbt, test.df)
mean(abs(bgbt.pred - as.data.frame(test.df)$Rings))


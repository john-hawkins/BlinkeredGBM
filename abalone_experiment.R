library(h2o)
h2o.init()

source("")

df 		<- h2o.importFile( path=normalizePath("data/abalone/Dataset.data") )
colnames(df) 	<- c('Sex', 'Length', 'Diameter', 'Height', 'Whole_weight', 'Shucked_weight', 'Viscera_weight', 'Shell_weight', 'Rings')

feature.list 	<- c('Sex', 'Length', 'Diameter', 'Height', 'Whole_weight', 'Shucked_weight', 'Viscera_weight', 'Shell_weight')
target 		<- "Rings"

train.df 	<- df[1:2500,]
valid.df 	<- df[2501:3133,]
test.df		<- df[3134:4177,]

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


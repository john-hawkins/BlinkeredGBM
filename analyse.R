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

rf.pred		<- h2o.predict(object=mod.rf, newdata=test.df)
gbm.pred	<- h2o.predict(object=mod.gbm, newdata=test.df)


mean(abs(rf.pred - test.df$Rings))
mean(abs(gbm.pred - test.df$Rings))

# NOW THE BLINKERED GB FOREST





library(rpart)
library(randomForest)
library(gbm)
library(data.table)

source("BlinkeredGBTreeModel.R")
source("BlinkeredGBLinearModel.R")

df 			<- fread("data/YearPredictionMSD/YearPredictionMSD.txt")
feat            	<- 1:90
featnames       	<- paste("feat", feat, sep="")
colnames(df)    	<- c('Year', featnames )

feature.list    	<- featnames
target          	<- "Year"

formu                   <-  as.formula(paste( target, "~", paste(feature.list, collapse = " + ")))

# DUE TO THE IMMENSE SIZE OF THIS DATA SET I NEEDED TO USE A MUCH SMALLER SUBSET SO THE RF WOULD  TRAIN IN REASOBALE TIME
train.df        	<- df[1:100000,]
valid.df        	<- df[100001:200000,]
test.df         	<- df[463716:nrow(df),]

mod.glm 		<- glm(formu, data=train.df)
mod.dt          	<- rpart(formu, data=train.df)
mod.rf			<- randomForest(formu, data=train.df, ntree=100)
mod.gbm			<- gbm(formu, data=train.df, n.trees=100, distribution="gaussian")
mod.bgbm                <- BlinkeredGBTreeModel(feature.list, target, train.df, valid.df )
mod.bgblm                <- BlinkeredGBLinearModel(feature.list, target, train.df, valid.df )

glm.preds		<- predict(mod.glm, test.df)
dt.preds        	<- predict(mod.dt, test.df)
rf.preds        	<- predict(mod.rf, test.df)
gbm.preds        	<- predict(mod.gbm, test.df, n.trees=100)
bgbm.preds              <- forecast(mod.bgbm, test.df)
bgblm.preds             <- forecast(mod.bgblm, test.df)

results.names           <- c("model/metric","GLM", "Decision Tree", "Random Forest", "GBM", "BGB Linear", "BGB Tree")

results.mae		<- c(
				"MAE",
				mean(abs(glm.preds - test.df$Year)),
				mean(abs(dt.preds - test.df$Year)),
				mean(abs(rf.preds - test.df$Year)),
				mean(abs(gbm.preds - test.df$Year)),
				mean(abs(bgblm.preds - test.df$Year)),
				mean(abs(bgbm.preds - test.df$Year))
			)
results.mape            <- c(
				"MAPE",
                                mean(100*abs(glm.preds - test.df$Year)/test.df$Year),
                                mean(100*abs(dt.preds - test.df$Year)/test.df$Year),
                                mean(100*abs(rf.preds - test.df$Year)/test.df$Year),
                                mean(100*abs(gbm.preds - test.df$Year)/test.df$Year),
                                mean(100*abs(bgbm.preds - test.df$Year)/test.df$Year),
                                mean(100*abs(bgblm.preds - test.df$Year)/test.df$Year)
                        )
results.mse             <- c(
				"MSE",
                                mean((glm.preds - test.df$Year)^2),
                                mean((dt.preds - test.df$Year)^2),
                                mean((rf.preds - test.df$Year)^2),
                                mean((gbm.preds - test.df$Year)^2),
                                mean((bgbm.preds - test.df$Year)^2),
                                mean((bgblm.preds - test.df$Year)^2)
                        )

results.tab		<- cbind(results.names, results.mae, results.mse, results.mape)

write.table(results.tab, file = "results/MSD_experiment.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)




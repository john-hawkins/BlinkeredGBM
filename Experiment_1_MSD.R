library(rpart)
library(randomForest)
library(gbm)
library(xgboost)
library(data.table)


df 			<- fread("data/YearPredictionMSD/YearPredictionMSD.txt")
feat            	<- 1:90
featnames       	<- paste("feat", feat, sep="")
colnames(df)    	<- c('Year', featnames )

feature.list    	<- featnames
target          	<- "Year"

formu                   <-  as.formula(paste( target, "~", paste(feature.list, collapse = " + ")))

train.df        	<- df[1:400000,]
valid.df        	<- df[400001:463715,]
test.df         	<- df[463716:nrow(df),]

mod.glm 		<- glm(formu, data=train.df)
mod.dt          	<- rpart(formu, data=train.df)
mod.rf			<- randomForest(formu, data=train.df, ntree=100)
mod.gbm			<- gbm(formu, data=train.df, n.trees=100, distribution="gaussian")

glm.preds		<- predict(mod.glm, test.df)
dt.preds        	<- predict(mod.dt, test.df)
rf.preds        	<- predict(mod.rf, test.df)
gbm.preds        	<- predict(mod.gbm, test.df, n.trees=100)

results.names		<- c("model/metric","GLM", "Decision Tree", "Random Forest", "GBM")
results.mae		<- c(
				"MAE",
				mean(abs(glm.preds - test.df$Year)),
				mean(abs(dt.preds - test.df$Year)),
				mean(abs(rf.preds - test.df$Year)),
				mean(abs(gbm.preds - test.df$Year))
			)
results.mape            <- c(
				"MAPE",
                                mean(100*abs(glm.preds - test.df$Year)/test.df$Year),
                                mean(100*abs(dt.preds - test.df$Year)/test.df$Year),
                                mean(100*abs(rf.preds - test.df$Year)/test.df$Year),
                                mean(100*abs(gbm.preds - test.df$Year)/test.df$Year)
                        )
results.mse             <- c(
				"MSE",
                                mean((glm.preds - test.df$Year)^2),
                                mean((dt.preds - test.df$Year)^2),
                                mean((rf.preds - test.df$Year)^2),
                                mean((gbm.preds - test.df$Year)^2)
                        )

results.tab		<- cbind(results.names, results.mae, results.mse, results.mape)

write.table(results.tab, file = "results/MSD_experiment.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)




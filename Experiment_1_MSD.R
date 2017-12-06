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

train.df        	<- df[1:450000,]
valid.df        	<- df[450001:463715,]
test.df         	<- df[463716:nrow(df),]

mod.glm 		<- glm(Year~., data=train.df)
mod.dt          	<- rpart(Year~., data=train.df)
mod.rf			<- randomForest(Year~., data=train.df)
mod.gbm			<- gbm(Year~., data=train.df, distribution = "gaussian")

glm.preds		<- predict(mod.glm, test.df)
dt.preds        	<- predict(mod.dt, test.df)
rf.preds        	<- predict(mod.rf, test.df)
gbm.preds        	<- predict(mod.gbm, test.df,n.trees=100)

results.names		<- c("model/metric","GLM", "Decision Tree", "Random Forest", "GBM")
results.mae		<- c(
				"MAE",
				mean(abs(glm.preds - test.local.df$Rings)),
				mean(abs(dt.preds - test.local.df$Rings)),
				mean(abs(rf.preds - test.local.df$Rings)),
				mean(abs(gbm.preds - test.local.df$Rings))
			)
results.mape            <- c(
				"MAPE",
                                mean(100*abs(glm.preds - test.local.df$Rings)/test.local.df$Rings),
                                mean(100*abs(dt.preds - test.local.df$Rings)/test.local.df$Rings),
                                mean(100*abs(rf.preds - test.local.df$Rings)/test.local.df$Rings),
                                mean(100*abs(gbm.preds - test.local.df$Rings)/test.local.df$Rings)
                        )
results.mse             <- c(
				"MSE",
                                mean((glm.preds - test.local.df$Rings)^2),
                                mean((dt.preds - test.local.df$Rings)^2),
                                mean((rf.preds - test.local.df$Rings)^2),
                                mean((gbm.preds - test.local.df$Rings)^2)
                        )

results.tab		<- cbind(results.names, results.mae, results.mse, results.mape)

write.table(results.tab, file = "results/abalone_experiment.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)




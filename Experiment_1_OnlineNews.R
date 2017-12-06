library(rpart)
library(randomForest)
library(gbm)
library(xgboost)
library(data.table)


df                	<- fread("data/OnlineNews/OnlineNewsPopularity.csv")

#
# Focus on Portuguese because there is more data
#

train.df        	<- df[1:20000,]
valid.df        	<- df[20001:24000,]
test.df         	<- df[24001:39644,]

feature.list 		<- colnames(df)[3:60]
target          	<- "shares"

formu			<-  as.formula(paste( target, "~", paste(feature.list, collapse = " + ")))

mod.glm 		<- glm(formu, data=train.df)
mod.dt          	<- rpart(formu, data=train.df)
mod.rf			<- randomForest(formu, data=train.df, ntree=100)
mod.gbm			<- gbm(formu, data=train.df, n.trees=100, distribution="gaussian")

glm.preds		<- predict(mod.glm, test.df)
dt.preds        	<- predict(mod.dt, test.df)
rf.preds        	<- predict(mod.rf, test.df)
gbm.preds        	<- predict(mod.gbm, test.df,n.trees=100)

results.names		<- c("model/metric","GLM", "Decision Tree", "Random Forest", "GBM")
results.mae		<- c(
				"MAE",
				mean(abs(glm.preds - test.df$shares)),
				mean(abs(dt.preds - test.df$shares)),
				mean(abs(rf.preds - test.df$shares)),
				mean(abs(gbm.preds - test.df$shares))
			)
results.mape            <- c(
				"MAPE",
                                mean(100*abs(glm.preds - test.df$shares)/test.df$shares),
                                mean(100*abs(dt.preds - test.df$shares)/test.df$shares),
                                mean(100*abs(rf.preds - test.df$shares)/test.df$shares),
                                mean(100*abs(gbm.preds - test.df$shares)/test.df$shares)
                        )
results.mse             <- c(
				"MSE",
                                mean((glm.preds - test.df$shares)^2),
                                mean((dt.preds - test.df$shares)^2),
                                mean((rf.preds - test.df$shares)^2),
                                mean((gbm.preds - test.df$shares)^2)
                        )

results.tab		<- cbind(results.names, results.mae, results.mse, results.mape)

write.table(results.tab, file = "results/online_news_experiment.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)




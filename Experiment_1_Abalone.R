library(rpart)
library(randomForest)
library(gbm)
library(xgboost)
library(data.table)

local.df 		<- fread("data/abalone/abalone.data")
colnames(local.df)    	<- c('Sex', 'Length', 'Diameter', 'Height', 'Whole_weight', 'Shucked_weight', 'Viscera_weight', 'Shell_weight', 'Rings')
local.df$Sex		<- as.factor(local.df$Sex)


train.local.df        	<- local.df[1:2500,]
valid.local.df        	<- local.df[2501:3133,]
test.local.df         	<- local.df[3134:4177,]

feature.list    	<- c('Sex', 'Length', 'Diameter', 'Height', 'Whole_weight', 'Shucked_weight', 'Viscera_weight', 'Shell_weight')
target          	<- "Rings"

mod.glm 		<- glm(Rings~., data=train.local.df)
mod.dt          	<- rpart(Rings~., data=train.local.df)
mod.rf			<- randomForest(Rings~., data=train.local.df)
mod.gbm			<- gbm(Rings~., data=train.local.df, distribution = "gaussian")

glm.preds		<- predict(mod.glm, test.local.df)
dt.preds        	<- predict(mod.dt, test.local.df)
rf.preds        	<- predict(mod.rf, test.local.df)
gbm.preds        	<- predict(mod.gbm, test.local.df,n.trees=100)

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




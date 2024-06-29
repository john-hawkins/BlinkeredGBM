#
# EXPERIMENT TWO
#
# GIVEN A REGRESSION DATA SET WITH TRAIN,VALID AND TEST SPLITS
# RUN A SERIES OF MODELS WITH A SIMPLE GRID SEARCH OVER COMMONLY
# TUNED META-PARAMETERS.
#
# THE GOAL IS TO SEE HOW THE MODELS PERFORMANCES RANK AFTER A TYPICAL
# ROUND OF META-PARAMETER TUNING
#
library(caret)
library(rpart)
library(randomForest)
library(gbm)
library(data.table)
source("BlinkeredGBTreeModel.R")


runExperimentTwo	<- function(feature.list, target, train.df, valid.df, test.df, outputfile ) {

	formu                   <-  as.formula(paste( target, "~", paste(feature.list, collapse = " + ")))

	#
	# WE USE THE CARET GRID SEARCH TO TEST A RANGE OF META-PARAMETERS

	control 		<- trainControl(method="repeatedcv", number=10, repeats=3)
	grid 			<- expand.grid( cp =c(0.1, 0.05, 0.03, 0.01, 0.005) )
	mod.dt 			<- train( formu, data=train.df, method="rpart", trControl=control, tuneGrid=grid)
	#print(mod.dt)

	mod.rf			<- randomForest(formu, data=train.df)
	mod.gbm			<- gbm(formu, data=train.df, distribution = "gaussian")

	mod.bgbm		<- BlinkeredGBTreeModel(feature.list, target, train.df, valid.df )
 
	dt.preds        	<- predict(mod.dt, test.df)
	rf.preds        	<- predict(mod.rf, test.df)
	gbm.preds        	<- predict(mod.gbm, test.df,n.trees=100)
	bgbm.preds               <- forecast(mod.bgbm, test.df)

	results.names		<- c("model/metric","GLM", "Decision Tree", "Random Forest", "GBM", "BGBM")
	results.mae		<- c(
				"MAE",
				mean(abs(glm.preds - test.df[[target]])),
				mean(abs(dt.preds - test.df[[target]])),
				mean(abs(rf.preds - test.df[[target]])),
				mean(abs(gbm.preds - test.df[[target]])),
				mean(abs(bgbm.preds - test.df[[target]]))
			)
	results.mape            <- c(
				"MAPE",
                                mean(100*abs(glm.preds - test.df[[target]])/(test.df[[target]]+1)),
                                mean(100*abs(dt.preds - test.df[[target]])/(test.df[[target]]+1)),
                                mean(100*abs(rf.preds - test.df[[target]])/(test.df[[target]]+1)),
                                mean(100*abs(gbm.preds - test.df[[target]])/(test.df[[target]]+1)),
                                mean(100*abs(bgbm.preds - test.df[[target]])/(test.df[[target]]+1))
                        )
	results.mse             <- c(
				"MSE",
                                mean((glm.preds - test.df[[target]])^2),
                                mean((dt.preds - test.df[[target]])^2),
                                mean((rf.preds - test.df[[target]])^2),
                                mean((gbm.preds - test.df[[target]])^2),
                                mean((bgbm.preds - test.df[[target]])^2)
                        )

	results.tab		<- cbind(results.names, results.mae, results.mse, results.mape)

	write.table(results.tab, file=outputfile, append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)
}


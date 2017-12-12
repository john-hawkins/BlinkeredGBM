#
# EXPERIMENT ONE
#
# GIVEN A REGRESSION DATA SET WITH TRAIN,VALID AND TEST SPLITS
# RUN A SERIES OF MODELS USING DEFAULT PARAMETERS AND
# COMPARE THE REUSLTS USING SEVERAL STANDARD METRICS.
#
# THE GOAL IS TO SEE HOW THE MODELS RANK UNDER THE PRESUMPTION
# THAT YOU WANT TO BUILD A MODEL QUICKLY, AND BELIEVE THAT META-PARAMETER
# OPTIMISATION WILL ONLY RESULT IN MODEST GAINS
#


library(rpart)
library(randomForest)
library(gbm)
library(data.table)
source("BlinkeredGBTreeModel.R")


runExperimentOne	<- function(feature.list, target, train.df, valid.df, test.df, outputfile ) {

 
	formu                   <-  as.formula(paste( target, "~", paste(feature.list, collapse = " + ")))

	mod.glm 		<- glm(formu, data=train.df)
	mod.dt          	<- rpart(formu, data=train.df)
	mod.rf			<- randomForest(formu, data=train.df)
	mod.gbm			<- gbm(formu, data=train.df, distribution = "gaussian")
	mod.bgbm		<- BlinkeredGBTreeModel(feature.list, target, train.df, valid.df )
 
	glm.preds		<- predict(mod.glm, test.df)
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
	results.tab
	#	write.table(results.tab, file=outputfile, append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)
}


library(rpart)
library(randomForest)
library(gbm)
library(xgboost)
library(data.table)
 
d1			<- read.table("data/StudentPerf/student-mat.csv",sep=";",header=TRUE)
d2			<- read.table("data/StudentPerf/student-por.csv",sep=";",header=TRUE)
 
df			<- merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

print(nrow(d1)) # 395 students with math
print(nrow(d2)) # 649 students with portuguese
print(nrow(df)) # 382 students with both math and portuguese

#
# Focus on Portuguese because there is more data
#

train.df        	<- d2[1:500,]
valid.df        	<- d2[501:550,]
test.df         	<- d2[551:649,]


#
# Predict G3 without G1 or G2 (This is a harder task)
feature.list    	<- c(
				'school', 'sex', 'age', 'address', 'famsize', 'Pstatus', 'Medu', 'Fedu', 'Mjob', 'Fjob', 'reason', 'nursery'
				, 'internet', 'guardian', 'traveltime', 'studytime', 'failures', 'schoolsup', 'amsup', 'paid', 'activities'
				, 'higher', 'romantic', 'famrel', 'freetime', 'goout', 'Dalc', 'Walc', 'health', 'absences')
target          	<- "G3"

formu                   <-  as.formula(paste( target, "~", paste(feature.list, collapse = " + ")))

mod.glm 		<- glm(formu, data=train.df)
mod.dt          	<- rpart(formu, data=train.df)
mod.rf			<- randomForest(formu, data=train.df)
mod.gbm			<- gbm(formu, data=train.df, distribution = "gaussian")

glm.preds		<- predict(mod.glm, test.df)
dt.preds        	<- predict(mod.dt, test.df)
rf.preds        	<- predict(mod.rf, test.df)
gbm.preds        	<- predict(mod.gbm, test.df,n.trees=100)

results.names		<- c("model/metric","GLM", "Decision Tree", "Random Forest", "GBM")
results.mae		<- c(
				"MAE",
				mean(abs(glm.preds - test.df$G3)),
				mean(abs(dt.preds - test.df$G3)),
				mean(abs(rf.preds - test.df$G3)),
				mean(abs(gbm.preds - test.df$G3))
			)
results.mape            <- c(
				"MAPE",
                                mean(100*abs(glm.preds - test.df$G3)/test.df$G3),
                                mean(100*abs(dt.preds - test.df$G3)/test.df$G3),
                                mean(100*abs(rf.preds - test.df$G3)/test.df$G3),
                                mean(100*abs(gbm.preds - test.df$G3)/test.df$G3)
                        )
results.mse             <- c(
				"MSE",
                                mean((glm.preds - test.df$G3)^2),
                                mean((dt.preds - test.df$G3)^2),
                                mean((rf.preds - test.df$G3)^2),
                                mean((gbm.preds - test.df$G3)^2)
                        )

results.tab		<- cbind(results.names, results.mae, results.mse, results.mape)

write.table(results.tab, file = "results/student_experiment.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)




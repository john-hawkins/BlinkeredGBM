library(data.table)
source("Experiment_1.R")
 
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
				, 'internet', 'guardian', 'traveltime', 'studytime', 'failures', 'schoolsup', 'famsup', 'paid', 'activities'
				, 'higher', 'romantic', 'famrel', 'freetime', 'goout', 'Dalc', 'Walc', 'health', 'absences')
target          	<- "G3"

runExperimentOne(feature.list, target, train.df, valid.df, test.df, "Exp_1_StudentPerf.csv" ) 

runExperimentTwo(feature.list, target, train.df, valid.df, test.df, "Exp_2_StudentPerf.csv" ) 


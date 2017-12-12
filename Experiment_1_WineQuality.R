library(data.table)

source("Experiment_1.R")
 
df			<- read.table("data/WineQuality/data.csv",sep=";", header=TRUE)

colnames(df) 		<- c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar", 
				"chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", "density",
				"pH", "sulphates", "alcohol", "quality")
 
train.df        	<- df[1:3500,]
valid.df        	<- df[3501:4000,]
test.df         	<- df[4001:4898,]

feature.list    	<- c(
				"fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar", 
                                "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", "density",
                                "pH", "sulphates", "alcohol"
				)
target          	<- "quality"

 
results.one 		<- runExperimentOne(feature.list, target, train.df, valid.df, test.df )

writeReusltsTab(results.one, "Exp_1_WineQuality.csv" )


# runExperimentTwo(feature.list, target, train.df, valid.df, test.df, "Exp_2_WineQuality.csv" )



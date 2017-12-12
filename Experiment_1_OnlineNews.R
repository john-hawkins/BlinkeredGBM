library(data.table)
source("Experiment_1.R")

df                	<- fread("data/OnlineNews/OnlineNewsPopularity.csv")

# Focus on Portuguese because there is more data

train.df        	<- df[1:20000,]
valid.df        	<- df[20001:24000,]
test.df         	<- df[24001:39644,]

feature.list 		<- colnames(df)[3:60]
target          	<- "shares"

results.one             <- runExperimentOne(feature.list, target, train.df, valid.df, test.df )

writeReusltsTab(results.one, "results/Exp_1_OnlineNews.csv" )



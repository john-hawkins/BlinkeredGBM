library(data.table)
source("Experiment_1.R")

local.df 		<- fread("data/abalone/abalone.data")
colnames(local.df)    	<- c('Sex', 'Length', 'Diameter', 'Height', 'Whole_weight', 'Shucked_weight', 'Viscera_weight', 'Shell_weight', 'Rings')
local.df$Sex		<- as.factor(local.df$Sex)


train.df        	<- local.df[1:2500,]
valid.df        	<- local.df[2501:3133,]
test.df         	<- local.df[3134:4177,]

feature.list    	<- c('Sex', 'Length', 'Diameter', 'Height', 'Whole_weight', 'Shucked_weight', 'Viscera_weight', 'Shell_weight')
target          	<- "Rings"


results.one             <- runExperimentOne(feature.list, target, train.df, valid.df, test.df )

writeReusltsTab(results.one, "results/Exp_1_Abalone.csv" )




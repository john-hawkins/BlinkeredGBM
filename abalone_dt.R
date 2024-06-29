
library(rpart)
library(data.table)

local.df <- fread("data/abalone/Dataset.data")
colnames(local.df)    <- c('Sex', 'Length', 'Diameter', 'Height', 'Whole_weight', 'Shucked_weight', 'Viscera_weight', 'Shell_weight', 'Rings')


train.local.df        <- local.df[1:2500,]
valid.local.df        <- local.df[2501:3133,]
test.local.df         <- local.df[3134:4177,]

feature.list    <- c('Sex', 'Length', 'Diameter', 'Height', 'Whole_weight', 'Shucked_weight', 'Viscera_weight', 'Shell_weight')
target          <- "Rings"


mod.dt          <- rpart(Rings~., data=train.local.df)

dt.preds	<- predict(mod.dt, test.local.df)

mean(abs(dt.preds - test.local.df$Rings))




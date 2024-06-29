######################################################################
# Blinkered Gradient Boosted Forest Model
#
# Take an H2O dataframe with features and a target column.
# Then builds a composite model from Random Forests. Each Gradient Boosting
# Layer will ignore a subset of the data determined by the with of the blinker.
#
# The scoring function contains additional meta-parameters
# 1) Shrinkage - How much to rely on the error correction when generating a 
#            a new forecast, (i.e. how much to shrink the correction applied
#		by each gradient boosting layer
######################################################################
setClass(
   "BlinkeredGBFModel",
   slots = c(
      features = "character",
      target = "character",
      regressors = "H2ORegressionModel"
   )
)


# ####################################################################################
# CONSTRUCTOR - BUILD MODEL USING RANDOM FORESTS 
# ####################################################################################
BlinkeredGBFModel = function(x, y, trainset, validset ) {



   # FIRST WE ADD A COLUMN CONTAINING THE ERROR
   errcol="baseline_model_error"
   trn.err     	<- trainset[[baseline]] - trainset[[y]]
   names(trn.err)	<- c(errcol)
   training      	<- h2o.cbind(trainset, trn.err)
   vd.err           <- validset[[baseline]] - validset[[y]]
   names(vd.err)    <- c(errcol)
   validation      <- h2o.cbind(validset, vd.err)	
   # NOW TRAIN THE REGRESSION MODELS FOR THE SEGMENTS
   regressr  <- h2o.randomForest(x=x, y=errcol, training, validation_frame=validation, max_depth=15)               
   new("ErrCorrModel", features=x, target=y, baseline=baseline, regressor=regressr )
}

# ####################################################################################
# CONSTRUCTOR - BUILD MODEL USING RANDOM FOREST (NO VALIDATION SET)
#             - CREATE ERROR CORRECTOR MODEL FROM THE SPECIFIED DATA
# ####################################################################################
ErrCorrModel = function(x, y, baseline, trainset ) {
   # FIRST WE ADD A COLUMN CONTAINING THE ERROR
   errcol="baseline_model_error"
   trn.err      <- trainset[[baseline]] - trainset[[y]]
   names(trn.err)       <- c(errcol)
   training             <- h2o.cbind(trainset, trn.err)
   # NOW TRAIN THE REGRESSION MODELS FOR THE SEGMENTS
   regressr  <- h2o.randomForest(x=x, y=errcol, training, max_depth=15)
   new("ErrCorrModel", features=x, target=y, baseline=baseline, regressor=regressr )
}

# ########################################################################
# SCORING FUNCTION FOR THE MODEL
# ########################################################################
setGeneric(
   name="forecast",
   def=function(theObject,...) {
      standardGeneric("forecast")
   }
)

setMethod(
        f="forecast",
        signature="ErrCorrModel",
        definition=function(theObject, dataset, shrinkage=0.5 ) {
           regressFcasts	<- h2o.predict(object=theObject@regressor, newdata=dataset)
	   finalPred 		<- dataset[[theObject@baseline]] - shrinkage*regressFcasts
 	   return(finalPred)
        }
)

# ########################################################################
# SERIALIZATION
# ########################################################################
setGeneric(
   name="saveModel",
   def=function(theObject,...) {
      standardGeneric("saveModel")
   }
)
setMethod(
        f="saveModel",
        signature="ErrCorrModel",
        definition=function( theObject, model_path, h2o_path ) {
		h2oModelFile <- h2o.saveModel(object=theObject@regressor, path=h2o_path, force=TRUE)
		fileConn<-file(model_path)
		writeLines(c(h2oModelFile, theObject@target, theObject@baseline, theObject@features ), fileConn)
		close(fileConn)
        }
)

# ########################################################################
# LOADING MODELS
# ########################################################################

loadErrCorrModel = function(model_path) {
	con=file(model_path,open="r")
	line=readLines(con)
	regressr <- h2o.loadModel(line[1])	
	new("ErrCorrModel", features=line[4:length(line)], target=line[2], baseline=line[3], regressor=regressr )
}


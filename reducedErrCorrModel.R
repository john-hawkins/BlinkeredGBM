######################################################################
# Segmented Error Correction Model
#
# Take a H2O dataframe with features, target and a baseline model.
# Then build a series of models designed to predict the error in a baseline model.
# However, we apply blinkers, meaning that we ignore error that is beyond the specified
# boundaries. 
#
######################################################################
setClass(
        "BlinkrdErrCorrModel",
        slots = c(
                splits = "numeric",
                features = "character",
                target = "character",
                baseline = "character",
                splitters   = "list",
                regressors  = "list",
		learning_rate = "numeric",
		delta = "numeric",
		tolerance = "numeric"
        )
)


# ####################################################################################
# CONSTRUCTOR - CREATE THE ARRAY OF MODELS FROM SPECIFIED DATA
# USE RANDOM FOREST AS THE BASE LEARNER
# ####################################################################################
BlinkrdErrCorrModel = function(splits, x, y, baseline, trainset, validset, learning_rate=0.1, delta=NULL, tolerance=0.1, epochs=10) {
        if(length(splits)<1 || class(splits)!="numeric") {
                print("Splits must be a vector of numeric starting points for blinkering.")
        } else {

		# CALCULATE STARTING ERROR
                trn.err     	<- trainset[[baseline]] - trainset[[y]]
                vd.err           <- validset[[baseline]] - validset[[y]]

		baseline.trn.mae	<- mean(abs(trn.err))
		baseline.vd.mae		<- mean(abs(vd.err))
		last.trn.mae		= baseline.trn.mae
		last.vd.mae		= baseline.vd.mae
		improvement 		= tolerance
		min.blinker		= min(abs(splits))
		forecast.col 		= baseline		
		forecast.col.stub	= forecast.col
		errcol 			= "model_error_target"
		errcol.stub		= errcol
		
		nmodz			= 0
                gammaz	 	      	= list()
                splitterz       	= list()
                regressrz       	= list()

		training 		= trainset
		validation 		= validset
                if(is.na(delta)) {
			delta = min.blinker/(epochs+1)
                }
		# ADD THE ERROR AS A TARGET COLUMN
		trn.err         	<- training[[forecast.col]] - training[[y]]
                names(trn.err)  	<- c(errcol)
                training        	<- h2o.cbind(training, trn.err)
                vd.err           	<- validation[[forecast.col]] - validation[[y]]
                names(vd.err)    	<- c(errcol)
                validation      	<- h2o.cbind(validation, vd.err)

		# WE ITERATE THROUGH THE LEARNING PROCESS WHILE EVER THE
		# MODEL IS STILL IMPROVING AND THE SMALLEST BLINKER IS GREATER THAN ZERO
		# WITH MAXIMUM ITERATIONS SET BY EPOCHS
		while( improvement >= tolerance && min.blinker > 0 && nmodz < epochs) {
			nmodz = nmodz + 1 
			### NOW THAT THE DATA SETS HAVE COLUMNS FOR THE CURRENT ERROR
			### WE TRAIN A CLASSIFIER TO PREDICT IF THE ERROR IS WITHIN THE BLINKERS

			### CREATE THE LABELS
                	segcol="error_segment"
                	trn.seg         <- ifelse(
                                                training[[errcol]]<splits[1],
                                                'below',
                                                ifelse(
                                                        training[[errcol]]>splits[length(splits)],
                                                        'above',
                                                        'between'
                                                )
                                        )
                	names(trn.seg)  <- c(segcol)
                	seg.training        <- h2o.cbind(training, trn.seg)
                	vd.seg         <- ifelse(
                                                validation[[errcol]]<splits[1],
                                                'below',
                                                ifelse(
                                                        validation[[errcol]]>splits[length(splits)],
                                                        'above',
                                                        'between'
                                                )
                                        )
                	names(vd.seg)  <- c(segcol)
                	seg.validation        <- h2o.cbind(validation, vd.seg)

			# NOW TRAIN A MULTINOMIAL CLASSIFICATION MODEL TO PREDICT THE ERROR SEGMENT
			splitterz[nmodz]  <- h2o.randomForest(x=x, y=segcol, seg.training, validation_frame=seg.validation)

			# FINALLY TRAIN A REGRESSION MODEL FOR THE MIDDLE SEGMENT
		        temp.within.train       <- training[training[[errcol]]>=splits[1] && training[[errcol]]<=splits[length(splits)], ]
                	temp.within.valid       <- validation[validation[[errcol]]>=splits[1] && validation[[errcol]]<=splits[length(splits)], ]	
			regressrz[nmodz]  	<- h2o.randomForest(x=x, y=errcol, max_depth=10, temp.within.train, validation_frame=temp.within.valid)			
			
			### NOW WE GENERATE THE FORECASTS AND ERROR FOR THE NEW MODEL AND ADD THEM AS COLUMNS
			trn.seg.preds	<- predict( object=splitterz[[nmodz]], newdata=training)
			vd.seg.preds	<- predict( object=splitterz[[nmodz]], newdata=validation)
			trn.reg.preds	<- predict( object=regressrz[[nmodz]], newdata=training)
			vd.reg.preds	<- predict( object=regressrz[[nmodz]], newdata=validation)
			
			#gammaz[nmodz]	<- fitGamma(training[[forecast.col]], learning_rate*(trn.seg.preds$between * trn.reg.preds) )

			trn.fcast	<- training[[forecast.col]] - learning_rate*(trn.seg.preds$between * trn.reg.preds)
			vd.fcast        <- validation[[forecast.col]] - learning_rate*(vd.seg.preds$between * vd.reg.preds)

			forecast.col	<- paste(forecast.col.stub, nmodz, sep='') 
			errcol    	<- paste(errcol.stub, nmodz, sep='')

			names(trn.fcast) <- c(forecast.col)
			names(vd.fcast) <- c(forecast.col)
                	trn.err         <- trn.fcast - training[[y]]
                	vd.err          <- vd.fcast - validation[[y]]
                	names(trn.err)  <- c(errcol)
                	names(vd.err)   <- c(errcol)

                	training        <- h2o.cbind(training, trn.fcast, trn.err)
                	validation      <- h2o.cbind(validation, vd.fcast, vd.err)

                	trn.mae        	<- mean(abs(trn.err))
                	vd.mae         	<- mean(abs(vd.err))
			improvement 	<- last.vd.mae - vd.mae
			splits[[1]]	<- splits[[1]] + delta
			splits[[2]]	<- splits[[2]] - delta
			min.blinker     <-  min(abs(splits))	
		}

                new("BlinkrdErrCorrModel", splits=splits, features=x, target=y, baseline=baseline, splitters=splitterz, regressors=regressrz )
        }
}

# ########################################
# FIND AN OPTIMAL GAMMA WEIGHT
# ########################################
fitGamma	<- function(targets, basemodel, gbmodel) {
#	names(targets)		<- c("target")
#	names(basemodel)	<- c("base")
#	names(gbmodel)		<- c("booster")
#	df			<- h2o.cbind(targets, basemodel, gbmodel)
#	lformula		<- target ~ base + booster
#	lfit			<- glm(lformula, df)
#	 	
}

# ####################################################################################
# CONSTRUCTOR FOR RANDOM FOREST - CREATE THE ARRAY OF MODELS FROM SPECIFIED DATA
# ####################################################################################
SegErrCorrGBM = function(splits, x, y, baseline, trainset, validset, softmaxbias=0.5, regressOnForecasts=TRUE) {
        if(length(splits)<1 || class(splits)!="numeric") {
                print("Splits must be a vector of numeric split points to apply to the target values")
        } else {
                # FIRST WE ADD A COLUMN CONTAINING THE ERROR
                errcol="baseline_model_error"
                trn.err         <- trainset[[baseline]] - trainset[[y]]
                names(trn.err)  <- c(errcol)
                training        <- h2o.cbind(trainset, trn.err)
                vd.err           <- validset[[baseline]] - validset[[y]]
                names(vd.err)    <- c(errcol)
                validation      <- h2o.cbind(validset, vd.err)

                # NOW A COLUMN WITH THE TARGET SEGMENTS
                # FOR THE MOMENT THIS WILL ONLY USE THE FIRST AND LAST SPLIT POINTS
                # TODO: VECTORIZE A LOOP OVER ALL SPLIT POINTS TO CREATE AN ARBITRARY MULTINOMIAL TARGET
                segcol="error_segment"
                trn.seg         <- ifelse(
                                                training[[errcol]]<splits[1],
                                                'below',
                                                ifelse(
                                                        training[[errcol]]>splits[length(splits)],
                                                        'above',
                                                        'between'
                                                )
                                        )
                names(trn.seg)  <- c(segcol)
                seg.training        <- h2o.cbind(training, trn.seg)
                vd.seg         <- ifelse(
                                                validation[[errcol]]<splits[1],
                                                'below',
                                                ifelse(
                                                        validation[[errcol]]>splits[length(splits)],
                                                        'above',
                                                        'between'
                                                )
                                        )
                names(vd.seg)  <- c(segcol)
                seg.validation        <- h2o.cbind(validation, vd.seg)

                # NOW TRAIN A MULTINOMIAL CLASSIFICATION MODEL TO PREDICT THE ERROR SEGMENT
                segmodel  <- h2o.gbm(x=x, y=segcol, seg.training, validation_frame=seg.validation)

                # NOW TRAIN THE REGRESSION MODELS FOR THE SEGMENTS

                temp.below.train      <- training[training[[errcol]]<splits[1], ]
                temp.above.train      <- training[training[[errcol]]>splits[length(splits)], ]
                temp.within.train      <- training[training[[errcol]]>=splits[1] && training[[errcol]]<=splits[length(splits)], ]
                temp.below.valid      <- validation[validation[[errcol]]<splits[1], ]
                temp.above.valid      <- validation[validation[[errcol]]>splits[length(splits)], ]
                temp.within.valid      <- validation[validation[[errcol]]>=splits[1] && validation[[errcol]]<=splits[length(splits)], ]

                regressrz       = list()
                regressrz[1]  <- h2o.gbm(x=x, y=errcol, temp.below.train, validation_frame=temp.below.valid)
                regressrz[2]  <- h2o.gbm(x=x, y=errcol, temp.within.train, validation_frame=temp.within.valid)
                regressrz[3]  <- h2o.gbm(x=x, y=errcol, temp.above.train, validation_frame=temp.above.valid)

                new("SegErrCorrModel", splits=splits, features=x, target=y, baseline=baseline, splitter=list(segmodel), regressors=regressrz )
        }
}

# ########################################################################
# SCORING FUNCTION FOR THE MODEL
# ########################################################################

setGeneric(
        name="forecast",
        def=function(theObject,...)
        {
                standardGeneric("forecast")
        }
)

setMethod(
        f="forecast",
        signature="SegErrCorrModel",
        definition=function(theObject, dataset, trust=0.5, softMaxBias=0.5, ignoreOutliers=FALSE)
	{
		regressFcasts       	= list()
                nmodz           	= length(theObject@regressors)
		segpreds		= h2o.predict(object=theObject@splitter[[1]], newdata=dataset)
                for(mdel in 1:nmodz) {
                   regressFcasts[[mdel]]	<- h2o.predict(object=theObject@regressors[[mdel]], newdata=dataset)
                }
		if(ignoreOutliers) {
                        smaxPred  <- ifelse(  segpreds$predict=='between', regressFcasts[[2]], 0 )
                        wtdPred   <- segpreds$between * regressFcasts[[2]] 
                        cmbPred   <- softMaxBias*smaxPred + (1-softMaxBias)*wtdPred
                        finalPred <- dataset[[theObject@baseline]] - trust*cmbPred
                        return(finalPred)
		} else {
                	smaxPred   <- ifelse(
                        	segpreds$predict=='below',regressFcasts[[1]], ifelse(
                        	        segpreds$predict=='above',regressFcasts[[3]], regressFcasts[[2]]
                	        )
                	)
                	wtdPred   <- segpreds$below * regressFcasts[[1]] + segpreds$between * regressFcasts[[2]] + segpreds$above * regressFcasts[[3]]
                	cmbPred   <- softMaxBias*smaxPred + (1-softMaxBias)*wtdPred
                	finalPred <- dataset[[theObject@baseline]] - trust*cmbPred
                	return(finalPred)
		}
        }
)

# #########################################################################




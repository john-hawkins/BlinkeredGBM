############################################################
#
###########################################################
#setClass(
#        "BlinkeredModel",
#        slots = c(
#                features	= "character",
#                target 		= "character",
#		basemodel 	= "H2ORegressionModel",
#                splitters   	= "H2OBinomialModel",
#                regressors  	= "H2ORegressionModel",
#		gammas		= "numeric",
#		learning_rate 	= "numeric",
#		blinker 	= "numeric",
#		delta 		= "numeric",
#		tolerance 	= "numeric",
#		epochs 		= "numeric"
#        )
#)

setClass(
        "BlinkeredModel",
        slots = c(
                features        = "character",
                target          = "character",
                basemodel       = "H2ORegressionModel",
                splitters       = "list",
                regressors      = "list",
                gammas          = "list",
                learning_rate   = "numeric",
                blinker         = "numeric",
                delta           = "numeric",
                tolerance       = "numeric",
                epochs          = "numeric"
        )
)

#
#learning_rate=0.1 
#tolerance=0.1 
#epochs=5
#
#x=feature.list
#y=target
#trainset=train.df
#validset=valid.df
#
##############################################################################
# Constructor
#############################################################################
BlinkeredModel = function(x, y, trainset, validset, learning_rate=0.2, blinker=NULL, delta=NULL, tolerance=0.001, epochs=5) {

	# CREATE THE INITIAL MODEL
	base.model		<- h2o.randomForest(x=x, y=y, trainset, validation_frame=validset)
	preds.train		<- predict( object=base.model, newdata=trainset)
	preds.valid		<- predict( object=base.model, newdata=validset)
	baseline		<- "base_model"
	names(preds.train)	<- c(baseline)
	names(preds.valid)	<- c(baseline)
	trainset 	        <- h2o.cbind(trainset, preds.train)
	validset 	        <- h2o.cbind(validset, preds.valid)

	# CALCULATE STARTING ERROR
        trn.err     		<- trainset[[baseline]] - trainset[[y]]
        vd.err           	<- validset[[baseline]] - validset[[y]]

	if(is.null(blinker)) {
		quants 		<- quantile(abs(trn.err), c(.1, .9))
		blinker 	<- quants[[2]] 
		delta           <- (quants[[2]]-quants[[1]])/epochs
	}
	cur.blinker 		<- blinker
	baseline.trn.mae	<- mean(abs(trn.err))
	baseline.vd.mae		<- mean(abs(vd.err))
	last.trn.mae		= baseline.trn.mae
	last.vd.mae		= baseline.vd.mae
	improvement 		= tolerance
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
		
	# ADD THE ERROR AS A TARGET COLUMN
	trn.err         	<- training[[forecast.col]] - training[[y]]
        names(trn.err)  	<- c(errcol)
        training        	<- h2o.cbind(training, trn.err)
        vd.err           	<- validation[[forecast.col]] - validation[[y]]
        names(vd.err)    	<- c(errcol)
        validation      	<- h2o.cbind(validation, vd.err)

	# WE ITERATE THROUGH THE LEARNING PROCESS WHILE EVER THE
	# MODEL IS STILL IMPROVING AND THE BLINKER IS GREATER THAN ZERO
	# WITH MAXIMUM ITERATIONS SET BY EPOCHS
	while( improvement >= tolerance && cur.blinker > 0 && nmodz < epochs) {
		nmodz = nmodz + 1 

		### WE TRAIN A CLASSIFIER TO PREDICT IF THE ERROR IS WITHIN THE BLINKERS
		### CREATE THE LABELS
               	segcol="error_segment"
               	trn.seg         <- ifelse(
                                         training[[errcol]] > cur.blinker,
                                         'outside',
                                         ifelse(
                                                 training[[errcol]] < (-cur.blinker),
                                                'outside',
                                                'between'
                                         )
                                   )
               	names(trn.seg)  <- c(segcol)
               	seg.training 	<- h2o.cbind(training, trn.seg)
               	vd.seg   	<- ifelse(
                                          validation[[errcol]] > cur.blinker,
                                          'outside',
                                          ifelse(
                                                 validation[[errcol]] < -cur.blinker,
                                                 'outside',
                                                 'between'
                                          )
                                   )
              	names(vd.seg)  <- c(segcol)
               	seg.validation        	<- h2o.cbind(validation, vd.seg)

		# NOW TRAIN A MULTINOMIAL CLASSIFICATION MODEL TO PREDICT THE ERROR SEGMENT
		splitterz[nmodz]  	<- h2o.randomForest(x=x, y=segcol, seg.training, validation_frame=seg.validation)

		# FINALLY TRAIN A REGRESSION MODEL FOR THE MIDDLE SEGMENT
	        temp.within.train       <- training[training[[errcol]] >= -cur.blinker && training[[errcol]] <= cur.blinker, ]
               	temp.within.valid       <- validation[validation[[errcol]] >= -cur.blinker && validation[[errcol]] <= cur.blinker, ]	
		regressrz[nmodz]  	<- h2o.randomForest(x=x, y=errcol, max_depth=10, temp.within.train, validation_frame=temp.within.valid)			
			
		### NOW WE GENERATE THE FORECASTS AND ERROR FOR THE NEW MODEL AND ADD THEM AS COLUMNS
		trn.seg.preds	<- predict( object=splitterz[[nmodz]], newdata=training)
		vd.seg.preds	<- predict( object=splitterz[[nmodz]], newdata=validation)
		trn.reg.preds	<- predict( object=regressrz[[nmodz]], newdata=training)
		vd.reg.preds	<- predict( object=regressrz[[nmodz]], newdata=validation)
		
		# WE SET GAMMA TO BE ONE FOR THE MOMENT
		gammaz[nmodz]	<- 1 

		trn.fcast	<- training[[forecast.col]] - learning_rate * gammaz[[nmodz]] * (trn.seg.preds$between * trn.reg.preds)
		vd.fcast        <- validation[[forecast.col]] - learning_rate * gammaz[[nmodz]] * (vd.seg.preds$between * vd.reg.preds)

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
		cur.blinker 	<- cur.blinker  - delta
		print(paste("Epoch ", nmodz, " Improvement: ", improvement, sep=""))
	}

        new("BlinkeredModel", features=x, target=y, 
		basemodel=base.model, 
		splitters=splitterz, 
		regressors=regressrz,
		gammas=gammaz, 
		learning_rate=learning_rate, 
		blinker=blinker, 
		delta=delta, 
		tolerance=tolerance, 
		epochs=epochs 
	)
}


#####################################################################
# Forecast
####################################################################

setGeneric(
        name="forecast",
        def=function(theObject,...)
        {
                standardGeneric("forecast")
        }
)

setMethod(
        f="forecast",
        signature="BlinkeredModel",
        definition=function(theObject, dataset, shrinkage=0.5, softMaxBias=0.5)
	{
		splitterFcasts          	= list()
		regressFcasts       		= list()
                totalFcasts           		= list()
                nmodz           		= length(theObject@regressors)
		basepreds			= h2o.predict(object=theObject@basemodel, newdata=dataset)
		currpred			= basepreds
                for(mdel in 1:nmodz) {
                   splitterFcasts[[mdel]]	<- h2o.predict(object=theObject@splitters[[mdel]], newdata=dataset)
                   regressFcasts[[mdel]]	<- h2o.predict(object=theObject@regressors[[mdel]], newdata=dataset)

                   # smaxPred  <- ifelse(  segpreds$predict=='between', regressFcasts[[2]], 0 )
                   # wtdPred   <- segpreds$between * regressFcasts[[2]]
                   # cmbPred   <- softMaxBias*smaxPred + (1-softMaxBias)*wtdPred

		   totalFcasts[[mdel]]		<- currpred - ( 
				theObject@learning_rate * theObject@gammas[[mdel]] * ( splitterFcasts[[mdel]]$between * regressFcasts[[mdel]] )
			)
		   currpred 			<- totalFcasts[[mdel]]
                }

        	totalFcasts[[nmodz]]
	}
)



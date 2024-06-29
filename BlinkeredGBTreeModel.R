#################################################################################
# Build a gradient boosted model using RPART decision trees as the
# base learner. Applying Blinkers at each gradient boosted step to 
# gradually ignore more and more of the outliers. 
#
# THIS IS NOT INTENDED AS A LARGE DATA SET PRODUCTION MODEL
# IT HAS BEEN BUILT AS A RAPID PROTOTYPE
##############################################################
library(rpart)
setOldClass("rpart")

setClass(
        "BlinkeredGBTreeModel",
        slots = c(
                features        = "character",
                target          = "character",
                basemodel       = "rpart",
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

############################################################################
# Build the model and return as an object of the above class
##########################################################################
BlinkeredGBTreeModel = function(x, y, tset, vset, learning_rate=0.2, blinker=NULL, delta=NULL, tolerance=0.001, epochs=10) {

	# ENSURE THAT THE DATA IS IN A DATA FRAME.
	trainset		<- as.data.frame(tset)
	validset		<- as.data.frame(vset) 

	# CREATE A FORMULA FROM THE SUPPLIED FEATURES AND TARGET
	mod.formula		<- as.formula( paste( paste( y, "~", sep=""), paste(x, collapse="+") ) )

	# CREATE THE INITIAL MODEL
	base.model		<- rpart(mod.formula, data=trainset)
	preds.train		<- cbind(predict( object=base.model, newdata=trainset))
	preds.valid		<- cbind(predict( object=base.model, newdata=validset))
	baseline		<- "base_model"
	colnames(preds.train)	<- c(baseline)
	colnames(preds.valid)	<- c(baseline)
	trainset 	        <- cbind(trainset, preds.train)
	validset 	        <- cbind(validset, preds.valid)

	# CALCULATE STARTING ERROR
        trn.err     		<- cbind( trainset[[baseline]] - trainset[[y]] )
        vd.err           	<- cbind( validset[[baseline]] - validset[[y]] )

	# IF BLINKER IS NOT SUPPLIED THEN WE START WITH THE 95th PERCENTILE
	if(is.null(blinker)) {
		quants 		<- quantile(abs(trn.err), c(.1, .95))
		blinker 	<- quants[[2]] 
		delta           <- (quants[[2]]-quants[[1]])/epochs
	}
	cur.blinker 		<- blinker
	baseline.trn.mae	<- mean(abs(trn.err))
	baseline.vd.mae		<- mean(abs(vd.err))
	last.trn.mae		<- baseline.trn.mae
	last.vd.mae		<- baseline.vd.mae
	improvement 		<- tolerance
	forecast.col 		<- baseline		
	forecast.col.stub	<- forecast.col
	errcol 			<- "model_error_target"
	errcol.stub		<- errcol
		
	nmodz			<- 0
        gammaz	 	      	<- list()
        splitterz       	<- list()
        regressrz       	<- list()

	training 		<- trainset
	validation 		<- validset
		
	# ADD THE ERROR AS A TARGET COLUMN
        colnames(trn.err)  	<- c(errcol)
        training        	<- cbind(training, trn.err)
        colnames(vd.err)    	<- c(errcol)
        validation      	<- cbind(validation, vd.err)

	# WE ITERATE THROUGH THE LEARNING PROCESS WHILE EVER THE
	# MODEL IS STILL IMPROVING AND THE BLINKER IS GREATER THAN ZERO
	# WITH MAXIMUM ITERATIONS SET BY EPOCHS
	while( improvement >= tolerance && cur.blinker > 0 && nmodz < epochs) {
		nmodz = nmodz + 1 

		### WE TRAIN A CLASSIFIER TO PREDICT IF THE ERROR IS WITHIN THE BLINKERS
		### CREATE THE LABELS
               	segcol="error_segment"
               	trn.seg         <- cbind(ifelse(
                                         training[[errcol]] > cur.blinker,
                                         'outside',
                                         ifelse(
                                                 training[[errcol]] < (-cur.blinker),
                                                'outside',
                                                'between'
                                         )
                                   ))
               	colnames(trn.seg)  <- c(segcol)
               	seg.training 	<- cbind(training, trn.seg)
               	vd.seg   	<- cbind(ifelse(
                                          validation[[errcol]] > cur.blinker,
                                          'outside',
                                          ifelse(
                                                 validation[[errcol]] < -cur.blinker,
                                                 'outside',
                                                 'between'
                                          )
                                   ))
              	colnames(vd.seg)  	<- c(segcol)
               	seg.validation 	<- cbind(validation, vd.seg)

		# NOW TRAIN A CLASSIFICATION MODEL TO PREDICT IF WE ARE WITHIN THE BLINKERS
		seg.formula		<- as.formula( paste( paste( segcol, "~", sep=""), paste(x, collapse="+") ) ) 
		splitterz[[nmodz]]        <- rpart(seg.formula, data=seg.training)

		# FINALLY TRAIN A REGRESSION MODEL FOR THE MIDDLE SEGMENT
	        temp.within.train       <- training[training[[errcol]] >= -cur.blinker & training[[errcol]] <= cur.blinker, ]
               	temp.within.valid       <- validation[validation[[errcol]] >= -cur.blinker & validation[[errcol]] <= cur.blinker, ]	
		reg.formula		<- as.formula( paste( paste( errcol, "~", sep=""), paste(x, collapse="+") ) ) 
		regressrz[[nmodz]]  	<- rpart(reg.formula, data=temp.within.train)
			
		### NOW WE GENERATE THE FORECASTS AND ERROR FOR THE NEW MODEL AND ADD THEM AS COLUMNS
		trn.seg.preds		<- cbind(predict( object=splitterz[[nmodz]], newdata=training) )
		vd.seg.preds		<- cbind(predict( object=splitterz[[nmodz]], newdata=validation) )
		trn.reg.preds		<- cbind(predict( object=regressrz[[nmodz]], newdata=training) )
		vd.reg.preds		<- cbind(predict( object=regressrz[[nmodz]], newdata=validation) )
		
		# WE SET GAMMA TO BE ONE FOR THE MOMENT
		gammaz[nmodz]		<- 1 

		trn.fcast		<- training[[forecast.col]] - learning_rate * gammaz[[nmodz]] * ( trn.seg.preds[,'between'] * trn.reg.preds)
		vd.fcast        	<- validation[[forecast.col]] - learning_rate * gammaz[[nmodz]] * (vd.seg.preds[,'between'] * vd.reg.preds)

		forecast.col		<- paste(forecast.col.stub, nmodz, sep='') 
		errcol    		<- paste(errcol.stub, nmodz, sep='')

		colnames(trn.fcast) 	<- c(forecast.col)
		colnames(vd.fcast) 	<- c(forecast.col)
               	trn.err         	<- trn.fcast - training[[y]]
               	vd.err          	<- vd.fcast - validation[[y]]
               	colnames(trn.err)  	<- c(errcol)
               	colnames(vd.err)   	<- c(errcol)

               	training        	<- cbind(training, trn.fcast, trn.err)
               	validation      	<- cbind(validation, vd.fcast, vd.err)

               	trn.mae        		<- mean(abs(trn.err))
               	vd.mae         		<- mean(abs(vd.err))
		improvement 		<- last.vd.mae - vd.mae
		cur.blinker 		<- cur.blinker  - delta

		print(paste("Epoch ", nmodz, " Improvement: ", improvement, sep=""))
	}

        new("BlinkeredGBTreeModel", features=x, target=y, 
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


#####################################################
# Forecasting Methods
####################################################

setGeneric(
        name="forecast",
        def=function(theObject,...)
        {
                standardGeneric("forecast")
        }
)

setMethod(
        f="forecast",
        signature="BlinkeredGBTreeModel",
        definition=function(theObject, dataset, shrinkage=0.5, softMaxBias=0.5)
	{
		tset                            <- as.data.frame(dataset)
		splitterFcasts          	= list()
		regressFcasts       		= list()
                totalFcasts           		= list()
                nmodz           		= length(theObject@regressors)
		basepreds			= predict(object=theObject@basemodel, newdata=tset)
		currpred			= basepreds
                for(mdel in 1:nmodz) {
                   splitterFcasts[[mdel]]	<- predict(object=theObject@splitters[[mdel]], newdata=tset)
                   regressFcasts[[mdel]]	<- predict(object=theObject@regressors[[mdel]], newdata=tset)

		   totalFcasts[[mdel]]		<- currpred - ( 
				theObject@learning_rate * theObject@gammas[[mdel]] * ( splitterFcasts[[mdel]][,'between']  * regressFcasts[[mdel]] )
			)
		   currpred 			<- totalFcasts[[mdel]]
                }

        	totalFcasts[[nmodz]]
	}
)

#########################################################


setGeneric(
        name="debugger",
        def=function(theObject,...)
        {
                standardGeneric("debugger")
        }
)

setMethod(
        f="debugger",
        signature="BlinkeredGBTreeModel",
        definition=function(theObject, dataset, shrinkage=0.5, softMaxBias=0.5)
        {
		tset  		              	<- as.data.frame(dataset)
                splitterFcasts                  = list()
                regressFcasts                   = list()
                totalFcasts                     = list()
                nmodz                           = length(theObject@regressors)
		print( paste("Running Debugger: ", nmodz, " models"))
		basepreds                       = predict(object=theObject@basemodel, newdata=tset)
		head(basepreds)
        }
)


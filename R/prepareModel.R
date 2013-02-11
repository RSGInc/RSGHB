prepareModel <- function(env)
{
     
     # Global variable setting
     env$gNP        <- length(unique(choicedata$ID))       # Number of people
     
     env$Choice     <- choicedata$Choice
     
     env$gNOBS      <- dim(choicedata)[1]
     
     env$TIMES      <- matrix(0,nrow=gNP,ncol=1)     		# Number of observations for each person
     env$TIMES[,1]  <- aggregate(choicedata$ID,by=list(choicedata$ID),length)[,2]
     
     env$gIDS       <- unlist(as.vector(mapply(rep,1:gNP,TIMES)))     # index map for individual to observation
     
     env$respIDs    <- unique(choicedata$ID)  
     
     # Matrix initialization
     # A must have NIV columns and 1 row
     # B must have NP columns and NIV rows
     # Dmat must have NIV columns and NIV rows and be symmetric
     
     if(length(gVarNamesNormal) > 0)
     {
          env$A <- matrix(0,nrow=env$gNIV,ncol=1)
          env$B <- matrix(0,nrow=env$gNP,ncol=env$gNIV)
          env$Dmat <- env$priorVariance * diag(env$gNIV)
 
          env$A[,1] <- svN        # initialize to analyst specified starting values
     
          env$B <- 1 + env$B
          env$B <- env$B * matrix(t(env$A),nrow=env$gNP,ncol=env$gNIV,byrow=T)
     }
     
}

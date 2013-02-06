prepareModel <- function()
{
     
     # Global variable setting
     gNP        <<- length(unique(choicedata$ID))       # Number of people
     
     Choice     <<- choicedata$Choice
     
     gNOBS      <<- dim(choicedata)[1]
     
     TIMES      <<- matrix(0,nrow=gNP,ncol=1)     		# Number of observations for each person
     TIMES[,1]  <<- aggregate(choicedata$ID,by=list(choicedata$ID),length)[,2]
     
     gIDS       <<- unlist(as.vector(mapply(rep,1:gNP,TIMES)))     # index map for individual to observation
     
     respIDs    <<- unique(choicedata$ID)  
     
     # Matrix initialization
     # A must have NIV columns and 1 row
     # B must have NP columns and NIV rows
     # Dmat must have NIV columns and NIV rows and be symmetric
     
     if(length(gVarNamesNormal) > 0)
     {
          A <<- matrix(0,nrow=gNIV,ncol=1)
          B <<- matrix(0,nrow=gNP,ncol=gNIV)
          Dmat <<- priorVariance * diag(gNIV)
 
          A[,1] <<- svN        # initialize to analyst specified starting values
     
          B <<- 1 + B
          B <<- B  * matrix(t(A),nrow=gNP,ncol=gNIV,byrow=T)
     }
     
}

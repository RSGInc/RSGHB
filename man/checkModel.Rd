\name{checkModel}
\alias{checkModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Validates structure of the model
}
\description{
 Validates the structure of the model prior to estimation by performing some diagnostic tests. For example, it looks to make sure required variables are set and are compatible with one another (e.g., there are distributional shapes specified for each of the random coefficients).
}
\details{

This function is called automatically from the main estimation functions and does not need to be called directly by the user though it can be if desired. 

This function performs the following diagnostic checks:

1. for the existing of the following variables - choicedata, gDIST, gNCREP, gNEREP, gNSKIP, gINFOSKIP, likelihood

2. to make sure the number of random coefficients (gNIV) plus the number of fixed coefficients (gFIV) is greater than 0.

3. to make sure there is an element in gDIST for each of the random coefficients.

4. to make sure that the elements of gDIST correspond to allowable distributions.

5. to make sure there are enough starting values for the random and fixed coefficents.

6. to make sure that there is a respondent identifier in the choicedata data.frame called ID.

7. to make sure that there is a choice column called Choice in the choicedata data.frame. 

}

\usage{
checkModel()
}

\value{
 Returns True if the model passes the diagnositic checks. Returns False otherwise. Prints error messages to the screen. 
}
\references{
}
\author{
 Jeffrey Dumont, RSG, Inc.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

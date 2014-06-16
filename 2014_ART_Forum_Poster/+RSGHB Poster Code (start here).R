### Built with R 3.1.0

### Set the working directory on your machine
setwd("C:/users/jeff.keller/desktop/")

library(RSGHB)

set.seed(1987) 

### Synthetic data parameters
N <- 1000 # Number of individuals
J <- 10   # Number of choice tasks per individual
k <- 4    # Number of alternatives
m <- 4    # Number of attributes

### Create the experimental design
design <- matrix(0, nrow = N*J, ncol = k*m + 1)

colnames(design) <- c("ID",
                      "alt1.price", "alt1.brand", "alt1.ff", "alt1.feat",
                      "alt2.price", "alt2.brand", "alt2.ff", "alt2.feat",
                      "alt3.price", "alt3.brand", "alt3.ff", "alt3.feat",
                      "alt4.price", "alt4.brand", "alt4.ff", "alt4.feat")

design[, "ID"] <- rep(1:N, each = J)
design[, 2:(k*m + 1)] <- sample(1:3, N*J*k*m, replace = TRUE)

### Sample synthetic utilities
design <- cbind(design, U.price = rep(rnorm(N), each = J)/2)
design <- cbind(design, U.brand1 = 0)
design <- cbind(design, U.brand2 = rep(rnorm(N, mean = 1), each = J))
design <- cbind(design, U.brand3 = rep(rnorm(N, mean = -0.8), each = J))
design <- cbind(design, U.ff1 = 0)
design <- cbind(design, U.ff2 = rep(rnorm(N, mean = 0.25), each = J))
design <- cbind(design, U.ff3 = rep(rnorm(N, mean = 0.60), each = J))
design <- cbind(design, U.feat1 = 0)
design <- cbind(design, U.feat2 = rep(rnorm(N, mean = 1), each = J))
design <- cbind(design, U.feat3 = rep(rnorm(N, mean = 1.5), each = J))

# Plot synthetic utilities
for (col in 18:ncol(design)) plot(density(design[,col]), main = colnames(design)[col])

### Latent variable code
# These are the parameters that need to be estimated
gamma1 <- 0.2
gamma2 <- -0.45
zeta1  <- 0.5
zeta2  <- 0.25 
tau    <- 0.25

# Build the structural equation of the form:
# LV = gamma1*demo1 + gamma2*demo2 + standard normal disturbance
design <- cbind(design, LV_disturbance = rep(rnorm(N, mean = 0), each = J))
design <- cbind(design, demo1 = rep(sample(1:3,N,replace=TRUE), each = J))
design <- cbind(design, demo2 = rep(sample(1:3,N,replace=TRUE), each = J))
design <- cbind(design, lv = gamma1 * design[,"demo1"] + gamma2 * design[,"demo2"] + design[,"LV_disturbance"])

# Build the measurement model of the form:
# I1 = zeta1 * lv + standard normal disturbance <= linear regression
# I2 = zeta2 * lv + standard normal disturbance <= linear regression
design <- cbind(design, residual1 = rep(rnorm(N, mean = 0), each = J))
design <- cbind(design, residual2 = rep(rnorm(N, mean = 0), each = J))

design <- cbind(design, indicator1 = design[,"lv"] * zeta1 + design[,"residual1"])
design <- cbind(design, indicator2 = design[,"lv"] * zeta2 + design[,"residual2"])

### Calculate alternative utilities
U <- matrix(0, nrow = nrow(design), ncol = k)
colnames(U) <- c("U1", "U2", "U3", "U4")
design <- cbind(design, U)

# Latent variable enters in the form:
# beta_price = -exp(B + tau * lv) where is B ~ N(A,D)

# Alt 1
design[,"U1"] <- design[,"alt1.price"] * -1 * exp(design[,"U.price"] + tau * design[,"lv"]) +
                 design[,c("U.brand1", "U.brand2", "U.brand3")][cbind(1:nrow(design), design[,"alt1.brand"])] +
                 design[,c("U.ff1", "U.ff2", "U.ff3")][cbind(1:nrow(design), design[,"alt1.ff"])] +
                 design[,c("U.feat1", "U.feat2", "U.feat3")][cbind(1:nrow(design), design[,"alt1.feat"])]

# Alt 2
design[,"U2"] <- design[,"alt2.price"] * -1 * exp(design[,"U.price"] + tau * design[,"lv"]) +
                 design[,c("U.brand1", "U.brand2", "U.brand3")][cbind(1:nrow(design), design[,"alt2.brand"])] +
                 design[,c("U.ff1", "U.ff2", "U.ff3")][cbind(1:nrow(design), design[,"alt2.ff"])] +
                 design[,c("U.feat1", "U.feat2", "U.feat3")][cbind(1:nrow(design), design[,"alt2.feat"])]

# Alt 3
design[,"U3"] <- design[,"alt3.price"] * -1 * exp(design[,"U.price"] + tau * design[,"lv"])+
                 design[,c("U.brand1", "U.brand2", "U.brand3")][cbind(1:nrow(design), design[,"alt3.brand"])] +
                 design[,c("U.ff1", "U.ff2", "U.ff3")][cbind(1:nrow(design), design[,"alt3.ff"])] +
                 design[,c("U.feat1", "U.feat2", "U.feat3")][cbind(1:nrow(design), design[,"alt3.feat"])]

# Alt 4
design[,"U4"] <- design[,"alt4.price"] * -1 * exp(design[,"U.price"] + tau * design[,"lv"]) +
                 design[,c("U.brand1", "U.brand2", "U.brand3")][cbind(1:nrow(design), design[,"alt4.brand"])] +
                 design[,c("U.ff1", "U.ff2", "U.ff3")][cbind(1:nrow(design), design[,"alt4.ff"])] +
                 design[,c("U.feat1", "U.feat2", "U.feat3")][cbind(1:nrow(design), design[,"alt4.feat"])]

### Calculate synthetic choices with a nested structure with form factors
U[,1:4] <- design[, c("U1", "U2", "U3", "U4")]

lambda <- 0.37
U_i_given_nest <- U / lambda

USum_nests <- cbind(rowSums(exp(U_i_given_nest) * (design[, c("alt1.ff", "alt2.ff", "alt3.ff", "alt4.ff")] == 1)),
                    rowSums(exp(U_i_given_nest) * (design[, c("alt1.ff", "alt2.ff", "alt3.ff", "alt4.ff")] == 2)),
                    rowSums(exp(U_i_given_nest) * (design[, c("alt1.ff", "alt2.ff", "alt3.ff", "alt4.ff")] == 3))
)

#Probability of choosing the alternative given the nest
P_i_given_nest <- exp(U_i_given_nest)/(cbind(USum_nests[cbind(1:nrow(design),design[, "alt1.ff"])],
                                             USum_nests[cbind(1:nrow(design),design[, "alt2.ff"])],
                                             USum_nests[cbind(1:nrow(design),design[, "alt3.ff"])],
                                             USum_nests[cbind(1:nrow(design),design[, "alt4.ff"])]
                                             ))

#Probability of the nest
P_nest <- exp(log(cbind(USum_nests[cbind(1:nrow(design),design[, "alt1.ff"])],
                        USum_nests[cbind(1:nrow(design),design[, "alt2.ff"])],
                        USum_nests[cbind(1:nrow(design),design[, "alt3.ff"])],
                        USum_nests[cbind(1:nrow(design),design[, "alt4.ff"])]
)) * lambda) /
  rowSums(exp(log(USum_nests) * lambda))

# Probability of the joint choice
P_sim <- P_nest * P_i_given_nest
P_sim <- t(apply(X = P_sim, MARGIN = 1, FUN = cumsum))

# Simulate choices
choice <- apply(X = P_sim > runif(N*J), MARGIN = 1, FUN = which.max)
design <- cbind(design, choice)

### Likelihoods function for Models 1-2
V <- matrix(0, nrow = nrow(design), ncol = k)
likelihoods1 <- function(fc, b) {
  
  cc <- 1
  bprice  <- b[,cc]; cc <- cc + 1
  bbrand2 <- b[,cc]; cc <- cc + 1
  bbrand3 <- b[,cc]; cc <- cc + 1
  bff2    <- b[,cc]; cc <- cc + 1
  bff3    <- b[,cc]; cc <- cc + 1
  bfeat2  <- b[,cc]; cc <- cc + 1
  bfeat3  <- b[,cc]; cc <- cc + 1
  
  V[,1] <- bprice * design[,"alt1.price"] +
           bbrand2 * (design[,"alt1.brand"] == 2) +
           bbrand3 * (design[,"alt1.brand"] == 3) +
           bff2 * (design[,"alt1.ff"] == 2) +
           bff3 * (design[,"alt1.ff"] == 3) +
           bfeat2 * (design[,"alt1.feat"] == 2) +
           bfeat3 * (design[,"alt1.feat"] == 3)
  
  V[,2] <- bprice * design[,"alt2.price"] +
           bbrand2 * (design[,"alt2.brand"] == 2) +
           bbrand3 * (design[,"alt2.brand"] == 3) +
           bff2 * (design[,"alt2.ff"] == 2) +
           bff3 * (design[,"alt2.ff"] == 3) +
           bfeat2 * (design[,"alt2.feat"] == 2) +
           bfeat3 * (design[,"alt2.feat"] == 3)
  
  V[,3] <- bprice * design[,"alt3.price"] +
           bbrand2 * (design[,"alt3.brand"] == 2) +
           bbrand3 * (design[,"alt3.brand"] == 3) +
           bff2 * (design[,"alt3.ff"] == 2) +
           bff3 * (design[,"alt3.ff"] == 3) +
           bfeat2 * (design[,"alt3.feat"] == 2) +
           bfeat3 * (design[,"alt3.feat"] == 3)
  
  V[,4] <- bprice * design[,"alt4.price"] +
           bbrand2 * (design[,"alt4.brand"] == 2) +
           bbrand3 * (design[,"alt4.brand"] == 3) +
           bff2 * (design[,"alt4.ff"] == 2) +
           bff3 * (design[,"alt4.ff"] == 3) +
           bfeat2 * (design[,"alt4.feat"] == 2) +
           bfeat3 * (design[,"alt4.feat"] == 3)
  
  p <- exp(V[cbind(1:nrow(design), design[,"choice"])])/rowSums(exp(V))
  return(p)
    
}

# Names for the Mixed Variables
gVarNamesNormal <- c("price", "brand2", "brand3", "ff2", "ff3", "feat2", "feat3")

# Names for the Fixed Variables
gVarNamesFixed <- c()

# Distributions for the Mixed Variables
gDIST <- rep(1,length(gVarNamesNormal))

#Starting Values
FC   <- rep(0,length(gVarNamesFixed))
svN   <- rep(0,length(gVarNamesNormal))

### Estimate Model 1
control <- list(
  modelname = "Model1",
  gSeed = 1987,
  gVarNamesFixed = gVarNamesFixed,
  gVarNamesNormal = gVarNamesNormal,
  FC = FC,
  gDIST = gDIST,
  svN = svN,
  gNCREP = 25000,
  gNEREP = 25000,
  gINFOSKIP = 500,
  gFULLCV = 0,
  nodiagnostics = TRUE
)

doHB(likelihoods1, data.frame(cbind(ID = design[,"ID"],design)), control)

### Likelihoods function for Model 2
ch <- cbind(1:nrow(design), design[,"choice"])

likelihoods2 <- function(fc, b) {
     
     cc <- 1
     bprice  <- b[,cc]; cc <- cc + 1
     bbrand2 <- b[,cc]; cc <- cc + 1
     bbrand3 <- b[,cc]; cc <- cc + 1
     bff2    <- b[,cc]; cc <- cc + 1
     bff3    <- b[,cc]; cc <- cc + 1
     bfeat2  <- b[,cc]; cc <- cc + 1
     bfeat3  <- b[,cc]; cc <- cc + 1
     LV_disturbance <- b[,cc]; cc <- cc + 1
     
     cc <- 1
     lambda  <- fc[cc]; cc <- cc + 1
     gamma1  <- fc[cc]; cc <- cc + 1
     gamma2  <- fc[cc]; cc <- cc + 1
     sigma1  <- fc[cc]; cc <- cc + 1
     sigma2  <- fc[cc]; cc <- cc + 1
     delta1  <- 0#fc[cc]; cc <- cc + 1
     delta2  <- 0#fc[cc]; cc <- cc + 1
     zeta1   <- fc[cc]; cc <- cc + 1
     zeta2   <- fc[cc]; cc <- cc + 1
     tau     <- fc[cc]; cc <- cc + 1
     
     LV <- gamma1 * design[,"demo1"] + gamma2 * design[,"demo2"] + LV_disturbance
     
     P_LV1 <- (exp( -0.5 * ((design[,"indicator1"] - (delta1 + zeta1 * LV)) / sigma1) ^ 2 ) / (sigma1 * sqrt(2 * pi))) ^ (1/J)
     P_LV2 <- (exp( -0.5 * ((design[,"indicator2"] - (delta2 + zeta2 * LV)) / sigma2) ^ 2 ) / (sigma2 * sqrt(2 * pi))) ^ (1/J)
     
     V[,1] <- -exp(bprice + tau * LV) * design[,"alt1.price"] +
          bbrand2 * (design[,"alt1.brand"] == 2) +
          bbrand3 * (design[,"alt1.brand"] == 3) +
          bff2 * (design[,"alt1.ff"] == 2) +
          bff3 * (design[,"alt1.ff"] == 3) +
          bfeat2 * (design[,"alt1.feat"] == 2) +
          bfeat3 * (design[,"alt1.feat"] == 3)
     
     V[,2] <- -exp(bprice + tau * LV) * design[,"alt2.price"] +
          bbrand2 * (design[,"alt2.brand"] == 2) +
          bbrand3 * (design[,"alt2.brand"] == 3) +
          bff2 * (design[,"alt2.ff"] == 2) +
          bff3 * (design[,"alt2.ff"] == 3) +
          bfeat2 * (design[,"alt2.feat"] == 2) +
          bfeat3 * (design[,"alt2.feat"] == 3)
     
     V[,3] <- -exp(bprice + tau * LV) * design[,"alt3.price"] +
          bbrand2 * (design[,"alt3.brand"] == 2) +
          bbrand3 * (design[,"alt3.brand"] == 3) +
          bff2 * (design[,"alt3.ff"] == 2) +
          bff3 * (design[,"alt3.ff"] == 3) +
          bfeat2 * (design[,"alt3.feat"] == 2) +
          bfeat3 * (design[,"alt3.feat"] == 3)
     
     V[,4] <- -exp(bprice + tau * LV) * design[,"alt4.price"] +
          bbrand2 * (design[,"alt4.brand"] == 2) +
          bbrand3 * (design[,"alt4.brand"] == 3) +
          bff2 * (design[,"alt4.ff"] == 2) +
          bff3 * (design[,"alt4.ff"] == 3) +
          bfeat2 * (design[,"alt4.feat"] == 2) +
          bfeat3 * (design[,"alt4.feat"] == 3)
     
     
     #Utility of the given nest
     V_i_given_nest <- V / lambda
     
     VSum_nests <- cbind(rowSums(exp(V_i_given_nest) * (design[, c("alt1.ff", "alt2.ff", "alt3.ff", "alt4.ff")] == 1)),
                         rowSums(exp(V_i_given_nest) * (design[, c("alt1.ff", "alt2.ff", "alt3.ff", "alt4.ff")] == 2)),
                         rowSums(exp(V_i_given_nest) * (design[, c("alt1.ff", "alt2.ff", "alt3.ff", "alt4.ff")] == 3))
     )
     
     #Probability of choosing the alternative given the nest
     P_i_given_nest <- exp(V_i_given_nest)[ch]/(VSum_nests[cbind(1:nrow(design),design[, c("alt1.ff", "alt2.ff", "alt3.ff", "alt4.ff")][ch])])
     
     #Probability of the nest
     P_nest <- exp(log(VSum_nests[cbind(1:nrow(design),design[, c("alt1.ff", "alt2.ff", "alt3.ff", "alt4.ff")][ch])]) * lambda) /
          rowSums(exp(log(VSum_nests) * lambda))
     
     #Probability of the choice
     p <- P_nest * P_i_given_nest
     
     #Joint probability of measurement and NL models
     p <- p * P_LV1 * P_LV2
     
     return(p)
     
}

### Estimate Model 4 (This is referred to as model 2 on the poster)

# Names for the Mixed Variables
gVarNamesNormal <- c("price", "brand2", "brand3", "ff2", "ff3", "feat2", "feat3", "LV_disturbance")

# Names for the Fixed Variables
gVarNamesFixed <- c("lambda", "gamma1", "gamma2", "sigma1", "sigma2", "zeta1", "zeta2", "tau")

# Distributions for the Mixed Variables
gDIST <- c(1, 1, 1, 1, 1, 1, 1, 1)

#Starting Values
FC   <- c(1, 0, 0, 1, 1, 0, 0, 0)
svN   <- rep(0,length(gVarNamesNormal))

control <- list(
  modelname = "Model2",
  gSeed = 1987,
  gVarNamesFixed = gVarNamesFixed,
  gVarNamesNormal = gVarNamesNormal,
  FC = FC,
  gDIST = gDIST,
  svN = svN,
  fixedA = c(NA, NA, NA, NA, NA, NA, NA, 0),
  fixedD = c(NA, NA, NA, NA, NA, NA, NA, 1),
  gFULLCV = 0,
  gNCREP = 25000,
  gNEREP = 25000,
  gINFOSKIP = 500,
  nodiagnostics = TRUE
)

doHB(likelihoods2, data.frame(ID = design[,"ID"]), control)


### Model Comparisons
actual.mean <- c(price = -exp(-1/8+3097/9600/2), # mean of a log normal is exp(mu + var/2)
                 brand2 = 1,
                 brand3 = -0.8,
                 ff2 = 0.25,
                 ff3 = 0.60,
                 feat2 = 1,
                 feat3 = 1.5)


### Model 1
A1 <- read.table(file = "Model1_A.csv", sep = ",", header = TRUE)[,-1]
A1.mean <- colMeans(A1)

# RMSE
RMSE1 <- sqrt(mean((actual.mean - A1.mean)^2))

# Plot
plot(actual.mean, A1.mean, xlim = c(-2,2), ylim = c(-2,2), xlab = "True", ylab = "Estimated")
lines(c(-10,10), c(-10,10), col = "#F68B1F")


### Model 2
A2 <- read.table(file = "Model2_A.csv", sep = ",", header = TRUE)[,-1]
F2 <- read.table(file = "Model2_F.csv", sep = ",", header = TRUE)[,-1]
F2.mean <- colMeans(F2)
A2.mean <- colMeans(A2)

# Calculate mean of underlying normal
price.mean <- A2.mean["price"] + F2.mean["tau"]*(F2.mean["gamma1"] * 2 + F2.mean["gamma2"] * 2 + 0)

# Calculate variance of underlying normal
model2.var <- read.table(file = "Model2_D.csv", sep = ",", header = TRUE)
price.var <- mean(model2.var[,2])
price.var <- (1)^2*price.var + (F2.mean["tau"]*F2.mean["gamma1"])^2*2/3 + (F2.mean["tau"]*F2.mean["gamma2"])^2*2/3 + (F2.mean["tau"])^2*1
A2.mean["price"] <- -exp(price.mean + price.var/2)

# RMSE
RMSE2 <- sqrt(mean((actual.mean - A2.mean[-8])^2))

plot(actual.mean, A1.mean, xlim = c(-2,3), ylim = c(-2,3), col = "#48484A", pch = 19,
     xlab = "True", ylab = "Estimated", main = "Model 1 and Model 2 Parameter Estimate Means")
points(actual.mean, A2.mean[-8], col = "#F68B1F", pch = 19)
lines(c(-10,10), c(-10,10), col = "#BA1222")

### Estimate Model 1 in CBCHB
# Create output for use in CBCHB (v5.5.2)
cbchb1 <- design[, c("ID", "alt1.price", "alt1.brand", "alt1.ff", "alt1.feat")]
colnames(cbchb1) <- c("ID", "price", "brand", "ff", "feat")
cbchb1 <- cbind(cbchb1, choice = (design[,"choice"] == 1)*1, alternative = 1, task = 1:J)

cbchb2 <- design[, c("ID", "alt2.price", "alt2.brand", "alt2.ff", "alt2.feat")]
colnames(cbchb2) <- c("ID", "price", "brand", "ff", "feat")
cbchb2 <- cbind(cbchb2, choice = (design[,"choice"] == 2)*1, alternative = 2, task = 1:J)

cbchb3 <- design[, c("ID", "alt3.price", "alt3.brand", "alt3.ff", "alt3.feat")]
colnames(cbchb3) <- c("ID", "price", "brand", "ff", "feat")
cbchb3 <- cbind(cbchb3, choice = (design[,"choice"] == 3)*1, alternative = 3, task = 1:J)

cbchb4 <- design[, c("ID", "alt4.price", "alt4.brand", "alt4.ff", "alt4.feat")]
colnames(cbchb4) <- c("ID", "price", "brand", "ff", "feat")
cbchb4 <- cbind(cbchb4, choice = (design[,"choice"] == 4)*1, alternative = 4, task = 1:J)

cbchb <- rbind(cbchb1, cbchb2, cbchb3, cbchb4)
cbchb <- cbchb[order(cbchb[,"ID"], cbchb[,"task"], cbchb[,"alternative"]),]
cbchb <- cbchb[,c("ID", "task", "alternative", "price", "brand", "ff", "feat", "choice")]

write.csv(cbchb, file = "synthetic poster data.csv", row.names = FALSE)

# Read in the CBCHB results and compare
cbchb.A <- read.csv("synthetic poster data_alpha.csv", col.names = c("ID", "price", "brand1", "brand2", "brand3", "ff1", "ff2", "ff3", "feat1", "feat2", "feat3"))
cbchb.A <- cbchb.A[25001:50000,]

# Zero base the first level rather than the last
cbchb.A[,c("brand1", "brand2", "brand3")] <- cbchb.A[,c("brand1", "brand2", "brand3")] - cbchb.A[,"brand1"]
cbchb.A[,c("ff1", "ff2", "ff3")] <- cbchb.A[,c("ff1", "ff2", "ff3")] - cbchb.A[,"ff1"]
cbchb.A[,c("feat1", "feat2", "feat3")] <- cbchb.A[,c("feat1", "feat2", "feat3")] - cbchb.A[,"feat1"]

# RMSE
RMSE.cbchb <- sqrt(mean((actual.mean - colMeans(cbchb.A[,-c(1,3,6,9)]))^2))

plot(actual.mean, colMeans(cbchb.A[,c("price", "brand2", "brand3", "ff2", "ff3", "feat2", "feat3")]))
lines(c(-10,10), c(-10,10), col = "red")



### Estimate Model 1 with bayesm
library(bayesm)
library(dummies)

# a matrix of independent variables
x <- rbind(
     cbind(ID = design[,"ID"], thecount = rep(1:10, N), alt = 1, design[, c("alt1.price", "alt1.brand", "alt1.ff", "alt1.feat")]),
     cbind(ID = design[,"ID"], thecount = rep(1:10, N), alt = 2, design[, c("alt2.price", "alt2.brand", "alt2.ff", "alt2.feat")]),
     cbind(ID = design[,"ID"], thecount = rep(1:10, N), alt = 3, design[, c("alt3.price", "alt3.brand", "alt3.ff", "alt3.feat")]),
     cbind(ID = design[,"ID"], thecount = rep(1:10, N), alt = 4, design[, c("alt4.price", "alt4.brand", "alt4.ff", "alt4.feat")])
)

# dummy code independent variables
x <- cbind(x[,c("ID", "thecount", "alt")], price = x[,"alt1.price"],
           dummy(x[,"alt1.brand"]),
           dummy(x[,"alt1.ff"]),
           dummy(x[,"alt1.feat"]))

# fixing the first levels to 0.
x <- x[,!"x1" == colnames(x)]

colnames(x) <- c("ID","thecount","alt","price","brand2","brand3","ff2","ff3","feat2","feat3")

# resorting the data by ID
x <- x[order(x[,"ID"], x[,"thecount"], x[,"alt"]),]

# a vector of the multinomial choices
y <- design[,"choice"]

# a list to hold the necessary objects for the MNL model
lgtdata <- NULL

# need to create a list that contains choices and independent variables by individual
for(i in 1:N)
{
     lgtdata[[i]] <- list(y = y[(1+J*(i-1)):(J*i)],
                          X = x[(1+J*k*(i-1)):(J*k*i), c("price", "brand2", "brand3", "ff2", "ff3", "feat2", "feat3")])
}

# check to see if the conversion worked
lgtdata[[1]]$X[1:8,]
design[1:2,1:17]

# set some model settings
Prior <- list(ncomp = 1)
Mcmc <- list(R = 25000, keep = 2)
Data <- list(p = k, lgtdata = lgtdata, Z = NULL)

# This can take a long time to run!
set.seed(1987)
model1 <- rhierMnlRwMixture(Data = Data, Prior = Prior, Mcmc = Mcmc)

# summary(model1$nmix)
# plot(model1$betadraw)
# plot(model1$nmix)

bayesm.results <- data.frame(price = rowMeans(model1$betadraw[,1,]),
                             brand2 = rowMeans(model1$betadraw[,2,],),
                             brand3 = rowMeans(model1$betadraw[,3,],),
                             ff2 = rowMeans(model1$betadraw[,4,],),
                             ff3 = rowMeans(model1$betadraw[,5,],),
                             feat2 = rowMeans(model1$betadraw[,6,],),
                             feat3 = rowMeans(model1$betadraw[,7,],))

colMeans(bayesm.results)

# RMSE
RMSE.bayesm <- sqrt(mean((actual.mean - colMeans(bayesm.results))^2))

save(model1, file = "bayesM_model1.RData")




### Other example models

# Estimate a model with constraints on the feature attribute
control <- list(
  modelname = "Model3",
  gSeed = 1987,
  gVarNamesFixed = NULL,
  gVarNamesNormal = c("price", "brand2", "brand3", "ff2", "ff3", "feat2", "feat3"),
  FC = NULL,
  gDIST = rep(1, 7),
  svN = rep(0, 7),
  nodiagnostics = TRUE,
  gNCREP = 25000,
  gNEREP = 25000,
  gINFOSKIP = 500,
  gFULLCV = 0,
  constraints = list(c(6, 2, 0), c(7, 2, 6))
)

likelihoods3 <- likelihoods1

doHB(likelihoods3, data.frame(ID = design[,"ID"]), control)

# Estimation a model with fixed brand coefficients
likelihoods4 <- function(fc, b) {
  
  cc <- 1
  bprice  <- b[,cc]; cc <- cc + 1
  bff2    <- b[,cc]; cc <- cc + 1
  bff3    <- b[,cc]; cc <- cc + 1
  bfeat2  <- b[,cc]; cc <- cc + 1
  bfeat3  <- b[,cc]; cc <- cc + 1
  
  cc <- 1
  bbrand2 <- fc[cc]; cc <- cc + 1
  bbrand3 <- fc[cc]; cc <- cc + 1
  
  V[,1] <- bprice * design[,"alt1.price"] +
    bbrand2 * (design[,"alt1.brand"] == 2) +
    bbrand3 * (design[,"alt1.brand"] == 3) +
    bff2 * (design[,"alt1.ff"] == 2) +
    bff3 * (design[,"alt1.ff"] == 3) +
    bfeat2 * (design[,"alt1.feat"] == 2) +
    bfeat3 * (design[,"alt1.feat"] == 3)
  
  V[,2] <- bprice * design[,"alt2.price"] +
    bbrand2 * (design[,"alt2.brand"] == 2) +
    bbrand3 * (design[,"alt2.brand"] == 3) +
    bff2 * (design[,"alt2.ff"] == 2) +
    bff3 * (design[,"alt2.ff"] == 3) +
    bfeat2 * (design[,"alt2.feat"] == 2) +
    bfeat3 * (design[,"alt2.feat"] == 3)
  
  V[,3] <- bprice * design[,"alt3.price"] +
    bbrand2 * (design[,"alt3.brand"] == 2) +
    bbrand3 * (design[,"alt3.brand"] == 3) +
    bff2 * (design[,"alt3.ff"] == 2) +
    bff3 * (design[,"alt3.ff"] == 3) +
    bfeat2 * (design[,"alt3.feat"] == 2) +
    bfeat3 * (design[,"alt3.feat"] == 3)
  
  V[,4] <- bprice * design[,"alt4.price"] +
    bbrand2 * (design[,"alt4.brand"] == 2) +
    bbrand3 * (design[,"alt4.brand"] == 3) +
    bff2 * (design[,"alt4.ff"] == 2) +
    bff3 * (design[,"alt4.ff"] == 3) +
    bfeat2 * (design[,"alt4.feat"] == 2) +
    bfeat3 * (design[,"alt4.feat"] == 3)
  
  p <- exp(V[cbind(1:nrow(design), design[,"choice"])])/rowSums(exp(V))
  return(p)
  
}

# Names for the Mixed Variables
gVarNamesNormal <- c("price", "ff2", "ff3", "feat2", "feat3")

# Names for the Fixed Variables
gVarNamesFixed <- c( "brand2", "brand3")

# Distributions for the Mixed Variables
gDIST <- rep(1,length(gVarNamesNormal))

#Starting Values
FC   <- rep(0,length(gVarNamesFixed))
svN   <- rep(0,length(gVarNamesNormal))

control <- list(
  modelname = "Model4",
  gSeed = 1987,
  gVarNamesFixed = c("brand2", "brand3"),
  gVarNamesNormal = c("price", "ff2", "ff3", "feat2", "feat3"),
  FC = rep(0, 2),
  gDIST = rep(1, 5),
  svN = rep(0, 5),
  gFULLCV = 0,
  gNCREP = 25000,
  gNEREP = 25000,
  gINFOSKIP = 500,
  nodiagnostics = TRUE
)

# Estimate Model 4
doHB(likelihoods4, data.frame(ID = design[,"ID"]), control)
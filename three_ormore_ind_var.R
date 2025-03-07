#==========Chi-goodness of fit=========
#comparing predicted freq to observed freq
#H0: Data follow expected ratio
#HA: Data do not follow expected ratio
####long-way##
k <- # number of categories
v <- k -1
observed <- c(,,,,)
predprob <- c(,,,,)
predicted <- sum(observed)* predprob
Chi.stat <- sum((observed - predicted)^2/predicted)
#note that you need to replace the observed and predicted for each
#category
crit.value.chi <- qchisq(p = 0.05, df = v, lower.tail = FALSE)
pvalue.chi <- pchisq(q = Chi.stat, df = v, lower.tail = FALSE)

###short-way
observed <- c(,,,,)
predicted.prob <- c(,,,,)
chisq.test(x = observed, p = predicted.prob)
#=================Yates correction for continuity, chi-fit===========
#when k = 2 and v = 1
observed1 <-
observed2 <-
prob1 <- 
prob2 <-
pred.1 <- (observed1 + observed2) * prob1
pred.2 <- (observed1 + observed2) * prob2
Chi.stat.yates <- (abs(observed1-pred1)- 0.5)^2/pred1 +
  (abs(observed2 - pred2) - 0.5)^2/ pred2
crit.value.chi.yate <- qchisq(p = 0.05, df = 1, lower.tail = FALSE)
pvalue.chi.yate - pchisq(q = crit.value.chi.yate, df = 1, lower.tail = FALSE)

#==========Standardized residuals, post chi-goodness of fit (rejected HO)==========
#did the chi goodness of fit and rejected the null meaning that
#the data do not follow the expected ratio
std.resids <- observed - predicted/sqrt(predicted)
std.residuals <- sqrt((observed - predicted)^2/predicted) #this will vectorize the chi values

#===========Log-likelihood ratio in R (chi)=========
observed <- c(,,,,)
n <- sum(observed)
pexpected <- c(,,,,)
expected <- n * pexpected
k <- 
v <- k -1
Gvalue <- 2*sum(observed*log(observed/expected))
crit.value <- qchisq(p = 0.5, df = v, lower.tail = FALSE)
pvalue <- pchisq(q = Gvalue, df = v, lower.tail = FALSE)

#Built-in likelihood-ratio test
library(DescTools)
observed <- c(,,,,)
pexpected <- c(,,,,)
GTest(x = oberved, p = pexpected, correct = "none")



#=================Creating a Contingency Table====================
data <- data.frame(FishSex=c("M","F","M","M","F","F","NA"),
                   Parasites=c("Yes","Yes","Yes","Yes","No","No","Yes"))
table(data$FishSex, data$Parasites, dnn = c("Fish sex",
                                            "Parasites"))  #give names for the resulting table


#==========Contingency Tables Analysis============
#in contingency table analyses we are seeing if our varaibles are related
#H0: variable 1 is independent of variable 2
#HA: variable 1 is related to variable 2

####long-way
#step 1: calculate predicted values of your contingency table
nrow <-
ncol <-
v <- (nrow(obs)-1)*(ncol(obs)-1)
obs <- matrix(c(#put in the counts here in correct format of table, ,
                , ),
                   nrow = , ncol =  , byrow = T)
Rvec <- rowSums(obs)
Cvec <- colSums(obs)
n <- sum(obs)

pred <- (Rvec %o% Cvec)/n 
print(round(pred,1))
#step 2: calculate chisq stat
chisq.terms <- (obs-pred)^2/pred
chisq.stat <- sum(chisq.terms)
pchisq(q=chisq.stat, df=v, lower.tail=FALSE)
qchisq(p=0.05, df=v, lower.tail=FALSE)

#####short-way
chisq.results <- chisq.test(obs) #requires the data as matrix
chisq.results$expected #returns f-hat_ij or the predicted values
chisq.results$residuals # returns the standardized residuals

#==========special case, 2x2 Contingency Table Analysis========
obs <- matrix(c(,,
                 ,), byrow=TRUE, nrow=2, ncol=2)
v <- (nrow(obs)-1)*(ncol(obs)-1) #(2-1)*(2-1) = 1 always for 2x2 contingency table
Rvec <- rowSums(obs)
Cvec <- colSums(obs)
n <- sum(obs)
chi.stat <- (obs[1,1]*obs[2,2] - obs[1,2]*obs[2,1])^2*n/
  (prod(Rvec,Cvec))

crit.value <- qchisq(p = 0.05, df = v, lower.tail = F)
pvalue <- pchisq(q = chi.stat, df = v, lower.tail = F)
signif(pvalue, digits = 2)

#one-liner
chisq.test(obs, correct=FALSE) #putting false turns off the Yates correction

#Yates continuity correction for 2x2 contingency tables
chi.stat.Yates <- (abs(det(obs))-n/2)^2*n/ (prod(Rvec, Cvec))
pvalue.Yates <- pchisq(q = chi.stat.Yates, df = v, lower.tail = F)
signif(pvalueAndroYates, digits = 2)

#one-liner for yates
chisq.test(obs, correct = TRUE)

#==============one-way ANOVA===============
#Assumes k samples come from normal dist
#Assumes variances are eaqual across k samples ***central assumtion
#H0: mu1 = mu2 = ... = muk
#HA: the mean is not the same for all k samples
#converting data file to long format to work with on ANOVA
#whether the independent variable is fixed or random effects breadth of conclusion
#if H0 not true we use the non-central F distribution
dataWide <- read.csv(file = "")
View(dataWide)
library(tidyr) #gather function in tidyr
dataLong <- gather(data = dataWide, 
                   key = independentvar, #independent variable
                   value = dependentvar, #dependent variable
                   #col1:col2, 
                   factor_key = TRUE)

#####long-way ANOVA
N <- length(dataLong)
n_i <- length(datawide[,1])
k <- 
groupmeans <- tapply(dataLong$dependentvar, datalong$independentvar, mean)
mu_i.vector <- as.vector(groupmeans)
#thru Meank <- mean(dataWide[,k])

totalMean <- mean(dataLong$dependentvar) #change dependent var to whatever you set as the value in gather

(totalSS <- sum((dataLong$dependentvar - totalMean)^2))
(totalDF <- N -1)
(groupsSS <- sum((n_i*((groupmeans - totalMean)^2))))
(groupsDF <- k - 1)
(errorSS <- totalSS - groupsSS)
(errorDF <- totalDF - groupsDF)
(groupsMS <- groupsSS / groupsDF) #variance among group means
(errorMS <- errorSS / errorDF) #variance within each sample so samplesd^2 or s^2
(Fstat <- groupsMS / errorMS)
Fcrit <- qf(p =0.05, df1 = k-1, df2 = N-k, lower.tail = FALSE)
(pvalue <- pf(q = Fstat, df1 = k-1, df2 = N-k, lower.tail = FALSE))
(signif(pvalue, digits = 2))
pvalue < 0.05

#####short-way ANOVA
aov.res <- aov(formula=dependentvar~independentvar, #change them to whatever you set in your key and value in gather
               data = dataLong)
summary(aov.res)
#checking if variances are similar across k samples
vars <- apply(X=data,
              MARGIN=2, FUN=var)
max(vars)/min(vars)
#the ratio above should be less than about 4 in order to assume equal variance

#===========Welch's test for ANOVA with dissimilar variances========
#use when the ratio of variance is larger than 4
welchANOVA <- oneway.test(formula = dependentvar~independentvar,
                          data = dataLong)

#===============Bartlett's Test for equal variances
#generally shouldn't test for equal var before ANOVA (can also use Levene's Test instead)
bartlett.test(formula = dependentvar~independentvar, data = dataLong)

#=================Calculating Power for ANOVA============
#####long-way
n <-
k <- 
N <- n * k
v1 <- k - 1 #groups DF
v2 <- N - k #error DF
alpha <- 0.05
errorMS <- #which you can get with anova(aov())
mu_i.vector <- #the means of each k sample, in the one-way ANOVA line 141
mu <- sum(mu_i.vector)/k

#find fcrit
f.crit <- qf(p = alpha, df1 = v1, df2 = v2, lower.tail = FALSE)

#find ncp
ncp <- n*(sum((mu_i-mu)^2))/errorMS

#find power
power <- pf(q = f.crit, df1 = v1, df2 = v2, ncp = ncp, lower.tail = FALSE)

#####short-way

k <- #number of groups
n <-  #equal samples in each group
errorMS <-  #within group variance, error MS from ANOVA
mu_i <- c(Mean1, Mean2, Mean3) #mean of each group, include more as needed
power.anova.test(groups=k, n=n,
                 between.var=var(mu_i),
                 within.var=errorMS,
                 sig.level=0.05) 
#apparently will calculate any of the 1 of 5 parameters in the arguments, here we leave out power so it calculates it for us
#theoretically could use this to calculate sample size for a given power or you could refer to Assignment 6




#===========Calculating Power for ANOVA from minimum detectable difference, delta============
k <- 
n<-
N <- n*k
MSE <- 
delta <- 
f.crit <- qf(p=0.05, df1=k-1, df2=N-k, lower.tail=FALSE) #critical value of F from the regular F distribution 
ncp <- n*delta^2/(2*MSE) #non-centrality parameter 
power <- pf(q=f.crit, df1=k-1, df2=N-k, ncp=ncp, lower.tail=FALSE)


#============Sample Size required for a Desired Power, ANOVA=========
#select a randomly large value of n
k <- 
n <-
N <- n*k
errorMS <- 
delta <- 
alpha <- 0.05
desiredPower <-
v1 <- k -1 #groups DF
v2 <- N-k #error DF
fstat <- qf(p = alpha, df1 = v1, df2 = v2, lower.tail = FALSE)
ncp <- n*(delta^2)/(2*errorMS) 
power <- pf(q = fstat, v1, v2, ncp = ncp, lower.tail = FALSE)
power == desiredPower #returns true or false. if false you need to repeat with new n value

#while loop for finding power
n <- 
power <- 0
while(power < 0.9){
  print(n)
  k <- 
  errorMS <- 
  delta <- 
  N <- n*k
  alpha <- 
  ncp <- (N/k)*(delta^2)/(2*errorMS)
  fstat <- qf(p = alpha, df1 = k-1, df2 = N - k, lower.tail = FALSE)
  power <- pf(q = fstat, df1 = k-1, df2 = N - k, ncp = ncp, lower.tail = FALSE)
  n <- n+1
}
#alternatively can use the power.anova.test() and include all parameters except n and it should estimate it for us
#can only use if you have all the arguments except n : groups=k, power, between.var=var(mu_i),within.var=errorMS,sig.level=0.05

#===========Minimum detectable difference, ANOVA========
delta <- sqrt((2*errorMS*ncp)/n) #keep replacing ncp till you find the min detectable difference (smallest delta)

#would do this like sample size one and just change delta iteratively

#====================Max number of k===================
phi <- sqrt((n*(delta^2))/(2*k*sd)) # series of guesses till you find the value of k small enough to prod desired power

#although I would do this how i did the sample size one and just change k iteratively

#================power for random effects ANOVA=========
#must use regular F-dist for power calcs
errorMS <-
groupsMS <-
N <-
k <-
v2 <- N - k
alpah <- 0.05
fstat <- qf(p = alpha, df1 = v1, df2 = v2, lower.tail = FALSE)
power.random <- (v2*errorMS*fstat)/((v2-2)*groupsMS)


#=================Tukey Test, equal sample sizes==============
#recall all tukey tests can be applied only when you get a significant pvalue in your anova
#H0: mu_a == mu_b 
#HA: mu_a =/= mu_b
#for all possible pairs of a and b
#make sure your data is in long format
####long-way###
N <- 
n_i <- 
k <-
k*(k-1)/2 # this will give you the number of comparisons you need
(orderedgroupmeans <- sort(tapply(dataLong$dependentvar, datalong$independentvar, mean), 
                   decreasing = FALSE))
vectorgroupmeans <- as.vector(orderedgroupMeans)

#finding the differences for the means in order where we evaluate the largest difference first
diffMean3.1 <- vectorgroupmeans[3] - vectorgroupmeans[1]
diffMean3.2 <- vectorgroupmeans[3] - vectorgroupmeans[2]
diffMean2.1 <- vectorgroupmeans[2] - vectorgroupmeans[1]


#for data visualization purposes
differences <- c(diffMean3.1, diffMean3.2, diffMean2.1)
codes <- c("3 v 1", "3 v 2", "2 v 3")
multiComparisons <- c(codes, differences)

#finding error MS & DF
errorMS <- #should have already calculated it in anova if not aov()
(signif(errorMS, digits = 2))
errorDF <- N - k
(signif(errorDF, digits = 2))

#find standard error
SE <- sqrt(errorMS/n_i)
(signif(SE, digits = 2))

#find the q-statistics
qStat1 <- abs(diffMean3.1)/SE
(signif(qStat1, digits = 2))
qStat2 <- abs(diffMean3.2)/SE
(signif(qStat2, digits = 2))
qStat3 <- abs(diffMean2.1)/ SE
(signif(qStat3, digits = 2))

#find critical value

Qcrit <- qtukey(p = 0.05, nmeans = k, df = N - k, lower.tail = FALSE)  #nmeans is number of means which should be k

#compare the calculated q stats with critical q value
qStat1 > Qcrit
qStat2 > Qcrit
qStat3 > Qcrit

#p-values
pvalue1 <- ptukey(q = qStat1, df = N - k, nmeans = k, lower.tail = FALSE)
(signif(pvalue1, digits =2))
pvalue2 <-  ptukey(q = qStat2, df = N - k, nmeans = k, lower.tail = FALSE)
(signif(pvalue2, digits = 2))
pvalue3 <- ptukey(q = qStat3, df = N - k, nmeans = k, lower.tail = FALSE)
(signif(pvalue3, digits = 2))

###short-way###
tukey.res <- TukeyHSD(x = aov.res, ordered = TRUE, conf.level = 0.95) #refer to shorthand oneway anova
print(tukey.res)

#===============Tukey-Kramer Test, unequal sample sizes=============
#same as above in that you need to calculate differences SE changes
N <- 
k <-
k*(k-1)/2 # this will give you the number of comparisons you need
(orderedgroupmeans <- sort(tapply(dataLong$dependentvar, datalong$independentvar, mean), 
                           decreasing = FALSE))
vectorgroupmeans <- as.vector(orderedgroupMeans)

#finding the differences for the means in order where we evaluate the largest difference first
diffMean3.1 <- vectorgroupmeans[3] - vectorgroupmeans[1]
diffMean3.2 <- vectorgroupmeans[3] - vectorgroupmeans[2]
diffMean2.1 <- vectorgroupmeans[2] - vectorgroupmeans[1]


#for data visualization purposes
differences <- c(diffMean3.1, diffMean3.2, diffMean2.1)
codes <- c("3 v 1", "3 v 2", "2 v 3")
multiComparisons <- c(codes, differences)

#finding error MS & DF
errorMS <- #should have already calculated it in anova if not aov()
  (signif(errorMS, digits = 2))
errorDF <- N - k
(signif(errorDF, digits = 2))
n1 <-
n2 <- 
n3 <-
sekramer1 <- sqrt(errorMS *(1/2)*((1/n3)+(1/n1)))
sekramer2 <- sqrt(errorMS *(1/2)*((1/n3)+(1/n2)))
sekramer3 <- sqrt(errorMS *(1/2)*((1/n2)+(1/n1)))

qStat1kramer <- abs(diffMean3.1)/sekramer1
(signif(qStat1kramer, digits = 2))
qStat2kramer <- abs(diffMean3.2)/sekramer2
(signif(qStat2, digits = 2))
qStat3kramer <- abs(diffMean2.1)/ sekramer3
(signif(qStat3, digits = 2))

pvalue1kramer <- ptukey(q = qStat1kramer, df = N - k, nmeans = k, lower.tail = FALSE)
(signif(pvalue1, digits =2))
pvalue2kramer <-  ptukey(q = qStat2kramer, df = N - k, nmeans = k, lower.tail = FALSE)
(signif(pvalue2, digits = 2))
pvalue3kramer <- ptukey(q = qStat3kramer, df = N - k, nmeans = k, lower.tail = FALSE)
(signif(pvalue3, digits = 2))

#TukeyHSD() automatically uses the Tukey-Kramer method for unbalanced designs

#===============CI for distinct samples (post-tukey)===============
alpha <- 0.05
n_i <-
N <-
k <- 
v <- N - k
errorMS <-

Xbar1 <- #this is one sample mean that is significantly different from all others
tcrit <-  qt(p = 0.975, df = v) 

lower <- Xbar1 - (tcrit *(sqrt(errorMS/n_i)))
upper <- Xbar1 + (tcrit *(sqrt(errorMS/n_i)))

#==============CI for samples that aren't sig diff (post-tukey)===============
alpha <-
N <-
k <-
v <- N-k
Mean1 <-
Mean2 <-
n1 <-
n2 <-
errorMS <- 
Xbar.pool <- (sum((n1*Mean1),(n2*Mean2)))/ (sum(n1, n2)) # can include more means and n's just put 2
tcrit <-  qt(p = 0.975, df = v) 

lower <- Xbar.pool - tcrit * (sqrt(errorMS/sum(n1, n2)))
upper <- Xbar.pool + tcrit * (sqrt(errorMS/sum(n1, n2)))


#=======================CI for differences b/w samples====================
meandiff <- Mean1 - Mean2
k <-
v <- N - k
SEkramer <- sqrt(errorMS *(1/2)*((1/n1)+(1/n1))) #if n's are different length
SEnormal <- sqrt(errorMS/n_i) #if n's are the same length

qcrit <- qtukey(p = 0.95, df = v, nmeans = k) #for alpha 0.05 bc 1-alpha = 0.95

lower <- meandiff - qcrit* #SEkramer or SEnormal
upper <- meandiff + qcrit* #SEkramer or SEnormal



#====================two-way ANOVA, Fixed-effects model (I)====================
#H01: there is no effect of factor A on the variable 
#H02: there is no effect of factor B on the variable
#H03: interaction of A and B does not affect variable.
#HA1: inverse
#HA2: inverse
#HA3: inverse

#data should be in long format
#first version taught
grandMean <- mean(dataLong$dependentvar)
(signif(grandMean, digits = 3))
N <-nrow(dataLong) 
n <- 
a <-  #factor a, number of levels
b <-  #factor b, number of levels
totalSS <- sum((dataLong$dependentvar - grandMean)^2)
(signif(totalSS, digits = 5))
totalDF <- N - 1

cell.means <- tapply(X = dataLong$dependentvar, 
                     INDEX = list(dataLong$factorA, dataLong$factorB), 
                     FUN = mean) #tapply prints results in alphabetical order fyi
(round(cell.means, digits = 1))
cellSS <- n*sum((cell.means - grandMean)^2)
(signif(cellSS, digits = 5))
cellDF <- a * b -1

errorSS <- totalSS - cellSS
(signif(errorSS, digits = 4))
errorDF <- totalDF - cellDF

A.means <- tapply(X = dataLong$dependentvar,
                  INDEX = dataLong$factorA, FUN = mean)
(round(A.means, digit = 1))
factorASS <- b*n*sum((A.means - grandMean)^2)
(signif(factorASS, digits = 5))
factorADF <- a - 1

B.means <- tapply(X = dataLong$dependentvar, 
                  INDEX = dataLong$factorB, FUN = mean)
(round(B.means, digit =1))
factorBSS <- a*n*sum((B.means - grandMean)^2)
(signif(factorBSS, digits = 4))
factorBDF <- b - 1

AxBSS <- cellSS - factorASS - factorBSS
(signif(AxBSS, digits = 4))
AxBDF <- cellDF - factorADF - factorBDF

#mean squares
factorAMS <- factorASS/factorADF
(signif(factorAMS, digits = 5))
factorBMS <- factorBSS/factorBDF
(signif(factorBMS, digits = 4))

AxBMS <- AxBSS/AxBDF
(signif(AxBMS, digits = 3))
errorMS <- errorSS/errorDF
(signif(errorMS, digits =3))
#F-statistic
F_AI <- factorAMS/errorMS
(signif(F_A, digits = 3))
F_BI <- factorBMS/errorMS
(signif(F_B, digits = 3))
F_AxBI <- AxBMS/errorMS
(signif(F_AxB, digits = 2))
#critical values of F
crit_A <- qf(p = 0.05, df1 = factorADF, df2 = errorDF, lower.tail = FALSE)
crit_B <- qf(p = 0.05, df1 = factorBDF, df2 = errorDF, lower.tail = FALSE)
crit_AxB <- qf(p = 0.05, df1 = AxBDF, df2 = errorDF, lower.tail = FALSE)

#p-values 
pval_AI <- pf(q = F_AI, df1 = factorADF, df2 = errorDF, lower.tail = FALSE)
(signif(pval_AI, digits = 3))
pval_BI <- pf(q = F_BI, df1 = factorBDF, df2 = errorDF, lower.tail = FALSE)
(signif(pval_BI, digits = 3))
pval_AxBI <- pf(q = F_AxBI, df1 = AxBDF, df2 = errorDF, lower.tail = FALSE)
(signif(pval_AxBI, digits = 3))

####short-version##
#make sure your factor a and b are defined as.factor() before proceeding or stringAsFactors = TRUE in read.csv()
dataLong$factorA <- as.factor(dataLong$factorA)
dataLong$factorB <- as.factor(dataLong$factorB)
aov.res <- aov(formula = dependentvar ~ factorA + factorB +
                 factorA:factorB, data = dataLong)
summary(aov.res)

#====================two-way ANOVA, Random-effects model (II)====================
#changes how we calculate F statistic, both factors random
F_AII <- factorAMS/AxBMS
F_BII <- factorBMS/AxBMS
F_AxBII <- AxBMS/errorMS

pval_AII <- pf(q = F_AII, df1 = factorADF, df2 = errorDF, lower.tail = FALSE)
(signif(pval_AII, digits = 3))
pval_BII <- pf(q = F_BII, df1 = factorBDF, df2 = errorDF, lower.tail = FALSE)
(signif(pval_BII, digits = 3))
pval_AxBII <- pf(q = F_AxBII, df1 = AxBDF, df2 = errorDF, lower.tail = FALSE)
(signif(pval_AxBII, digits = 3))


#====================two-way ANOVA, Mixed-effects model (III)====================
#changes how we calculate F statistic but the rest is the same, factor A fixed, factor B random
F_AIII <- factorAMS/AxBMS
F_BIII <- factorBMS/errorMS
F_AxBIII <- AxBMS/errorMS

pval_AIII <- pf(q = F_AIII, df1 = factorADF, df2 = errorDF, lower.tail = FALSE)
(signif(pval_AIII, digits = 3))
pval_BIII <- pf(q = F_BIII, df1 = factorBDF, df2 = errorDF, lower.tail = FALSE)
(signif(pval_BIII, digits = 3))
pval_AxBIII <- pf(q = F_AxBIII, df1 = AxBDF, df2 = errorDF, lower.tail = FALSE)
(signif(pval_AxBIII, digits = 3))

#==========================two-way ANOVA, no replication in cells, randomized block/repeated-measures=======================
#no interaction test possible so there will be no section for that
a <-  #factor a, number of levels
b <-  #factor b, number of levels
N <-

grandMean <- mean(dataLong$dependentvar)
(signif(grandMean, digits = 3))
totalSS <- sum((dataLong$dependentvar - grandMean)^2) #just Xij instead of Xijl
(signif(totalSS, digits = 5))
totalDF <- N - 1

A.means <- tapply(X = dataLong$dependentvar,
                  INDEX = dataLong$factorA, FUN = mean) 
(round(A.means, digit = 1))
factorASS <- b*sum((A.means - grandMean)^2)
(signif(factorASS, digits = 5))
factorADF <- a - 1

B.means <- tapply(X = dataLong$dependentvar, 
                  INDEX = dataLong$factorB, FUN = mean)
(round(B.means, digit =1))
factorBSS <- a*sum((B.means - grandMean)^2)
(signif(factorBSS, digits = 4))
factorBDF <- b - 1

remainderSS <- totalSS - factorASS - factorBSS
remainderDF <- totalDF - factorADF - factorBDF

factorAMS <- factorASS/factorADF
(signif(factorAMS, digits = 5))
factorBMS <- factorBSS/factorBDF
(signif(factorBMS, digits = 4))
remainderMS <- remainderSS/remainderDF

#for Model I ANOVA, 
#test both F_A and F_B with caution

#for Model II ANOVA, both factors random
F_AII_norep <- factorAMS/remainderMS
F_BII_norep <- factorBMS/remainderMS

#for Model III ANOVA, RANDOMIZED BLOCK DESIGN and REPEATED MEASURES DESIGN
F_AII_norep <- factorAMS/remainderMS
#for F_B test with caution

pval_AII_norep <- pf(q=F_AII_norep, df1 = factorADF, 
                     df2 = remainderDF, lower.tail = FALSE)

###########randomized block or repeated measures short-way
aov.res <- aov(formula = dependentvar ~treatment+block, data = dataLong) #note doesnt include treatment:block
summary(aov.res) #residuals = remainder

#=====================interaction plot, two-way ANOVA======================
interaction.plot(x.factor = dataLong$factorA, trace.factor = dataLong$factorB,
                 response = dataLong$dependentvar,
                 xlab = , ylab = , 
                 trace.label=)
#note large interaction effect is seen as a big difference in slopes of lines

#================Levene's Test for Homogeneity of Variance (two-way ANOVA)==============
library(car)
leveneTest(dependentvar ~ factorA*factorB, data = dataLong)
#if p<0.05 we reject equal var

#==================ANOVA, testing normality=================
plot(aov.res, 1) #residuals v fitted
plot(aov.res, 2) #Q-Q plot

resids <- residuals(aov.res)
shapiro.test(x = resids) #returns shapiro-wilk test
# if p<0.05 we reject normality

#====================non-parametric two-way ANOVA===================
#aligned rank transform 
#when your data is not normal, although this method is debated over
library(ARTool)
m <- art(formula = dependentvar ~ factorA*factorB, data = dataLong)
anova(m)


#===================log transformation of data===============
data_prime <- ln(dataLong$dependentvar +1)

###95% CI of lognormal transformation
mean_log <- mean(data_prime)
variance <- var(data_prime)
n <- length(data_prime)
v <- n - 1
SE <- sqrt(variance/n)
tcrit <- qt(p = 0.05/2, df = v)

lower_log <- mean_log - tcrit*SE
upper_log <- mean_log + tcrit*SE

lower <- exp(lower_log) - 1
upper <- exp(upper_log) -1

#==============square-root transformation of data==============
data_prime <- sqrt(dataLong$dependentvar + 0.5)

###95% CI of square-root transformed data
mean_sq <- mean(data_prime)
variance <- var(data_prime)
n <- length(data_prime)
v <- n - 1
SE <- sqrt(variance/n)
tcrit <- qt(p = 0.05/2, df = v)

lower_sq <- mean_sq - tcrit*SE
upper_sq <- mean_sq + tcrit*SE

lower <- (lower_log^2) - 0.5
upper <- (upper_log^2) - 0.5

#=================arcsine transformation of data==================
data_prime <- asin(sqrt(dataLong$dependentvar))

###95% CI of arcsine transformed data
mean_arc <- mean(data_prime)
variance <- var(data_prime)
n <- length(data_prime)
v <- n - 1
SE <- sqrt(variance/n)
tcrit <- qt(p = 0.05/2, df = v)

lower_arc <- mean_arc - tcrit*SE
upper_arc <- mean_arc + tcrit*SE

lower <- (sin(lower_arc))^2
upper <- (sin(upper_arc))^2

#================additional transformations============
#logit transformation for percentage data
p_prime <- ln(proportions/1-proportions) #used for regression problems
#arcsine for percentage data close to 0 and 1
p_prime <- arcsin(sqrt((data+(3/8))/(length(data)+(3/4)))) #hard to back-transform
#reciprocal transfromation
data_prime <- 1/data
#square transfromation
data_prime = data^2

#===================random permutation test for means, simulations==================
data <- read.csv(, stringsAsFactors = FALSE)
hist(data$dependent)
test.stat <- (mean(data[data$independent == "indie1", 2]) - 
                mean(data[data$independent == "indie2", 2]))
nindie1 <- nrow(data[data$independent == "indie1", 2])
nindie2 <- nrow(mean(data[data$independent == "indie2", 2]))

Nsim <- 2000
sample.dist <- vector(length = Nsim)
ntotal <-nrow(data)

for(i in 1:Nsim){
  indie1.indicies <- sample(x = 1:ntotal, size = nindie1, 
                            replace = FALSE) 
  sample.dist[i] <- mean(data[indie1.indicies, 2]) - 
    mean(data[-indie1.indicies,2])
}
hist(sample.dist)                
sum(sample.dist > test.stat) #gives you a bunch of trues and false's
#you rerun the for loop and the sum then you find out how often 
#samples in your sample.dist exceed the test.stat

pval <- 2*sum(sample.dist > test.stat)/Nsim
print(pval)

#==================random permutation test for paired samples, simulations==================
XX <- read.csv()
teststat <- sum(XX$Diff) #Must have the differences b/w the conditions of each pair as it's own column
niter <- 2000000
results <- vector(length=niter)
for(i in 1:niter){
  signs <- sample(x = c(-1,1), size = length(XX$npairs),
                  replace = TRUE)
  results[i] <- sum(XX$Diff*signs)
}
pval <-sum(results >= teststat)/niter



#=======permutations of data=======
choose(n, k) 

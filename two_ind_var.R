#=======================Finding n for a desired CI=======================
#define half-width (d), alpha, variance, sample size 
#Note, d can be defined as "the 95% confidence interval for mean beak length must be no wider than 3 mm" 
#meaning d = 3/2

for (i in 1:10) {
  n <- variance * (qt(p = alpha/2, df = ceiling(n)-1)^2)/ d^2 # d = half width
  print(n)
}
ceiling (n)


#=======================Finding n for a given power========================
##for one-sample t-test testing mu == mu0

#define variance, delta (mu -mu0), alpha, beta, sample size, one-tail or two-tail
# where const <- variance/delta^2
# if one-tailed p = alpha 
#if two-tailed p = alpha/2 
for ( i in 1:10) {
  n <- const*(((qt(p = alpha, df = ceiling(n) - 1, lower.tail = F)) 
                           + (qt(p = beta, df = ceiling(n) - 1, lower.tail = F)))^2)
  print(n)
  
}
ceiling(n)


#================Finding minimum detectable difference, delta==================
##for one-sample t-distribution

#define alpha, beta, n, and sampale SD
#plug into
delta <- (samp.sd/sqrt(n))* ((qt(p = alpha, df = n - 1, lower.tail = F)) 
                             + (qt(p = beta, df = n - 1, lower.tail = F)))


#==================Estimating Power===================
#define hypothesized diff b/w means (delta), alpha, sample size, sample SE
samp.SE <- samp.sd/sqrt(n)
alphaT <- qt(p = alpha, df = n -1, lower.tail = F)
betaT <- (delta/samp.SE) - alphaT
beta <- pt(q = beta, df = n - 1, lower.tail = F)
power <- 1 - beta

#============================one-sample Z-test for mean==================
zstat <- Xbar - mu/ se
pnorm(zstat)
qnorm()

#==================one-sample t-test for mean====================
n <-
v <- n-1
se.samp <- s/sqrt(n)
tstat <- Xbar - mu / se.samp

lefttail <- pt(q = tstat, df = v)


#===================one-sample variance Chi-squared test================
v <- n -1
samp.var <- sample.sd^2
pop.var <- pop.sd^2
chistat <- v*samp.var/pop.var

lefttail <- pchisq(q = chistat, df = v)
righttail <- pchisq(q = chistat, df = v, lower.tail = F)
pvalue.twotail <- 2*min(lefttail, righttail)

#=================two-sample t-test for means with similar var================
n1 <-
n2 <-
alpha <- 
var1 <- var() #or sd1/sqrt(n1)
var2 <- var() #or sd2/sqrt(n2)
mean1 <- mean()
mean2 <- mean()
df <- n1 + n2 -2

#pooled variance for the population
varpool <- ((n1-1)*var1 + (n2-1)*var2)/df
print(varpool)

#SE of difference between means 
SEdiff <- sqrt((varpool/n1) + (varpool/n2))
print(SEdiff)

#t statistic
tobs <-(mean1 - mean2)/SEdiff
print(tstat)

#critical values
qt(p=alpha/2, df = df) # lower value for a two tailed with alpha 0.05
qt(p=1 - alpha/2, df = df) #upper value

#p-value 
areatoleft <- pt(q=tobs, df=df)
areatoright <- pt(q=tobs, df=df, left.tail = F)
pvalue <- 2*min(areatoleft, areatoright)
print(pvalue)


#==================Welch-test, two-sample t-test for mean with different var==============
#variances considered different enough when it's by more than a factor of 3
#defining variables
n1 <- 
n2 <- 
X1 <- 
X2 <- 
s1 <- 
s2 <-
se1 <- s1/sqrt(n1)
se2 <- s2/sqrt(n2)
sq.se1 <- se1^2
sq.se2 <- se2^2
df1 <- n1 - 1
df2 <- n2 - 1

#t prime statistic
tprime.num <- X1 - X2
tprime.denom <- sqrt(sq.se1 + sq.se2)
tprime <- tprime.num/tprime.denom

#v prime 
vprime.num <- (sq.se1+sq.se2)^2
vprim.denom1 <- ((sq.se1)^2)/df1
vprim.denom2 <- ((sq.se2)^2)/df2
vprim.denom <- vprim.denom1 + vprim.denom2
vprime <- vprime.num/vprim.denom

#critical values
qt(p=alpha/2, df = vprime) # lower value for a two tailed with alpha 0.05
qt(p= 1 - alpha/2, df = vprime) #upper value

#p-value
lefttail <- pt(q = tprime, df = vprime) #area we look at depends on the value of tprime and question
pvalue <- 2*lefttail

#============two-sample variance ratio F-test==========
#F-distribution used for two-sample variance ratio F-test
var1 <- 
  var2 <- 
  n1 <-
  n2 <-
  Fstat <- var1/var2 #larger value goes in numerator
v1 <- n1 -1 # if var2 ends up in numerator put n as n2 here in v1
v2 <- n2 -1 # vice versa

pf(Fstat, df1 = v1, df2 = v2)

#=================paired-sample t-test==============
##must have differences that are normally distributed
data1<-
data2<- 
diffs <- data1 - data2 #vector of differences
mean.diff <- mean(diffs)
se.diff <- sd(diffs)/sqrt(length(diffs))
tstat <- mean.diff/se.diff
areatoleft <- pt(q = tstat, df = (length(diffs) -1)) #change according to directionality and one tail vs two tail
pvalue <- 

#CI calc
mean.diff - (qt(p = alpha, df = (length(diffs)-1))*se.diff) #lower
mean.diff + (qt(p = alpha, df = (length(diffs)-1))*se.diff) #upper

#=============Wilcoxon Test============
##non-parametric
##when differences are not normally distributed

#Calculating T+ and T- for the wilcoxon test
data1 <-
data2<- 
diffs <- data1 - data2 #vector of differences
diffs <- diffs[diffs != 0] # deletes the zeros
absd <- abs(diffs) #absolute values of the differences
ranks <- rank(absd) # vector of ranks, accounting for tied ranks
posranks <- ranks[diffs>0]
Tplus <- sum(posranks)
negranks <- ranks [diffs<0]
Tminus <- sum(negranks)
n <- length(diffs)
smallerT <- min(Tplus,Tminus) #Find the smaller of the two values

#The critical T value
Talpha2n <- qsignrank(p = 0.05/2, n = n)

#Two-tailed Wilcoxon Test
#H0: measures from pop 1 == measures from pop 2
#HA: measures from pop 1 =/= measures from pop 2
#reject H0 when Tminus less than or equal to Talpha(2),n
#or reject H0 when Tplus less than or equal to Talpha(2),n
smallerT <- min(Tplus, Tminus)
leftarea <- psignrank(q = smallerT, n = n)
pvalue <- 2*leftarea

#one-tailed Wilcoxon Test: population 1 expected to be bigger
#H0: measures from pop 1 <|== measures from pop 2
#HA: measures from pop 1 > measures from pop 2
#reject H0 if Tminus <|== to Talpha(2),n

pvalue <- psignrank(q = Tminus, n = n) #area to left of Tminus

#if u do the shortcut with one tailed pop 1 expected to be bigger input for argument alternative = "greater"

#one-tailed test: population 2 expected to be bigger
#H0: measures from pop 1 >|== to measures from pop 2
#HA: measures from pop 1 < measures from pop 2
#reject H0 if Tplus <|== to Talpha(2),n

pvalue <- psignrank(q = Tplus, n = n) #area to left of Tplus

#==========Using the wilcox.test shortcut=============
wilcox.test(x = xvector, y = yvector, paired = T, alternative = )
#specifying paired = T gives identical answers to running the test the long way around
#handles tied ranks differently so it will have slightly diff estimates of p-value when it does

#=========================Using the t.test shortcuts==============
t.test(x, alternative = "two.sided", mu = 0, conf.level = 0.95) #one-sample two-tailed t-test for mean
t.test(x, alternative = "less", mu = 0, conf.level =0.95) # one sample one-sided (directionality looking at the left tail, for right say "greater") t-test for mean
t.test(x, y, alternative = "two.sided", mu = 0, var.equal = T, conf.level = 0.95) #two-sample two-tailed t-test for the means of samples with similar variances
t.test(x, y, alternative = "two.sided", mu = 0, var.equal = F, conf.level =0.95) #Welch test
t.test(x, y, paired = T) #used for a paired sample t-test


#===============CI's for two-sample t-tests=================
#Case 1: different means, equal variances (for each of the two sample means)
n1 <-
n2 <-
Xbar1 <-
Xbar2 <-
s1 <-
s2 <-
var1 <- s1/sqrt(n1)
var2 <- s2/sqrt(n2)
v <- n1 + n2 -2

varpool <- ((n1-1)*var1 + (n2-1)*var2)/v
print(varpool)

tcrit <- qt(q = alpha, df = v) #or alpha/2, tail depends on question

lower <- Xbar1 - tcrit * (sqrt(varpool/n1)) #insert X2 and n2 for other sample CI
upper <- Xbar1 + tcrit * (sqrt(varpool/n1))

#Case 1: different means, equal variances (for the difference b/w sample means)
#same v and varpool variables as above but needs
SEdiff <- sqrt((varpool/n1) + (varpool/n2))
print(SEdiff)

lower <- (Xbar1 - Xbar2) - tcrit * SEdiff
upper <- (Xbar1 - Xbar2) + tcrit * SEdiff

#Case 2: equal means, equal variances (for the common mean)
Xbar1 <-
Xbar2 <-
n1 <-
n2 <-
v <- n1 + n2 -2

#Calculate weighted mean
Xpool <- ((n1*Xbar1) + (n2*Xbar2))/ (n1+n2)
print(Xpool)

#pooled variance
varpool <- ((n1-1)*var1 + (n2-1)*var2)/v
print(varpool)

tcrit <- qt()

lower <- Xpool - (tcrit * (sqrt(varpool/(n1+n2))))
upper <- Xpool - (tcrit * (sqrt(varpool/(n1+n2))))

#Case 3: different means, different variances (for the population means of each sample)
n1 <- 
n2 <- 
X1 <- 
X2 <- 
s1 <- 
s2 <-
se1 <- s1/sqrt(n1)
se2 <- s2/sqrt(n2)
sq.se1 <- se1^2
sq.se2 <- se2^2

df1 <- n1 -1
df2 <- n2 - 1

vprime.num <- (sq.se1+sq.se2)^2
vprim.denom1 <- ((sq.se1)^2)/df1
vprim.denom2 <- ((sq.se2)^2)/df2
vprim.denom <- vprim.denom1 + vprim.denom2

vprime <- vprime.num/vprim.denom

tcrit<- qt(p = 0.05/2, df = vprime, lower.tail = F)

lower <- X1 - tcrit* se1 #plug in X2 and se2 for both lower and upper for pop 2
upper <- X1 + tcrit* se1

#Case 3: different means, different variances (for the difference b/w the means)
#same variables as above

lower <- (Xbar1 - Xbar2) - (tcrit * (sqrt(sq.se1+sq.se2)))
upper <- (Xbar1 - Xbar2) + (tcrit * (sqrt(sq.se1+sq.se2)))


#Case 4: equal means, different variances (for the joint mean)
Xbar1 <-
Xbar2 <-
n1 <-
n2 <-
vt <- n1 + n2 -1

Xpool <- ((n1*Xbar1) + (n2*Xbar2))/ (n1+n2)
print(Xpool)

st.sqrt <- ((Xbar1 - Xpool) + (Xbar2 - Xpool))^2/vt

lower <- Xpool - (tcrit * sqrt(st.sqrt/(n1+n2)))
upper <- Xpool + (tcrit * sqrt(st.sqrt/(n1+n2)))

#========geometric mean===========
geo <- exp(mean(log( vector)))


#======covariance=====
X <- rnorm(20)     #random normal sample of 20 numbers
Y <- rnorm(20)     #random normal sample of 20 numbers
print(cov(X,Y))    #0.045
print(var(X+Y))    #2.401
print(var(X) + var(Y) + 2*cov(X,Y)) #2.401
print(var(X-Y))    #2.219
print(var(X) + var(Y) - 2*cov(X,Y)) #2.219

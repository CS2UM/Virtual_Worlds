options(scipen=999)
library(RODBC)
library(ggplot2)
library(data.table)
library(lfe)
library(scales)
library(stargazer)
library(MASS)


############
# PART 1 - tools
############

# running an SQL query and putting the output in a data.table
# conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_METRICS;Trusted Connection=true;Integrated Security=true;');
# uc17 <- data.table(
#  sqlQuery(conn, "           
#           SELECT (something)
#           FROM (some table)
#           JOIN (do some joins)
#           WHERE (some conditions) 
#           ")

# some pre-processed character level observations
# I am using data.table
# for a benchmarking comparison of data.table, data frames, and dplyr see:
# https://github.com/Rdatatable/data.table/wiki/Benchmarks-:-Grouping
load(file="uc17.Rdata")
str(uc17)
summary(uc17)
uc17[1:20]

# Analysis: identity and agression related
uc17[,mean(violence),by=CGender]
uc17[,mean(violence),by=CCareer]
# male + military|Industry 
m1 <- lm(violence ~ CGender + CCareer   ,
           data=uc17)
stargazer(m1,single.row = FALSE,type="text")
# effects seem to hold over duration of game play
ggplot(uc17,
       aes(x=LogonHours, y=violence,group=CGender,color=CGender)) + 
  geom_smooth(span=10,size=1,se=T) + 
  labs(title = "", x = "Logon Hours", y = "Violence?",color="Char. Gender") +
  scale_color_brewer(palette="Set1")
ggplot(uc17,
       aes(x=LogonHours, y=violence,group=CCareer,color=CCareer)) + 
  geom_smooth(span=10,size=1,se=T) + 
  labs(title = "", x = "Logon Hours", y = "Violence?",color="Char. Career") +
  scale_color_brewer(palette="Set1")
# effects still present when controlling for duration
m2 <- lm(violence ~ CGender + CCareer + LogonHours  ,
         data=uc17)
stargazer(m1,m2,single.row = FALSE,type="text")


############
# PART 2 - inference
############

# issue: unobserved behavioral intentions

# partial solution 1: players often assume multiple identities
# can examine variance on agression within player and by identity type 

# how many characters created for the same account?
uc17[,charN := .N,by=ID]
setkey(uc17,charN)
uc17[,.N,by=charN]

# detrend within ID
uc17[charN > 1, viol_dev := (violence - mean(violence)),by=ID]
uc17[charN > 1,mean(viol_dev),by=list(CGender)]
uc17[charN > 1,mean(viol_dev),by=list(CCareer)]

# or use fixed effects
# note that there 37555 subjects with repeated observations 
nrow(uc17[charN>1,.N,by=ID])
# throwing in IDs as factors will choke your computer
# m3 <- lm(violence ~ CGender + CCareer + LogonHours + as.factor(ID)  ,data=uc17[charN > 1])
# high dimensional fixed effects packages de-trend the data before running regression
# for Stata see REGHDFE
m3 <- felm(violence ~ CGender + CCareer + LogonHours | ID  ,data=uc17[charN > 1])
stargazer(m2,m3,single.row = FALSE,type="text")


# partial solution 2: exploit natural experiments within the game
# looking for shocks, natural randomness, or rule/interface changes 
# things that come from the developers

# show image for career selection rates over time
# for 2 years career path was randomly assigned

# split sample into self-selected and random careers periods
setkey(uc17,randomC,CCareer)
uc17[,mean(violence),by=list(CCareer,randomC)]
m4 <- lm(violence ~ CGender + CCareer + LogonHours,
           data=uc17[randomC==0])
m5 <- lm(violence ~ CGender + CCareer + LogonHours  ,
           data=uc17[randomC==1])
stargazer(m2,m4,m5,single.row = FALSE,type="text")




############
# PART 3 - sharing
############

# issue: can't share accurate population data

# solution: reverse engineer from descriptive stats

# "TRUE" results and descriptive stats in presentation
# descriptive stats
means <- c(.339,.718,.449,.300,2.118,.333)
sds <- c(.473,.450,.497,.458,2.310,.471)
rs <-  matrix(
  c( 
     1,  0.043,  0.036, -0.015,  0.654, -0.051,
 0.043,      1,  0.041, -0.014, -0.016,  0.042,
 0.036,  0.041,      1, -0.591, -0.018, -0.165,
-0.015, -0.014, -0.591,      1,  0.019,  0.052,
 0.654, -0.016, -0.018,  0.019,      1, -0.081,
-0.051,  0.042, -0.165,  0.052, -0.081, 1
     ),ncol = 6, byrow = TRUE)

# manually compute var-cov
varcov <- matrix(0, ncol=length(sds), nrow=length(sds))
diag(varcov) <- sds
# variance on the diagonal
# covariance = (corxy * ( ( sd(dat$y)*sd(dat$x) )))
# e.g. cov(violence,male) = .043 * (.473*.450) = 0.00915255
varcov <- varcov %*% rs %*% varcov
colnames(varcov) <- c("violence","male","military","industry","Log_hours","randomC")
rownames(varcov) <- c("violence","male","military","industry","Log_hours","randomC")
varcov

# manually calculate betas
# subset cov(x) matrix (drop y)
X <-varcov[c(2,3,4,5),c(-1,-6)]
# subset cov(x,y) matrix
y <- varcov[1,2:5]
# solve for betas
# betas = inv(X'X) * X'y
# full equation
solve(t(X) %*% X) %*% t(X) %*% y
solve(X, y)

# use MASS to create simulated data
# in STATA use the CORR2DATA package
set.seed(1)
uc17synth <- data.table(mvrnorm(1000000, 
               mu = means,
               Sigma = varcov,
               empirical = TRUE))
names(uc17synth) <- c("violence","male","military","industry","Log_hours","randomC")
# indentical regression results
m6 <- lm(violence ~ male + industry + military  + Log_hours  ,
         data=uc17synth)
stargazer(m6,single.row = FALSE,type="text")

# BUT these are NOT a recreation of the original data
ggplot(uc17synth, aes(male)) + geom_histogram(binwidth = .01) 
ggplot(uc17synth, aes(military)) + geom_histogram(binwidth = .01)
ggplot(uc17synth, aes(violence)) + geom_histogram(binwidth = .01)
ggplot(uc17synth, aes(randomC)) + geom_histogram(binwidth = .01)

# explore some issues with the data
# male and military correlated
# see figure on presentation
ggplot(uc17synth, aes(x=male, y=military)) + 
  geom_smooth(size=1,se=T) 
# Interaction NOT valid
m7 <- lm(violence ~ male * military + industry + Log_hours  ,
         data=uc17synth)
# can change the DV
m8 <- lm(Log_hours ~ male + industry + military    ,
           data=uc17synth)
# can drop rhs variables
m9 <- lm(violence ~ male + industry + military   ,
         data=uc17synth)
stargazer(m6,m7,m8,m9,single.row = FALSE,type="text")


library(psych)
library(stats)
library(lme4)
library(readr)

WideData <- read_csv("http://github.com/jon-may/inthedancersmind/raw/master/FinalOverallData.csv")


LongData<-reshape(WideData, 
                  varying=c("cab1a",'Cab1No','cab2a','Cab2No','cab3a','Cab3No'),
                  v.names=c("cab","cabNo"),
                  timevar = "session", 
                  times = c("1",'2','3'), 
                  new.row.names = 1:1000,
                  direction = "long")
LongData$cabNoF<-as.factor(LongData$cabNo)

attach(WideData)


# We recruited 240 students in total, 111 to the control cohort in 2015 
# (76 females, 68%; 24 males, 22%; 11 did not state sex), 
# and 129 to the imagery cohort in 2016 (103 females, 80%, 24 males, 19%; 2 did not state sex). 

table(Cohort)
table(sex, Cohort)

# 204 were recruited from Trinity-Laban Conservatoire of Music and Dance; 
# 36 from Coventry University. 
table(GROUP)
table(GROUP,Cohort)

# Ages at recruitment overall ranged from 17.9 years to 28.2 years, 
# with a median of 19.0, 
describe(TestAge)

# and did not differ between cohorts t(238) = 0.25, p = .800. 
c1<-WideData[Cohort==1,]
c2<-WideData[Cohort==2,]
t.test(c1$TestAge,c2$TestAge,var.equal=TRUE)

# or institution  t(238) = 0.44, p = .662 
cov<-WideData[GROUP=="COV",]
tl<-WideData[GROUP!="COV",]
t.test(cov$TestAge,tl$TestAge,var.equal=TRUE)

# There were 187 whose native language was English, 
# and 53 (22%) who spoke English as a second language. 

table(LANG)

# All of the latter spoke English to IETL Level 6 (Competent User, with effective command of the language despite some inaccuracies).
# 
# The ATTA was completed by 215 students, with a mean Total of 62.1(SD=5.9) 
# and mean Level of 4.4 (SD=1.5), 

describe(ATTATOT)
describe(ATTAL)

# and there was no difference in these scores between the control and imagery cohorts 
# (Total: t(213) = 1.52, p = .130; Level: t(213) = 1.66, p = .098). 

t.test(c1$ATTATOT,c2$ATTATOT,var.equal = TRUE)
t.test(c1$ATTAL,c2$ATTAL,var.equal = TRUE)

#  or the institutions (Total: t(213) = 1.79, p = .074; Level: t(213) = 1.79, p = .075)
t.test(cov$ATTATOT,tl$ATTATOT,var.equal = TRUE)
t.test(cov$ATTAL,tl$ATTAL,var.equal = TRUE)

# 
# The non-native English speakers (Total M=58.8, SD=6.4; Level: M = 3.7, SD=1.5) 
# did score lower than the native English speakers 
# (Total M=63.1, SD=5.4, t(213) = 4.66, p < .001; Level: M = 4.6, SD=1.4, 
# t(213) = 3.87, p < .001). 
# 
describeBy(ATTATOT, group=LANG)
t.test(WideData[LANG==0,]$ATTATOT,WideData[LANG==1,]$ATTATOT,var.equal = TRUE)
describeBy(ATTAL, group=LANG)
t.test(WideData[LANG==0,]$ATTAL,WideData[LANG==1,]$ATTAL,var.equal = TRUE)



# Of the complete sample, 217 students completed the Flexible Thinking Tests (FTT) 
# at the first testing session, 
# 188 at the second session, and 126 at the final session. 

describeBy(LongData$cab, group=LongData$session)

# 
# Combining the data from all sessions, FTT scores were normally distributed 
# (skew = .35, kurtosis = .07) and ranged from 3 to 40, with a mean of 19.6. 
# 

describe(LongData$cab)

# The five different versions of the FTT were each completed by between 99 and 109 
# different individuals over the course of the study, 
# with people completing between one and three tests. 

table(LongData$cabNo)

# A simple oneway ANOVA showed no difference in means between the five versions
# F(4, 526) = 1.39, p = .237, etasq = .01; 

oneway.test(LongData$cab ~ LongData$cabNo, var.equal=TRUE)
oneway1<-aov(LongData$cab ~ LongData$cabNoF)
summary(oneway1)
etaSquared(oneway1)

# a linear mixed model with participant added as a random effect 
# also showed no differences between the versions, 
# entered as a fixed effect F(4, 520.6) = 1.31, p = .265, 
# and none of the versions’ coefficients were statistically significant.
# 
# HAVE NOT DONE THIS YET

     
     
     
# As with the ATTA, the FTT scores were lower for non-native English speakers 
# (first session: M=14.0, SD=4.9; second: M=18.0, SD=6.3; final: M=18.1, SD=6.0) 
# than for native English speakers 
# (first session: M=18.7, SD=5.8, t(215) = 5.10, p < .001; 
# second: M=21.4, SD=5.7, t(186)=3.40, p = .001; 
# final: M=22.4, SD=6.5, t(124) = 3.00, p = .003). 

describeBy(cab1a,group=LANG)
describeBy(cab2a,group=LANG)
describeBy(cab3a,group=LANG)

t.test(WideData[LANG==0,]$cab1a,WideData[LANG==1,]$cab1a,var.equal = TRUE)
t.test(WideData[LANG==0,]$cab2a,WideData[LANG==1,]$cab2a,var.equal = TRUE)
t.test(WideData[LANG==0,]$cab3a,WideData[LANG==1,]$cab3a,var.equal = TRUE)

# The control cohort and the imagery cohort did not differ in FTT scores at
# any of the testing sessions, when all those attending each session were 
# compared directly (first: t(215) = 1.48, p = .141; 
t.test(c1$cab1a,c2$cab1a,var.equal = TRUE)
# second: t(186)=0.38, p = .707; 
t.test(c1$cab2a,c2$cab2a,var.equal = TRUE)
# final: t(124)=1.03, p = .305). 
t.test(c1$cab3a,c2$cab3a,var.equal = TRUE)


#We had not expected to detect any differential change in the two cohorts’ 
#FTT scores by the second session, immediately after the imagery training, 
# and an ANOVA comparing the change from the first to second sessions showed 
#just a main an effect of time F(1,169) = 53.4, p < .001, etasq = .24, 
# no effect of cohort F<1, nor an interaction of time by cohort F<1.

# HAVE NOT DONE THIS YET

# However, we had predicted that FTT scores would differ by the time of the
# third session. An ANOVA comparing the change from first to final session 
# showed both an effect of session F(1,115) = 21.5, p < .001, etasq = .16, 
# and an interaction with cohort F(1,115) = 4.49, p = .036, etasq = .04; 
# again there was no main effect of cohort F<1. 

# HAVE NOT DONE THIS YET

# Two-tailed t tests on the students who completed both the first and final 
# sessions showed that the change in the control cohort’s FTT scores was not 
#statistically significant t(56) = 1.79, p = .079 
t.test(c1$cab1a,c1$cab3a,paired=TRUE, var.equal = TRUE)

#but that the imagery group did improve t(59) = 4.76, p < .001, (see figure X)
t.test(c2$cab1a,c2$cab3a,paired=TRUE, var.equal = TRUE)

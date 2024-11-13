library(arm)
library(ggplot2)
library(car)
library(tidyr)
library(dplyr)
library(gridExtra)

dat.debt <- read.csv("EOTST211_2033_individual.csv", header = TRUE)
# Removing the NSID column
dat.debt <- dat.debt[, -1]

# Getting rid of missing data
dat.debt[dat.debt <= -1] <- NA

# Applying factor to categorical columns
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,
          33,35,38,39,40,41,42,43)
dat.debt[cols] <- lapply(dat.debt[cols], factor)

# Merging predictors

# Sample size, and also a bit of common sense, employee and employed
dat.debt$W1wrk1aMP <- with(dat.debt, Recode(W1wrk1aMP, "c(1) = 'FullTimeEmployee'"))
dat.debt$W1wrk1aMP <- with(dat.debt, Recode(W1wrk1aMP, "c(2) = 'PartTimeEmployee'"))
dat.debt$W1wrk1aMP <- with(dat.debt, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.debt$W1wrk1aMP <- with(dat.debt, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.debt$W1wrk1aMP<-relevel(dat.debt$W1wrk1aMP,ref="FullTimeEmployee")

# Renaming so easier for me to interpret
dat.debt$W1condur5MP <- with(dat.debt, Recode(W1condur5MP, "c(1) = 'Yes'"))
dat.debt$W1condur5MP <- with(dat.debt, Recode(W1condur5MP, "c(2) = 'No'"))
dat.debt$W1condur5MP<-relevel(dat.debt$W1condur5MP,ref="Yes")

# Renaming 
dat.debt$W1hea2MP <- with(dat.debt, Recode(W1hea2MP, "c(1) = 'Yes'"))
dat.debt$W1hea2MP <- with(dat.debt, Recode(W1hea2MP, "c(2) = 'No'"))
dat.debt$W1hea2MP<-relevel(dat.debt$W1hea2MP,ref="No")

# Ordering and sample size, as it is more likely that YP has low number of younger siblings 
dat.debt$W1NoldBroHS <- with(dat.debt, Recode(W1NoldBroHS, "c(0) = 0"))
dat.debt$W1NoldBroHS <- with(dat.debt, Recode(W1NoldBroHS, "c(1) = 1"))
dat.debt$W1NoldBroHS <- with(dat.debt, Recode(W1NoldBroHS, "c(2,3,4,5,6,7,8,9,12) = '>2'"))
dat.debt$W1NoldBroHS<-relevel(dat.debt$W1NoldBroHS,ref='0')

# Merged based on sample size
dat.debt$W1InCarHH <- with(dat.debt, Recode(W1InCarHH, "c(1) = 'PrivateHousehold'"))
dat.debt$W1InCarHH <- with(dat.debt, Recode(W1InCarHH, "c(2,3,4) = 'Other'"))
dat.debt$W1InCarHH<-relevel(dat.debt$W1InCarHH,ref='PrivateHousehold')

# Based on sample size
dat.debt$W1hous12HH <- with(dat.debt, Recode(W1hous12HH, "c(1) = 'Owned'"))
dat.debt$W1hous12HH <- with(dat.debt, Recode(W1hous12HH, "c(2) = 'BeingBought'"))
dat.debt$W1hous12HH <- with(dat.debt, Recode(W1hous12HH, "c(4) = 'RentFromCouncilOrNewTown'"))
dat.debt$W1hous12HH <- with(dat.debt, Recode(W1hous12HH, "c(3,5,6,7,8) = 'Other'"))
dat.debt$W1hous12HH<-relevel(dat.debt$W1hous12HH,ref='Owned')

# Renaming
dat.debt$W1usevcHH <- with(dat.debt, Recode(W1usevcHH, "c(1) = 'Yes'"))
dat.debt$W1usevcHH <- with(dat.debt, Recode(W1usevcHH, "c(2) = 'No'"))
dat.debt$W1usevcHH<-relevel(dat.debt$W1usevcHH,ref="Yes")

# Sample size 
dat.debt$W1hiqualmum <- with(dat.debt, Recode(W1hiqualmum, "c(2) = 'FirstDegree'"))
dat.debt$W1hiqualmum <- with(dat.debt, Recode(W1hiqualmum, "c(7) = 'A-Levels'"))
dat.debt$W1hiqualmum <- with(dat.debt, Recode(W1hiqualmum, "c(15) = 'GCSEa-c'"))
dat.debt$W1hiqualmum <- with(dat.debt, Recode(W1hiqualmum, "c(20) = 'NoneMentioned'"))
dat.debt$W1hiqualmum <- with(dat.debt, Recode(W1hiqualmum, "c(1,3,4,5,6,8,9,10,11,12,13,14,16,17,18,19) = 'Other'"))
dat.debt$W1hiqualmum<-relevel(dat.debt$W1hiqualmum,ref='NoneMentioned')

# Renaming
dat.debt$W1wrkfullmum <- with(dat.debt, Recode(W1wrkfullmum, "c(1) = 'FullTime'"))
dat.debt$W1wrkfullmum <- with(dat.debt, Recode(W1wrkfullmum, "c(2) = 'PartTime'"))
dat.debt$W1wrkfullmum <- with(dat.debt, Recode(W1wrkfullmum, "c(3) = 'NotWorking'"))
dat.debt$W1wrkfullmum<-relevel(dat.debt$W1wrkfullmum,ref='FullTime')

# Sample size 
dat.debt$W1empsmum <- with(dat.debt, Recode(W1empsmum, "c(1) = 'PaidWork>30h'"))
dat.debt$W1empsmum <- with(dat.debt, Recode(W1empsmum, "c(2) = 'PaidWork<30h'"))
dat.debt$W1empsmum <- with(dat.debt, Recode(W1empsmum, "c(6) = 'LookingAfterHousehold'"))
dat.debt$W1empsmum <- with(dat.debt, Recode(W1empsmum, "c(3,4,5,7,8,9) = 'Other'"))
dat.debt$W1empsmum<-relevel(dat.debt$W1empsmum,ref='PaidWork>30h')

# Renaming
dat.debt$W1IndSchool <- with(dat.debt, Recode(W1IndSchool, "c(0) = 'Maintained'"))
dat.debt$W1IndSchool <- with(dat.debt, Recode(W1IndSchool, "c(1) = 'Independent'"))
dat.debt$W1IndSchool<-relevel(dat.debt$W1IndSchool,ref="Maintained")

# Sample size 
dat.debt$W1marstatmum <- with(dat.debt, Recode(W1marstatmum, "c(3) = 'LivingWithPartner'"))
dat.debt$W1marstatmum <- with(dat.debt, Recode(W1marstatmum, "c(2) = 'MarriedLiving'"))
dat.debt$W1marstatmum <- with(dat.debt, Recode(W1marstatmum, "c(5) = 'Divorced'"))
dat.debt$W1marstatmum <- with(dat.debt, Recode(W1marstatmum, "c(1,4,6,7) = 'Other'"))
dat.debt$W1marstatmum<-relevel(dat.debt$W1marstatmum,ref='MarriedLiving')

# Ordering and sample size, as there are usually less than 3 dependent children in household
dat.debt$W1depkids <- with(dat.debt, Recode(W1depkids, "c(1) = '1'"))
dat.debt$W1depkids <- with(dat.debt, Recode(W1depkids, "c(2) = '2'"))
dat.debt$W1depkids <- with(dat.debt, Recode(W1depkids, "c(3) = '3'"))
dat.debt$W1depkids <- with(dat.debt, Recode(W1depkids, "c(4,5,6,7,8,9,10) = '>3'"))
dat.debt$W1depkids<-relevel(dat.debt$W1depkids,ref='1')


# Renaming
dat.debt$W1famtyp2 <- with(dat.debt, Recode(W1famtyp2, "c(0) = 'No'"))
dat.debt$W1famtyp2 <- with(dat.debt, Recode(W1famtyp2, "c(1) = 'Yes'"))
dat.debt$W1famtyp2<-relevel(dat.debt$W1famtyp2,ref="No")

# Sample size
dat.debt$W1nssecfam <- with(dat.debt, Recode(W1nssecfam, "c(1) = 'HigherManagerial/Professional'"))
dat.debt$W1nssecfam <- with(dat.debt, Recode(W1nssecfam, "c(2) = 'LowerManagerial/Professional'"))
dat.debt$W1nssecfam <- with(dat.debt, Recode(W1nssecfam, "c(4) = 'SmallEmployers/OwnAccountWorkers'"))
dat.debt$W1nssecfam <- with(dat.debt, Recode(W1nssecfam, "c(3,5,6,7,8) = 'Other'"))
dat.debt$W1nssecfam<-relevel(dat.debt$W1nssecfam,ref='HigherManagerial/Professional')

# Sample size but some merges are not because of sample size because it is 
# unreasonable to merge ethnic groups based on sample size, so merged based on 
# ethnic groups from the UK governemnt
# https://www.ethnicity-facts-figures.service.gov.uk/style-guide/ethnic-groups
dat.debt$W1ethgrpYP <- with(dat.debt, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.debt$W1ethgrpYP <- with(dat.debt, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.debt$W1ethgrpYP <- with(dat.debt, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.debt$W1ethgrpYP <- with(dat.debt, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.debt$W1ethgrpYP <- with(dat.debt, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.debt$W1ethgrpYP<-relevel(dat.debt$W1ethgrpYP,ref="White")

# renaming
dat.debt$W1heposs9YP <- with(dat.debt, Recode(W1heposs9YP, "c(1) = 'VeryLikely'"))
dat.debt$W1heposs9YP <- with(dat.debt, Recode(W1heposs9YP, "c(2) = 'FairlyLikely'"))
dat.debt$W1heposs9YP <- with(dat.debt, Recode(W1heposs9YP, "c(3) = 'NotVeryLikely'"))
dat.debt$W1heposs9YP <- with(dat.debt, Recode(W1heposs9YP, "c(4) = 'NotAtAllLikely'"))
dat.debt$W1heposs9YP<-relevel(dat.debt$W1heposs9YP,ref="VeryLikely")

# Ordering
dat.debt$W1hwndayYP <- with(dat.debt, Recode(W1hwndayYP, "c(0,1) = '0-1'"))
dat.debt$W1hwndayYP <- with(dat.debt, Recode(W1hwndayYP, "c(2,3) = '2-3'"))
dat.debt$W1hwndayYP <- with(dat.debt, Recode(W1hwndayYP,"c(4,5) = '4-5'"))
dat.debt$W1hwndayYP<-relevel(dat.debt$W1hwndayYP,ref="0-1")

# renaming
dat.debt$W1truantYP <- with(dat.debt, Recode(W1truantYP, "c(1) = 'Yes'"))
dat.debt$W1truantYP <- with(dat.debt, Recode(W1truantYP, "c(2) = 'No'"))
dat.debt$W1truantYP<-relevel(dat.debt$W1truantYP,ref="No")

# renaming
dat.debt$W1alceverYP <- with(dat.debt, Recode(W1alceverYP, "c(1) = 'Yes'"))
dat.debt$W1alceverYP <- with(dat.debt, Recode(W1alceverYP, "c(2) = 'No'"))
dat.debt$W1alceverYP<-relevel(dat.debt$W1alceverYP,ref="No")

# renaming
dat.debt$W1bulrc <- with(dat.debt, Recode(W1bulrc, "c(1) = 'Yes'"))
dat.debt$W1bulrc <- with(dat.debt, Recode(W1bulrc, "c(2) = 'No'"))
dat.debt$W1bulrc<-relevel(dat.debt$W1bulrc,ref="No")

# renaming
dat.debt$W1disabYP <- with(dat.debt, Recode(W1disabYP, "c(1) = 'DisabilitySchoolingAffected'"))
dat.debt$W1disabYP <- with(dat.debt, Recode(W1disabYP, "c(2) = 'DisabilitySchoolingNotAffected'"))
dat.debt$W1disabYP <- with(dat.debt, Recode(W1disabYP, "c(3) = 'NoDisability'"))
dat.debt$W1disabYP<-relevel(dat.debt$W1disabYP,ref="NoDisability")

# renaming
dat.debt$W2disc1YP <- with(dat.debt, Recode(W2disc1YP, "c(1) = 'Yes'"))
dat.debt$W2disc1YP <- with(dat.debt, Recode(W2disc1YP, "c(2) = 'No'"))
dat.debt$W2disc1YP<-relevel(dat.debt$W2disc1YP,ref="No")

# renaming
dat.debt$W2depressYP <- with(dat.debt, Recode(W2depressYP, "c(1) = 'NotAtAll'"))
dat.debt$W2depressYP <- with(dat.debt, Recode(W2depressYP, "c(2) = 'NoMoreThanUsual'"))
dat.debt$W2depressYP <- with(dat.debt, Recode(W2depressYP, "c(3) = 'RatherMoreThanUsual'"))
dat.debt$W2depressYP <- with(dat.debt, Recode(W2depressYP, "c(4) = 'MuchMoreThanUsual'"))
dat.debt$W2depressYP<-relevel(dat.debt$W2depressYP,ref="NotAtAll")

# renaming
dat.debt$W6JobYP <- with(dat.debt, Recode(W6JobYP, "c(1) = 'Yes'"))
dat.debt$W6JobYP <- with(dat.debt, Recode(W6JobYP, "c(2) = 'No'"))
dat.debt$W6JobYP<-relevel(dat.debt$W6JobYP,ref="No")

# renaming
dat.debt$W6UnivYP <- with(dat.debt, Recode(W6UnivYP, "c(1) = 'Yes'"))
dat.debt$W6UnivYP <- with(dat.debt, Recode(W6UnivYP, "c(2) = 'No'"))
dat.debt$W6UnivYP<-relevel(dat.debt$W6UnivYP,ref="No")

# Sample size
dat.debt$W6acqno <- with(dat.debt, Recode(W6acqno, "c(1) = 'First/OtherDegree'"))
dat.debt$W6acqno <- with(dat.debt, Recode(W6acqno, "c(3) = '2+AL/AS'"))
dat.debt$W6acqno <- with(dat.debt, Recode(W6acqno,"c(4) = '5+GCSE'"))
dat.debt$W6acqno <- with(dat.debt, Recode(W6acqno,"c(9) = 'NoAcademicStudyAim'"))
dat.debt$W6acqno <- with(dat.debt, Recode(W6acqno,"c(2,5,6,7,8) = 'Other'"))
dat.debt$W6acqno<-relevel(dat.debt$W6acqno,ref="First/OtherDegree")

# renaming
dat.debt$W6gcse <- with(dat.debt, Recode(W6gcse, "c(1) = '5+'"))
dat.debt$W6gcse <- with(dat.debt, Recode(W6gcse, "c(2) = '1-4'"))
dat.debt$W6gcse <- with(dat.debt, Recode(W6gcse, "c(3) = 'Unknown'"))
dat.debt$W6gcse <- with(dat.debt, Recode(W6gcse, "c(4) = 'None'"))
dat.debt$W6gcse<-relevel(dat.debt$W6gcse,ref="None")

# renaming
dat.debt$W6als <- with(dat.debt, Recode(W6als, "c(1) = '2+'"))
dat.debt$W6als <- with(dat.debt, Recode(W6als, "c(2) = '1'"))
dat.debt$W6als <- with(dat.debt, Recode(W6als, "c(3) = 'NotKnown'"))
dat.debt$W6als <- with(dat.debt, Recode(W6als, "c(4) = 'None'"))
dat.debt$W6als<-relevel(dat.debt$W6als,ref="None")

# Renaming
dat.debt$W6OwnchiDV <- with(dat.debt, Recode(W6OwnchiDV, "c(1) = 'Yes'"))
dat.debt$W6OwnchiDV <- with(dat.debt, Recode(W6OwnchiDV, "c(2) = 'No'"))
dat.debt$W6OwnchiDV<-relevel(dat.debt$W6OwnchiDV,ref="No")

# renaming
dat.debt$W8DDEGP <- with(dat.debt, Recode(W8DDEGP, "c(0) = 'NoDegree'"))
dat.debt$W8DDEGP <- with(dat.debt, Recode(W8DDEGP, "c(1) = 'First/HigherDegree'"))
dat.debt$W8DDEGP<-relevel(dat.debt$W8DDEGP,ref="NoDegree")

# sample size
dat.debt$W8DMARSTAT <- with(dat.debt, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.debt$W8DMARSTAT <- with(dat.debt, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.debt$W8DMARSTAT <- with(dat.debt, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.debt$W8DMARSTAT<-relevel(dat.debt$W8DMARSTAT,ref="Single")

# sample size
dat.debt$W8DACTIVITYC <- with(dat.debt, Recode(W8DACTIVITYC, "c(1) = 'Employee'"))
dat.debt$W8DACTIVITYC <- with(dat.debt, Recode(W8DACTIVITYC, "c(2) = 'SelfEmployed'"))
dat.debt$W8DACTIVITYC <- with(dat.debt, Recode(W8DACTIVITYC, "c(4) = 'UnEmployed'"))
dat.debt$W8DACTIVITYC <- with(dat.debt, Recode(W8DACTIVITYC, "c(5) = 'Education'"))
dat.debt$W8DACTIVITYC <- with(dat.debt, Recode(W8DACTIVITYC, "c(3,6,7,8,10) = 'Other'"))
dat.debt$W8DACTIVITYC <- with(dat.debt, Recode(W8DACTIVITYC, "c(9) = 'LookingAfterHome'"))
dat.debt$W8DACTIVITYC<-relevel(dat.debt$W8DACTIVITYC,ref="Employee")

# rename
dat.debt$W8DWRK <- with(dat.debt, Recode(W8DWRK, "c(1) = 'CurrentlyEmployed'"))
dat.debt$W8DWRK <- with(dat.debt, Recode(W8DWRK, "c(2) = 'NotCurrentlyEmployed'"))
dat.debt$W8DWRK<-relevel(dat.debt$W8DWRK,ref="CurrentlyEmployed")

# rename
dat.debt$W8CMSEX <- with(dat.debt, Recode(W8CMSEX, "c(1) = 'Male'"))
dat.debt$W8CMSEX <- with(dat.debt, Recode(W8CMSEX, "c(2) = 'Female'"))
dat.debt$W8CMSEX<-relevel(dat.debt$W8CMSEX,ref="Female")

# sample size
dat.debt$W8TENURE <- with(dat.debt, Recode(W8TENURE, "c(2) = 'BuyWithMortgage/Loan'"))
dat.debt$W8TENURE <- with(dat.debt, Recode(W8TENURE, "c(4) = 'RentInc'"))
dat.debt$W8TENURE <- with(dat.debt, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.debt$W8TENURE <- with(dat.debt, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.debt$W8TENURE<-relevel(dat.debt$W8TENURE,ref="RentInc")

# natural ordering
dat.debt$W8QMAFI <- with(dat.debt, Recode(W8QMAFI, "c(1,2) = 'LivingComfortably/DoingAlright'"))
dat.debt$W8QMAFI <- with(dat.debt, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.debt$W8QMAFI <- with(dat.debt, Recode(W8QMAFI, "c(4,5) = 'QuiteDifficult/VeryDifficult'"))
dat.debt$W8QMAFI<-relevel(dat.debt$W8QMAFI,ref="LivingComfortably/DoingAlright")

summary(dat.debt)


## Plots
dat.debt.low<-subset(dat.debt,W8QDEB2<=12000)
# Subsetting so you can actually have a good view of the plots

# Plots for continuous outcome
# continuous data scatterplots
special.dat <- gather(data = dat.debt.low[, c(1,2,25,34,36,37,44,45)], -W8QDEB2, key = "var", value = "value")
p1 <- ggplot(special.dat, aes(x = value, y = W8QDEB2)) + geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~var, scales = "free_x")
p1 <- p1 + theme_bw() + scale_fill_grey() + theme(legend.position = "none")
p1

# categorical data bar plots
special.dat <- gather(data = dat.debt.low[, -c(1,2,25,34,36,37,44)], -W8QDEB2, key = "var", value = "value")
p2 <- ggplot(special.dat, aes(x = factor(value), y = W8QDEB2)) + geom_point(size = 0.5) +
  geom_boxplot() + facet_wrap(~var, scales = "free_x")

p2 <- p2 + theme_bw() + scale_fill_grey() + theme(legend.position = "none")
p2

# Consider having more than 0 debt to be having debt
dat.debt$hasdebt<-ifelse(dat.debt$W8QDEB2>0,1,0)

# Just to take a look
table(dat.debt$hasdebt)

# Plots for binary outcome and categorical predictors

special.dat <- gather(data = dat.debt[, -c(1,2,25,34,36,37,44,45)], -hasdebt, key = "var", value = "value")
p2 <- ggplot(special.dat, aes(x = factor(value), y = hasdebt))  +
  geom_bar(stat='identity') + facet_wrap(~var, scales = "free_x")
p2

# Checking the amount of missing data because it would affect in my analysis
colSums(is.na(dat.debt))/nrow(dat.debt)

# W1GrssyrMP, W8DDEGP and W8DAGEYCH with a lot of missing data, 44.9%, 38.5% and 84.2% respectively
# Remove from initial model

grssyr.glm <- glm(hasdebt ~ W1GrssyrMP, data = dat.debt, family=binomial(link="logit"))
summary(grssyr.glm)

ddegp.glm <- glm(hasdebt ~ W8DDEGP, data = dat.debt, family=binomial(link="logit"))
summary(ddegp.glm)

dageych.glm <- glm(hasdebt ~ W8DAGEYCH, data = dat.debt, family=binomial(link="logit"))
summary(dageych.glm)


# Original W8QDEB2 not needed in my model
dat.debt <- dat.debt[,-c(45)]


## Just to check the multicollinearity of predictors, running vif function
debt.lm <- lm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1condur5MP
              + W1hea2MP + W1NoldBroHS + W1InCarHH + W1hous12HH + W1usevcHH + W1hiqualmum 
              + W1wrkfullmum + W1empsmum + W1IndSchool + W1marstatmum + W1depkids + W1famtyp2 
              + W1nssecfam + W1ethgrpYP + W1heposs9YP + W6acqno + W8DACTIVITYC 
              + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
              + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
              + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE 
              + W8QMAFI + W8DINCW, data = dat.debt)
alias(debt.lm)

empsmum.glm <- glm(hasdebt ~ W1empsmum, data = dat.debt, family=binomial(link="logit"))
summary(empsmum.glm)

wrkfullmum.glm <- glm(hasdebt ~ W1wrkfullmum, data = dat.debt, family=binomial(link="logit"))
summary(wrkfullmum.glm)

debt.lm <- lm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1condur5MP
              + W1hea2MP + W1NoldBroHS + W1InCarHH + W1hous12HH + W1usevcHH + W1hiqualmum 
              + W1wrkfullmum + W1IndSchool + W1marstatmum + W1depkids + W1famtyp2 
              + W1nssecfam + W1ethgrpYP + W1heposs9YP + W6acqno + W8DACTIVITYC 
              + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
              + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
              + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE 
              + W8QMAFI + W8DINCW, data = dat.debt)
alias(debt.lm)
vif(debt.lm)

dactivityc <- glm(hasdebt ~ W8DACTIVITYC, data = dat.debt, family=binomial(link="logit"))
summary(dactivityc)

acqno.glm <- glm(hasdebt ~ W6acqno, data = dat.debt, family=binomial(link="logit"))
summary(acqno.glm)

marstatmum.glm <- glm(hasdebt ~ W1marstatmum, data = dat.debt, family=binomial(link="logit"))
summary(marstatmum.glm)

debt.lm <- lm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1condur5MP
              + W1hea2MP + W1NoldBroHS + W1InCarHH + W1hous12HH + W1usevcHH 
              + W1hiqualmum + W1IndSchool + W1depkids + W1famtyp2 
              + W1nssecfam + W1ethgrpYP + W1heposs9YP
              + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
              + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
              + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE
              + W8QMAFI + W8DINCW, data = dat.debt)
vif(debt.lm)

# W1empsmum removed 
# W1wrkfullmum, W1marstatmum removed  because of gvif >10
# Ran a univariate logistic regression

## Initial Model
debt.glm.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1condur5MP
              + W1hea2MP + W1NoldBroHS + W1InCarHH + W1hous12HH + W1usevcHH 
              + W1hiqualmum + W1IndSchool + W1depkids + W1famtyp2 
              + W1nssecfam + W1ethgrpYP + W1heposs9YP
              + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
              + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
              + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE
              + W8QMAFI + W8DINCW, data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.1)

deviances.difference <- debt.glm.1$null.deviance - debt.glm.1$deviance
df <- debt.glm.1$df.null - debt.glm.1$df.residual
pvalue <- pchisq(deviances.difference, df, lower.tail = FALSE)
pvalue
# significant 

# Need to do this or else the NA values in the rows would be included in the observed dataset
dat.debt.copy <- subset(dat.debt, select=-c(1,11,14,35,37))

# Classification tables/Confusion matrix
pred.glm<-as.numeric(debt.glm.1$fitted.values>0.5)
glm.dat<-data.frame(predicted=pred.glm, observed=na.omit(dat.debt.copy)$hasdebt)
tab<- table(glm.dat)
tab <- rbind(tab, c(round(prop.table(tab,2)[1, 1], 2), round((prop.table(tab,2)[2, 2]), 2)))
rownames(tab) <- c("pred=0", "pred=1","%corr")
colnames(tab) <- c("obs=0", "obs=1")
tab

anova(debt.glm.1, test="Chisq")

## Second Model
# Removing predictors that are not significant at 50%
# W1InCarHH, W1usevcHH,W1IndSchool, W1hwndayYP, W2ghq12scr
debt.glm.2<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1condur5MP
              + W1hea2MP + W1NoldBroHS + W1hous12HH  
              + W1hiqualmum + W1depkids + W1famtyp2 
              + W1nssecfam + W1ethgrpYP + W1heposs9YP
              + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
              + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
              + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
              + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE
              + W8QMAFI + W8DINCW, data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.2)

# Classification tables/Confusion matrix
dat.debt.copy <- subset(dat.debt, select=-c(1,7,9,11,13,14,20,25,35,37))
pred.glm<-as.numeric(debt.glm.2$fitted.values>0.5)
glm.dat<-data.frame(predicted=pred.glm, observed=na.omit(dat.debt.copy)$hasdebt)
tab<- table(glm.dat)
tab <- rbind(tab, c(round(prop.table(tab,2)[1, 1], 2), round((prop.table(tab,2)[2, 2]), 2)))
rownames(tab) <- c("pred=0", "pred=1","%corr")
colnames(tab) <- c("obs=0", "obs=1")
tab

# Checking Anova Test
anova(debt.glm.2,test="Chisq")

# Hypothesis testing between the first model
# Need to create a copy and reduce the dataset so that any two models are comparable
dat.debt.no.na <- subset(dat.debt, select= -c(1,11,14,30,35,37,39))
dat.debt.no.na <- na.omit(dat.debt.no.na)

debt.glm.1.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1condur5MP
                  + W1hea2MP + W1NoldBroHS + W1InCarHH + W1hous12HH + W1usevcHH 
                  + W1hiqualmum + W1IndSchool + W1depkids + W1famtyp2 
                  + W1nssecfam + W1ethgrpYP + W1heposs9YP
                  + W1hwndayYP + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
                  + W2ghq12scr + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
                  + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE
                  + W8QMAFI + W8DINCW, data = dat.debt.no.na, family=binomial(link="logit"))


debt.glm.2.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1condur5MP
                  + W1hea2MP + W1NoldBroHS + W1hous12HH  
                  + W1hiqualmum + W1depkids + W1famtyp2 
                  + W1nssecfam + W1ethgrpYP + W1heposs9YP
                  + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
                  + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
                  + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE
                  + W8QMAFI + W8DINCW, data = dat.debt.no.na, family=binomial(link="logit"))

anova(debt.glm.2.1,debt.glm.1.1,test="Chisq")
# No evidence to suggest that the larger model is better, so will continue with the smaller model

summary(debt.glm.2)

## Third model, removing predictors with no levels significant at 20%
# W1condur5MP, W1hea2MP, W1depkids, W1truantYP, W2disc1YP, W2depressYP, W6UnivYP, W6gcse, W8CMSEX, W8DINCW
debt.glm.3<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1NoldBroHS + W1hous12HH+ W1hiqualmum + W1famtyp2 
                + W1nssecfam + W1ethgrpYP + W1heposs9YP + W1alceverYP + W1bulrc + W1disabYP
                + W6JobYP + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.3)


# Classification tables/Confusion matrix
dat.debt.copy <- subset(dat.debt, select=-c(1,4,5,7,9,11,13,14,15,20,21,25,26,27,29,31,35,37,41,44))
pred.glm<-as.numeric(debt.glm.3$fitted.values>0.5)
glm.dat<-data.frame(predicted=pred.glm, observed=na.omit(dat.debt.copy)$hasdebt)
tab<- table(glm.dat)
tab <- rbind(tab, c(round(prop.table(tab,2)[1, 1], 2), round((prop.table(tab,2)[2, 2]), 2)))
rownames(tab) <- c("pred=0", "pred=1","%corr")
colnames(tab) <- c("obs=0", "obs=1")
tab

# Hypothesis testing
dat.debt.no.na <- subset(dat.debt, select= -c(1,7,9,11,13,14,20,25,35,37))
dat.debt.no.na <- na.omit(dat.debt.no.na)

debt.glm.2.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1condur5MP
                  + W1hea2MP + W1NoldBroHS + W1hous12HH  
                  + W1hiqualmum + W1depkids + W1famtyp2 
                  + W1nssecfam + W1ethgrpYP + W1heposs9YP
                  + W1truantYP + W1alceverYP + W1bulrc + W1disabYP
                  + W2disc1YP + W2depressYP + W6JobYP + W6UnivYP
                  + W6gcse + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8CMSEX + W8TENURE
                  + W8QMAFI + W8DINCW, data = dat.debt.no.na, family=binomial(link="logit"))

debt.glm.3.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1NoldBroHS + W1hous12HH+ W1hiqualmum + W1famtyp2 
                + W1nssecfam + W1ethgrpYP + W1heposs9YP + W1alceverYP + W1bulrc + W1disabYP
                + W6JobYP + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                data = dat.debt.no.na, family=binomial(link="logit"))

anova(debt.glm.3.1,debt.glm.2.1,test="Chisq")
# Again not enough evidence to suggest that the larger model is better

## Fourth Model, removing predictors non-signifcant at 10%
# W1nssecfam, W1NoldBroHS, W6als, W6OwnchiDV
debt.glm.4<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1heposs9YP + W1alceverYP + W1bulrc + W1disabYP
                + W6JobYP + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.4)

# Classification tables/Confusion matrix
dat.debt.copy <- subset(dat.debt, select=-c(1,4,5,6,7,9,11,13,14,15,17,20,21,25,26,27,29,31,32,33,35,37,41,44))
pred.glm<-as.numeric(debt.glm.4$fitted.values>0.5)
glm.dat<-data.frame(predicted=pred.glm, observed=na.omit(dat.debt.copy)$hasdebt)
tab<- table(glm.dat)
tab <- rbind(tab, c(round(prop.table(tab,2)[1, 1], 2), round((prop.table(tab,2)[2, 2]), 2)))
rownames(tab) <- c("pred=0", "pred=1","%corr")
colnames(tab) <- c("obs=0", "obs=1")
tab

# Hypothesis testing

dat.debt.no.na <- subset(dat.debt, select= -c(1,4,5,7,9,11,13,14,15,20,21,25,26,27,29,31,35,37,41,44))
dat.debt.no.na <- na.omit(dat.debt.no.na)

debt.glm.3.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1NoldBroHS + W1hous12HH+ W1hiqualmum + W1famtyp2 
                  + W1nssecfam + W1ethgrpYP + W1heposs9YP + W1alceverYP + W1bulrc + W1disabYP
                  + W6JobYP + W6als + W6OwnchiDV + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                  data = dat.debt.no.na, family=binomial(link="logit"))

debt.glm.4.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1heposs9YP + W1alceverYP + W1bulrc + W1disabYP
                + W6JobYP + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                data = dat.debt.no.na, family=binomial(link="logit"))

anova(debt.glm.4.1,debt.glm.3.1,test="Chisq")
# Again not enough evidence to suggest that the larger model is better


## 5th Model, 0.05 p-value
# W1heposs9YP, W1disabYP, W1bulrc
debt.glm.5<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1alceverYP + W6JobYP + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.5)

# Classification tables/Confusion matrix
dat.debt.copy <- subset(dat.debt, select=-c(1,4,5,6,7,9,11,13,14,15,17,19,20,21,23,24,25,26,27,29,31,32,33,35,37,41,44))
pred.glm<-as.numeric(debt.glm.5$fitted.values>0.5)
glm.dat<-data.frame(predicted=pred.glm, observed=na.omit(dat.debt.copy)$hasdebt)
tab<- table(glm.dat)
tab <- rbind(tab, c(round(prop.table(tab,2)[1, 1], 2), round((prop.table(tab,2)[2, 2]), 2)))
rownames(tab) <- c("pred=0", "pred=1","%corr")
colnames(tab) <- c("obs=0", "obs=1")
tab

# Hypothesis testing
dat.debt.no.na <- subset(dat.debt, select= -c(1,4,5,6,7,9,11,13,14,15,17,20,21,25,26,27,29,31,32,33,35,37,41,44))
dat.debt.no.na <- na.omit(dat.debt.no.na)

debt.glm.4.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                  + W1ethgrpYP + W1heposs9YP + W1alceverYP + W1bulrc + W1disabYP
                  + W6JobYP + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                  data = dat.debt.no.na, family=binomial(link="logit"))

debt.glm.5.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                  + W1ethgrpYP + W1alceverYP + W1bulrc + W6JobYP + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                  data = dat.debt.no.na, family=binomial(link="logit"))

anova(debt.glm.5.1,debt.glm.4.1,test="Chisq")
# Again not enough evidence to suggest that the larger model is better

anova(debt.glm.5, test="Chisq")

deviances.difference <- debt.glm.5$null.deviance - debt.glm.5$deviance
df <- debt.glm.5$df.null - debt.glm.5$df.residual
pvalue <- pchisq(deviances.difference, df, lower.tail = FALSE)
pvalue
# significant


## Sixth model, removing W1hiqualmum and W1ethgrpYP because only one level significant, 
# but only doing hypothesis test
dat.debt.no.na <- subset(dat.debt, select= -c(1,4,5,6,7,9,11,13,14,15,17,19,20,21,24,25,26,27,29,31,32,33,35,37,41,44))
dat.debt.no.na <- na.omit(dat.debt.no.na)

debt.glm.6<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH + W1famtyp2 
                  + W1alceverYP + W1bulrc + W6JobYP + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                  data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.6)

debt.glm.5.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                  + W1ethgrpYP + W1alceverYP + W1bulrc + W6JobYP + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                  data = dat.debt.no.na, family=binomial(link="logit"))

debt.glm.6.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH + W1hiqualmum + W1famtyp2 
                  + W1alceverYP + W1bulrc + W6JobYP + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                  data = dat.debt.no.na, family=binomial(link="logit"))

anova(debt.glm.6.1,debt.glm.5.1,test="Chisq")
# Significant p-value so it means that the larger model is better 


## 7th model
# Go with 5th model, adding back the initally removed predictors back in, due to multicollinearity 

# But need to check vif multicollinearity first before adding the predictors back in 
debt.lm <- lm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
               + W1ethgrpYP + W1alceverYP + W6JobYP + W6DebtattYP + W8DGHQSC
               + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
               + W6acqno + W8DACTIVITYC + W1wrkfullmum + W1marstatmum,
               data = dat.debt)
vif(debt.lm)

# Only W6acqno can be added back in due to multicollinearity reasons 
dat.debt.no.na <- subset(dat.debt, select= -c(1,11,14,35,37,39))
dat.debt.no.na <- na.omit(dat.debt.no.na)


debt.glm.7<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1alceverYP + W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
                , data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.7)

# It is significant

debt.glm.6.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH + W1famtyp2 
                  + W1alceverYP + W1bulrc + W6JobYP + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI, 
                  data = dat.debt.no.na, family=binomial(link="logit"))

debt.glm.7.1<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1alceverYP + W6JobYP + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
                + W6acqno,
                data = dat.debt.no.na, family=binomial(link="logit"))


anova(debt.glm.6.1,debt.glm.7.1,test="Chisq")
# Significant so the larger model is better

## Not Centering predictors

# continuing with debt.glm.7

hist(dat.debt$W1yschat1)
hist(dat.debt$W6DebtattYP)
hist(dat.debt$W8DGHQSC)

# Looking at the histograms, decided not to center

## Interactions
# Using debt.glm.7
debt.glm.7<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1alceverYP + W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
                , data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.7)

# W8QMAFI*W6JobYP
debt.glm.9 <- glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH + W1hiqualmum + W1famtyp2 
                  + W1ethgrpYP + W1alceverYP + W6acqno + W6DebtattYP + W8DGHQSC
                  + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI*W6JobYP
                  , data = dat.debt, family=binomial(link="logit"))
  
summary(debt.glm.9)

# W1alceverYP*W6JobYP
debt.glm.9<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH + W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1alceverYP*W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
                , data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.9)

# W1hiqualmum*W1famtyp2 
debt.glm.9<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH + W1hiqualmum*W1famtyp2 
                + W1ethgrpYP + W1alceverYP + W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
                , data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.9)

# W8DWRK*W8TENURE
debt.glm.9<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1alceverYP + W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK*W8TENURE + W8QMAFI
                , data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.9)

# W8QMAFI*W1hous12HH
debt.glm.9<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1alceverYP + W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI*W1hous12HH
                , data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.9)

# W1hous12HH*W1alceverYP
debt.glm.9<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH*W1alceverYP + W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
                , data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.9)

# W1hous12HH*W8TENUR
debt.glm.9<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH*W8TENURE + W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1alceverYP + W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8QMAFI
                , data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.9)

# W8DWRK*W6acqno
debt.glm.9<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH + W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1alceverYP + W6JobYP + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK*W6acqno + W8TENURE + W8QMAFI
                , data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.9)

# Did not manage to find any significant interactions so going with original model

## Final Model
debt.glm.final<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH+ W1hiqualmum + W1famtyp2 
                + W1ethgrpYP + W1alceverYP + W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
                , data = dat.debt, family=binomial(link="logit"))

summary(debt.glm.final)

anova(debt.glm.final, test="Chisq")

# Deviance calculation
deviances.difference <- debt.glm.final$null.deviance - debt.glm.final$deviance
df <- debt.glm.final$df.null - debt.glm.final$df.residual
pvalue <- pchisq(deviances.difference, df, lower.tail = FALSE)
pvalue
# significant 

# Classification tables/Confusion matrix
dat.debt.copy <- subset(dat.debt, select=c(2,3,8,10,16,18,22,28,30,34,36,38,40,42,43,45))
pred.glm<-as.numeric(debt.glm.final$fitted.values>0.5)
glm.dat<-data.frame(predicted=pred.glm, observed=na.omit(dat.debt.copy)$hasdebt)
tab<- table(glm.dat)
tab <- rbind(tab, c(round(prop.table(tab,2)[1, 1], 2), round((prop.table(tab,2)[2, 2]), 2)))
rownames(tab) <- c("pred=0", "pred=1","%corr")
colnames(tab) <- c("obs=0", "obs=1")
tab

## Missing Data to missing for categorical predictors
dat.debt.missing <- read.csv("EOTST211_2033_individual.csv", header = TRUE)
# Removing the NSID column
dat.debt.missing <- dat.debt.missing[, -1]

# Changing missing data to a categorical predictor "Missing"
dat.debt.missing$W1wrk1aMP[dat.debt.missing$W1wrk1aMP <= -1] <- "Missing"
dat.debt.missing$W1hous12HH[dat.debt.missing$W1hous12HH <= -1] <- "Missing"
dat.debt.missing$W1hiqualmum[dat.debt.missing$W1hiqualmum <= -1] <- "Missing"
dat.debt.missing$W1famtyp2[dat.debt.missing$W1famtyp2 <= -1] <- "Missing"
dat.debt.missing$W1ethgrpYP[dat.debt.missing$W1ethgrpYP <= -1] <- "Missing"
dat.debt.missing$W1alceverYP[dat.debt.missing$W1alceverYP <= -1] <- "Missing"
dat.debt.missing$W6JobYP[dat.debt.missing$W6JobYP <= -1] <- "Missing"
dat.debt.missing$W6acqno[dat.debt.missing$W6acqno <= -1] <- "Missing"
dat.debt.missing$W8DMARSTAT[dat.debt.missing$W8DMARSTAT <= -1] <- "Missing"
dat.debt.missing$W8DWRK[dat.debt.missing$W8DWRK <= -1] <- "Missing"
dat.debt.missing$W8TENURE[dat.debt.missing$W8TENURE <= -1] <- "Missing"
dat.debt.missing$W8QMAFI[dat.debt.missing$W8QMAFI <= -1] <- "Missing"

# Applying factor to categorical columns
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,
          33,35,38,39,40,41,42,43)
dat.debt.missing[cols] <- lapply(dat.debt.missing[cols], factor)

dat.debt.missing$W1wrk1aMP <- with(dat.debt.missing, Recode(W1wrk1aMP, "c(1) = 'FullTimeEmployee'"))
dat.debt.missing$W1wrk1aMP <- with(dat.debt.missing, Recode(W1wrk1aMP, "c(2) = 'PartTimeEmployee'"))
dat.debt.missing$W1wrk1aMP <- with(dat.debt.missing, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.debt.missing$W1wrk1aMP <- with(dat.debt.missing, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.debt.missing$W1wrk1aMP<-relevel(dat.debt.missing$W1wrk1aMP,ref="FullTimeEmployee")

# Based on sample size and a bit of common sense
dat.debt.missing$W1hous12HH <- with(dat.debt.missing, Recode(W1hous12HH, "c(1) = 'Owned'"))
dat.debt.missing$W1hous12HH <- with(dat.debt.missing, Recode(W1hous12HH, "c(2) = 'BeingBought'"))
dat.debt.missing$W1hous12HH <- with(dat.debt.missing, Recode(W1hous12HH, "c(4) = 'RentFromCouncilOrNewTown'"))
dat.debt.missing$W1hous12HH <- with(dat.debt.missing, Recode(W1hous12HH, "c(3,5,6,7,8) = 'Other'"))
dat.debt.missing$W1hous12HH<-relevel(dat.debt.missing$W1hous12HH,ref='Owned')

# Sample size 
dat.debt.missing$W1hiqualmum <- with(dat.debt.missing, Recode(W1hiqualmum, "c(2) = 'FirstDegree'"))
dat.debt.missing$W1hiqualmum <- with(dat.debt.missing, Recode(W1hiqualmum, "c(7) = 'A-Levels'"))
dat.debt.missing$W1hiqualmum <- with(dat.debt.missing, Recode(W1hiqualmum, "c(15) = 'GCSEa-c'"))
dat.debt.missing$W1hiqualmum <- with(dat.debt.missing, Recode(W1hiqualmum, "c(20) = 'NoneMentioned'"))
dat.debt.missing$W1hiqualmum <- with(dat.debt.missing, Recode(W1hiqualmum, "c(1,3,4,5,6,8,9,10,11,12,13,14,16,17,18,19) = 'Other'"))
dat.debt.missing$W1hiqualmum<-relevel(dat.debt.missing$W1hiqualmum,ref='NoneMentioned')

# Renaming
dat.debt.missing$W1famtyp2 <- with(dat.debt.missing, Recode(W1famtyp2, "c(0) = 'No'"))
dat.debt.missing$W1famtyp2 <- with(dat.debt.missing, Recode(W1famtyp2, "c(1) = 'Yes'"))
dat.debt.missing$W1famtyp2<-relevel(dat.debt.missing$W1famtyp2,ref="No")

# Sample size but some merges are not because of sample size because it is 
# unreasonable to merge ethnic groups based on sample size, so merged based on 
# ethnic groups from the UK governemnt
# https://www.ethnicity-facts-figures.service.gov.uk/style-guide/ethnic-groups
dat.debt.missing$W1ethgrpYP <- with(dat.debt.missing, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.debt.missing$W1ethgrpYP <- with(dat.debt.missing, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.debt.missing$W1ethgrpYP <- with(dat.debt.missing, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.debt.missing$W1ethgrpYP <- with(dat.debt.missing, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.debt.missing$W1ethgrpYP <- with(dat.debt.missing, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.debt.missing$W1ethgrpYP<-relevel(dat.debt.missing$W1ethgrpYP,ref="White")

# renaming
dat.debt.missing$W1alceverYP <- with(dat.debt.missing, Recode(W1alceverYP, "c(1) = 'Yes'"))
dat.debt.missing$W1alceverYP <- with(dat.debt.missing, Recode(W1alceverYP, "c(2) = 'No'"))
dat.debt.missing$W1alceverYP<-relevel(dat.debt.missing$W1alceverYP,ref="No")

# renaming
dat.debt.missing$W6JobYP <- with(dat.debt.missing, Recode(W6JobYP, "c(1) = 'Yes'"))
dat.debt.missing$W6JobYP <- with(dat.debt.missing, Recode(W6JobYP, "c(2) = 'No'"))
dat.debt.missing$W6JobYP<-relevel(dat.debt.missing$W6JobYP,ref="No")

# Sample size
dat.debt.missing$W6acqno <- with(dat.debt.missing, Recode(W6acqno, "c(1) = 'First/OtherDegree'"))
dat.debt.missing$W6acqno <- with(dat.debt.missing, Recode(W6acqno, "c(3) = '2+AL/AS'"))
dat.debt.missing$W6acqno <- with(dat.debt.missing, Recode(W6acqno,"c(4) = '5+GCSE'"))
dat.debt.missing$W6acqno <- with(dat.debt.missing, Recode(W6acqno,"c(9) = 'NoAcademicStudyAim'"))
dat.debt.missing$W6acqno <- with(dat.debt.missing, Recode(W6acqno,"c(2,5,6,7,8) = 'Other'"))
dat.debt.missing$W6acqno<-relevel(dat.debt.missing$W6acqno,ref="First/OtherDegree")

# sample size
dat.debt.missing$W8DMARSTAT <- with(dat.debt.missing, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.debt.missing$W8DMARSTAT <- with(dat.debt.missing, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.debt.missing$W8DMARSTAT <- with(dat.debt.missing, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.debt.missing$W8DMARSTAT<-relevel(dat.debt.missing$W8DMARSTAT,ref="Single")

# rename
dat.debt.missing$W8DWRK <- with(dat.debt.missing, Recode(W8DWRK, "c(1) = 'CurrentlyEmployed'"))
dat.debt.missing$W8DWRK <- with(dat.debt.missing, Recode(W8DWRK, "c(2) = 'NotCurrentlyEmployed'"))
dat.debt.missing$W8DWRK<-relevel(dat.debt.missing$W8DWRK,ref="CurrentlyEmployed")

# sample size
dat.debt.missing$W8TENURE <- with(dat.debt.missing, Recode(W8TENURE, "c(2) = 'BuyWithMortgage/Loan'"))
dat.debt.missing$W8TENURE <- with(dat.debt.missing, Recode(W8TENURE, "c(4) = 'RentInc'"))
dat.debt.missing$W8TENURE <- with(dat.debt.missing, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.debt.missing$W8TENURE <- with(dat.debt.missing, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.debt.missing$W8TENURE<-relevel(dat.debt.missing$W8TENURE,ref="RentInc")

# natural ordering
dat.debt.missing$W8QMAFI <- with(dat.debt.missing, Recode(W8QMAFI, "c(1,2) = 'LivingComfortably/DoingAlright'"))
dat.debt.missing$W8QMAFI <- with(dat.debt.missing, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.debt.missing$W8QMAFI <- with(dat.debt.missing, Recode(W8QMAFI, "c(4,5) = 'QuiteDifficult/VeryDifficult'"))
dat.debt.missing$W8QMAFI<-relevel(dat.debt.missing$W8QMAFI,ref="LivingComfortably/DoingAlright")

dat.debt.missing$hasdebt<-ifelse(dat.debt.missing$W8QDEB2>0,1,0)
dat.debt.missing <- dat.debt.missing[,-c(45)]

debt.glm.missing<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH + W1hiqualmum + W1famtyp2 
                    + W1ethgrpYP + W1alceverYP + W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                    + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
                    , data = dat.debt.missing, family=binomial(link="logit"))

summary(debt.glm.missing)

anova(debt.glm.missing, test="Chisq")

# Original dataset
# Leaving it as it is will the negative values as categorical predictors
dat.debt.missing <- read.csv("EOTST211_2033_individual.csv", header = TRUE)
# Removing the NSID column
dat.debt.missing <- dat.debt.missing[, -1]

# Changing the missing data in continuous predictors to NA first
dat.debt.missing$W1yschat1[dat.debt.missing$W1yschat1 <= -1 ] <- NA
dat.debt.missing$W6DebtattYP[dat.debt.missing$W6DebtattYP <= -1 ] <- NA
dat.debt.missing$W8DGHQSC[dat.debt.missing$W8DGHQSC <= -1 ] <- NA

# Applying factor to categorical columns
cols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,
          33,35,38,39,40,41,42,43)
dat.debt.missing[cols] <- lapply(dat.debt.missing[cols], factor)


dat.debt.missing$W1wrk1aMP <- with(dat.debt.missing, Recode(W1wrk1aMP, "c(1) = 'FullTimeEmployee'"))
dat.debt.missing$W1wrk1aMP <- with(dat.debt.missing, Recode(W1wrk1aMP, "c(2) = 'PartTimeEmployee'"))
dat.debt.missing$W1wrk1aMP <- with(dat.debt.missing, Recode(W1wrk1aMP,"c(10) = 'LookingAfterFamily'"))
dat.debt.missing$W1wrk1aMP <- with(dat.debt.missing, Recode(W1wrk1aMP,"c(3,4,5,6,7,8,9,11,12) = 'Other'"))
dat.debt.missing$W1wrk1aMP<-relevel(dat.debt.missing$W1wrk1aMP,ref="FullTimeEmployee")

# Based on sample size and a bit of common sense
dat.debt.missing$W1hous12HH <- with(dat.debt.missing, Recode(W1hous12HH, "c(1) = 'Owned'"))
dat.debt.missing$W1hous12HH <- with(dat.debt.missing, Recode(W1hous12HH, "c(2) = 'BeingBought'"))
dat.debt.missing$W1hous12HH <- with(dat.debt.missing, Recode(W1hous12HH, "c(4) = 'RentFromCouncilOrNewTown'"))
dat.debt.missing$W1hous12HH <- with(dat.debt.missing, Recode(W1hous12HH, "c(3,5,6,7,8) = 'Other'"))
dat.debt.missing$W1hous12HH<-relevel(dat.debt.missing$W1hous12HH,ref='Owned')

# Sample size 
dat.debt.missing$W1hiqualmum <- with(dat.debt.missing, Recode(W1hiqualmum, "c(2) = 'FirstDegree'"))
dat.debt.missing$W1hiqualmum <- with(dat.debt.missing, Recode(W1hiqualmum, "c(7) = 'A-Levels'"))
dat.debt.missing$W1hiqualmum <- with(dat.debt.missing, Recode(W1hiqualmum, "c(15) = 'GCSEa-c'"))
dat.debt.missing$W1hiqualmum <- with(dat.debt.missing, Recode(W1hiqualmum, "c(20) = 'NoneMentioned'"))
dat.debt.missing$W1hiqualmum <- with(dat.debt.missing, Recode(W1hiqualmum, "c(1,3,4,5,6,8,9,10,11,12,13,14,16,17,18,19) = 'Other'"))
dat.debt.missing$W1hiqualmum<-relevel(dat.debt.missing$W1hiqualmum,ref='NoneMentioned')

# Renaming
dat.debt.missing$W1famtyp2 <- with(dat.debt.missing, Recode(W1famtyp2, "c(0) = 'No'"))
dat.debt.missing$W1famtyp2 <- with(dat.debt.missing, Recode(W1famtyp2, "c(1) = 'Yes'"))
dat.debt.missing$W1famtyp2<-relevel(dat.debt.missing$W1famtyp2,ref="No")

# Sample size but some merges are not because of sample size because it is 
# unreasonable to merge ethnic groups based on sample size, so merged based on 
# ethnic groups from the UK governemnt
# https://www.ethnicity-facts-figures.service.gov.uk/style-guide/ethnic-groups
dat.debt.missing$W1ethgrpYP <- with(dat.debt.missing, Recode(W1ethgrpYP, "c(1) = 'White'"))
dat.debt.missing$W1ethgrpYP <- with(dat.debt.missing, Recode(W1ethgrpYP, "c(2) = 'Mixed/MultipleEthnicGroups'"))
dat.debt.missing$W1ethgrpYP <- with(dat.debt.missing, Recode(W1ethgrpYP,"c(3,4,5) = 'Asian/AsianBritish'"))
dat.debt.missing$W1ethgrpYP <- with(dat.debt.missing, Recode(W1ethgrpYP,"c(6,7) = 'Black/BlackBritish/Caribbean/African'"))
dat.debt.missing$W1ethgrpYP <- with(dat.debt.missing, Recode(W1ethgrpYP,"c(8) = 'Other'"))
dat.debt.missing$W1ethgrpYP<-relevel(dat.debt.missing$W1ethgrpYP,ref="White")

# renaming
dat.debt.missing$W1alceverYP <- with(dat.debt.missing, Recode(W1alceverYP, "c(1) = 'Yes'"))
dat.debt.missing$W1alceverYP <- with(dat.debt.missing, Recode(W1alceverYP, "c(2) = 'No'"))
dat.debt.missing$W1alceverYP<-relevel(dat.debt.missing$W1alceverYP,ref="No")

# renaming
dat.debt.missing$W6JobYP <- with(dat.debt.missing, Recode(W6JobYP, "c(1) = 'Yes'"))
dat.debt.missing$W6JobYP <- with(dat.debt.missing, Recode(W6JobYP, "c(2) = 'No'"))
dat.debt.missing$W6JobYP<-relevel(dat.debt.missing$W6JobYP,ref="No")

# Sample size
dat.debt.missing$W6acqno <- with(dat.debt.missing, Recode(W6acqno, "c(1) = 'First/OtherDegree'"))
dat.debt.missing$W6acqno <- with(dat.debt.missing, Recode(W6acqno, "c(3) = '2+AL/AS'"))
dat.debt.missing$W6acqno <- with(dat.debt.missing, Recode(W6acqno,"c(4) = '5+GCSE'"))
dat.debt.missing$W6acqno <- with(dat.debt.missing, Recode(W6acqno,"c(9) = 'NoAcademicStudyAim'"))
dat.debt.missing$W6acqno <- with(dat.debt.missing, Recode(W6acqno,"c(2,5,6,7,8) = 'Other'"))
dat.debt.missing$W6acqno<-relevel(dat.debt.missing$W6acqno,ref="First/OtherDegree")

# sample size
dat.debt.missing$W8DMARSTAT <- with(dat.debt.missing, Recode(W8DMARSTAT, "c(1) = 'Single'"))
dat.debt.missing$W8DMARSTAT <- with(dat.debt.missing, Recode(W8DMARSTAT, "c(2) = 'Married'"))
dat.debt.missing$W8DMARSTAT <- with(dat.debt.missing, Recode(W8DMARSTAT, "c(3,4,5,6,7,8,9) = 'Other'"))
dat.debt.missing$W8DMARSTAT<-relevel(dat.debt.missing$W8DMARSTAT,ref="Single")

# rename
dat.debt.missing$W8DWRK <- with(dat.debt.missing, Recode(W8DWRK, "c(1) = 'CurrentlyEmployed'"))
dat.debt.missing$W8DWRK <- with(dat.debt.missing, Recode(W8DWRK, "c(2) = 'NotCurrentlyEmployed'"))
dat.debt.missing$W8DWRK<-relevel(dat.debt.missing$W8DWRK,ref="CurrentlyEmployed")

# sample size
dat.debt.missing$W8TENURE <- with(dat.debt.missing, Recode(W8TENURE, "c(2) = 'BuyWithMortgage/Loan'"))
dat.debt.missing$W8TENURE <- with(dat.debt.missing, Recode(W8TENURE, "c(4) = 'RentInc'"))
dat.debt.missing$W8TENURE <- with(dat.debt.missing, Recode(W8TENURE, "c(5) = 'RentFree'"))
dat.debt.missing$W8TENURE <- with(dat.debt.missing, Recode(W8TENURE, "c(1,3,6,7) = 'Other'"))
dat.debt.missing$W8TENURE<-relevel(dat.debt.missing$W8TENURE,ref="RentInc")

# natural ordering
dat.debt.missing$W8QMAFI <- with(dat.debt.missing, Recode(W8QMAFI, "c(1,2) = 'LivingComfortably/DoingAlright'"))
dat.debt.missing$W8QMAFI <- with(dat.debt.missing, Recode(W8QMAFI, "c(3) = 'GettingBy'"))
dat.debt.missing$W8QMAFI <- with(dat.debt.missing, Recode(W8QMAFI, "c(4,5) = 'QuiteDifficult/VeryDifficult'"))
dat.debt.missing$W8QMAFI<-relevel(dat.debt.missing$W8QMAFI,ref="LivingComfortably/DoingAlright")

dat.debt.missing$hasdebt<-ifelse(dat.debt.missing$W8QDEB2>0,1,0)
dat.debt.missing <- dat.debt.missing[,-c(45)]

debt.glm.missing<-glm(hasdebt ~ W1yschat1 + W1wrk1aMP + W1hous12HH + W1hiqualmum + W1famtyp2 
                      + W1ethgrpYP + W1alceverYP + W6JobYP + W6acqno + W6DebtattYP + W8DGHQSC
                      + W8DMARSTAT + W8DWRK + W8TENURE + W8QMAFI
                      , data = dat.debt.missing, family=binomial(link="logit"))

summary(debt.glm.missing)

anova(debt.glm.missing, test="Chisq")

## Predicting Probabilities and Risk Ratios
dat.debt.copy <- subset(dat.debt, select=c(2,3,8,10,16,18,22,28,30,34,36,38,40,42,43,45))
pred.logit<-predict(debt.glm.final,dat.debt.copy)
pred.probs<-invlogit(pred.logit)
dat.debt.copy$predict <- pred.probs


# Log Odds
exp(coef(debt.glm.final))


#Statistics 202 Project

#NCHS_Injury_Mortality_United_States_1999_2014
#Source: https://catalog.data.gov/dataset/nchs-injury-mortality-united-states-1999a2014
#Column 1: YEAR (1999-2014)
#Column 2: SEX (Both Sexes, Females, Males)
#Column 3: AGE (All ages, < 15, 15-24, 25-44, 45-64, 65-74, 75+)
#Column 4: RACE (All races, hispanicanic, non-hispanicanic black, non-hispanicanic white)
#Column 5: INJURY MECHANISM (All mechanisms, All Other Specified, All Other Transport,
  # Cut/pierce, Drowning, Fall, Fire hot object/substance, Firearm, Motor Vechicle Traffic, 
  # Poisoning, Suffocation, unspecified)
#Column 6: INJURY INTENT (All intentions, Homicide, Legal Intervention/War, Suicide, 
  # Undetermined, Unintentional)
#Column 7: DEATHS (integer count)
#Column 8: POPULATION (integer count)
#Column 9-16: not important

#Import Data
library(readr)
mortal_data <- read_csv("~/Desktop/STATS/202/mortalitydata.csv")
View(mortal_data)

p_mortal_data <- as.vector(mortal_data[,7]/mortal_data[,8])
relcount <- round(p_mortal_data * 100000,2)
mortal_data <- cbind(mortal_data, relcount)
relcount <- mortal_data[,9]

#######################################################
#Suicide from 1999 to 2014
#######################################################
suicide <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="All Ages" 
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="All Mechanisms"
     & mortal_data[i,6]=="Suicide"){
      r <- as.vector(mortal_data[i,1:9])
      suicide <- rbind(suicide, r)
  }
}

year <- suicide[,1]
relcount_suicide <- suicide[,9]

#SUICIDE RATES
plot(year, relcount_suicide, xlab="Year", ylab = "Death Count per 100,000", 
     main = "Suicide Rates in the U.S. from 1999-2014", family="serif", xaxt="n"
     , type="l")
axis(1,at=year,labels=year, family="serif")


#######################################################
#Suicide by Firearm from 1999 to 2014
#######################################################
firearm <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="All Ages" 
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    firearm <- rbind(firearm, r)
  }
}

year <- firearm[,1]
relcount_firearm <- firearm[,9]

#SUICIDE RATES BY FIREARM
plot(year, relcount_firearm, xlab="Year", ylab = "Death Count per 100,000", 
      main = "Suicide Rates in the U.S. from 1999-2014", family="serif", 
      xaxt="n", type="l") 
axis(1,at=year,labels=year, family="serif")




#######################################################
#Suicide by Firearm Across Genders from 1999 to 2014
#######################################################
male <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Male" & mortal_data[i,3]=="All Ages"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    male <- rbind(male, r)
  }
}

year <- male[,1]
relcount_male<- male[,9]

female <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Female" & mortal_data[i,3]=="All Ages"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    female <- rbind(female, r)
  }
}

year <- female[,1]
relcount_female<- female[,9]

## SUICIDE RATES BY GENDER (SEPARATED)
par(mfrow=c(1,2))
plot(year, relcount_male, xlab="Year", ylab = "Death Count per 100,000", 
     main = "Male Suicide Rates in the U.S. from 1999-2014", family="serif", 
     xaxt="n", type="l", col="blue") 
axis(1,at=year,labels=year, family="serif")
plot(year, relcount_female, xlab="Year", ylab = "Death Count per 100,000", 
     main = "Female Suicide Rates in the U.S. from 1999-2014", family="serif", 
     xaxt="n", type="l", col="pink") 
axis(1,at=year,labels=year, family="serif")

## SUICIDE RATES BY GENDER (ON ONE GRAPH)
op <- par(family = "serif")
par(mfrow=c(1,1))
plot(x=year, y=relcount_male, ylim=c(0,1.1*max(relcount_male)),
     col='blue', type='l',
     main='Suicide Rates by Gender in U.S.', xlab='year', ylab='Death Count per 100,000',
     xaxt='n', lwd=2)
points(x=year, y=relcount_female, col='pink', type='l', lwd=2)
axis(1,at=year,labels=year)
legend(2010.5,7, c('Males','Females'), 
       col=c('blue','pink'), cex=0.7, lwd=2)
par(op)


#######################################################
#Suicide by Firearm Across Races from 1999 to 2014
#######################################################
hispanic <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="All Ages"
     & mortal_data[i,4]=="Hispanic" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    hispanic <- rbind(hispanic, r)
  }
}

white <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="All Ages"
     & mortal_data[i,4]=="Non-Hispanic white" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    white <- rbind(white, r)
  }
}

black <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="All Ages"
     & mortal_data[i,4]=="Non-Hispanic black" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    black <- rbind(black, r)
  }
}

relcount_hispanic <- hispanic[,9]
relcount_white <- white[,9]
relcount_black <- black[,9]

## SUICIDE RATES BY RACE (SEPARATED)
par(mfrow=c(1,3))

plot(year, 
     relcount_hispanic, 
     xlab="Year", 
     ylab = "Death Count per 100,000", 
     main = "Hispanic Firearm Suicide Rates", 
     family="serif", 
     xaxt="n", 
     type="l") 

axis(1,at=year,labels=year, family="serif")

plot(year, relcount_white, xlab="Year", ylab = "Death Count per 100,000", 
     main = "Non-Hispanic White Firearm Suicide Rates", family="serif", 
     xaxt="n", type="l") 

axis(1,at=year,labels=year, family="serif")
plot(year, relcount_black, xlab="Year", ylab = "Death Count per 100,000", 
     main = "Non-Hispanic Black Firearm Suicide Rates", family="serif", 
     xaxt="n", type="l") 

axis(1,at=year,labels=year, family="serif")

## SUICIDE RATES BY RACE (ON ONE GRAPH)
op <- par(family = "serif")
par(mfrow=c(1,1))
plot(x=year, y=relcount_hispanic, ylim=c(0,1.1*max(relcount_white)),
            col='red', type='l',
            main='Suicide Rates by Race in U.S.', xlab='Year', ylab='Death Count per 100,000',
            xaxt='n', lwd=2)
points(x=year, y=relcount_white, col='blue', type='l', lwd=2)
points(x=year, y=relcount_black, col='black', type='l', lwd=2)
axis(1,at=year,labels=year)
legend(2010.5,7, c('Hispanic','Non-Hispanic White','Non-Hispanic Black'), 
       col=c('red','blue','black'), cex=0.7, lwd=2)
par(op)


#######################################################
#Suicide by Firearm Across Age Groups from 1999 to 2014
#######################################################
teen <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    teen <- rbind(teen, r)
  }
}

young <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="15–24"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    young <- rbind(young, r)
  }
}

adult <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="25–44"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    adult <- rbind(adult, r)
  }
}

mid <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="45–64"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    mid <- rbind(mid, r)
  }
}

senior <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="65–74"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    senior <- rbind(senior, r)
  }
}

old <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="75+"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    old <- rbind(old, r)
  }
}

relcount_teen <- teen[,9]
relcount_young <- young[,9]
relcount_adult <- adult[,9]
relcount_mid <- mid[,9]
relcount_senior <- senior[,9]
relcount_old <- old[,9]

## SUICIDE RATES BY AGE (ZOOMED IN)
op <- par(family = "serif")
plot(x=year, y=relcount_teen, ylim=c(0,1.1*12),
     col='red', type='l',
     main='Suicide Rates by Age in U.S.', xlab='year', ylab='Death Count per 100,000',
     xaxt='n', lwd=2)
points(x=year, y=relcount_young, col='orange', type='l', lwd=2)
points(x=year, y=relcount_adult, col='yellow', type='l', lwd=2)
points(x=year, y=relcount_mid, col='green', type='l', lwd=2)
points(x=year, y=relcount_senior, col='blue', type='l', lwd=2)
points(x=year, y=relcount_old, col='pink', type='l', lwd=2)
axis(1,at=year,labels=year)
legend(2010.5,7, c('< 15','15-24','25-44','45-64',
                   '65-74','75+'), 
       col=c('red','orange','yellow','green','blue','pink'), cex=0.7, lwd=2)
par(op)

## SUICIDE RATES BY AGE (ZOOMED OUT)
par(mfrow=c(2,3))
plot(year, relcount_teen, xlab="Year", ylab = "Death Count per 100,000", 
     main = "< 15", family="serif", 
     xaxt="n", type="l") 
axis(1,at=year,labels=year, family="serif")
plot(year, relcount_young, xlab="Year", ylab = "Death Count per 100,000", 
     main = "15-24", family="serif", 
     xaxt="n", type="l") 
axis(1,at=year,labels=year, family="serif")
plot(year, relcount_adult, xlab="Year", ylab = "Death Count per 100,000", 
     main = "25-44", family="serif", 
     xaxt="n", type="l") 
axis(1,at=year,labels=year, family="serif")
plot(year, relcount_mid, xlab="Year", ylab = "Death Count per 100,000", 
     main = "45-64", family="serif", 
     xaxt="n", type="l") 
axis(1,at=year,labels=year, family="serif")
plot(year, relcount_senior, xlab="Year", ylab = "Death Count per 100,000", 
     main = "65-74", family="serif", 
     xaxt="n", type="l") 
axis(1,at=year,labels=year, family="serif")
plot(year, relcount_old, xlab="Year", ylab = "Death Count per 100,000", 
     main = "75+", family="serif", 
     xaxt="n", type="l") 
axis(1,at=year,labels=year, family="serif")



##################################################################
#Suicide by Firearm by Gender Across Age Groups from 1999 to 2014
##################################################################
# < 15 male versus female

mteen <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Male" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    mteen <- rbind(mteen, r)
  }
}

fteen <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Female" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    fteen <- rbind(fteen, r)
  }
}

relcount_mteen <- mteen[,9]
relcount_fteen <- fteen[,9]

# 15-24 male versus female

myoung <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Male" & mortal_data[i,3]=="15–24"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    myoung <- rbind(myoung, r)
  }
}

fyoung <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Female" & mortal_data[i,3]=="15–24"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    fyoung <- rbind(fyoung, r)
  }
}

relcount_myoung <- myoung[,9]
relcount_fyoung <- fyoung[,9]

# 25-44 male versus female

madult <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Male" & mortal_data[i,3]=="25–44"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    madult <- rbind(madult, r)
  }
}


fadult <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Female" & mortal_data[i,3]=="25–44"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    fadult <- rbind(fadult, r)
  }
}

relcount_madult <- madult[,9]
relcount_fadult <- fadult[,9]

# 45-64 male versus female

mmid <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Male" & mortal_data[i,3]=="45–64"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    mmid <- rbind(mmid, r)
  }
}

fmid <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Female" & mortal_data[i,3]=="45–64"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    fmid <- rbind(fmid, r)
  }
}

relcount_mmid <- mmid[,9]
relcount_fmid <- fmid[,9]

# 65-74 male versus female

msenior <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Male" & mortal_data[i,3]=="65–74"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    msenior <- rbind(msenior, r)
  }
}

fsenior <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Female" & mortal_data[i,3]=="65–74"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    fsenior <- rbind(fsenior, r)
  }
}

relcount_msenior <- msenior[,9]
relcount_fsenior <- fsenior[,9]

# 75+ male versus female

mold <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Male" & mortal_data[i,3]=="75+"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    mold <- rbind(mold, r)
  }
}

fold <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Female" & mortal_data[i,3]=="75+"
     & mortal_data[i,4]=="All races" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    fold <- rbind(fold, r)
  }
}

relcount_mold <- mold[,9]
relcount_fold <- fold[,9]


# LINE GRAPHS...SEPARATED BY AGE GROUPS + GENDERS
op <- par(family = "serif")
par(mfrow=c(2,3))
#TEEN
plot(year, relcount_mteen,  ylim=c(0,1.1*max(relcount_mteen)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "< 15", xaxt="n", type="l", 
     col="blue", lwd=2) 
axis(1,at=year,labels=year)
points(x=year, y=relcount_fteen, col='pink', type='l', lwd=2)
axis(1,at=year,labels=year)
legend('top', c('Males','Females'), 
       col=c('blue','pink'), cex=0.5, lwd=2)

#YOUNG
plot(year, relcount_myoung,  ylim=c(0,1.1*max(relcount_myoung)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "15-24", xaxt="n", type="l", 
     col="blue", lwd=2) 
axis(1,at=year,labels=year)
points(x=year, y=relcount_fyoung, col='pink', type='l', lwd=2)
axis(1,at=year,labels=year)
legend(2008,6, c('Males','Females'), 
       col=c('blue','pink'), cex=0.5, lwd=2)

#ADULT
plot(year, relcount_madult,  ylim=c(0,1.1*max(relcount_madult)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "25-44", xaxt="n", type="l", 
     col="blue", lwd=2) 
axis(1,at=year,labels=year)
points(x=year, y=relcount_fadult, col='pink', type='l', lwd=2)
axis(1,at=year,labels=year)
legend(2008,7, c('Males','Females'), 
       col=c('blue','pink'), cex=0.5, lwd=2)

#MID
plot(year, relcount_mmid,  ylim=c(0,1.1*max(relcount_mmid)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "45-64", xaxt="n", type="l", 
     col="blue", lwd=2) 
axis(1,at=year,labels=year)
points(x=year, y=relcount_fmid, col='pink', type='l', lwd=2)
axis(1,at=year,labels=year)
legend(2008,10, c('Males','Females'), 
       col=c('blue','pink'), cex=0.5, lwd=2)

#SENIOR
plot(year, relcount_msenior,  ylim=c(0,1.1*max(relcount_msenior)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "65-74", xaxt="n", type="l", 
     col="blue", lwd=2) 
axis(1,at=year,labels=year)
points(x=year, y=relcount_fsenior, col='pink', type='l', lwd=2)
axis(1,at=year,labels=year)
legend(2008,15, c('Males','Females'), 
       col=c('blue','pink'), cex=0.5, lwd=2)

#OLD
plot(year, relcount_mold,  ylim=c(0,1.1*max(relcount_mold)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "75+", xaxt="n", type="l", 
     col="blue", lwd=2) 
axis(1,at=year,labels=year)
points(x=year, y=relcount_fold, col='pink', type='l', lwd=2)
axis(1,at=year,labels=year)
legend(2008,20, c('Males','Females'), 
       col=c('blue','pink'), cex=0.5, lwd=2)

par(op)

# BAR GRAPHS...SEPARATED BY AGE GROUPS + GENDERS
op <- par(family = "serif")
par(mfrow=c(2,3))

#TEENS
teengender <- matrix(c(relcount_fteen,relcount_mteen), nrow=16, ncol=2)
rownames(teengender) <- as.character(year)
barplot(t(teengender), main="< 15", xlab="Years",
        col=c("hotpink1","cadetblue1"), beside=TRUE)
legend('top', c("Females","Males"), col=c("hotpink1","cadetblue1"), cex=0.5, lwd=2)

#YOUNG
younggender <- matrix(c(relcount_fyoung,relcount_myoung), nrow=16, ncol=2)
rownames(younggender) <- as.character(year)
barplot(t(younggender), main="15-24", xlab="Years",
        col=c("hotpink1","cadetblue1"), beside=TRUE)

#ADULT
adultgender <- matrix(c(relcount_fadult,relcount_madult), nrow=16, ncol=2)
rownames(adultgender) <- as.character(year)
barplot(t(adultgender), main="25-44", xlab="Years",
        col=c("hotpink1","cadetblue1"), beside=TRUE)

#MID
midgender <- matrix(c(relcount_fmid,relcount_mmid), nrow=16, ncol=2)
rownames(midgender) <- as.character(year)
barplot(t(midgender), main="45-64", xlab="Years",
        col=c("hotpink1","cadetblue1"), beside=TRUE)

#SENIOR
seniorgender <- matrix(c(relcount_fsenior,relcount_msenior), nrow=16, ncol=2)
rownames(seniorgender) <- as.character(year)
barplot(t(seniorgender), main="65-74", xlab="Years",
        col=c("hotpink1","cadetblue1"), beside=TRUE)

#OLD
oldgender <- matrix(c(relcount_fold,relcount_mold), nrow=16, ncol=2)
rownames(oldgender) <- as.character(year)
barplot(t(oldgender), main="75+", xlab="Years",
        col=c("hotpink1","cadetblue1"), beside=TRUE)

par(op)




###################################################################
# Suicide by Firearm by Races Across Age Groups from 1999 to 2014
###################################################################

#TEEN
teenhisp <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="Hispanic" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    teenhisp <- rbind(teenhisp, r)
  }
}

teenwhite <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="Non-Hispanic white" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    teenwhite <- rbind(teenwhite, r)
  }
}

teenblack <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="Non-Hispanic black" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    teenblack <- rbind(teenblack, r)
  }
}

relcount_teenhisp <- teenhisp[,9]
relcount_teenwhite <- teenwhite[,9]
relcount_teenblack <- teenblack[,9]

#YOUNG
younghisp <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="15–24"
     & mortal_data[i,4]=="Hispanic" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    younghisp <- rbind(younghisp, r)
  }
}

youngwhite <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="15–24"
     & mortal_data[i,4]=="Non-Hispanic white" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    youngwhite <- rbind(youngwhite, r)
  }
}

youngblack <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="15–24"
     & mortal_data[i,4]=="Non-Hispanic black" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    youngblack <- rbind(youngblack, r)
  }
}

relcount_younghisp <- younghisp[,9]
relcount_youngwhite <- youngwhite[,9]
relcount_youngblack <- youngblack[,9]

#ADULT
adulthisp <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="25–44"
     & mortal_data[i,4]=="Hispanic" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    adulthisp <- rbind(adulthisp, r)
  }
}

adultwhite <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="25–44"
     & mortal_data[i,4]=="Non-Hispanic white" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    adultwhite <- rbind(adultwhite, r)
  }
}

adultblack <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="25–44"
     & mortal_data[i,4]=="Non-Hispanic black" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    adultblack <- rbind(adultblack, r)
  }
}

relcount_adulthisp <- adulthisp[,9]
relcount_adultwhite <- adultwhite[,9]
relcount_adultblack <- adultblack[,9]

#MID
midhisp <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="45–64"
     & mortal_data[i,4]=="Hispanic" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    midhisp <- rbind(midhisp, r)
  }
}

midwhite <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="45–64"
     & mortal_data[i,4]=="Non-Hispanic white" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    midwhite <- rbind(midwhite, r)
  }
}

midblack <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="45–64"
     & mortal_data[i,4]=="Non-Hispanic black" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    midblack <- rbind(midblack, r)
  }
}

relcount_midhisp <- midhisp[,9]
relcount_midwhite <- midwhite[,9]
relcount_midblack <- midblack[,9]


#SENIOR
seniorhisp <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="65–74"
     & mortal_data[i,4]=="Hispanic" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    seniorhisp <- rbind(seniorhisp, r)
  }
}

seniorwhite <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="65–74"
     & mortal_data[i,4]=="Non-Hispanic white" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    seniorwhite <- rbind(seniorwhite, r)
  }
}

seniorblack <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="65–74"
     & mortal_data[i,4]=="Non-Hispanic black" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    seniorblack <- rbind(seniorblack, r)
  }
}

relcount_seniorhisp <- seniorhisp[,9]
relcount_seniorwhite <- seniorwhite[,9]
relcount_seniorblack <- seniorblack[,9]

#OLD
oldhisp <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="75+"
     & mortal_data[i,4]=="Hispanic" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    oldhisp <- rbind(oldhisp, r)
  }
}

oldwhite <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="75+"
     & mortal_data[i,4]=="Non-Hispanic white" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    oldwhite <- rbind(oldwhite, r)
  }
}

oldblack <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Both sexes" & mortal_data[i,3]=="75+"
     & mortal_data[i,4]=="Non-Hispanic black" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    oldblack <- rbind(oldblack, r)
  }
}

relcount_oldhisp <- oldhisp[,9]
relcount_oldwhite <- oldwhite[,9]
relcount_oldblack <- oldblack[,9]


# LINE GRAPHS...SEPARATED BY AGE GROUPS + RACE
op <- par(family = "serif")
par(mfrow=c(2,3), oma = c(4, 0, 4, 0))

#TEENS
plot(year, relcount_teenwhite,  ylim=c(0,1.1*max(relcount_teenwhite)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "< 15", xaxt="n", type="l", 
     col="black", lwd=2) 
points(x=year, y=relcount_teenhisp, col='red', type='l', lwd=2)
points(x=year, y=relcount_teenblack, col='blue', type='l', lwd=2)
axis(1,at=year,labels=year)

#YOUNG
plot(year, relcount_youngwhite,  ylim=c(0,1.1*max(relcount_youngwhite)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "15-24", xaxt="n", type="l", 
     col="black", lwd=2) 
points(x=year, y=relcount_younghisp, col='red', type='l', lwd=2)
points(x=year, y=relcount_youngblack, col='blue', type='l', lwd=2)
axis(1,at=year,labels=year)

#ADULT
plot(year, relcount_adultwhite,  ylim=c(0,1.1*max(relcount_adultwhite)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "25-44", xaxt="n", type="l", 
     col="black", lwd=2) 
points(x=year, y=relcount_adulthisp, col='red', type='l', lwd=2)
points(x=year, y=relcount_adultblack, col='blue', type='l', lwd=2)
axis(1,at=year,labels=year)

#MID
plot(year, relcount_midwhite,  ylim=c(0,1.1*max(relcount_midwhite)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "45-64", xaxt="n", type="l", 
     col="black", lwd=2) 
points(x=year, y=relcount_midhisp, col='red', type='l', lwd=2)
points(x=year, y=relcount_midblack, col='blue', type='l', lwd=2)
axis(1,at=year,labels=year)

#SENIOR
plot(year, relcount_seniorwhite,  ylim=c(0,1.1*max(relcount_seniorwhite)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "65-74", xaxt="n", type="l", 
     col="black", lwd=2) 
points(x=year, y=relcount_seniorhisp, col='red', type='l', lwd=2)
points(x=year, y=relcount_seniorblack, col='blue', type='l', lwd=2)
axis(1,at=year,labels=year)

#OLD
plot(year, relcount_oldwhite,  ylim=c(0,1.1*max(relcount_oldwhite)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "75+", xaxt="n", type="l", 
     col="black", lwd=2) 
points(x=year, y=relcount_oldhisp, col='red', type='l', lwd=2)
points(x=year, y=relcount_oldblack, col='blue', type='l', lwd=2)
axis(1,at=year,labels=year)

legend("bottom", inset=c(0,-2), horiz=TRUE, c("Hispanic","White","Black"), col=c("red","black","blue"), cex=0.5, lwd=2)
mtext(text = "Suicide by Age Groups and Race", outer = TRUE, side=3, cex = 1.5)
par(op)


##############################################################################
# Suicide by Firearm by Races and Gender Across Age Groups from 1999 to 2014
##############################################################################

# TEENS (< 15) GROUPED BY RACE

# LINE GRAPH
op <- par(family = "serif")
par(mfrow=c(1,1))

plot(year, relcount_teenwhite,  ylim=c(0,1.1*max(relcount_teenwhite)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "< 15", xaxt="n", type="l", 
     col="black", lwd=2) 
points(x=year, y=relcount_teenhisp, col='red', type='l', lwd=2)
points(x=year, y=relcount_teenblack, col='blue', type='l', lwd=2)
axis(1,at=year,labels=year)
par(op)

# < 15 GROUPED BY RACE + GENDER

mteen_hisp <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Male" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="Hispanic" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    mteen_hisp <- rbind(mteen_hisp, r)
  }
}
mteen_white <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Male" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="Non-Hispanic white" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    mteen_white <- rbind(mteen_white, r)
  }
}
mteen_black <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Male" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="Non-Hispanic black" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    mteen_black <- rbind(mteen_black, r)
  }
}

fteen_hisp <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Female" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="Hispanic" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    fteen_hisp <- rbind(fteen_hisp, r)
  }
}
fteen_white <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Female" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="Non-Hispanic white" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    fteen_white <- rbind(fteen_white, r)
  }
}
fteen_black <- matrix(NA, nrow=0, ncol=8)
n <- nrow(mortal_data)
for (i in 1:n){
  if(mortal_data[i,2]=="Female" & mortal_data[i,3]=="< 15"
     & mortal_data[i,4]=="Non-Hispanic black" & mortal_data[i,5]=="Firearm"
     & mortal_data[i,6]=="Suicide"){
    r <- as.vector(mortal_data[i,1:9])
    fteen_black <- rbind(fteen_black, r)
  }
}

relcount_mteen_hisp <- mteen_hisp[,9]
relcount_mteen_white <- mteen_white[,9]
relcount_mteen_black <- mteen_black[,9]
relcount_fteen_hisp <- fteen_hisp[,9]
relcount_fteen_white <- fteen_white[,9]
relcount_fteen_black <- fteen_black[,9]

op <- par(family = "serif")
par(mfrow=c(1,2), oma = c(4, 0, 4, 0))

plot(year, relcount_mteen_white,  ylim=c(0,1.1*max(relcount_mteen_white)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "< 15 yo Males", xaxt="n", type="l", 
     col="black", lwd=2) 
points(x=year, y=relcount_mteen_hisp, col='red', type='l', lwd=2)
points(x=year, y=relcount_mteen_black, col='blue', type='l', lwd=2)
axis(1,at=year,labels=year)

plot(year, relcount_fteen_white,  ylim=c(0,1.1*max(relcount_fteen_white)), xlab="Year", 
     ylab = "Death Count per 100,000", main = "< 15 yo Females", xaxt="n", type="l", 
     col="black", lwd=2) 
points(x=year, y=relcount_fteen_hisp, col='red', type='l', lwd=2)
points(x=year, y=relcount_fteen_black, col='blue', type='l', lwd=2)
axis(1,at=year,labels=year)

legend("bottom", inset=c(0,-1), horiz=TRUE, c("Hispanic","White","Black"), col=c("red","black","blue"), cex=0.5, lwd=2)
mtext(text = "Suicide by Teens Across Races and Gender", outer = TRUE, side=3, cex = 1.5)
par(op)





#############################
####### Survey data #########
#############################

n <- read.delim(file.choose())
head(n)
keep <- c(n$Serial, n$SHA, n$hastprim, n$ageband2, n$EdAttn3, n$Car, n$NSSEC8)
adhs.data <- matrix(keep, ncol = 7) 
adhs.data <- data.frame(adhs.data)
column.names <- c("Serial", "SHA", "hastprim", "ageband2", "EdATTn3", "Car","NSSEC8")
names(adhs.data) <- column.names
head(adhs.data)
summary(adhs.data)
summary(adhs.data$SHA)
tail(adhs.data)
head(adhs.data)

sum(adhs.data$EdATTn3)

ADHS <- adhs.data
head(ADHS)

ADHS$Female <- ifelse(ADHS$Sex==2, 1, 0)
head(ADHS)
ADHS$Male <- ifelse(ADHS$Sex==1, 1, 0)
head(ADHS)

################################# online example ###################################
													     #
#df <- data.frame(Species_code = c(101, 103,101,99,101)) # your data 		     #
#> df$Presence_absence <- ifelse(df$Species_code==101, 1, 0) # this does the trick  #
#> df													     #
#  Species_code Presence_absence								     #
#1          101                1								     #
#2          103                0								     # 
#3          101                1								     #
#4           99                0 								     #
#5          101                1								     #
													     #
####################################################################################

#df[df==-1] <- NA # way to assign, then omit NAs
#df <- na.omit(df)

#e.g.

#ADHS$EdATTn3[ADHS$EdATTn3==-1] <- NA
#head(ADHS)

## removing the NAs from the car column ##

ADHS$Car[ADHS$Car==-1] <- NA
ADHS$Car[ADHS$Car==-2] <- NA
ADHS$Car[ADHS$Car==-6] <- NA
ADHS$Car[ADHS$Car==-7] <- NA
ADHS$Car[ADHS$Car==-8] <- NA
ADHS$Car[ADHS$Car==-9] <- NA

ADHS$Car
ADHS <- na.omit(ADHS)
ADHS$Car

## removing the NAs from the NSSEC8 column ##

ADHS$NSSEC8[ADHS$NSSEC8==97.0] <- NA
ADHS$NSSEC8[ADHS$NSSEC8==-1.0] <- NA
ADHS$NSSEC8[ADHS$NSSEC8==-2.0] <- NA
ADHS$NSSEC8[ADHS$NSSEC8==-6.0] <- NA
ADHS$NSSEC8[ADHS$NSSEC8==-7.0] <- NA
ADHS$NSSEC8[ADHS$NSSEC8==-8.0] <- NA
ADHS$NSSEC8[ADHS$NSSEC8==-9.0] <- NA

ADHS$NSSEC8
summary(ADHS$NSSEC8)

ADHS <- na.omit(ADHS)
ADHS$NSSEC8

## removing the NAs from the EdATTn3 column ##

ADHS$EdATTn3[ADHS$EdATTn3==-1] <- NA
ADHS$EdATTn3[ADHS$EdATTn3==-2] <- NA
ADHS$EdATTn3[ADHS$EdATTn3==-6] <- NA
ADHS$EdATTn3[ADHS$EdATTn3==-7] <- NA
ADHS$EdATTn3[ADHS$EdATTn3==-8] <- NA
ADHS$EdATTn3[ADHS$EdATTn3==-9] <- NA

ADHS$EdATTn3
summary(ADHS$EdATTn3)

ADHS <- na.omit(ADHS)
ADHS$EdATTn3
summary(ADHS$EdATTn3)
length(ADHS)

## removing NAs from the ageband2 column ##

ADHS$ageband2[ADHS$ageband2==-1] <- NA
ADHS$ageband2[ADHS$ageband2==-2] <- NA
ADHS$ageband2[ADHS$ageband2==-6] <- NA
ADHS$ageband2[ADHS$ageband2==-7] <- NA
ADHS$ageband2[ADHS$ageband2==-8] <- NA
ADHS$ageband2[ADHS$ageband2==-9] <- NA 

ADHS$ageband2 # turns out there are no NAs in this column

## removing NAs from the DVHsize column ##

ADHS$DVHsize[ADHS$DVHsize==-1] <- NA
ADHS$DVHsize[ADHS$DVHsize==-2] <- NA
ADHS$DVHsize[ADHS$DVHsize==-6] <- NA
ADHS$DVHsize[ADHS$DVHsize==-7] <- NA
ADHS$DVHsize[ADHS$DVHsize==-8] <- NA
ADHS$DVHsize[ADHS$DVHsize==-9] <- NA

ADHS$DVHsize # no NAs here either

## removing NAs from hastprim column ##

ADHS$hastprim[ADHS$hastprim==-1] <- NA
ADHS$hastprim[ADHS$hastprim==-2] <- NA
ADHS$hastprim[ADHS$hastprim==-6] <- NA
ADHS$hastprim[ADHS$hastprim==-7] <- NA
ADHS$hastprim[ADHS$hastprim==-8] <- NA
ADHS$hastprim[ADHS$hastprim==-9] <- NA

summary(ADHS$hastprim)
ADHS$hastprim # lot of NAs there!!

ADHS <- na.omit(ADHS)
ADHS$hastprim #leaves 5398 people/records left in the survey

## check the region column just to be sure###

ADHS$SHA[ADHS$SHA==-1] <- NA
ADHS$SHA[ADHS$SHA==-2] <- NA
ADHS$SHA[ADHS$SHA==-6] <- NA
ADHS$SHA[ADHS$SHA==-7] <- NA
ADHS$SHA[ADHS$SHA==-8] <- NA
ADHS$SHA[ADHS$SHA==-9] <- NA
ADHS <- na.omit(ADHS)
ADHS$SHA # No NA's

##### Now need to split the columns into the binary format #####
##### e.g. five columns for age, etc. Gender already done ######

head(ADHS)

# Car
ADHS$HasCar <- ifelse(ADHS$Car==1, 1, 0)
head(ADHS)
ADHS$NoCar <- ifelse(ADHS$Car==2, 1, 0)
head(ADHS)

# EdATTn3
ADHS$Highqual <- ifelse(ADHS$EdATTn3==1, 1, 0)
head(ADHS)
ADHS$Otherqual <- ifelse(ADHS$EdATTn3==2, 1, 0)
head(ADHS)

# ageband2

ADHS$a16to24 <- ifelse(ADHS$ageband2==1, 1, 0)
ADHS$a25to44 <- ifelse(ADHS$ageband2==2, 1, 0)
ADHS$a45to64 <- ifelse(ADHS$ageband2==3, 1, 0)
ADHS$a65to84 <- ifelse(ADHS$ageband2==4, 1, 0)
ADHS$a85plus <- ifelse(ADHS$ageband2==5, 1, 0)
head(ADHS)

# NSSEC8

ADHS$largeemphighmanoc <- ifelse(ADHS$NSSEC8==1.1, 1, 0)
ADHS$higherprofoc <- ifelse(ADHS$NSSEC8==1.2, 1, 0)
ADHS$lowermanprofoc <- ifelse(ADHS$NSSEC8==2.0, 1, 0)
ADHS$interoc <- ifelse(ADHS$NSSEC8==3.0, 1, 0)
ADHS$smallempOAW <- ifelse(ADHS$NSSEC8==4.0, 1, 0)
ADHS$lowsuptechop <- ifelse(ADHS$NSSEC8==5.0, 1, 0)
ADHS$semiroutoc <- ifelse(ADHS$NSSEC8==6.0, 1, 0)
ADHS$routoc <- ifelse(ADHS$NSSEC8==7.0, 1, 0)
ADHS$neverworkLTU <- ifelse(ADHS$NSSEC8==8.0, 1, 0)
head(ADHS)

# DVHsize

ADHS$hh1 <- ifelse(ADHS$DVHsize==1, 1, 0)
ADHS$hh2 <- ifelse(ADHS$DVHsize==2, 1, 0)
ADHS$hh3 <- ifelse(ADHS$DVHsize==3, 1, 0)
ADHS$hh4 <- ifelse(ADHS$DVHsize==4, 1, 0)
ADHS$hh5 <- ifelse(ADHS$DVHsize==5, 1, 0)
ADHS$hh6 <- ifelse(ADHS$DVHsize==6, 1, 0)
ADHS$hh7 <- ifelse(ADHS$DVHsize==7, 1, 0)
ADHS$hh8 <- ifelse(ADHS$DVHsize==8, 1, 0)
ADHS$hh9 <- ifelse(ADHS$DVHsize==9, 1, 0)
ADHS$hh10 <- ifelse(ADHS$DVHsize==10, 1, 0)
head(ADHS)

# hastprim

ADHS$Decay <- ifelse(ADHS$hastprim==1, 1, 0)
ADHS$Nodecay <- ifelse(ADHS$hastprim==2, 1, 0)
head(ADHS)

#### removing the original tables (that are not in binary format) ####

keep2 <- c(ADHS$Serial, ADHS$SHA, ADHS$Decay, ADHS$HasCar, ADHS$NoCar, ADHS$Highqual, 
	     ADHS$Otherqual, ADHS$a16to24, ADHS$a25to44, ADHS$a45to64, ADHS$a65to84, ADHS$a85plus, 
	     ADHS$largeemphighmanoc, ADHS$higherprofoc, ADHS$lowermanprofoc, ADHS$interoc, ADHS$smallempOAW,
	     ADHS$lowsuptechop, ADHS$semiroutoc, ADHS$routoc, ADHS$neverworkLTU)
ADHS <- matrix(keep2, ncol = 21)
ADHS <- data.frame(ADHS)
head(ADHS)
columnnames <- c("Serial", "SHA", "Decay", "HasCar", "NoCar", "Highqual", 
	     "Otherqual", "a16to24", "a25to44", "a45to64", "a65to84", "a85plus", 
	     "largeemphighmanoc", "higherprofoc", "lowermanprofoc", "interoc", "smallempOAW",
	     "lowsuptechop", "semiroutoc", "routoc", "neverworkLTU")
names(ADHS) <- columnnames
head(ADHS)
tail(ADHS)

#### creating a binary category for whether a person if from Yorks and Humber or not ####

ADHS$Region <- ifelse(ADHS$SHA==3, 1, 0)
head(ADHS)
tail(ADHS)
summary(ADHS$Region)
print(ADHS$Region)

## how to identify where the 1's are - i.e. Yorks and Humber, and get rid of the 0's ##

ADHS[apply(ADHS[c(22)],1,function(z) !any(z==0)),]
survey.data <- ADHS[apply(ADHS[c(22)],1,function(z) !any(z==0)),]
head(survey.data)
survey.data$Region <- NULL # taking out the extra column for region
head(survey.data)

#################################################
##### Adult Dental Health Survey data ready #####
#################################################

#################################################
################## Census data ##################
#################################################

## setting up car data
car <- read.csv(file.choose(), header = T, stringsAsFactors = FALSE) # last bit keeps all variables 
											     as they are rather than convert
											     them to factors
head(car)
keeping <- c(car$LSOA_CODE, car$LSOA_NAME, car$All.households, car$No.cars.vans.in.hh, 
		 car$X1.car.van.in.hh, car$X2.cars.vans.in.hh, car$X3.cars.vans.in.hh, 
		 car$X4..or.more.cars.vans.in.hh, car$all.cars.vans.in.area)
car.data <- matrix(keeping, ncol = 9)
car.data <- data.frame(car.data)
column.n <- c("LSOAcode", "LSOAname", "ALLhh", "Nocar", "car1", "car2", "car3", 
		  "cars4", "ALLcars")
names(car.data) <- column.n
head(car.data)

car.data$ALLhh <- as.numeric(levels(car.data$ALLhh)[car.data$ALLhh]) # turns the car values into
car.data$Nocar <- as.numeric(levels(car.data$Nocar)[car.data$Nocar]) # numbers, except the ALLcars
car.data$car1 <- as.numeric(levels(car.data$car1)[car.data$car1])    # variable, but don't need
car.data$car2 <- as.numeric(levels(car.data$car2)[car.data$car2])    # that anyway
car.data$car3 <- as.numeric(levels(car.data$car3)[car.data$car3])
car.data$cars4 <- as.numeric(levels(car.data$cars4)[car.data$cars4])
car.data$ALLcars <- as.numeric(levels(car.data$ALLcars)[car.data$ALLcars])
head(car.data)
str(car.data)

car.data$Hascar <- c(car.data$car1 + car.data$car2 + car.data$car3 + car.data$cars4)
head(car.data)

carskeep <- c(car.data$Nocar, car.data$Hascar)
CARS <- matrix(carskeep, ncol = 2)
CARS <- data.frame(CARS)
names(CARS) <- c("Nocar", "Hascar")
head(CARS) # This doesn't have the LSOA code included
CARS <- data.frame(car.data$LSOAcode, CARS)
names(CARS) <- c("LSOAcode", "Nocar", "Hascar")
head(CARS) # Now it has LSOA codes attached

## setting up the gender data - DONT USE IF USING THE REFINED MODEL!!

G <- read.csv(file.choose(), header = T, stringsAsFactors = FALSE)
head(G)
Gkeeping <- c(G$LSOA_CODE, G$ALLres, G$Males, G$Females)
gender.data <- matrix(Gkeeping, ncol = 4)
gender.data <- data.frame(gender.data)
names(gender.data) <- c("LSOAcode", "Allres", "Males", "Females")
head(gender.data)
gender.data$ALLres <- as.numeric(levels(gender.data$ALLres)[gender.data$ALLres])
gender.data$Males <- as.numeric(levels(gender.data$Males)[gender.data$Males])
gender.data$Females <- as.numeric(levels(gender.data$Females)[gender.data$Females]) # doesn't work for ALLres, but doesn't matter
head(gender.data)
str(gender.data)

## setting up the education variable

E <- read.csv(file.choose(), header = T, stringsAsFactors = FALSE)
head(E)
Ekeep <- c(E$LSOA_CODE, E$LSOA_NAME, E$Allres, E$Noqual, E$Level1, E$Level2, E$Appren, E$Level3, E$L4andup, E$otherqual)
qual.data <- matrix(Ekeep, ncol = 10)
qual.data <- data.frame(qual.data)
names(qual.data) <- c("LSOAcode", "LSOAname", "Allres", "Noqual", "Level1", "Level2", "Appren", "Level3", "L4andup", "otherqual")
head(qual.data)

qual.data$allres <- as.numeric(levels(qual.data$allres)[qual.data$allres])
qual.data$Noqual <- as.numeric(levels(qual.data$Noqual)[qual.data$Noqual])
qual.data$Level1 <- as.numeric(levels(qual.data$Level1)[qual.data$Level1])
qual.data$Level2 <- as.numeric(levels(qual.data$Level2)[qual.data$Level2])
qual.data$Appren <- as.numeric(levels(qual.data$Appren)[qual.data$Appren])
qual.data$Level3 <- as.numeric(levels(qual.data$Level3)[qual.data$Level3])
qual.data$L4andup <- as.numeric(levels(qual.data$L4andup)[qual.data$L4andup])
qual.data$otherqual <- as.numeric(levels(qual.data$otherqual)[qual.data$otherqual])
head(qual.data)
str(qual.data)

qual.data$lowerqual <- c(qual.data$Noqual + qual.data$Level1 + qual.data$Level2 + qual.data$Appren +
				 qual.data$Level3 + qual.data$otherqual)
head(qual.data)

Ekeep2 <- c(qual.data$L4andup, qual.data$lowerqual)
QUAL <- matrix(Ekeep2, ncol = 2)
QUAL <- data.frame(QUAL)
QUAL <- data.frame(qual.data$LSOAcode, QUAL)
names(QUAL) <- c("LSOAcode", "L4+", "Otherqual")
head(QUAL)

## setting up the agegroup data

a <- read.csv(file.choose(), header = T, stringsAsFactors = F)
head(a)
akeep <- c(a$LSOA_CODE, a$ALLres, a$a16to17, a$a18to19, a$a20to24, a$a25to29, a$a30to44, a$a45to59, a$a60to64, a$a65to74, a$a75to84, a$a85to89, a$a90.)
age.data <- matrix(akeep, ncol = 13)
age.data <- data.frame(age.data)
names(age.data) <- c("LSOAcode", "ALLres", "a16to17", "a18to19", "a20to24", "a25to29", "a30to44", "a45to59", "a60to64", "a65to74", "a75to84", "a85to89", "a90")
head(age.data)
str(age.data)

age.data$a16to17 <- as.numeric(levels(age.data$a16to17)[age.data$a16to17])
age.data$a18to19 <- as.numeric(levels(age.data$a18to19)[age.data$a18to19])
age.data$a20to24 <- as.numeric(levels(age.data$a20to24)[age.data$a20to24])
age.data$a25to29 <- as.numeric(levels(age.data$a25to29)[age.data$a25to29])
age.data$a30to44 <- as.numeric(levels(age.data$a30to44)[age.data$a30to44])
age.data$a45to59 <- as.numeric(levels(age.data$a45to59)[age.data$a45to59])
age.data$a60to64 <- as.numeric(levels(age.data$a60to64)[age.data$a60to64])
age.data$a65to74 <- as.numeric(levels(age.data$a65to74)[age.data$a65to74])
age.data$a75to84 <- as.numeric(levels(age.data$a75to84)[age.data$a75to84])
age.data$a85to89 <- as.numeric(levels(age.data$a85to89)[age.data$a85to89])
age.data$a90 <- as.numeric(levels(age.data$a90)[age.data$a90])

age.data$a16to24 <- c(age.data$a16to17 + age.data$a18to19 + age.data$a20to24)
age.data$a25to44 <- c(age.data$a25to29 + age.data$a30to44)
age.data$a45to64 <- c(age.data$a45to59 + age.data$a60to64)
age.data$a65to84 <- c(age.data$a65to74 + age.data$a75to84)
age.data$a85plus <- c(age.data$a85to89 + age.data$a90)
head(age.data)


akeep2 <- c(age.data$a16to24, age.data$a25to44, age.data$a45to64, age.data$a65to84, age.data$a85plus)
AGE <- matrix(akeep2, ncol = 5)
AGE <- data.frame(AGE)
AGE <- data.frame(age.data$LSOAcode, AGE)
names(AGE) <- c("LSOAcode", "a16to24", "a25to44", "a45to64", "a65to84", "a85plus")
head(AGE)
str(AGE)

## setting up the NSSEC variable ##

ns <-  read.csv(file.choose(), header = T)
head(ns)
str(ns)
nskeep <- c(ns$n1.1, ns$n1.2, ns$n2, ns$n3, ns$n4, ns$n5, ns$n6, ns$n7, ns$n8)
ns.data <- matrix(nskeep, ncol = 9)
ns.data <- data.frame(ns.data)
ns.data <- data.frame(ns$LSOA_CODE, ns.data)
names(ns.data) <- c("LSOAcode", "largeemphighmanoc", "higherprofoc", "lowermanprofoc", "interoc", "smallempOAW", "lowsuptechop", "semiroutoc", "routoc", "neverworkLTU")
head(ns.data)
str(ns.data)

## joining all the census data together ##

head(CARS)
head(QUAL)
head(AGE)
head(ns.data)

census.data <- merge(CARS, QUAL, by="LSOAcode")
census.data <- merge(census.data, AGE, by="LSOAcode")
census.data <- merge(census.data, ns.data, by="LSOAcode")
head(census.data)
tail(census.data)

####################################################################
######################### Census data ready ########################
####################################################################

head(census.data)
head(survey.data)

#### joining census and survey data ####

census.data$key <- c(9999)
head(census.data)
ADHS$key <- c(9999)
head(ADHS)
microsim.data <- merge(census.data, ADHS, by="key")
microsim.data <- rbind(census.data, ADHS)
microsim.data <- cbind(census.data, ADHS)
library(plyr)
microsim.data <- rbind.fill(census.data[c("key")], ADHS[c("key", "SHA", "No decay")])
head(microsim.data)
tail(microsim.data)
rm(microsim.data)
summary(QUAL)
view(QUAL)


####################################################################
##### regression testing R2 of demographic variables (that fit #####
##### census data) from ADHS						   #####
####################################################################

v <- read.csv(file.choose(), header = F)
head(v)
v <- data.frame(v)
names(v) <- c("Serial", "SHA", "DVHsize", "Sex", "xMarSta2", "ageband2", "Hhldr", "LSIII", 
		  "GenHlthg", "Car", "EdATTn3", "NSSEC8", "hastprim", "haspcav")
head(v)
library(lmSupport)
attach(v)
fit <- lm(hastprim ~ DVHsize + Sex + xMarSta2 + ageband2 + Hhldr + LSIII + GenHlthg + Car + EdATTn3 + NSSEC8)


####################################################################
########## Attempt at spatial microsimulation ######################
####################################################################

## Column totals of survey variables ##

sum(survey.data$HasCar)
sum(survey.data$NoCar)
sum(survey.data$Highqual)
sum(survey.data$Otherqual)
sum(survey.data$a16to24)
sum(survey.data$a25to44)
sum(survey.data$a45to64)
sum(survey.data$a65to84)
sum(survey.data$a85plus)
sum(survey.data$largeemphighmanoc)
sum(survey.data$higherprofoc)
sum(survey.data$lowermanprofoc)
sum(survey.data$smallempOAW)
sum(survey.data$lowsuptechop)
sum(survey.data$semiroutoc)
sum(survey.data$routoc)
sum(survey.data$neverworkLTU)

head(census.data)
head(survey.data)

for (i in 1:nrow(census.data)){
     wi <- census.data[,2]/survey.data[,5]
     print (wi)
     }

for (j in 1:nrow(census.data)) {
    wi[which(survey.data$NoCar == 1), j] <- census.data[j, 2]/survey.data[j, 5]
    w2[which(survey.data$NoCar == 0), j] <- census.data[j, 2]/survey.data[j, 5]
    }

for (j in 1:nrow(census.data)) {
    wi <- census.data[j, 2]/survey.data[j, 5]
    w2 <- census.data[j, 2]/survey.data[j, 5]
    }


wi <- 1*census.data[, 2]/(sum(survey.data$NoCar))
head(wi)
w2 <- wi*census.data[, 3]/(sum(survey.data$HasCar))
head(w2)
colSums(survey.data)

## CONSTRAINTS NEED TO BE APPLIED IN ORDER OF R2 IMPORTANCE!!!

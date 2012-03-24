##################################################################
# inschoolAddHealth.R
# Michael Metcalf Bishop, bishop@uchicago.edu

# Data Source: Wave 1, In-School Survey, Longitudinal Study of Adolescent Health
# Purpose: Prepare data for analysis, reduce need for codebook
#          Original variables are recoded such that
#             1) most are given more meaningful variable names
#             2) all missing codes become system missing
#             3) many vars are given labels
#             4) most categorical variables are encoded as class "factor" with labelled responses
#         
# Note: Change the location of the data indicated in the first few lines of code.
#       Then, if you already have the listed packages installed, the remainder of the code may
#       be executed at once.

#       Inevitably, one must make some arbitrary choices, (e.g. see coding of race and ethnicity)
#       I've tried to make everything as transparent as possible, but please see
#       the in school codebook to be certain you understand the data
#       

# Acknowledgements: I owe thanks to Joyce Tabor and James Moody for assistance but any errors are my own. 
#                   Please submit/push suggestions for improvements and I'll ad your name here.
##################################################################

 
library(car)
library(Hmisc)

#load("J:/R/inschool.RData")
inschool <- sasxport.get("J:/DataAll/Inschool.xpt") # In School Survey, Wave I, provided by Add Health
#save.image("J:/R/inschool.RData")

# Creating aid2 with new ids where aid is missing
inschool$aid2 <- ifelse(is.na(inschool$aid), NA, as.numeric(as.character(inschool$aid)))
missingIds <- is.na(inschool$aid2)
replacementIds <- seq(length(missingIds))
inschool$aid2[missingIds] <- replacementIds[missingIds]
rm(missingIds, replacementIds)
describe(duplicated(inschool$aid2)) #check that there are no duplicate ids
label(inschool$aid2) <- "aid - Respondent Identifier - with missing values replaced"

inschool$age <- car::recode(inschool$s1, '99=NA; ', as.factor.result=FALSE) # 99 = multiple response (29n) 406 total missing

inschool$male.is <- car::recode(inschool$s2, '1=1; 2=0; 9=NA; ', as.factor.result=FALSE) # 9 = multiple response (51n) 731 total missing

inschool$grade <- car::recode(inschool$s3, '13=NA; 99=NA; ', as.factor.result=FALSE) 
# 13 = school w/o gradelevels (207n), 99 = multiple response (55n) 803 total missing

inschool$hispanic.is <- car::recode(inschool$s4, '8=0; 9=0', as.factor.result=FALSE) # 8 = don't know (5751n) 9 =  multiple response (10n)
describe(inschool$hispanic.is)

inschool$hispback.is <- car::recode(inschool$s5, '97=NA; 98=6; 99=6', as.factor.result=FALSE) # 97=non-hispanic, 98 = don't know (560n) 99 =  multiple response (287n)
inschool$hispback.is <- factor(inschool$hispback.is, levels=c(1,2,3,4,5,6), 
                               labels=c("Mexican", "Chicano/a", "Cuban", "Puerto Rican", "C./S. American", "Other"))
describe(inschool$hispback.is)
table(inschool$s5, inschool$s4, useNA="ifany")

inschool$white.is <- inschool$s6a # 54567 white
inschool$black.is <- inschool$s6b # 1762 black
inschool$asian.is <- inschool$s6c # 6312 Asian
inschool$amerind.is <- inschool$s6d # 4906 American Indian
inschool$other.is <- inschool$s6e   # 8785 Other race

inschool$asianback.is <- car::recode(inschool$s7, '97=NA; 99=7', as.factor.result=FALSE) # 97=legitimate skip, 99 =  multiple response (531n)
inschool$asianback.is <- factor(inschool$asianback.is, levels=c(1,2,3,4,5,6,7), 
                               labels=c("Chinese", "Filipino", "Japanese", "Asian Indian", 
                                        "Korean", "Vietnamese", "Other"))
describe(inschool$hispback.is)
table(inschool$s7, inschool$s6c, useNA="ifany")

inschool$immig.is <- car::recode(inschool$s8, '0=1; 1=0; 9=NA; ', as.factor.result=FALSE) #immigrant = Not born in U.S. or first 6 months
# yes 78709n, no 8474n, 9 multiple response 15n, skipped 2920 


inschool$racenum.is <- with(inschool, ifelse(is.na(white.is), 0, white.is + black.is + amerind.is + asian.is + other.is))
label(inschool$racenum.is) <- "# racial categories chosen: white+black+amerind+asian+other .is"
describe(inschool$racenum.is)
attach(inschool)
table(racenum.is==1, whiteod.is, useNA="ifany")
table(racenum.is==4, black.is)
table(racenum.is==4, amerind.is)
table(racenum.is==4, asian.is)
table(racenum.is==4, other.is)

inschool$racecat.is <- ifelse(racenum.is==1, ifelse(white.is==1, 1, 
                                             ifelse(black.is==1, 2,
                                             ifelse(amerind.is==1, 3, 
                                             ifelse(asian.is==1, 4, 
                                             ifelse(other.is==1, 5, 55))))),NA)

inschool$racecat.is <- factor(inschool$racecat.is, labels = c("White", "Black", "Am. Indian", "Asian", "Other"))
label(inschool$racecat.is) <- "Racial category chosen, NA if > 1 or < 1"
describe(inschool$racecat.is)


inschool$whiteod.is <- with(inschool, ifelse(racenum.is==1 & hispanic.is!=1 & white.is==1, 1, 0))
inschool$blackod.is <- with(inschool, ifelse(racenum.is==1 & hispanic.is!=1 & black.is==1, 1, 0))
inschool$amerindod.is <- with(inschool, ifelse(racenum.is==1 & hispanic.is!=1 & amerind.is==1, 1, 0))
inschool$asianod.is <- with(inschool, ifelse(racenum.is==1 & hispanic.is!=1 & asian.is==1, 1, 0))
inschool$otherod.is <- with(inschool, ifelse(racenum.is==1 & hispanic.is!=1 & other.is==1, 1, 0))

inschool$whitehod.is <- with(inschool, ifelse(racenum.is==1 & hispanic.is==1 & white.is==1, 1, 0))
inschool$blackhod.is <- with(inschool, ifelse(racenum.is==1 & hispanic.is==1 & black.is==1, 1, 0))
inschool$amerindhod.is <- with(inschool, ifelse(racenum.is==1 & hispanic.is==1 & amerind.is==1, 1, 0))
inschool$asianhod.is <- with(inschool, ifelse(racenum.is==1 & hispanic.is==1 & asian.is==1, 1, 0))
inschool$otherhod.is <- with(inschool, ifelse(racenum.is==1 & hispanic.is==1 & other.is==1, 1, 0))

# race variables ending in od.is are indicators for single race & non-hispanic in school survey
# race variables ending in hod.is are indicators for single race & hispanic in school survey
# these variables are missing if and only if race is true, but hispanic is missing

describe(inschool$whiteod.is)
describe(inschool$blackod.is)
describe(inschool$asianhod.is)

inschool$racecat.h.is <- ifelse((racenum.is==1 & hispanic.is==1), 
                                            ifelse(white.is==1, 1, 
                                            ifelse(black.is==1, 2, 
                                            ifelse(amerind.is==1, 3, 
                                            ifelse(asian.is==1, 4, 
                                            ifelse(other.is==1, 5, 55))))),NA)

inschool$racecat.h.is <- factor(inschool$racecat.h.is, labels= c("H White", "H Black", "H AmInd", "H Asian", "H Other"))
label(inschool$racecat.h.is) <- "missing for hispanic = 0 or NA, or > 1 or < 1 race"

inschool$racecat.nh.is <- ifelse((racenum.is==1 & hispanic.is!=1), 
                                        ifelse(white.is==1, 1, 
                                        ifelse(black.is==1, 2, 
                                        ifelse(amerind.is==1, 3, 
                                        ifelse(asian.is==1, 4, 
                                        ifelse(other.is==1, 5, 55))))),NA)

inschool$racecat.nh.is <- factor(inschool$racecat.nh.is, labels= c("NH White", "NH Black", "NH AmInd", "NH Asian", "NH Other"))
label(inschool$racecat.nh.is) <- "missing for hispanic = 1 or NA, or > 1 or < 1 race"

describe(inschool$racecat.h.is)
describe(inschool$racecat.nh.is)
describe(inschool$hispanic.is)

table(inschool$hispanic.is, inschool$racecat.h.is, useNA="ifany")
table(inschool$hispanic.is, inschool$racecat.nh.is, useNA="ifany")
table(inschool$racenum.is, inschool$racecat.h.is, useNA="ifany")
table(inschool$racenum.is, inschool$racecat.nh.is, useNA="ifany")


inschool$hisp.nm.is  <- with(inschool, ifelse(hispanic.is==1 &  black.is==0 & amerind.is==0 & asian.is==0, 1, 0))
label(inschool$hisp.nm.is) <- "Hispanic, not minority race (black, Am.Ind. or Asian)"
describe(inschool$hisp.nm.is)

inschool$raceg5.is <- with(inschool, ifelse(whiteod.is!=1 & blackod.is!=1 & asianod.is!=1 & hisp.nm.is!=1, 1, 0))
inschool$raceg5.is <- with(inschool, ifelse(is.na(raceg5.is), 1, raceg5.is))
label(inschool$raceg5.is) <- "Indicator for race/ethnicity not fitting neatly into other categories"
describe(inschool$raceg5.is)

table(inschool$raceg5.is, inschool$hispanic.is, useNA="ifany")
table(inschool$racenum.is, inschool$hispanic.is, useNA="ifany")
table(inschool$sschlcde, inschool$hispanic.is, useNA="ifany")


inschool$racesum.is <- with(inschool, whiteod.is + blackod.is +asianod.is + hisp.nm.is + raceg5.is)
label(inschool$racesum.is) <- "whiteod.is + blackod.is +asianod.is + hisp.nm.is + raceg5.is"
describe(inschool$racesum.is)

inschool$race.is <- with(inschool, ifelse(whiteod.is==1,1,
                                   ifelse(blackod.is==1,2,
                                   ifelse(asianod.is==1,4,
                                   ifelse(hisp.nm.is==1,6,
                                   ifelse(raceg5.is ==1,7,NA))))))

inschool$race.is <- factor(inschool$race.is, labels = c("White", "Black", "Asian", "Hispanic", "Other"))
label(inschool$race.is) <- "Race/Ethnicity, multiple categories = other, except Hispanic + white or other"
# race.is = NA if and only if hispanic.is = NA and otherhod.is = 0 or = NA
inschool$racen.is <- ifelse(is.na(inschool$race.is), 5, inschool$race.is)

inschool$racen.is <- factor(inschool$racen.is, labels = c("White", "Black", "Asian", "Hispanic", "Other"))
label(inschool$racen.is) <- "Race/Ethnicity, NA or multiple cat. = other, except Hispanic + white or other"

describe(inschool$race.is)
describe(inschool$racen.is)

inschool$engrad <- car::recode(inschool$s10a, '1=4; 2=3; 3=2; 4=1; 5:9=NA', as.factor.result=FALSE)
# 5=didn't take subject (1486n), 7=sch no letter grades (5132n), 8=don't know (5055n), 9=multiple response (114n)
inschool$magrad <- car::recode(inschool$s10b, '1=4; 2=3; 3=2; 4=1; 5:9=NA', as.factor.result=FALSE)
# 5=didn't take subject (3584n), 7=sch no letter grades (5132n), 8=don't know (4358n), 9=multiple response (101n)
inschool$sograd <- car::recode(inschool$s10c, '1=4; 2=3; 3=2; 4=1; 5:9=NA', as.factor.result=FALSE)
  # 5=didn't take subject (8105n), 7=sch no letter grades (5132n), 8=don't know (4661n), 9=multiple response (76n)
inschool$scgrad <- car::recode(inschool$s10d, '1=4; 2=3; 3=2; 4=1; 5:9=NA', as.factor.result=FALSE)
# 5=didn't take subject (7684n), 7=sch no letter grades (5132n), 8=don't know (4593n), 9=multiple response (76n)

label(inschool$engrad) <- "Grade in English/Language Arts"
label(inschool$magrad) <- "Grade in Math"
label(inschool$sograd) <- "Grade in Social Studies/History"
label(inschool$scgrad) <- "Grade in Science"

inschool$notakesubj <- apply(inschool[c("s10a", "s10b", "s10c", "s10d")],1,function(x){sum(x == 5,na.rm = TRUE)})
describe(inschool$notakesubj)
label(inschool$notakesubj)  <- "# of 4 core subjects not taken"

inschool$takesubj <- apply(inschool[c("s10a", "s10b", "s10c", "s10d")],1,function(x){sum(x < 5,na.rm = TRUE)})
label(inschool$takesubj)  <- "# of 4 core subjects received grades"

inschool$gpa.is <- rowMeans(subset(inschool, select=c("engrad", "magrad", "sograd", "scgrad")), na.rm=TRUE)
describe(inschool$gpa.is)


# Do you live with your biological mother, stepmother, foster mother, or adoptive mother?
inschool$lwomom <- car::recode(inschool$s11, '0=1; 1=0; 7:99=NA', as.factor.result=F) # 9 = multiple response (26n)
label(inschool$lwomom) <- "Lives w/o mom (bio/step/foster/adoptive)"


inschool$momed <- car::recode(inschool$s12, '3=4; 4=3; 9:99=NA', as.factor.result=F)  #switch GED & HS diploma
# s12 coding: 1=8th grade or less, 2=more than 8th, 4=GED, 3=high school grad,
# 5=vocational training, 6=some college, 7=college grad, 8=post-college training,
# 9=he went to school but I don't know what level, 10=she never went to school 
#11=don't know if she went to school, 97=don't live w/her, 99=multiple response (943n)

describe(inschool$momed)

inschool$momimmig <- car::recode(inschool$s13, '0=1; 1=0; 7:9=NA', as.factor.result=F) 
  # 7=don't live with her, 8=I don't know (1149n), 9 = multiple response (16n)
label(inschool$momimmig) <- "Mom not born in U.S.A."

inschool$momjob <- factor(inschool$s14, levels=c(1:20, 97, 99), labels=c(
  "HOMEMAKER",
  "PROFESSIONAL 1, such as doctor, lawyer, scientist",
  "PROFESSIONAL 2, such as teacher, librarian, nurse",
  "MANAGER, such as executive, director",
  "TECHNICAL, such as computer specialist, radiologist",
  "OFFICE WORKER, such as bookkeeper, office clerk, secretary",
  "SALES WORKER, such as insurance agent, store clerk",
  "RESTAURANT WORKER OR PERSONAL SERVICE, such as waitress, housekeeper",
  "CRAFTSPERSON, such as toolmaker, woodworker",
  "CONSTRUCTION WORKER, such as carpenter, crane operator",
  "MECHANIC, such as electrician, plumber, machinist",
  "FACTORY WORKER OR LABORER, such as assembler, janitor",
  "TRANSPORTATION, such as bus driver, taxi driver",
  "MILITARY OR SECURITY, such as police officer, soldier, fire fighter",
  "FARM OR FISHERY WORKER",
  "She doesn’ t work, but she is not disabled.",
  "She is disabled, and therefore doesn’ t work.",
  "She is retired.",
  "She receives Public Assistance, such as welfare.",
  "She works, but I don’ t know what her job is.",
  "legitimate skip: don't live with mom",
  "multiple response"))
label(inschool$momjob) <- "Mom's job, what comes closest?"
table(inschool$momjob, useNA="ifany")

inschool$momnowork <- car::recode(inschool$s15, '0=1; 1=0; 7:9=NA', as.factor.result=F)
# 7 don't live with her (7134n), 8=don't know (2618n), 9=multiple response (29n)
label(inschool$momnowork) <- "Mom doesn't work for pay indicator"

table(inschool$momnowork, useNA="ifany")
table(inschool$momjob, inschool$momnowork, useNA="ifany")
table(inschool$momnowork, inschool$momjob, useNA="ifany")

inschool$momcare <- car::recode(inschool$s16, '7:9=NA', as.factor.result=F)
inschool$momcare <- factor(inschool$momcare, levels=c(1:5), labels=c("not at all", "a little", "some", "quite a bit", "very much"))
describe(inschool$momcare)
label(inschool$momcare) <- "How much do you think your Mom cares about you"
inschool$momnocare <- car::recode(inschool$s16, '1:4=1; 5=0; 7:9=NA', as.factor.result=F)
label(inschool$momnocare) <- "Mom cares 'some' or less"
inschool$lwodad <- car::recode(inschool$s17, '0=1; 1=0; 7:99=NA', as.factor.result=F) # 9 = multiple response (28n)
label(inschool$lwodad) <- "Lives w/o Dad (bio/step/foster/adoptive)"

inschool$daded <- car::recode(inschool$s18, '3=4; 4=3; 9:99=NA', as.factor.result=F) #switch GED & HS diploma
# s18 coding: 1=8th grade or less, 2=more than 8th, 4=GED, 3=high school grad,
# 5=vocational training, 6=some college, 7=college grad, 8=post-college training,
# 9=he went to school but I don't know what level, 10=he never went to school 
#11=don't know if he went to school, 97=don't live w/him, 99=multiple response (605n)

inschool$dadimmig <- car::recode(inschool$s19, '0=1; 1=0; 7:9=NA', as.factor.result=F) 
# 7=don't live with him, 8=don't know (1083n), 9 = multiple response (16n)
label(inschool$dadimmig) <- "Dad not born in U.S.A."
inschool$dadjob <- car::recode(inschool$s20, '97:99=NA', as.factor.result=F)
inschool$dadjob <- factor(inschool$s20, levels=c(1:20, 97,99), labels=c(
  "HOMEMAKER",
  "PROFESSIONAL 1, such as doctor, lawyer, scientist",
  "PROFESSIONAL 2, such as teacher, librarian, nurse",
  "MANAGER, such as executive, director",
  "TECHNICAL, such as computer specialist, radiologist",
  "OFFICE WORKER, such as bookkeeper, office clerk, secretary",
  "SALES WORKER, such as insurance agent, store clerk",
  "RESTAURANT WORKER OR PERSONAL SERVICE, such as waitress, housekeeper",
  "CRAFTSPERSON, such as toolmaker, woodworker",
  "CONSTRUCTION WORKER, such as carpenter, crane operator",
  "MECHANIC, such as electrician, plumber, machinist",
  "FACTORY WORKER OR LABORER, such as assembler, janitor",
  "TRANSPORTATION, such as bus driver, taxi driver",
  "MILITARY OR SECURITY, such as police officer, soldier, fire fighter",
  "FARM OR FISHERY WORKER",
  "She doesn’ t work, but she is not disabled.",
  "She is disabled, and therefore doesn’ t work.",
  "She is retired.",
  "She receives Public Assistance, such as welfare.",
  "She works, but I don’ t know what her job is.",
  "legitimate skip: don't live with dad",
  "multiple response"))
label(inschool$dadjob) <- "Dad's job, what comes closest?"
table(inschool$dadjob, useNA="ifany")


inschool$dadcare <- car::recode(inschool$s22, '7:9=NA', as.factor.result=F)
inschool$dadcare <- factor(inschool$dadcare, levels=c(1:5), labels=c("not at all", "a little", "some", "quite a bit", "very much"))
describe(inschool$dadcare)
label(inschool$dadcare) <- "How much do you think your Dad cares about you?"
inschool$dadnocare <- car::recode(inschool$s22, '1:3=1; 4:5=0; 7:9=NA', as.factor.result=F)
label(inschool$dadnocare) <- "Dad cares 'some' or less"

inschool$twin <- car::recode(inschool$s23, '9=NA', as.factor.result=F) # 9=multiple response (14n)
inschool$twinhome <- car::recode(inschool$s24, '7=NA; 9=NA', as.factor.result=F) # 9=multiple response (15n)
inschool$adopt <- car::recode(inschool$s25, '9=NA', as.factor.result=F) #9=m.r. (21n)
inschool$livebio <- car::recode(inschool$s26, '7:9=NA', as.factor.result=F) #9=m.r. (12n)
label(inschool$livebio) <- "Adopted, but live with either of your biological parents?"


#family variables (inschool)
inschool$hsize <- car::recode(inschool$s27, '7:99=NA', as.factor.result=F) # 7=shelter/group-home
label(inschool$hsize) <- "household size, including respondent"
inschool$grouphome <- car::recode(inschool$s27, '1:6=0; 7=1; 99=NA', as.factor.result=F) # 7=shelter/group-home
label(inschool$grouphome) <- "I live in a shelter or group-home"
inschool$hsize712 <- car::recode(inschool$s28, '97:99=NA', as.factor.result=F)
#97=legitimate skip, 98=error, q28>q27, 99=m.r. (30n)
inschool$hsize712 <- with(inschool, ifelse(s28==97 & s27==1, 0, hsize712))
label(inschool$hsize712) <- "# household members grade 7-12, not including respondent"

inschool$good2728 <- with(inschool, ifelse(s28==98 | (s27<6 & s27==s28), FALSE, TRUE))
inschool$good2728 <- with(inschool, ifelse(s28==98 | (s27<6 & s27==hsize712), FALSE, TRUE))
label(inschool$good2728) <- "True if s27 & s28 are not contradictory" #missing if either are missing
# Note that s28/hsize712 makes missing some data which contradicts s27, while leaving s27 intact
# It seems about as plausible that the s27 was bad data while s28 was good as the other way around
# The correct response is to be suspicious of both s27/hsize and s28/hsize712 if good2728 is false

describe(inschool$good2728)

with(inschool, table(s28, s27, useNA="ifany"))
with(inschool, table(s28, good2728, useNA="ifany"))
with(inschool, table(s27, good2728, useNA="ifany"))
with(inschool, table(s28, good2728, useNA="ifany"))
with(inschool, table(s27, good2728, useNA="ifany"))

with(inschool, table(s28, s27, useNA="ifany"))

names(inschool)
describe(inschool$hsize712)
table(inschool$s28, inschool$s27, useNA="ifany")
with(inschool, table(s27==s28, s27, useNA="ifany"))

inschool$sib1gennum <- with(inschool, s29a + s30a)
inschool$sib2gennum <- with(inschool, s29b + s30b)
inschool$sib3gennum <- with(inschool, s29c + s30c)
inschool$sib4gennum <- with(inschool, s29d + s30d)
inschool$sib5gennum <- with(inschool, s29e + s30e)

inschool$sibgensum0 <- with(inschool, (sib1gennum==0) + (sib2gennum==0) +
  (sib3gennum==0) + (sib4gennum==0) + (sib5gennum==0))

inschool$sibgensum1 <- with(inschool, (sib1gennum==1) + (sib2gennum==1) +
  (sib3gennum==1) + (sib4gennum==1) + (sib5gennum==1)) # num gave 1 and only 1 sib gender

inschool$sibgensum2 <- with(inschool, (sib1gennum==2) + (sib2gennum==2) +
                          (sib3gennum==2) + (sib4gennum==2) + (sib5gennum==2))

# inschool$sibgennumsum <- with(inschool, sibgensum0 + sibgensum1 + sibgensum2)
# describe(inschool$sibgennumsum) # should always equal 5 and it does
# inschool$sibgennumsum <- NULL


describe(inschool$sibgensum0) 
describe(inschool$sibgensum1) 
describe(inschool$sibgensum2)

inschool$good2830 <- with(inschool, ifelse(is.na(s28)==FALSE & sibgensum2==0 & hsize712==sibgensum1,TRUE, FALSE))
label(inschool$good2830) <- "True means Q28 is not inconsistent w/ Q29-Q30"
# if good2830 is false, this means s28/hsize712 is NA or the same sibling is labeled both male and female
#                       therefore s28 and the sibling variables which follow are suspect
# depressingly, it is false 23% of the time
# 
describe(inschool$good2830)

table(inschool$good2830, inschool$sibgensum0, useNA="ifany")
table(inschool$good2830, inschool$hsize712, useNA="ifany")

table(inschool$good2830, inschool$hsize-inschool$hsize712-1, useNA="ifany") 

# disconcerting facts about the Q27-Q30 on gender of household members in grades 7-12

# even when good2830, some conflict between Q27 and Q28, e.g. hsize712 + 1 > hsize

# of those who claim to have 5 who are males (s29a+s29b+s29c+s29d+s29e), 
#     *every single case!* has at least one male also identified as female
# subtracting off the # males also identified as female, 
#     most who claim 4 males grades 7-12 in house claim less than 4 people either gender in Q28
# to reduce the magnitude of the errors I set the max # of bro/sis to hsize712 - 1
# and set them to NA when hsize712 is missing
inschool$bro712 <- with(inschool, s29a+s29b+s29c+s29d+s29e-sibgensum2)

inschool$bro712 <- with(inschool, ifelse(bro712>hsize712, hsize712, bro712))
label(inschool$bro712) <- "# of male household members in grade 7-12"
describe(inschool$bro712)
table(inschool$good2830, inschool$bro712, useNA="ifany")


inschool$sis712 <- with(inschool, s30a+s30b+s30c+s30d+s30e-sibgensum2)
inschool$sis712 <- with(inschool, ifelse(sis712<0, 0, sis712))
inschool$sis712 <- with(inschool, ifelse(sis712>hsize712, hsize712, sis712))
label(inschool$sis712) <- "# of female household members in grade 7-12"
describe(inschool$sis712)

with(inschool, table(hsize712, bro712, useNA="ifany"))
with(inschool, table(hsize712, bro712, good2830, useNA="ifany"))
with(inschool, table(hsize, sis712, useNA="ifany"))
with(inschool, table(hsize, hsize712, useNA="ifany"))
with(inschool, table(hsize-hsize712, useNA="ifany"))

describe(inschool$hsize712)

inschool$sib1gennum <- NULL
inschool$sib2gennum <- NULL
inschool$sib3gennum <- NULL
inschool$sib4gennum <- NULL
inschool$sib5gennum <- NULL

inschool$sibgensum0 <- NULL
inschool$sibgensum1 <- NULL
inschool$sibgensum2 <- NULL

# Above I took a first pass at cleaning up some data on # of household members grade 7-12 =~ siblings
# I'm not going to try to do that for the subsequent questions about the siblings
# If you're going to use it, please make some attempt to improve it

inschool$bmot712 <- with(inschool, s31a+s31b+s31c+s31d+s31e)
label(inschool$bmot712) <- "# siblings share biological mother - CHECK DATA"
inschool$bfat712 <- with(inschool, s32a+s32b+s32c+s32d+s32e)
label(inschool$bfat712) <- "# siblings share biological father - CHECK DATA"
inschool$sibsch712 <- with(inschool, s33a+s33b+s33c+s33d+s33e)
label(inschool$bmot712) <- "# siblings attend same school - BAD DATA"


# s34a through s43e are about activities with nominated friends (inschool)
# this data may be as bad as the sibling data... maybe one could argue the questions
# are less confusing, but there are more of them, and they are later in the survey
# one important thing to do will be to consider these data in relation to the friendship data,
# collected as part of this survey, but found in sfriend.xpt and/or network.xpt
# Descriptives show that there are almost as many, sometimes more, people claiming
# to do ____ with all 5 friends, as compared to doing it with 4.
#    This despite the fact that some nominate 4, rather than 5 friends

inschool$housemsum <- with(inschool, s34a + s34b + s34c + s34d + s34e)
inschool$weekdaymsum <- with(inschool, s35a + s35b + s35c + s35d + s35e)
inschool$weekendmsum <- with(inschool, s36a + s36b + s36c + s36d + s36e)
inschool$talkprobmsum <- with(inschool, s37a + s37b + s37c + s37d + s37e)
inschool$phonemsum <- with(inschool, s38a + s38b + s38c + s38d + s38e)

inschool$housefsum <- with(inschool, s39a + s39b + s39c + s39d + s39e)
inschool$weekdayfsum <- with(inschool, s40a + s40b + s40c + s40d + s40e)
inschool$weekendfsum <- with(inschool, s41a + s41b + s41c + s41d + s41e)
inschool$talkprobfsum <- with(inschool, s42a + s42b + s42c + s42d + s42e)
inschool$phonefsum <- with(inschool, s43a + s43b + s43c + s43d + s43e)

label(inschool$housemsum) <- "# male friends' houses visited in past 7 days"
label(inschool$weekdaymsum) <-"# male friends hung out with after school past 7 days" 
label(inschool$weekendmsum) <- "# male friends spent time together last weekend"
label(inschool$phonemsum) <- "# male friends talked on the phone in past 7 days"

label(inschool$housefsum) <- "# female friends' houses visited in past 7 days"
label(inschool$weekdayfsum) <-"# female friends hung out with after school past 7 days" 
label(inschool$weekendfsum) <- "# female friends spent time together last weekend"
label(inschool$phonefsum) <- "# female friends talked on the phone in past 7 days"

label(inschool$talkprobmsum) <- "# male friends talked with about a problem past 7 days"
label(inschool$talkprobfsum) <- "# female friends talked with about a problem past 7 days"                 

describe(inschool$housemsum)
describe(inschool$weekdaymsum)
describe(inschool$weekendmsum)
describe(inschool$talkprobmsum)
describe(inschool$housemsum)

describe(inschool$housemsum[which(inschool$male.is==1)])
describe(inschool$weekdaymsum)
describe(inschool$weekendmsum)
describe(inschool$talkprobmsum)
describe(inschool$housemsum)

table(inschool$male.is, inschool$housemsum, useNA="ifany")
table(inschool$male.is, inschool$weekdaymsum, useNA="ifany")
table(inschool$male.is, inschool$weekendmsum, useNA="ifany")
table(inschool$male.is, inschool$talkprobmsum, useNA="ifany")



inschool$french  <- inschool$s44a1 #3454
inschool$german  <- inschool$s44a2 #1218
inschool$latin   <- inschool$s44a3 #1484
inschool$spanish <- inschool$s44a4 #6659
inschool$book    <- inschool$s44a5 #1044
inschool$compucl <- inschool$s44a6 #2724
inschool$debate  <- inschool$s44a7 #2061
inschool$drama   <- inschool$s44a8 #5987
inschool$farmer  <- inschool$s44a9 #1663
inschool$histcl  <- inschool$s44a10 #1028
inschool$math    <- inschool$s44a11 #2899
inschool$science <- inschool$s44a12 #2961
inschool$band    <- inschool$s44a13 #10531
inschool$cheer   <- inschool$s44a14 #7258
inschool$chorus  <- inschool$s44a15 #8563
inschool$orchest <- inschool$s44a16 #1966
inschool$othercl <- inschool$s44a17 #15709
inschool$baseb   <- inschool$s44a18 #14039
inschool$basketb <- inschool$s44a19 #16435
inschool$fhockey <- inschool$s44a20 #1085
inschool$footbal <- inschool$s44a21 #11396
inschool$ihockey <- inschool$s44a22 #1908
inschool$soccer  <- inschool$s44a23 #7440
inschool$swimmin <- inschool$s44a24 #4676
inschool$tennis  <- inschool$s44a25 #4369
inschool$track   <- inschool$s44a26 #10714
inschool$volball <- inschool$s44a27 #6662
inschool$wrestle <- inschool$s44a28 #3645
inschool$osport  <- inschool$s44a29 #8021
inschool$newspap <- inschool$s44a30 #3927
inschool$honorso <- inschool$s44a31 #8265
inschool$stucoun <- inschool$s44a32 #6645
inschool$yearbok <- inschool$s44a33 #6819
inschool$noclubs <- inschool$s44 #15380

                      


inschool$clubnum <- rowSums(subset(inschool, select=s44a1:s44a33))

describe(inschool$clubnum)
table(inschool$clubnum, inschool$noclubs, useNA="ifany")

inschool$club.acad <- with(inschool, french + german + latin + book + compucl + debate + histcl + math + science + newspap) 
# removed from earlier version: honorso + stucoun + yearbok + spanish
#     because honor society is automatic for high achievers,
#             student council and yearbook are mainly social
#             spanish was confounded by Hispanic school/identity 
label(inschool$club.acad) <- "# of academic clubs"
inschool$club.acadsr <- sqrt(inschool$club.acad)
label(inschool$club.acadsr) <- "sqrt # of academic clubs"


inschool$ClubMusic =  with(inschool, band + chorus + orchest)

inschool$sportpop = with(inschool, baseb + basketb + footbal + soccer)

inschool$sportpop2 <- with(inschool, baseb + basketb + footbal + soccer + track)
label(inschool$sportpop2) <- "# of popular male sports played"
inschool$sportpop2sr <- sqrt(inschool$sportpop2)
label(inschool$sportpop2sr) <- "sqrt # of popular male sports played"                                     

inschool$fsport <- baseb + basketb +fhockey + soccer + swimmin + track + volball;
label(inschool$fsport) <- "# of popular female sports played"
inschool$fsportsr <- sqrt(inschool$fsport)
label(inschool$fsportsr) <- "sqrt # of popular female sports played"


inschool$sportpd <- with(inschool, ifelse(sportpop >=1, 1, 0))


                 
# Questions 45A-45F "What are the chances you will..."
inschool$live35 <- car::recode(inschool$s45a, '99=NA; ', as.factor.result=FALSE)
#inschool$live35 <- factor(inschool$live35, levels = c(0,1,2,3,4,5,6,7,8),
#                          labels=c("no chance", "< some chance", "some chance",
#                                   "< 50-50", "about 50-50", "> 50-50", "pretty likely", 
#                                   "> pretty likely", "it will happen"))
describe(inschool$live35)
inschool$marry25 <- car::recode(inschool$s45b, '99=NA; ', as.factor.result=FALSE)
inschool$kill21 <- car::recode(inschool$s45c, '13=NA; 99=NA; ', as.factor.result=FALSE)
inschool$hiv <- car::recode(inschool$s45d, '99=NA; ', as.factor.result=FALSE)
inschool$college <- car::recode(inschool$s45e, '99=NA; ', as.factor.result=FALSE)
inschool$midclass <- car::recode(inschool$s45f, '99=NA; ', as.factor.result=FALSE)

describe(inschool$marry25)


inschool$tteach <- car::recode(inschool$s46a, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (120n)
inschool$tteach <- factor(inschool$tteach, levels = c(1,0,2,3,4), labels = c("just a few times", "never",  "about once a week", "almost everyday", "everyday"))
label(tteach) <- "Get in trouble with the teacher"
describe(inschool$tteach)

inschool$tattent <- car::recode(inschool$s46b, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (69n)
inschool$tattent <- factor(inschool$tattent, levels = c(2,0,1,3,4), labels = c("about once a week", "never", "just a few times", "almost everyday", "everyday"))
label(inschool$tattent) <- "Trouble Paying Attention to Teacher"

inschool$thmwk <- car::recode(inschool$s46c, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (92n)
inschool$thmwk <- factor(inschool$thmwk, levels = c(2,0,1,3,4), labels = c("about once a week", "never", "just a few times", "almost everyday", "everyday"))
label(inschool$thmwk) <- "Trouble Getting Homework Done"

describe(inschool$thmwk)


inschool$tstud <- car::recode(inschool$s46d, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (63n)
inschool$tstud <- factor(inschool$thmwk, levels = c(2,0,1,3,4), labels = c("about once a week", "never", "just a few times", "almost everyday", "everyday"))
label(inschool$thmwk) <- "Trouble Getting Homework Done"


inschool$tv <- car::recode(inschool$s47, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (120n)
inschool$tv <- factor(inschool$tv, levels = c(2,0,1,3,4), labels = c("1 to 2", "none", "less than 1", "3 to 4", "more than 4")) #error in survey, no 2-3 hr 
describe(inschool$tv)


inschool$effort <- car::recode(inschool$s48, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (70n)
inschool$effort <- factor(inschool$effort, levels = c(3,1,2,4), labels = c("hard enough", "never", "not very hard", "very hard"))
label(inschool$effort) <- "How hard do you try to get your schoolwork done?"
describe(inschool$effort)


inschool$alcohold <- car::recode(inschool$s49, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (25n) (5257 additional missing)
label(inschool$alcohold) <- "Drank alcohol >2-3 times in life? W1" # "more than just a sip"
describe(inschool$alcohold)


inschool$health <- car::recode(inschool$s50, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (102n) (4575 additional missing)

inschool$health <- factor(inschool$health, levels = c(1,2,3,4,5), 
                          labels = c("excellent", "very good", "good", "fair", "poor"))
label(inschool$health) <- "In general, how is your health?"


inschool$physical <- car::recode(inschool$s51, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (56n) (5338 additional missing)

inschool$physical <- factor(inschool$physical, levels = c(1,2,3,4,5), 
                          labels = c("< 12 months", "1 to 2 years", "> 2 years", "don't remember", "never had one"))
label(inschool$health) <- "When did you last have a physical exam?"


inschool$dental <- car::recode(inschool$s52, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (37n) (5312 additional missing)

inschool$dental <- factor(inschool$dental, levels = c(1,2,3,4,5), 
                            labels = c("< 12 months", "1 to 2 years", "> 2 years", "don't remember", "never had one"))
label(inschool$dental) <- "When did you last have a dental exam?"

inschool$mental <- car::recode(inschool$s53, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (58n) (6579 additional missing)

inschool$mental <- factor(inschool$mental, levels = c(1,2,3,4,5), 
                          labels = c("< 12 months", "1 to 2 years", "> 2 years", "don't remember", "never had one"))
label(inschool$mental) <- "When did you last have counseling/testing or any mental health service?"




#Do you have difficulty using your hands, arms, legs, or feet because of 
#a physical condition that has lasted for the past twelve months or more?
inschool$physprob <- car::recode(inschool$s54, '9=NA; ', as.factor.result=FALSE) # 9 = multiple response (16n) (6666 additional missing)
# if no, skip Q55A-55D

inschool$heartprob <- car::recode(inschool$s55a, '7=NA; 9=NA; ', as.factor.result=FALSE) 
inschool$asthma <- car::recode(inschool$s55b, '7=NA; 9=NA; ', as.factor.result=FALSE)
inschool$obreath <- car::recode(inschool$s55c, '7=NA; 9=NA; ', as.factor.result=FALSE)
inschool$oprob <- car::recode(inschool$s55d, '7=NA; 9=NA; ', as.factor.result=FALSE)

inschool$helpwalk <- car::recode(inschool$s56, '9=NA; ', as.factor.result=FALSE)

inschool$brace <- car::recode(inschool$s57, '9=NA; ', as.factor.result=FALSE)
inschool$prosthesis <- car::recode(inschool$s58, '9=NA; ', as.factor.result=FALSE)

inschool$smoke <- car::recode(inschool$s59a, '99=NA; ', as.factor.result=FALSE)
inschool$smoke <- factor(inschool$smoke, levels = c(0,1,2,3,4,5,6), 
                          labels = c("never", "1-2 times", "<=1/month", "2-3 days/month",
                                     "1-2/week", "3-5/week", "nearly every day"))
label(inschool$smoke) <- "How often did you smoke cigarettes, past yr?"
describe(inschool$smoke)


inschool$alcohol <- car::recode(inschool$s59b, '99=NA; ', as.factor.result=FALSE)
inschool$alcohol <- factor(inschool$alcohol, levels = c(0,1,2,3,4,5,6), 
                         labels = c("never", "1-2 times", "<=1/month", "2-3 days/month",
                                    "1-2/week", "3-5/week", "nearly every day"))
label(inschool$alcohol) <- "How often drink beer/wine/liquor, past yr?"
describe(inschool$alcohol)

inschool$drunk <- car::recode(inschool$s59c, '99=NA; ', as.factor.result=FALSE)
inschool$drunk <- factor(inschool$drunk, levels = c(0,1,2,3,4,5,6), 
                           labels = c("never", "1-2 times", "<=1/month", "2-3 days/month",
                                      "1-2/week", "3-5/week", "nearly every day"))
label(inschool$drunk) <- "How often did you get drunk, past yr?"
describe(inschool$drunk)



inschool$race <- car::recode(inschool$s59d, '99=NA; ', as.factor.result=FALSE)
inschool$race <- factor(inschool$race, levels = c(0,1,2,3,4,5,6), 
                           labels = c("never", "1-2 times", "<=1/month", "2-3 days/month",
                                      "1-2/week", "3-5/week", "nearly every day"))
label(inschool$race) <- "How often did you race on bike/skateboard/roller blaes/boat/car?"
describe(inschool$race)



inschool$dare <- car::recode(inschool$s59e, '99=NA; ', as.factor.result=FALSE)
inschool$dare <- factor(inschool$dare, levels = c(0,1,2,3,4,5,6), 
                           labels = c("never", "1-2 times", "<=1/month", "2-3 days/month",
                                      "1-2/week", "3-5/week", "nearly every day"))
label(inschool$dare) <- "How often do something dangerous because you were dared, past yr?"
describe(inschool$dare)


inschool$liepar <- car::recode(inschool$s59f, '99=NA; ', as.factor.result=FALSE)
inschool$liepar <- factor(inschool$liepar, levels = c(0,1,2,3,4,5,6), 
                        labels = c("never", "1-2 times", "<=1/month", "2-3 days/month",
                                   "1-2/week", "3-5/week", "nearly every day"))
label(inschool$liepar) <- "How often did you lie to your parents, past yr?"
describe(inschool$liepar)


inschool$skipsc <- car::recode(inschool$s59g, '99=NA; ', as.factor.result=FALSE)
inschool$skipsc <- factor(inschool$skipsc, levels = c(0,1,2,3,4,5,6), 
                          labels = c("never", "1-2 times", "<=1/month", "2-3 days/month",
                                     "1-2/week", "3-5/week", "nearly every day"))
label(inschool$skipsc) <- "How often did you skip school without excuse, past yr?"
describe(inschool$skipsc)

# Questions 60a-60o
inschool$sick <- car::recode(inschool$s60a, '9=NA; ', as.factor.result=FALSE)
inschool$sick <- factor(inschool$sick, levels = c(0,1,2,3,4), 
                          labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$sick) <- "How often did you feel really sick, past month?"
describe(inschool$sick)

inschool$tired <- car::recode(inschool$s60b, '9=NA; ', as.factor.result=FALSE)
inschool$tired <- factor(inschool$tired, levels = c(0,1,2,3,4), 
                        labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$tired) <- "How often did you wake up feeling tired, past month?"
describe(inschool$tired)

inschool$skin <- car::recode(inschool$s60c, '9=NA; ', as.factor.result=FALSE)
inschool$skin <- factor(inschool$skin, levels = c(0,1,2,3,4), 
                        labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$skin) <- "How often did you have skin problems, itching/pimples, past month?"
describe(inschool$skin)

inschool$dizzy <- car::recode(inschool$s60d, '9=NA; ', as.factor.result=FALSE)
inschool$dizzy <- factor(inschool$dizzy, levels = c(0,1,2,3,4), 
                        labels = c("never", "rarely", "occasionally", "often", "everyday"))
label(inschool$dizzy) <- "How often were you dizzy, past month?"
describe(inschool$dizzy)

inschool$chest <- car::recode(inschool$s60e, '9=NA; ', as.factor.result=FALSE)
inschool$chest <- factor(inschool$chest, levels = c(0,1,2,3,4), 
                        labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$chest) <- "How often did you have chest pain, past month?"
describe(inschool$chest)

inschool$headache <- car::recode(inschool$s60f, '9=NA; ', as.factor.result=FALSE)
inschool$headache <- factor(inschool$headache, levels = c(0,1,2,3,4), 
                        labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$headache) <- "How often did you feel really headache, past month?"
describe(inschool$headache)


inschool$bodyache <- car::recode(inschool$s60g, '9=NA; ', as.factor.result=FALSE)
inschool$bodyache <- factor(inschool$bodyache, levels = c(0,1,2,3,4), 
                        labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$bodyache) <- "How often aches/pains in muscles/joints, past month?"
describe(inschool$bodyache)

inschool$stomachache <- car::recode(inschool$s60h, '9=NA; ', as.factor.result=FALSE)
inschool$stomachache <- factor(inschool$stomachache, levels = c(0,1,2,3,4), 
                          labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$stomachache) <- "How often did you have a stomachache, past month?"
describe(inschool$stomachache)

inschool$appetite <- car::recode(inschool$s60i, '9=NA; ', as.factor.result=FALSE)
inschool$appetite <- factor(inschool$appetite, levels = c(0,1,2,3,4), 
                      labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$appetite) <- "How often trouble eating / poor appetite, past month?"
describe(inschool$appetite)

inschool$sleep <- car::recode(inschool$s60j, '9=NA; ', as.factor.result=FALSE)
inschool$sleep <- factor(inschool$sleep, levels = c(0,1,2,3,4), 
                      labels = c("never", "rarely", "occasionally", "often", "everyday"))
label(inschool$sleep) <- "How often did you have trouble falling/staying asleep, past month?"
describe(inschool$sleep)


inschool$depress <- car::recode(inschool$s60k, '9=NA; ', as.factor.result=FALSE)
inschool$depress <- factor(inschool$depress, levels = c(0,1,2,3,4), 
                      labels = c("never", "rarely", "occasionally", "often", "everyday"))
label(inschool$depress) <- "How often did you feel depressed or blue, past month?"
describe(inschool$depress)


inschool$trelax <- car::recode(inschool$s60l, '9=NA; ', as.factor.result=FALSE)
inschool$trelax <- factor(inschool$trelax, levels = c(0,1,2,3,4), 
                         labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$trelax) <- "How often did you have trouble relaxing, past month?"
describe(inschool$trelax)


inschool$moody <- car::recode(inschool$s60m, '9=NA; ', as.factor.result=FALSE)
inschool$moody <- factor(inschool$moody, levels = c(0,1,2,3,4), 
                         labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$moody) <- "How often were you moody, past month?"
describe(inschool$moody)


inschool$cry <- car::recode(inschool$s60n, '9=NA; ', as.factor.result=FALSE)
inschool$cry <- factor(inschool$cry, levels = c(0,1,2,3,4), 
                         labels = c("never", "rarely", "occasionally", "often",
                                    "everyday"))
label(inschool$cry) <- "How often did you cry a lot, past month?"
describe(inschool$cry)


inschool$afraid <- car::recode(inschool$s60o, '9=NA; ', as.factor.result=FALSE)
inschool$afraid <- factor(inschool$afraid, levels = c(0,1,2,3,4), 
                         labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$afraid) <- "How often did you feel really afraid, past month?"
describe(inschool$afraid)


inschool$misssch <- car::recode(inschool$s61a, '9=NA; ', as.factor.result=FALSE)
inschool$misssch <- factor(inschool$misssch, levels = c(0,1,2,3,4), 
                         labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$misssch) <- "How often health/emotional problem cause you to miss school, past month?"
describe(inschool$misssch)


inschool$misssocial <- car::recode(inschool$s61b, '9=NA; ', as.factor.result=FALSE)
inschool$misssocial <- factor(inschool$misssocial, levels = c(0,1,2,3,4), 
                         labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$misssocial) <- "How often health/emotional problem cause you to miss social activity, past month?"
describe(inschool$misssocial)


inschool$twalk <- car::recode(inschool$s61c, '9=NA; ', as.factor.result=FALSE)
inschool$twalk <- factor(inschool$twalk, levels = c(0,1,2,3,4), 
                         labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$twalk) <- "How often had trouble walking, past month?"
describe(inschool$twalk)


inschool$trun <- car::recode(inschool$s61d, '9=NA; ', as.factor.result=FALSE)
inschool$trun <- factor(inschool$trun, levels = c(0,1,2,3,4), 
                         labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$trun) <- "How often had trouble running, past month?"
describe(inschool$trun)


inschool$tbend <- car::recode(inschool$s61e, '9=NA; ', as.factor.result=FALSE)
inschool$tbend <- factor(inschool$tbend, levels = c(0,1,2,3,4), 
                         labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$tbend) <- "How often had trouble bending or lifting, past month?"
describe(inschool$tbend)


inschool$thands <- car::recode(inschool$s61f, '9=NA; ', as.factor.result=FALSE)
inschool$thands <- factor(inschool$thands, levels = c(0,1,2,3,4), 
                         labels = c("never", "rarely", "occasionally", "often","everyday"))
label(inschool$thands) <- "How often had trouble using hands or fingers, past month?"
describe(inschool$thands)


inschool$energy <- car::recode(inschool$s62a, '9=NA; ', as.factor.result=FALSE)
inschool$energy <- factor(inschool$energy, levels = c(1,2,3,4,5), 
                   labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$energy) <- "I have a lot of energy"
describe(inschool$energy)


inschool$closesch <- car::recode(inschool$s62b, '9=NA; ', as.factor.result=FALSE)
inschool$closesch <- factor(inschool$closesch, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$closesch) <- "I feel close to people at this school"
describe(inschool$closesch)

inschool$raresick <- car::recode(inschool$s62c, '9=NA; ', as.factor.result=FALSE)
inschool$raresick <- factor(inschool$raresick, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$raresick) <- "I am seldom sick"
describe(inschool$raresick)


inschool$briefsick <- car::recode(inschool$s62d, '9=NA; ', as.factor.result=FALSE)
inschool$briefsick <- factor(inschool$briefsick, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$briefsick) <- "When I do get sick, I get better quickly"
describe(inschool$briefsick)


inschool$partsch <- car::recode(inschool$s62e, '9=NA; ', as.factor.result=FALSE)
inschool$partsch <- factor(inschool$partsch, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$partsch) <- "I feel like I am a part of this school"
describe(inschool$partsch)


inschool$coord <- car::recode(inschool$s62f, '9=NA; ', as.factor.result=FALSE)
inschool$coord <- factor(inschool$coord, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$coord) <- "I am well coordinated"
describe(inschool$coord)



inschool$prejudice <- car::recode(inschool$s62g, '9=NA; ', as.factor.result=FALSE)
inschool$prejudice <- factor(inschool$prejudice, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$prejudice) <- "Students at this school are prejudiced"
describe(inschool$prejudice)


inschool$goodqual <- car::recode(inschool$s62h, '9=NA; ', as.factor.result=FALSE)
inschool$goodqual <- factor(inschool$goodqual, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$goodqual) <- "I have a lot of good qualities"
describe(inschool$goodqual)


inschool$happysch <- car::recode(inschool$s62i, '9=NA; ', as.factor.result=FALSE)
inschool$happysch <- factor(inschool$happysch, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$happysch) <- "I am happy to be at this school"
describe(inschool$happysch)


inschool$physfit <- car::recode(inschool$s62j, '9=NA; ', as.factor.result=FALSE)
inschool$physfit <- factor(inschool$physfit, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$physfit) <- "I am physically fit"
describe(inschool$physfit)


inschool$proud <- car::recode(inschool$s62k, '9=NA; ', as.factor.result=FALSE)
inschool$proud <- factor(inschool$proud, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$proud) <- "I have a lot to be proud of"
describe(inschool$proud)


inschool$fairteac <- car::recode(inschool$s62l, '9=NA; ', as.factor.result=FALSE)
inschool$fairteac <- factor(inschool$fairteac, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$fairteac) <- "Teachers at school treat students fairly"
describe(inschool$fairteac)


inschool$likeself <- car::recode(inschool$s62m, '9=NA; ', as.factor.result=FALSE)
inschool$likeself <- factor(inschool$likeself, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$likeself) <- "I like myself just the way I am"
describe(inschool$likeself)


inschool$doright <- car::recode(inschool$s62n, '9=NA; ', as.factor.result=FALSE)
inschool$doright <- factor(inschool$doright, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$doright) <- "I feel I am doing everything just right"
describe(inschool$doright)



inschool$accepted <- car::recode(inschool$s62o, '9=NA; ', as.factor.result=FALSE)
inschool$accepted <- factor(inschool$accepted, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$accepted) <- "I feel socially accepted"
describe(inschool$accepted)


inschool$loved <- car::recode(inschool$s62p, '9=NA; ', as.factor.result=FALSE)
inschool$loved <- factor(inschool$loved, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$loved) <- "I feel loved and wanted"
describe(inschool$loved)


inschool$safeneig <- car::recode(inschool$s62q, '9=NA; ', as.factor.result=FALSE)
inschool$safeneig <- factor(inschool$safeneig, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$safeneig) <- "I feel safe in my neighborhood"
describe(inschool$safeneig)


inschool$safesch <- car::recode(inschool$s62r, '9=NA; ', as.factor.result=FALSE)
inschool$safesch <- factor(inschool$safesch, levels = c(1,2,3,4,5), 
                          labels = c("strongly agree", "agree", "neither", "disagree","strongly disagree"))
label(inschool$safesch) <- "I feel safe in my school"
describe(inschool$safesch)



inschool$exercise <- car::recode(inschool$s63, '9=NA; ', as.factor.result=FALSE)
inschool$exercise <- factor(inschool$exercise, levels = c(0,1,2,3,4), 
                          labels = c("never", "1 or 2", "3-5", "6-7", "7+"))
label(inschool$exercise) <- "Work/play/exercise enough to sweat and breathe heavily, times/week"
describe(inschool$exercise)


inschool$fight <- car::recode(inschool$s64, '9=NA; ', as.factor.result=FALSE)
inschool$fight <- factor(inschool$fight, levels = c(0,1,2,3,4), 
                            labels = c("never", "1 or 2", "3-5", "6-7", "7+"))
label(inschool$fight) <- "Physical fight, How often in past year"
describe(inschool$fight)


inschool$nogodoc <- car::recode(inschool$s65, '9=NA; ', as.factor.result=FALSE)
describe(inschool$nogodoc)





#####################################################################
# Missing Data and Multiple Response Counts
# What does it mean when one, or many, variables are coded system-missing?
# Did the student skip the questions or did the machine reader fail
#   If desire to answer is the problem, why was desire lacking?
# What does it say about the student's character, mood, social context, etc.
#   
# The same questions can be asked of respondents giving multiple-responses.
# 
# I created variables to help summarize these patterns.
# There are many ways you might think about them, even without a strong
# theory, its worth including them as covariates in some analyses as
# a sensitivity analysis.
#
# In creating these missing and multiple response counts, I chose variables
# which I thought people "should" be able to give *one* answer without
# too much difficulty
                       
   
inschool$mrs1s13 <- apply(inschool[c("s1", "s2", "s3", "s8", "s9", "s11", "s13")],1,function(x){sum(x == c(99,9,99,9,99,9,9),na.rm = TRUE)})
inschool$mrs21s25 <- apply(inschool[c("s21", "s22", "s23", "s25")],1,function(x){sum(x == 9,na.rm = TRUE)})
inschool$mrs45 <- apply(inschool[c("s45a", "s45b", "s45c", "s45d")],1,function(x){sum(x == 99,na.rm = TRUE)})
inschool$mrs46 <- apply(inschool[c("s46a", "s46b", "s46c", "s46d")],1,function(x){sum(x == 9,na.rm = TRUE)})
inschool$mrs51s63 <- apply(inschool[c("s51", "s61f", "s62p", "s62q","s62r","s63")],1,function(x){sum(x == 9,na.rm = TRUE)})

inschool$mrsum <- with(inschool, mrs1s13 + mrs21s25 + mrs45 + mrs46 + mrs51s63)
label(inschool$mrsum) <- "How many 'multiple response' out of 25 possible"
describe(inschool$mrsum)

inschool$mrinfo <- with(inschool, ifelse(mrsum==0, 0, ifelse(mrsum==1, 1, ifelse(mrsum>1, 2, NA)))) 
inschool$mrinfo <- factor(inschool$mrinfo, levels(0:2), labels=c("0","1","2-25"))
label(inschool$mrinfo) <- "Factor: How many 'multiple response' out of 25 possible"
describe(inschool$mrinfo)


inschool$mrs1s23 <- NULL
inschool$mrs21s25 <- NULL
inschool$mrs45 <- NULL
inschool$mrs46 <- NULL
inschool$mrs51s63 <- NULL

# Missing Data Indicators, whether it indicates a character trait, or the social context,
                        # some measure like this may be a useful covariate in many analyses
                      

inschool$nainschs1s9 <- with(inschool, is.na(s1) + is.na(s2) + is.na(s3) + + is.na(s4) + is.na(s8) + is.na(s9))
inschool$nainschs17s26 <- with(inschool, is.na(s17) + is.na(s22) + is.na(s23) + is.na(s24) + is.na(s25) + is.na(s26)) 
inschool$nainschs45s46d <- with(inschool, is.na(s45a) + is.na(s45b) + is.na(s45c) + is.na(s45d) + is.na(s45e) + is.na(s45f) + is.na(s46a) + is.na(s46b) + is.na(s46c) + is.na(s46d))
inschool$nainschs51s63 <- with(inschool, is.na(s51) +is.na(s61f) + is.na(s62p)+ is.na(s62q)+ is.na(s62r) + is.na(s63))

inschool$nainschsum <- with(inschool, nainschs1s9 + nainschs17s26 + nainschs45s46d + nainschs51s63)
label(inschool$nainschsum) <- c("How many NA on 28 inschool questions?")
table(inschool$nainschsum)
describe(inschool$nainschsum)
                      
inschool$nainschinfo <- with(inschool, ifelse(nainschsum==0, 0, ifelse(nainschsum==1, 1, 
                                      ifelse(nainschsum<=6, 2, ifelse(nainschsum<=16, 3,
                                      ifelse(nainschsum>16, 4, NA))))))
                      
inschool$nainschinfo <- factor(inschool$nainschinfo, levels = c(0:4), labels=c("0", "1", "2-6", "7-16", "17-28"))
label(inschool$nainschinfo) <- c("Factor: How many NA on 28 inschool questions?")
describe(inschool$nainschinfo)

inschool$nainschs1s9 <- NULL
inschool$nainschs17s26 <- NULL
inschool$nainschs45s46d <- NULL
inschool$nainschs51s63 <- NULL


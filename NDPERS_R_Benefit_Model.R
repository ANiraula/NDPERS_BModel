rm(list = ls())
library("readxl")
library(tidyverse)
library(zoo)
setwd(getwd())

FileName <- '/Users/anilniraula/databaseR/NDPERS_BM_Inputs.xlsx'
#FileName <- "https://github.com/ANiraula/NDPERS_BModel/blob/main/NDPERS_BM_Inputs.xlsx?raw=true"

#urlfile="https://github.com/ANiraula/NDPERS_BModel/blob/main/NDPERS_BM_Inputs.xlsx?raw=true"
#inputs<-read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
#inputs <- setDT(inputs)
YearStart <- 2021
Age <- 20:120
YOS <- 0:100
RetirementAge <- 20:120
Years <- 2011:2121    #(why 2121? Because 120 - 20 + 2021 = 2121)
#Updated from 2010 to 2011

#Assigning individual  Variables
model_inputs <- read_excel(FileName, sheet = 'Main')

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}

#Import key data tables
SurvivalRates <- read_excel(FileName, sheet = 'Mortality Rates')#Updated* (to RP-2010 General)
#View(SurvivalRates)
#View(MaleMP)
MaleMP <- read_excel(FileName, sheet = 'MP-2019_Male') #Updated* (to MP-2019)
FemaleMP <- read_excel(FileName, sheet = 'MP-2019_Female')#Updated* (to MP-2019)
SalaryGrowth <- read_excel(FileName, sheet = "Salary Growth")#Updated* (How to combined YOS & AGE increases?)
### Addition ###
SalaryGrowthYOS <- read_excel(FileName, sheet = "Salary Growth YOS")#Added* (to combine YOS & AGE increases)
################
SalaryEntry <- read_excel(FileName, sheet = "Salary and Headcount") %>% #Updated*
select(entry_age, start_sal, count_start)#Updated*

##############
TerminationRateAfter5 <- read_excel(FileName, sheet = 'Termination Rates after 5')#Updated*
TerminationRateBefore5 <- read_excel(FileName, sheet = 'Termination Rates before 5')#Updated*
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')#Updated*
#View(RetirementRates)

### Adding scaling factors
#scale.act.male <- 0.92 
#scale.ret.male <- 1.03
#scale.act.female <- 0.92 
#scale.ret.female <- 1.01 

#Function for determining retirement eligibility (including normal retirement, unreduced early retirement, and reduced early retirement)

### Updated* ###
IsRetirementEligible <- function(Age, YOS){
  Check = ifelse((Age >= NormalRetAgeI) & (YOS >= NormalYOSI) |
                  (Age >= NormalRetRuleAge) & (YOS + Age >= NormalRetRule) |
                  (YOS + Age >= ReduceRetRule) & Age >= ReduceRetAge, TRUE, FALSE)
  return(Check)
}
################

#These rates dont change so they're outside the function
#Transform base mortality rates and mortality improvement rates
MaleMP <- MaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
  mutate(Years = as.numeric(Years))

MaleMP_ultimate <- MaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_male = MP_male) %>% 
  select(-Years)

FemaleMP <- FemaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
  mutate(Years = as.numeric(Years))

FemaleMP_ultimate <- FemaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_female = MP_female) %>% 
  select(-Years)


##Mortality calculations
#Expand grid for ages 20-120 and years 2010 to 2121 (why 2121? Because 120 - 20 + 2021 = 2121)
MortalityTable <- expand_grid(Age, Years)

SurvivalRates <- SurvivalRates %>% mutate_all(as.numeric)

#Join base mortality table with mortality improvement table and calculate the final mortality rates
MortalityTable <- MortalityTable %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP, by = c("Age", "Years")) %>% 
  left_join(FemaleMP, by = c("Age", "Years")) %>% 
  left_join(MaleMP_ultimate, by = "Age") %>% 
  left_join(FemaleMP_ultimate, by = "Age") %>% 
  mutate(MaleMP_final = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male),
         FemaleMP_final = ifelse(Years > max(FemaleMP$Years),  MP_ultimate_female, MP_female),
         entry_age = Age - (Years - YearStart),
         YOS = Age - entry_age) %>% 
  group_by(Age) %>%
  
  #MPcumprod is the cumulative product of (1 - MP rates), starting from 2011. We use it later so make life easy and calculate now
  mutate(MPcumprod_male = cumprod(1 - MaleMP_final),
         #Started mort. table from 2011 (instead of 2010) 
         #to cumsum over 2011+ & then multiply by 2010 MP-2019
         #removed /(1 - MaleMP_final[Years == 2010])
         MPcumprod_female = cumprod(1 - FemaleMP_final),
         mort_male = ifelse(IsRetirementEligible(Age, YOS)==F, PubG_2010_employee_male * ScaleMultipleMaleAct, #Adding adj. facctors
                            PubG_2010_healthy_retiree_male * ScaleMultipleMaleRet) * MPcumprod_male,
         mort_female = ifelse(IsRetirementEligible(Age, YOS)==F, PubG_2010_employee_female * ScaleMultipleFemaleAct,
                              PubG_2010_healthy_retiree_female * ScaleMultipleFemaleRet) * MPcumprod_female,
         mort = (mort_male + mort_female)/2) %>% 
         #Recalcualting average
  filter(Years >= 2021, entry_age >= 25) %>% 
  ungroup()

#############
#############

#filter out the necessary variables
MortalityTable <- MortalityTable %>% select(Age, Years, entry_age, mort) %>% 
  arrange(entry_age) 

#View(MortalityTable)
######################
######################

#Separation Rates
SeparationRates <- expand_grid(Age, YOS) %>% 
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(entry_age, Age) %>% 
  left_join(TerminationRateAfter5, by = "Age") %>%
  left_join(TerminationRateBefore5, by = "YOS") %>% 
  left_join(RetirementRates, by = c("Age"))

#colnames(SeparationRates)
######################

#If you're retirement eligible, use the retirement rates, then checks YOS < 5 and use the regular termination rates
SeparationRates <- SeparationRates %>% 
  mutate(retirement_cond = IsRetirementEligible(Age,YOS),
         SepRateMale = ifelse(retirement_cond == T, RetRate,
                              ifelse(YOS < 5, TermBefore5Male, TermAfter5Male)),
         SepRateFemale = ifelse(retirement_cond == T,RetRate,
                                ifelse(YOS < 5, TermBefore5Female, TermAfter5Female)),
         SepRate = ((SepRateMale+SepRateFemale)/2)) %>% 
  group_by(entry_age) %>% 
  mutate(RemainingProb = cumprod(1 - lag(SepRate, default = 0)),
         SepProb = lag(RemainingProb, default = 1) - RemainingProb) %>% 
  ungroup()

#View(SeparationRates)

#How to make sure YOS base separation + Salary growth is applied (joint + ultimate column?)

#Filter out unecessary values
SeparationRates <- SeparationRates %>% select(Age,YOS,SepProb)

#colnames(SalaryGrowth)[2] <- "YOS"
#Create a long-form table of Age and YOS and merge with salary data
SalaryData <- expand_grid(Age, YOS) %>% 
  mutate(entry_age = Age - YOS) %>%    #Add entry age
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(entry_age) %>% 
  left_join(SalaryEntry, by = "entry_age") %>% 
  left_join(SalaryGrowthYOS, by = c("YOS")) %>%
  left_join(SalaryGrowth, by = c("Age")) %>%
  ### Additions ###
  mutate_all(as.numeric) %>% 
  replace(is.na(.), 0)   %>%
  mutate(salary_increase = ifelse(YOS < 3, salary_increase_yos,salary_increase_age))
  ######################

#Updated (Added SalaryGrowth YOS tab + rule to use YOS vs. Age)
#%>%
 # select(-salary_increase_yos, -salary_increase_age)
#View(SalaryData)

#View(SalaryGrowth) 
#View(SalaryEntry)
#View(SalaryData)

#Custom function to calculate cumulative future values
cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}



#################
#################
#################
#################


#View(SalaryData))
#Calculate FAS and cumulative EE contributions
#colnames(SalaryData)[7] <- "salary_increase"
SalaryData <- SalaryData %>% 
  
  group_by(entry_age) %>% 
  mutate(Salary = start_sal*cumprod(1+lag(salary_increase,default = 0)),
         #Salary = pmin(Salary_gross, salary_cap),
         # IRSSalaryCap = pmin(Salary,IRSCompLimit),
         FinalAvgSalary = rollmean(lag(Salary), k = FinAvgSalaryYears, fill = NA, align = "right"),
         EEContrib = EE_Contrib*Salary,
         DBEEBalance = cumFV(Interest, EEContrib),
         CumulativeWage = cumFV(ARR, Salary)) %>% 
  ungroup()


#Survival Probability and Annuity Factor
AnnFactorData <- MortalityTable %>% 
  select(Age, entry_age, mort) %>%
  group_by(entry_age) %>% 
  mutate(surv = cumprod(1 - lag(mort, default = 0)),
         surv_DR = surv/(1+ARR)^(Age - entry_age),
         surv_DR_COLA = surv_DR * (1+COLA)^(Age - entry_age),
         AnnuityFactor = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA) %>% 
  ungroup()

#View(data.frame(shift(AnnFactorData$surv_DR_COLA, n = 1:101, type = "lead")))

#View(AnnFactorData)
#Reduced Factor
#Unreduced retirement benefit (for those hired after 2018) when:
#Age 65 and 5 YOS
#30 YOS
#Age 62 with 20 YOS
#Age + YOS >= 80
#Reduced retirement benefit when:
#Age 60 and 10 YOS: reduced by 3% per year prior to age 62
ReducedFactor <- expand_grid(20:120,0:100)
colnames(ReducedFactor) <- c('RetirementAge','YOS')

### Updated* ###
#((2/3*12/100) Per Year between 60 and 65) 
ReducedFactor <- ReducedFactor %>% 
  mutate(RF = ifelse(RetirementAge >= NormalRetAgeI & YOS >= NormalYOSI |
                       RetirementAge >= NormalRetRuleAge & (YOS + RetirementAge >= NormalRetRule), 1,
                     ifelse(RetirementAge >= ReduceRetAge, pmin(1 - ((2/3*12/100)*(NormalRetAgeI - RetirementAge)),1), 0)))
#View(ReducedFactor)

# ReducedFactor_test <- ReducedFactor %>% pivot_wider(names_from = YOS, values_from = RF)

#Benefits, Annuity Factor and Present Value 
#system.time(
  
BenefitsTable <- expand_grid(Age, YOS, RetirementAge) %>% 
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(entry_age, Age, RetirementAge) %>% 
  left_join(SalaryData, by = c("Age", "YOS", "entry_age")) %>% 
  left_join(ReducedFactor, by = c("RetirementAge", "YOS")) %>% 
  left_join(AnnFactorData %>% select(Age, entry_age, surv_DR, AnnuityFactor), by = c("RetirementAge" = "Age", "entry_age")) %>%
  #Rename surv_DR and AF to make clear that these variables are at retirement
  rename(surv_DR_ret = surv_DR, AF_Ret = AnnuityFactor) %>% 
  #Rejoin the table to get the surv_DR for the termination age
  left_join(AnnFactorData %>% select(Age, entry_age, surv_DR), by = c("Age", "entry_age")) %>% 
  mutate(#GradedMult = BenMult1*YOS,
         ReducedFactMult = RF*BenMult1, #Removed Graded Multiplier*
         AnnFactorAdj = AF_Ret * surv_DR_ret / surv_DR,
         #MinBenefit = ifelse(YOS >= 10, MinRetBen, 0),      #Minimum retirement benefit of $3,600 per year for any member with at least 10 YOS
         PensionBenefit = ReducedFactMult * FinalAvgSalary*YOS,
         PresentValue = ifelse(Age > RetirementAge, 0, PensionBenefit*AnnFactorAdj))

#)

#View(BenefitsTable %>% filter(entry_age > 22))

#The max benefit is done outside the table because it will be merged with Salary data
OptimumBenefit <- BenefitsTable %>% 
  group_by(entry_age, Age) %>% 
  summarise(MaxBenefit = max(PresentValue)) %>%
  mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
  ungroup()

#View(SalaryData)

#Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
SalaryData <- SalaryData %>% 
  left_join(OptimumBenefit, by = c("Age", "entry_age")) %>% 
  left_join(SeparationRates, by = c("Age", "YOS")) %>%
  mutate(PenWealth = pmax(DBEEBalance,MaxBenefit),
         PVPenWealth = PenWealth/(1 + ARR)^YOS * SepProb,
         PVCumWage = CumulativeWage/(1 + ARR)^YOS * SepProb) 

#View(SalaryData)

#Calculate normal cost rate for each entry age
NormalCost <- SalaryData %>% 
  group_by(entry_age) %>% 
  summarise(normal_cost = sum(PVPenWealth, na.rm=T)/sum(PVCumWage, na.rm=T)) %>% 
  ungroup()

#View(NormalCost)

#View(NormalCost)

#Calculate the aggregate normal cost
NC_aggregate <- sum(NormalCost$normal_cost * SalaryEntry$start_sal * SalaryEntry$count_start, na.rm=T)/
  sum(SalaryEntry$start_sal * SalaryEntry$count_start, na.rm=T)


#Calculate the aggregate normal cost
NC_aggregate

########################
########################
library(parallel)
n.cores <- detectCores()
n.cores

x.list <- sapply(1:10000, list)

x.list
system.time({
  cluster <- makeCluster(n.cores)
  parLapply(cluster, x.list, function(x){sample(1:1000, 2000, replace = T)})}
)

system.time(
  
  lapply(x.list, function(x){sample(1:1000, 2000, replace = T)})
)
#########

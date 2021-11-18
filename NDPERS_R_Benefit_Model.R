####################################
# NDPERS Normal Cost/Benefit Model #
####################################

rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(zoo)
#setwd(getwd())


#FileName <- 'NDPERS_BM_Inputs.xlsx'
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
#View(TerminationRateBefore5)
#View(RetirementRates)

### Adding scaling factors
#scale.act.male <- 0.92 
#scale.ret.male <- 1.03
#scale.act.female <- 0.92 
#scale.ret.female <- 1.01 
#NormalRetRuleAge
#Function for determining retirement eligibility (including normal retirement, unreduced early retirement, and reduced early retirement)

################
# Main rule: Retirement Eligibility
################

IsRetirementEligible <- function(Age, YOS){
  Check = ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI) |
                 (Age >= NormalRetRuleAge) & (YOS + Age >= NormalRetRule) |
                 (Age >= NormalRetRuleAge & YOS >= NormalYOSI) |
                 (YOS + Age >= NormalRetRule), TRUE, FALSE)
  return(Check)
}

################
# New rule: 3 Retirement Types
################

RetirementType <- function(Age, YOS){
  
  Check = ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI), "Normal No Rule of 90",
                ifelse((Age >= NormalRetRuleAge) & (YOS + Age >= NormalRetRule), "Normal With Rule of 90",
                ifelse((Age >= NormalRetRuleAge & YOS >= NormalYOSI) |
                         (YOS + Age >= NormalRetRule), "Reduced","No")))
  
 return(Check)
}

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

SurvivalRates <- SurvivalRates %>% mutate_all(as.numeric)   #why do we need this step?


##### Mortality Function #####
### Automate into a package ###

mortality <- function(data = MortalityTable,
                      SurvivalRates = SurvivalRates,
                      MaleMP = MaleMP,
                      FemaleMP = FemaleMP,
                      MaleMP_ultimate = MaleMP_ultimate,
                      FemaleMP_ultimate = FemaleMP_ultimate
                      ){
  
  MortalityTable <- data %>% 
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
    filter(Years >= 2021, entry_age >= 20) %>% 
    ungroup()
  
  MortalityTable
  
}

##### Mortality Function #####
MortalityTable <- mortality(data = MortalityTable,
                      SurvivalRates = SurvivalRates,
                      MaleMP = MaleMP,
                      FemaleMP = FemaleMP,
                      MaleMP_ultimate = MaleMP_ultimate,
                      FemaleMP_ultimate = FemaleMP_ultimate)

#Join base mortality table with mortality improvement table and calculate the final mortality rates
# MortalityTable <- MortalityTable %>% 
#   left_join(SurvivalRates, by = "Age") %>% 
#   left_join(MaleMP, by = c("Age", "Years")) %>% 
#   left_join(FemaleMP, by = c("Age", "Years")) %>% 
#   left_join(MaleMP_ultimate, by = "Age") %>% 
#   left_join(FemaleMP_ultimate, by = "Age") %>% 
#   mutate(MaleMP_final = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male),
#          FemaleMP_final = ifelse(Years > max(FemaleMP$Years),  MP_ultimate_female, MP_female),
#          entry_age = Age - (Years - YearStart),
#          YOS = Age - entry_age) %>% 
#   group_by(Age) %>%
#   
#   #MPcumprod is the cumulative product of (1 - MP rates), starting from 2011. We use it later so make life easy and calculate now
#   mutate(MPcumprod_male = cumprod(1 - MaleMP_final),
#          #Started mort. table from 2011 (instead of 2010) 
#          #to cumsum over 2011+ & then multiply by 2010 MP-2019
#          #removed /(1 - MaleMP_final[Years == 2010])
#          MPcumprod_female = cumprod(1 - FemaleMP_final),
#          mort_male = ifelse(IsRetirementEligible(Age, YOS)==F, PubG_2010_employee_male * ScaleMultipleMaleAct, #Adding adj. facctors
#                             PubG_2010_healthy_retiree_male * ScaleMultipleMaleRet) * MPcumprod_male,
#          mort_female = ifelse(IsRetirementEligible(Age, YOS)==F, PubG_2010_employee_female * ScaleMultipleFemaleAct,
#                               PubG_2010_healthy_retiree_female * ScaleMultipleFemaleRet) * MPcumprod_female,
#          mort = (mort_male + mort_female)/2) %>% 
#          #Recalcualting average
#   filter(Years >= 2021, entry_age >= 20) %>% 
#   ungroup()

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
  left_join(TerminationRateBefore5, by = c("YOS","Age")) %>% # Joining by YOS & AGE
  left_join(RetirementRates, by = c("Age")) %>%
  ### Additions ###
  mutate_all(as.numeric) %>% 
  replace(is.na(.), 0) %>%
  mutate(TermBefore5 = TermBefore5Under30 + TermBefore5B3039+ TermBefore5Over39)#Combine 3 Term rates into 1 final column

#View(SeparationRates)

######################

#View(SeparationRates %>% select(RetirementType(SeparationRates$Age,SeparationRates$YOS)[1]))

#If you're retirement eligible, use the retirement rates, then checks YOS < 5 and use the regular termination rates
SeparationRates <- SeparationRates %>% 
  mutate(retirement_type = RetirementType(Age,YOS),
         
         SepRateMale = ifelse(retirement_type == "Normal With Rule of 90", UnreducedRule90,
                              ifelse(retirement_type == "Normal No Rule of 90", UnreducedNoRule90,
                                     ifelse(retirement_type == "Reduced", Reduced,     #Using 3 ifelse statements for 3 retirement conditions
                                            ifelse(YOS < 5, TermBefore5, TermAfter5)))),
         SepRateFemale = ifelse(retirement_type == "Normal With Rule of 90", UnreducedRule90,
                                ifelse(retirement_type == "Normal No Rule of 90", UnreducedNoRule90,
                                       ifelse(retirement_type == "Reduced", Reduced,     #Using 3 ifelse statements for 3 retirement conditions
                                              ifelse(YOS < 5, TermBefore5, TermAfter5)))),
         SepRate = ((SepRateMale+SepRateFemale)/2)) %>% 
  group_by(entry_age) %>% 
  mutate(RemainingProb = cumprod(1 - lag(SepRate, default = 0)),
         SepProb = lag(RemainingProb, default = 1) - RemainingProb) %>% 
  ungroup()

#Filter out unecessary values
SeparationRates <- SeparationRates %>% select(Age, YOS, RemainingProb, SepProb)

#Custom function to calculate cumulative future values

### Automate into a package ###

cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}

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
  mutate(salary_increase = ifelse(YOS < 3, salary_increase_yos,salary_increase_age))


#######################################
#################
#################
#################

#Calculate FAS and cumulative EE contributions
#colnames(SalaryData)[7] <- "salary_increase"
SalaryData <- SalaryData %>% 
  
  group_by(entry_age) %>% 
  mutate(Salary = start_sal*cumprod(1+lag(salary_increase,default = 0)),
         #Salary = pmin(Salary_gross, salary_cap),
         # IRSSalaryCap = pmin(Salary,IRSCompLimit),
         FinalAvgSalary = rollmean(lag(Salary), k = FinAvgSalaryYears, fill = NA, align = "right"),
         EEContrib = DB_EE_cont*Salary,
         DBEEBalance = cumFV(Interest, EEContrib),
         CumulativeWage = cumFV(ARR, Salary)) %>% 
  ungroup()


#Survival Probability and Annuity Factor
#View(MortalityTable)
AnnuityF <- function(data = MortalityTable,
                     ColaType = "Simple"){
  
  AnnFactorData <- MortalityTable %>% 
    select(Age, entry_age, mort) %>%
    group_by(entry_age) %>% 
    mutate(surv = cumprod(1 - lag(mort, default = 0)),
           surv_DR = surv/(1+ARR)^(Age - entry_age),
           surv_DR_COLA = surv_DR * ifelse(ColaType == "Simple", 1+(COLA * (Age - entry_age)), (1+COLA)^(Age - entry_age)),
           AnnuityFactor = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA) %>% 
    ungroup()
  
  AnnFactorData
  
}

AnnFactorData <- AnnuityF(data = MortalityTable,
                     ColaType = "Compound")

# AnnFactorData <- MortalityTable %>% 
#   select(Age, entry_age, mort) %>%
#   group_by(entry_age) %>% 
#   mutate(surv = cumprod(1 - lag(mort, default = 0)),
#          surv_DR = surv/(1+ARR)^(Age - entry_age),
#          surv_DR_COLA = surv_DR * (1+COLA)^(Age - entry_age),
#          AnnuityFactor = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA) %>% 
#   ungroup()

#View(data.frame(shift(AnnFactorData$surv_DR_COLA, n = 1:101, type = "lead")))

### Additions -> Calculating:
### 1. Earliest Age of Normal Retirement
### 2. Calculating Years between Early Retirement & Normal Retirement
### 3. Retirement Type
### 4. A benefit that begins before age 65 (or Rule of 90, if earlier) is reduced by 2/3 of one percent for each month before the earlier of age 65 or the age at which the Rule of 90 is met.

#Add entry_age to AnnFactorData + keep toNormRetYears
########
ReducedFactor <- expand_grid(Age, YOS) %>% 
  arrange(YOS) %>% 
  mutate(norm_retire = ifelse(RetirementType(Age, YOS) %in% c("Normal No Rule of 90", "Normal With Rule of 90"), 1, 0)) %>% 
  group_by(YOS) %>% 
  mutate(AgeNormRet = 120 - sum(norm_retire) + 1,     #This is the earliest age of normal retirement given the YOS
         YearsNormRet = AgeNormRet - Age,
         RetType = RetirementType(Age, YOS),
         RF = ifelse(RetType == "Reduced", 1 - (2/3*12/100)*YearsNormRet,
                     ifelse(RetType == "No", 0, 1))) %>% 
  rename(RetirementAge = Age) %>% 
  ungroup() 

# 
# ReducedFactor <- expand_grid(Age, YOS)
#   
# ReducedFactor$FirstRetAge <- ifelse(IsRetirementEligible(ReducedFactor$Age,ReducedFactor$YOS) == T & 
#                                       RetirementType(ReducedFactor$Age,ReducedFactor$YOS) == "Reduced",
#                                         ReducedFactor$Age,0)
# 
# ReducedFactor$RetType <- RetirementType(ReducedFactor$Age,ReducedFactor$YOS)
# 
# 
# ReducedFactor$YearsNormRet <- ifelse(ReducedFactor$RetType == "Reduced" & ReducedFactor$Age < 60,
#                                 (60- ReducedFactor$Age),
#                         ifelse(ReducedFactor$RetType == "Reduced" & ReducedFactor$Age >= 60,
#                                90-(ReducedFactor$Age+ReducedFactor$YOS),0))

#### Saving results into ReducedFactor 

### Updated* ###
#((2/3*12/100) Per Years untill Normal Retirement (if qualifies for Reduced)) 

##Adjusting code to use calculated YearsToNormRet column for early ret. penalties
# ReducedFactor <- ReducedFactor %>% 
#   mutate(RF = ifelse(RetType  == "Reduced",
#                             (1 - ((2/3*12/100)*(YearsNormRet))),
#                      ifelse(RetType  == "No",0,1))) %>%
#   mutate(RF = ifelse(RF <0, 0, RF)) %>%
#   rename(RetirementAge = Age)

#View(ReducedFactor)

# ReducedFactor_test <- ReducedFactor %>% pivot_wider(names_from = YOS, values_from = RF)

#Benefits, Annuity Factor and Present Value 
#system.time(
  
BenefitsTable <- expand_grid(Age, YOS, RetirementAge) %>% 
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(entry_age, Age, RetirementAge) %>% 
  left_join(SalaryData, by = c("Age", "YOS", "entry_age")) %>% 
  left_join(ReducedFactor %>% select(RetirementAge, YOS, RF), by = c("RetirementAge", "YOS")) %>%
  left_join(AnnFactorData %>% select(Age, entry_age, surv_DR, AnnuityFactor), by = c("RetirementAge" = "Age", "entry_age")) %>%
  #Rename surv_DR and AF to make clear that these variables are at retirement
  rename(surv_DR_ret = surv_DR, AF_Ret = AnnuityFactor) %>% 
  #Rejoin the table to get the surv_DR for the termination age
  left_join(AnnFactorData %>% select(Age, entry_age, surv_DR), by = c("Age", "entry_age")) %>% 
  mutate(ReducedFactMult = RF*BenMult, 
         AnnFactorAdj = AF_Ret * surv_DR_ret / surv_DR,
         PensionBenefit = ReducedFactMult * FinalAvgSalary*YOS,
         PresentValue = ifelse(Age > RetirementAge, 0, PensionBenefit*AnnFactorAdj))

#)

#For a given combination of entry age and termination age, the member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
OptimumBenefit <- BenefitsTable %>% 
  group_by(entry_age, Age) %>% 
  summarise(MaxBenefit = max(PresentValue)) %>%
  mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
  ungroup()

####### Benefit Accrual & Normal Cost #######
#### Real Pension Wealth = Pension Wealth adjusted for inflation
#### Actuarial PV of Pension Wealth = Pension Wealth 
#Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
#####################################
SalaryData <- SalaryData %>% 
  left_join(OptimumBenefit, by = c("Age", "entry_age")) %>% 
  left_join(SeparationRates, by = c("Age", "YOS")) %>%
  mutate(PenWealth = pmax(DBEEBalance,MaxBenefit),        #Members are assumed to elect the option with the greatest PV between a refund with interest and a deferred benefit
         RealPenWealth = PenWealth/(1 + assum_infl)^YOS,
         PVPenWealth = PenWealth/(1 + ARR)^YOS * SepProb,
         PVCumWage = CumulativeWage/(1 + ARR)^YOS * SepProb)


#Calculate normal cost rate for each entry age
NormalCost <- SalaryData %>% 
  group_by(entry_age) %>% 
  summarise(normal_cost = sum(PVPenWealth)/sum(PVCumWage)) %>% 
  ungroup()

#View(NormalCost)

#Calculate the aggregate normal cost
NC_aggregate <- sum(NormalCost$normal_cost * SalaryEntry$start_sal * SalaryEntry$count_start)/
  sum(SalaryEntry$start_sal * SalaryEntry$count_start)

#Calculate the aggregate normal cost
NC_aggregate
################################

####### DC Account Balance 
SalaryData2 <- SalaryData %>% 
  filter(entry_age == HiringAge) %>% 
  select(Age, YOS, entry_age, start_sal, salary_increase, Salary, RemainingProb) %>% 
  mutate(DC_EEContrib = Salary * DC_EE_cont,
         DC_ERContrib = Salary * DC_ER_cont,
         DC_Contrib = DC_EEContrib + DC_ERContrib,
         DC_balance = cumFV(DC_return, DC_Contrib),
         RealDC_balance = DC_balance/(1 + assum_infl)^YOS) %>% 
  left_join(SalaryData %>% select(Age, YOS, RealPenWealth), by = c("Age", "YOS")) %>% 
  mutate(RealHybridWealth = RealDC_balance + RealPenWealth)


#View(SalaryData)
## Graphing PWealth accrual [ALL ENTRY AGES]

ggplot(SalaryData, aes(Age,RealPenWealth/1000, group = entry_age, col = as.factor(entry_age)))+
  geom_line(size = 1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x), 
                     name = "Age (Entry age at 27)", expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(0, 5000, by = 100),labels = function(x) paste0("$",x), 
                     name = "Present Value of Pension Wealth ($Thousands)", expand = c(0,0)) 
##################################


######### Graphing SINGLE ENTRY AGE + RETENTION
palette_reason <- list(Orange="#FF6633",
                       LightOrange="#FF9900",
                       DarkGrey="#333333", 
                       LightGrey= "#CCCCCC", 
                       SpaceGrey ="#A69FA1",
                       DarkBlue="#0066CC", 
                       GreyBlue= "#6699CC", 
                       Yellow= "#FFCC33",
                       LightBlue = "#66B2FF", 
                       SatBlue = "#3366CC", 
                       Green = "#669900",LightGreen = "#00CC66", Red = "#CC0000",LightRed="#FF0000")


colnames(SalaryData2)[13] <- "PVPenWealth"
e.age <- unique(SalaryData2$entry_age)
SalaryData2 <- data.frame(SalaryData2)
SalaryData2$entry_age <- as.numeric(SalaryData2$entry_age)
# #View(SalaryData2)
# 
SalaryData2 <- SalaryData2 %>% filter(entry_age == 22)
SalaryData2 <- SalaryData2 %>% filter(Age < 81)
SalaryData2$PVPenWealth <- as.numeric(SalaryData2$PVPenWealth)
y_max <- max(SalaryData2$PVPenWealth)


####
pwealth <- ggplot(SalaryData2, aes(Age,PVPenWealth/1000))+
  geom_line(aes(group = 1,
                text = paste0("Age: ", Age,
                              "<br>Pension Wealth: $",round(PVPenWealth/1000,1), " Thousands")),size = 1.25, color = palette_reason$SatBlue)+
  geom_line(aes(Age, RealDC_balance/1000,
                group = 2,
                text = paste0("Age: ", Age,
                              "<br>DC Wealth: $", round(RealDC_balance/1000,1), " Thousands")), size = 1.25, color = palette_reason$Orange)+
  geom_line(aes(Age, RemainingProb* (y_max/1000),
                group = 3,
                text = paste0("Age: ", Age,
                              "<br>Members Remaining: ", round(RemainingProb*100,1), "%")), size = 1.25, color = palette_reason$LightBlue, linetype = "dashed")+
  scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x),
                     name = paste0("Age (Entry age at 22 )"), expand = c(0,0)) +
  
  scale_y_continuous(breaks = seq(0, 5000, by = 100),limits = c(0, y_max/1000*1.1), labels = function(x) paste0("$",x),
                     sec.axis = sec_axis(~./(y_max/100), breaks = scales::pretty_breaks(n = 10), name = "Percent of Members Remaining",
                                         labels = function(b) paste0(round(b, 0), "%")), 
                     name = "Present Value of Pension Wealth ($Thousands)", expand = c(0,0)) +
  theme_bw()+
  theme(   #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    plot.margin = margin(0.5, 0.5,0.5,0.5, "cm"),
    axis.text.y = element_text(size=11, color = "black"),
    axis.text.y.right = element_text(size=11, color = "black"),
    axis.text.y.left = element_text(size=11, color = "black"),
    axis.text.x = element_text(size=11, color = "black"),
    legend.title = element_text(size = 9, colour = "black", face = "bold"))

library(plotly)
ggplotly(pwealth, tooltip = c("text"))

#######################

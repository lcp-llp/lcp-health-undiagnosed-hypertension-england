
# Set up ####---------------------------------------------

## Libraries

#install.packages("pacman")
pacman::p_load(tidyverse, haven, data.table, srvyr, odbc, DBI, scales,openxlsx)

## Data sources and functions
## Imports Health Survey for England yearly individual level files provided by the UK data archive in SPSS format
## Imports ONS mid-year population estimates for England regions by 5 year age-group held on DHSC databases
source("datalocation_hse.R")

# Standard deviation of proportion
sdprop <- function(x) { qnorm(1-(1-CI)/2)*x }

# Standard deviation from standard error
sd_se <- function(se, samp) {se*sqrt(samp)}

#---
# HSE import and find Variables of interest ####-----------------------------------
#---

# Constant for rescaling the weights
cons <- 10000

# Combined HSE years are re-weighted using agreed method from HSE

## 2017, 2018 & 2019 HSE
vars1 <- c("wt_nurse","omsysval","omdiaval","hbp140om2","hy140om2", "everbp", "bp1", "bprespc",
           "sex","age16g5","ag16g10","gor1","bpmedd2",
           "height","weight","htval","wtval","sys1om","sys2om","sys3om","dias1om","dias2om","dias3om")

hse17 <- haven::read_sav(paste0(pth,"HSE17i EUL v1.sav"), user_na = F) %>% 
  rename_with(tolower) %>% mutate(everbp = bp1) %>% #no everbp in 2017 supplied dataset
  select(psu, cluster=cluster_nurse, all_of(vars1),qimd) %>% zap_labels %>% 
  mutate(psu1 = as.numeric(paste0("17", as.character(psu))),
         cluster1 = as.numeric(paste0("17", as.character(cluster))),
         z_wt_nurse = ifelse(is.na(wt_nurse), NA, cons/sum(wt_nurse, na.rm = T) * wt_nurse), year = "2017")

hse18 <- haven::read_sav(paste0(pth,"HSE 2018 SL 20220412.sav"), user_na = F) %>% 
  rename_with(tolower) %>% 
  select(psu=psu_scr,cluster=cluster95, all_of(vars1),qimd) %>% zap_labels %>% 
  mutate(psu1 = as.numeric(paste0("18", as.character(psu))),
         cluster1 = as.numeric(paste0("18", as.character(cluster))),
         z_wt_nurse = ifelse(is.na(wt_nurse), NA, cons/sum(wt_nurse, na.rm = T) * wt_nurse), year = "2018")

hse19 <- haven::read_sav(paste0(pth,"HSE 2019 SL 20220330.sav"), user_na = F) %>% 
  rename_with(tolower) %>% 
  select(psu=psu_scr, cluster=cluster94, all_of(vars1),qimd = qimd19) %>% zap_labels %>% 
  mutate(psu1 = as.numeric(paste0("19", as.character(psu))),
         cluster1 = as.numeric(paste0("19", as.character(cluster))),
         z_wt_nurse = ifelse(is.na(wt_nurse), NA, cons/sum(wt_nurse, na.rm = T) * wt_nurse), year = "2019")

raw_datasets <- list(hse17,hse18,hse19)

## Only include valid BP readings (need 3), excludes if respondent was pregnant or had eaten in last half hour
# 40+ population (age16g5 > 6)
# Exclude any readings >= 50 & omsysval < 251 & omdiaval >= 20

df1 <- bind_rows(raw_datasets) %>% 
  filter(bprespc == 1, age16g5 > 6, omsysval >= 50 & omsysval < 251 & omdiaval >= 20) %>% 
  group_by(year) %>% 
  mutate(z_wt_nurse = ifelse(is.na(wt_nurse), NA, cons/sum(wt_nurse, na.rm = T) * wt_nurse)) %>% 
  ungroup() %>% 
  mutate(wt1 = case_when(is.na(wt_nurse) ~ 0, TRUE ~ 1), cnt = sum(wt1),
         z_wt_nurse_a = cnt/(length(raw_datasets)*cons) * z_wt_nurse)

# mean(df1$z_wt_nurse_a, na.rm = T) # should equal 1
rm(hse17,hse18,hse19)

#---
# Create datasets ####
#---

## Label and create factors ####
data <- df1 %>% 
  mutate(ag16g10 = factor(ag16g10, levels = c(1,2,3,4,5,6,7),
                          labels = c("16-24","25-34", "35-44","45-54", "55-64","65-74", "75+")),
         ag16851 = case_match(age16g5, c(1,2,3) ~ 1, c(16,17) ~ 14, .default = age16g5-2),
         ag1685 = factor(ag16851, levels = 1:14,
                         labels = c("16-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                                    "70-74","75-79","80-84","85+")),
         Sex = case_match(sex, 1 ~ 1, 2 ~ 0, .default = sex),
         IMD = factor(qimd, levels = 1:5, 
                      labels = c("1: Least deprived","2","3","4","5: Most deprived")),
         Region = factor(gor1, levels = 1:9, 
                         labels = c("North East","North West","Yorkshire and The Humber",
                                    "East Midlands","West Midlands","East of England","London",
                                    "South East","South West")),
         AgeEst = case_when(age16g5 == 1 ~ 16, age16g5 == 2 ~ 18, TRUE ~ (age16g5+1)*5+2),  #Used just below mid point in the ageband
         hypertensive = case_match(bp1, 1 ~ 1, 2 ~ 0, .default = 0),
         antihypertensives = case_match(bpmedd2, 1 ~ 1, .default = 0),
         height = htval / 100,
         history_of_CVD = 0,
         times_since_diag = 0)

## ProofBP algorithm applied ####

data$predicted_systolic <- data$sys1om + 33.57419 +
  (data$AgeEst * 0.6269306) +
  (data$Sex * -3.598565) +
  (data$sys1om * -0.036267) +
  ((data$sys3om - data$sys1om) * 0.3617946) +
  ((data$wtval / (data$height * data$height)) * -0.2093273) +
  (data$times_since_diag * 0.175593) +
  (data$hypertensive * -5.069816) +
  (data$antihypertensives * 6.942526) +
  ((data$sys1om - data$dias1om) * -0.6181946) +
  ((data$AgeEst * data$sys1om) * -0.0077222) +
  ((data$AgeEst * (data$sys1om - data$dias1om)) * 0.009603) +
  ((data$Sex * (data$wtval / (data$height * data$height))) * 0.2976424) +
  ((data$Sex * data$times_since_diag) * -0.2587568) +
  ((data$Sex * data$antihypertensives) * -14.73537) +
  ((data$Sex * data$hypertensive) * 13.3899)

data$predicted_diastolic <- data$dias1om + 59.34239 +
  (data$AgeEst * -0.3340627) +
  (data$Sex * 3.328766) +
  (data$dias1om * -0.4658008) +
  ((data$dias3om - data$dias1om) * -0.4040305) +
  ((data$wtval / (data$height * data$height)) * -0.6620654) +
  (data$hypertensive * -0.0296759) +
  (data$antihypertensives * 10.45892) +
  (data$history_of_CVD * -11.07324) +
  ((data$sys1om - data$dias1om) * -0.060376) +
  ((data$AgeEst * (data$dias3om - data$dias1om)) * 0.0118679) +
  ((data$AgeEst * (data$wtval / (data$height * data$height))) * 0.0097736) +
  ((data$AgeEst * data$history_of_CVD) * 0.1766609) +
  ((data$AgeEst * data$antihypertensives) * -0.133922) +
  ((data$Sex * data$antihypertensives) * -7.997599) +
  ((data$Sex * data$hypertensive) * 4.628107)

## Predicted diastolic with everyone having a history of CVD
data$predicted_diastolicCVD <- data$dias1om + 59.34239 +
  (data$AgeEst * -0.3340627) +
  (data$Sex * 3.328766) +
  (data$dias1om * -0.4658008) +
  ((data$dias3om - data$dias1om) * -0.4040305) +
  ((data$wtval / (data$height * data$height)) * -0.6620654) +
  (data$hypertensive * -0.0296759) +
  (data$antihypertensives * 10.45892) +
  (1 * -11.07324) +  # CVD main effect
  ((data$sys1om - data$dias1om) * -0.060376) +
  ((data$AgeEst * (data$dias3om - data$dias1om)) * 0.0118679) +
  ((data$AgeEst * (data$wtval / (data$height * data$height))) * 0.0097736) +
  ((data$AgeEst * 1) * 0.1766609) +  # CVD interaction with age
  ((data$AgeEst * data$antihypertensives) * -0.133922) +
  ((data$Sex * data$antihypertensives) * -7.997599) +
  ((data$Sex * data$hypertensive) * 4.628107)

## Add hypertensive flags ####
data1 <- data %>% filter(!is.na(wtval), !is.na(htval)) %>% 
  mutate(
    #Hypertensive in all respondents 
    hypHm = case_when((omsysval >= 135 | omdiaval >= 85) ~ 1, TRUE ~ 0),
    hyp = case_when(omsysval >= 140 | omdiaval >= 90 ~ 1, TRUE ~ 0),
    hypHmpred = case_when((predicted_systolic >= 135 | predicted_diastolic >= 85)  ~ 1, TRUE ~ 0),
    hyppred = case_when(predicted_systolic >= 140 | predicted_diastolic >= 90 ~ 1, TRUE ~ 0),
    hyp_pluspred = case_when(hypHmpred == 1 & hyp == 1 ~ 1, TRUE ~ 0),
    
    #Hypertensive in those without diagnosed hypertension 
    hyp135 = case_when(hypHm == 1 & hypertensive == 0 ~ 1, TRUE ~ 0),
    hyp140 = case_when(hyp == 1 & hypertensive == 0 ~ 1, TRUE ~ 0),
    hyppredict135 = case_when(hypHmpred == 1 & hypertensive == 0 ~ 1, TRUE ~ 0),
    hyppredict140 = case_when(hyppred == 1 & hypertensive == 0 ~ 1, TRUE ~ 0),
    hyp140pred135 = case_when(hyp_pluspred == 1 & hypertensive == 0 ~ 1, TRUE ~ 0)
  )

#---
# Final dataset #####
#---

## Only include hypertensive patients
data2 <- data1 %>% filter(hypertensive == 0)
rm(df1,data)

#---
# Results ####----------------------------------------------------------
#---

## Have to set global options to run an "adjustment" where only one PSU per stratum
## Happens as the dataset is cut down from the original to include only people without undiagnosed hypertension
## see here: https://stackoverflow.com/questions/55975478/problems-due-to-having-too-many-single-psus-at-stage-one
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

## Adjusted 40+ dataset ####
## Using srvyr to include adjusted SEs
strat_design <- data2 %>%
  as_survey_design(id = psu1, strata = cluster1, weight = z_wt_nurse_a)

strat_design1 <- data1 %>%
  as_survey_design(id = psu1, strata = cluster1, weight = z_wt_nurse_a)

## Confidence limit ####
CI <- 0.95

## Denominators ####
denomsReg <- strat_design %>% group_by(Region) %>% 
  summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a)))

denomsAge <- strat_design %>% group_by(ag1685) %>% 
  summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a)))

denomsSex <- strat_design %>% group_by(Sex) %>% 
  summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a)))

denoms <- strat_design %>%
  summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a)))

denoms <- strat_design %>%
  summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a)))

denomsAge1 <- strat_design1 %>% group_by(Typ = ag1685) %>% 
  summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a))) %>% 
  bind_rows(strat_design1 %>% group_by(Typ = Sex) %>%
              summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a))) %>% 
              mutate(Typ = case_match(Typ, 0 ~ "Female", 1 ~ "Male"))) %>% 
  bind_rows(strat_design1 %>%
              summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a))) %>% 
              mutate(Typ = "Total"))

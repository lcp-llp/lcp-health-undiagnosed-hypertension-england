
##-----------------HSE import and find Variables of interest----------------------

source("R/hse_data_paper.R")

##----- Flow chart and missingness -------------

samp_raw <- bind_rows(raw_datasets) %>% summarise(cnt = n()) %>% mutate(Samp = "Raw HSE")
samp_age <- bind_rows(raw_datasets) %>% filter(age16g5 > 6) %>% summarise(cnt = n()) %>% mutate(Samp = "40+")
samp_validBP <- bind_rows(raw_datasets) %>% filter(bprespc == 1, age16g5 > 6) %>% 
  summarise(cnt = n()) %>% mutate(Samp = "Valid BP")
samp_40 <- data1 %>% summarise(cnt = n()) %>% mutate(Samp = "High/low BP")
samp_pred <- data1 %>% filter(!is.na(wtval), !is.na(htval)) %>% 
  summarise(cnt = n()) %>% mutate(Samp = "Missing height/weight")
samp_40_undiag <- data2 %>% summarise(cnt = n()) %>% mutate(Samp = "Undiagnosed hypertension")

samp_flow <- bind_rows(samp_raw, samp_age, samp_validBP, samp_pred, samp_40_undiag) %>% 
  mutate(Removed = cnt-lag(cnt, default = 0)) %>% 
  select(Name = Samp, Sample = cnt, Removed)

rm(samp_raw, samp_age, samp_validBP, samp_pred, samp_40_undiag, raw_datasets)

##------ Table 1 ---------------------

mnames1 <- c("omsysval","omdiaval","predicted_systolic","predicted_diastolic")

mnsAll <- strat_design %>%
  summarise(across(all_of(mnames1),
                   ~ survey_mean(.x, vartype = "se"))) %>% bind_cols(denoms) %>% mutate(Typ = "Total") %>% 
  relocate(any_of(c("Typ","N","N_weighted"))) %>% 
  bind_rows(strat_design %>% group_by(Typ = Sex) %>% 
              summarise(across(all_of(mnames1),
                               ~ survey_mean(.x, vartype = "se"))) %>% left_join(denomsSex, by = c("Typ"="Sex")) %>% 
              mutate(Typ = case_match(Typ, 0 ~ "Female", 1 ~ "Male")) )%>% 
  bind_rows(strat_design %>% group_by(Typ = ag1685) %>% 
              summarise(across(all_of(mnames1),
                               ~ survey_mean(.x, vartype = "se"))) %>% left_join(denomsAge, by = c("Typ"="ag1685"))) %>% 
  bind_rows(strat_design %>% group_by(Typ = Region) %>% 
              summarise(across(all_of(mnames1),
                               ~ survey_mean(.x, vartype = "se"))) %>% left_join(denomsReg, by = c("Typ"="Region"))) %>%
  mutate(across(ends_with("_se"), ~ sd_se(.x, N_weighted))) %>% 
  rename_with( ~str_replace(.x,"_se","_sd") ,ends_with("_se"))

propsAll_obs <- strat_design %>% group_by(hyp = hypHm) %>%
  summarise(Observed135 = survey_prop(vartype = "ci", level = CI)) %>% mutate(Typ = "Total") %>% 
  bind_rows(strat_design %>% group_by(Typ=Sex, hyp = hypHm) %>%
              summarise(Observed135 = survey_prop(vartype = "ci", level = CI)) %>% 
              mutate(Typ = case_match(Typ, 0 ~ "Female", 1 ~ "Male")) ) %>% 
  bind_rows(strat_design %>% group_by(Typ=ag1685, hyp = hypHm) %>%
              summarise(Observed135 = survey_prop(vartype = "ci", level = CI)) ) %>% 
  bind_rows(strat_design %>% group_by(Typ=Region, hyp = hypHm) %>%
              summarise(Observed135 = survey_prop(vartype = "ci", level = CI)) )

propsAll_Pred <- strat_design %>% group_by(hyp = hypHmpred) %>%
  summarise(Predicted135 = survey_prop(vartype = "ci", level = CI)) %>% mutate(Typ = "Total") %>% 
  bind_rows(strat_design %>% group_by(Typ=Sex, hyp = hypHmpred) %>%
              summarise(Predicted135 = survey_prop(vartype = "ci", level = CI)) %>% 
              mutate(Typ = case_match(Typ, 0 ~ "Female", 1 ~ "Male")) ) %>% 
  bind_rows(strat_design %>% group_by(Typ=ag1685, hyp = hypHmpred) %>%
              summarise(Predicted135 = survey_prop(vartype = "ci", level = CI)) ) %>% 
  bind_rows(strat_design %>% group_by(Typ=Region, hyp = hypHmpred) %>%
              summarise(Predicted135 = survey_prop(vartype = "ci", level = CI)) ) 

propsAll <- propsAll_obs %>% left_join(propsAll_Pred) %>% filter(hyp == 1) %>% 
  relocate(Typ) %>% select(-hyp) %>% 
  mutate(across(.cols = -Typ, ~ round(.x*100, 2)))

## Table 1
tab1 <- mnsAll %>% left_join(propsAll)
rm(propsAll_obs, propsAll_Pred, propsAll, mnsAll)


##------ Table 2 ---------------------

## Population denominators in 2021
# Uses internal DHSC data for regional population estimates for the year 2021
# Derived directly from ONS mid-year population estimates for the year 2021
Pops <- Populations_DHSC %>% 
  mutate(ag1685 = factor(ag16851, levels = 1:14,
                         labels = c("16-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                                    "70-74","75-79","80-84","85+")) ) %>% select(-ag16851)

Pop <- Pops %>% filter(ag1685 %notin% c("16-24","25-29","30-34","35-39"))
EngTot <- Pop %>% summarise(Pop = sum(Pop)) %>% mutate(Typ = "Total") %>% relocate(Typ)
Engsex <- Pop %>% group_by(Typ = Sex) %>% summarise(Pop = sum(Pop))
Engage <- Pop %>% group_by(Typ = ag1685) %>% summarise(Pop = sum(Pop))
Engreg <- Pop %>% group_by(Typ = Region) %>% summarise(Pop = sum(Pop))
Poptab <- bind_rows(EngTot, Engsex, Engage, Engreg)
Poptab1 <- bind_rows(EngTot, Engsex, Engage)
rm(EngTot,Engsex,Engage,Engreg)

## Table 2
tab2 <- strat_design1 %>% group_by(hyp = hyp140) %>%
  summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI) ) %>%
  mutate(Type = "Observed_140/90", Typ = "Total") %>% 
  bind_rows(strat_design1 %>% group_by(hyp = hyp140pred135) %>%
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI) ) %>% 
              mutate(Type = "Observed_140/90 & predicted 135/85", Typ = "Total")) %>%
  bind_rows(strat_design1 %>% group_by(hyp = hyppredict135) %>%
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI) ) %>% 
              mutate(Type = "Predicted_135/85", Typ = "Total")) %>%
  bind_rows(strat_design1 %>% group_by(Typ = Sex, hyp = hyp140) %>%
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI)) %>% 
              mutate(Typ = case_match(Typ, 0 ~ "Female", 1 ~ "Male"),
                     Type = "Observed_140/90") ) %>% 
  bind_rows(strat_design1 %>% group_by(Typ = Sex, hyp = hyp140pred135) %>%
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI)) %>% 
              mutate(Typ = case_match(Typ, 0 ~ "Female", 1 ~ "Male"),
                     Type = "Observed_140/90 & predicted 135/85") ) %>% 
  bind_rows(strat_design1 %>% group_by(Typ = Sex, hyp = hyppredict135) %>%
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI)) %>% 
              mutate(Typ = case_match(Typ, 0 ~ "Female", 1 ~ "Male"),
                     Type = "Predicted_135/85") ) %>%
  bind_rows(strat_design1 %>% group_by(Typ = ag1685, hyp = hyp140) %>%
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI)) %>% 
              mutate(Type = "Observed_140/90") ) %>% 
  bind_rows(strat_design1 %>% group_by(Typ = ag1685, hyp = hyp140pred135) %>%
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI)) %>% 
              mutate(Type = "Observed_140/90 & predicted 135/85") ) %>%
  bind_rows(strat_design1 %>% group_by(Typ = ag1685, hyp = hyppredict135) %>%
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI)) %>% 
              mutate(Type = "Predicted_135/85") ) %>% 
  left_join(Poptab1) %>% filter(hyp == 1) %>% left_join(denomsAge1) %>% 
  mutate(Pop2021 = round(Pop*prop),
         Per = paste0(as.character(round(prop*100,1)), " (",
                      as.character(round(prop_low*100,1)),"-",as.character(round(prop_upp*100,1)),")")) %>% 
  select(Type, Age = Typ, N, prop, prop_low, prop_upp, `Prevalence %` = Per, Pop2021)


##------ Table 3 ---------------------

propsAll_obs <- strat_design %>% group_by(hyp = hyp) %>%
  summarise(Observed140 = survey_prop(vartype = "ci", level = CI)) %>% mutate(Typ = "Total") %>% 
  bind_rows(strat_design %>% group_by(Typ=Sex, hyp = hyp) %>%
              summarise(Observed140 = survey_prop(vartype = "ci", level = CI)) %>% 
              mutate(Typ = case_match(Typ, 0 ~ "Female", 1 ~ "Male")) ) %>% 
  bind_rows(strat_design %>% group_by( Typ=ag1685, hyp = hyp) %>%
              summarise(Observed140 = survey_prop(vartype = "ci", level = CI)) ) %>% 
  bind_rows(strat_design %>% group_by(Typ=Region, hyp = hyp) %>%
              summarise(Observed140 = survey_prop(vartype = "ci", level = CI)) )

propsAll_Pred <- strat_design %>% group_by(hyp = hyp_pluspred) %>%
  summarise(Observed140_135pred = survey_prop(vartype = "ci", level = CI)) %>% mutate(Typ = "Total") %>% 
  bind_rows(strat_design %>% group_by(Typ=Sex, hyp = hyp_pluspred) %>%
              summarise(Observed140_135pred = survey_prop(vartype = "ci", level = CI)) %>% 
              mutate(Typ = case_match(Typ, 0 ~ "Female", 1 ~ "Male")) ) %>% 
  bind_rows(strat_design %>% group_by(Typ=ag1685, hyp = hyp_pluspred) %>%
              summarise(Observed140_135pred = survey_prop(vartype = "ci", level = CI)) ) %>% 
  bind_rows(strat_design %>% group_by(Typ=Region, hyp = hyp_pluspred) %>%
              summarise(Observed140_135pred = survey_prop(vartype = "ci", level = CI)) ) 

propsAll <- propsAll_obs %>% left_join(propsAll_Pred) %>% filter(hyp == 1) %>% 
  relocate(Typ) %>% select(-hyp) %>% 
  mutate(across(.cols = -Typ, ~ round(.x*100, 2)))

## Table 3
tab3 <- propsAll
rm(propsAll_obs, propsAll_Pred, propsAll)


##------ Supplement ---------------------

## Mean values for undiagnosed hypertension respondents
mnames <- c("omsysval","omdiaval","predicted_systolic","predicted_diastolic","predicted_diastolicCVD")

S1_tab <- strat_design %>%
  summarise(across(all_of(mnames),
                   ~ survey_mean(.x, vartype = "se"))) %>% bind_cols(denoms) %>% mutate(Typ = "Persons 40+") %>% 
  relocate(any_of(c("Typ","N","N_weighted"))) %>% 
  bind_rows(strat_design %>% group_by(Typ = Sex) %>% 
              summarise(across(all_of(mnames),
                               ~ survey_mean(.x, vartype = "se"))) %>% left_join(denomsSex, by = c("Typ"="Sex")) %>% 
              mutate(Typ = case_match(Typ, 0 ~ "Female", 1 ~ "Male")) )%>% 
  bind_rows(strat_design %>% group_by(Typ = ag1685) %>% 
              summarise(across(all_of(mnames),
                               ~ survey_mean(.x, vartype = "se"))) %>% left_join(denomsAge, by = c("Typ"="ag1685"))) %>% 
  bind_rows(strat_design %>% group_by(Typ = Region) %>% 
              summarise(across(all_of(mnames),
                               ~ survey_mean(.x, vartype = "se"))) %>% left_join(denomsReg, by = c("Typ"="Region"))) %>%
  mutate(across(ends_with("_se"), ~ sd_se(.x, N_weighted))) %>% 
  rename_with( ~str_replace(.x,"_se","_sd") ,ends_with("_se"))

## IMD analysis
# Age groups
ageGrp <- data.frame(
  ag1685 = factor(c("40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")), 
  agegrp = factor(c("40-49","40-49","50-59","50-59","60-69","60-69","70+","70+","70+","70+")) )

## IMD by year
strat_1 <- data2 %>% left_join(ageGrp) %>% #filter(Year == 2017) %>% 
  as_survey_design(id = psu, strata = cluster, weight = wt_nurse)

denomsIMD <- strat_1 %>% group_by(agegrp, IMD, Year = year) %>%
  summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a))) %>% 
  bind_rows(strat_1 %>% group_by(IMD, Year = year) %>%
              summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a))) %>% 
              mutate(agegrp = "Total") )

S1_fig <- strat_1 %>% group_by(agegrp, IMD, Year = year, hyp = hypHmpred) %>%
  summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI) ) %>% 
  bind_rows(strat_1 %>% group_by(IMD, Year = year, hyp = hypHmpred) %>% 
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI) ) %>% 
              mutate(agegrp = "Total")) %>% ungroup() %>% 
  filter(hyp == 1) %>% select(-hyp, -prop_se) %>% 
  mutate(Per = paste0(as.character(round(prop*100,1)), " (",
                      as.character(round(prop_low*100,1)),"-",as.character(round(prop_upp*100,1)),")")) %>% 
  left_join(denomsIMD)

## IMD combined year by age
strat_2 <- data2 %>% left_join(ageGrp) %>% 
  as_survey_design(id = psu1, strata = cluster1, weight = z_wt_nurse_a)

denomsIMD1 <- strat_2 %>% group_by(agegrp, IMD) %>%
  summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a))) %>% 
  bind_rows(strat_1 %>% group_by(IMD) %>%
              summarise(N = n(), N_weighted = round(sum(z_wt_nurse_a))) %>% 
              mutate(agegrp = "Total") )

S2_fig <- strat_2 %>% group_by(agegrp, IMD, hyp = hypHmpred) %>% 
  summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI) ) %>% 
  bind_rows(strat_2 %>% group_by(IMD, hyp = hypHmpred) %>%
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = CI) ) %>% 
              mutate(agegrp = "Total")) %>% ungroup() %>% 
  filter(hyp == 1) %>% select(-hyp, -prop_se) %>% 
  mutate(Per = paste0(as.character(round(prop*100,1)), " (",
                      as.character(round(prop_low*100,1)),"-",as.character(round(prop_upp*100,1)),")")) %>% 
  left_join(denomsIMD1)


S5_fig <- strat_design %>% group_by(ag1685) %>%
  summarise(across(all_of(mnames),
                   ~ survey_mean(.x, vartype = "se"))) %>% left_join(denomsAge, by = "ag1685") %>% 
  transmute(Type = ag1685, N, N_weighted,
            Observed_systolic = paste0(round(omsysval,1)," (",round(omsysval_se,2),")"),
            Predicted_systolic = paste0(round(predicted_systolic,1)," (",round(predicted_systolic_se,2),")"),
            Observed_diastolic = paste0(round(omdiaval,1)," (",round(omdiaval_se,2),")"),
            Predicted_diastolic = paste0(round(predicted_diastolic,1)," (",round(predicted_diastolic_se,2),")"),
            Predicted_diastoliccvd = paste0(round(predicted_diastolicCVD,1)," (",round(predicted_diastolicCVD_se,2),")") )


##--------- Excel workbook of tables (un-formatted) --------------------
## Add tables to a workbook
wb <- createWorkbook()
addWorksheet(wb, sheet="Flow")
addWorksheet(wb, sheet="Table_1_data")
addWorksheet(wb, sheet="Table_2_data")
addWorksheet(wb, sheet="Table_3_data")
addWorksheet(wb, sheet="Table_S1_data")
addWorksheet(wb, sheet="Fig_S1_data")
addWorksheet(wb, sheet="Fig_S2_data")
addWorksheet(wb, sheet="Fig_S5_data")
writeData(wb,sheet="Flow", samp_flow)
writeData(wb,sheet="Table_1_data", tab1)
writeData(wb,sheet="Table_2_data", tab2)
writeData(wb,sheet="Table_3_data", tab3)
writeData(wb,sheet="Table_S1_data", S1_tab)
writeData(wb,sheet="Fig_S1_data", S1_fig)
writeData(wb,sheet="Fig_S2_data", S2_fig)
writeData(wb,sheet="Fig_S5_data", S5_fig)
bp_filepath <- paste0(pth1, "/hse_ProofBP_tables_final.xlsx")
saveWorkbook(wb, bp_filepath, overwrite = TRUE)

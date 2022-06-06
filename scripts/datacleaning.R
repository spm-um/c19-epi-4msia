#load packages
rm(list=ls())
required_packages <- c("tidyverse", "EpiEstim", "readxl", "readr", "zoo", "lubridate")
not_installed <- required_packages[!(required_packages %in% installed.packages()[ , "Package"])]    
if(length(not_installed)) install.packages(not_installed)                                           
suppressWarnings(lapply(required_packages, require, character.only = TRUE))

# National cluster data
#call the data
national <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_malaysia.csv")

#extract cluster data
cluster <- national %>% select("date", "cases_new", "cases_import", 
                               "cluster_import", "cluster_religious", "cluster_community", "cluster_highRisk", 
                               "cluster_education", "cluster_detentionCentre", "cluster_workplace")

#write to the data
write.csv(cluster, "data/cluster.csv")


# National case and death data
#call the data
cases_mas <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_malaysia.csv")
deaths_mas <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/deaths_malaysia.csv")
cases_state <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_state.csv")
deaths_state <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/deaths_state.csv")

#extract natioal data
cases_mas <- cases_mas %>% mutate(cases_truncate=cases_new-cases_import-cluster_detentionCentre,
                                  state="Malaysia",
                                  ma_7day_cases=rollmean(cases_new, k = 7, fill = "extend"),
                                  ma_7day_cases_truncate=rollmean(cases_truncate, k = 7, fill = "extend"),
                                  ma_7day_cases_truncate=ifelse(is.na(ma_7day_cases_truncate),ma_7day_cases, ma_7day_cases_truncate),
                                  rollcases_7d=rollsum(cases_new, k=7,fill=NA, align="right"),
                                  time=seq(1, nrow(cases_mas),1)) %>%
  select("date","time", "state", "cases_new", "cases_import", "cases_recovered", "cases_active", "cases_cluster", "cases_pvax", "cases_fvax", "cases_child", 
         "cases_adolescent", "cases_adult", "cases_elderly", "cases_truncate", "ma_7day_cases", "ma_7day_cases_truncate", "rollcases_7d")

#extract state case data
cases_state <- cases_state %>% mutate(cases_truncate=cases_new-cases_import) %>%
  group_by(state) %>%mutate(ma_7day_cases=rollmean(cases_new, k = 7, fill = "extend"),
                            ma_7day_cases_truncate=rollmean(cases_truncate, k = 7, fill = "extend"),
                            ma_7day_cases_truncate=ifelse(is.na(ma_7day_cases_truncate),ma_7day_cases, ma_7day_cases_truncate),
                            rollcases_7d=rollsum(cases_new, k=7,fill=NA, align="right"),
                            time=seq(1, nrow(cases_mas),1)) %>%
  select("date", "time", "state", "cases_new", "cases_import", "cases_recovered", "cases_active", "cases_cluster", "cases_pvax", "cases_fvax", "cases_child", 
         "cases_adolescent", "cases_adult", "cases_elderly", "cases_truncate", "ma_7day_cases", "ma_7day_cases_truncate", "rollcases_7d")

#extract national deaths
deaths_mas <-  deaths_mas %>% mutate(state="Malaysia",
                                     ma_7day_deaths=rollmean(deaths_new, k = 7, fill = "extend"),
                                     rolldeaths_7d=rollsum(deaths_new, k=7,fill=NA, align="right"))

#extract state deaths
deaths_state <-  deaths_state %>% group_by(state) %>%
  mutate(ma_7day_deaths=rollmean(deaths_new, k = 7, fill = "extend"),
         rolldeaths_7d=rollsum(deaths_new, k=7,fill=NA, align="right"))

#merge sets
cases <- bind_rows (cases_mas, cases_state)
deaths <- bind_rows(deaths_mas, deaths_state)
transmission <- left_join(cases, deaths, by=c("date", "state")) 

#divide into rt set and estimate
rt_input <- transmission %>% select (date, state, ma_7day_cases_truncate) %>%
  pivot_wider(id_cols=date,
              names_from=state, 
              values_from=ma_7day_cases_truncate) %>%
  replace(is.na(.), 0)

#create a dummy table
rt_output <- data.frame(time=seq(8,nrow(rt_input),1),
                        "Malaysia" = rep(NA, nrow(rt_input)-7),    # Create example data
                        "Johor" = rep(NA, nrow(rt_input)-7),
                        "Kedah" = rep(NA, nrow(rt_input)-7),
                        "Kelantan" = rep(NA, nrow(rt_input)-7),    # Create example data
                        "Melaka" = rep(NA, nrow(rt_input)-7),
                        "Negeri Sembilan" = rep(NA, nrow(rt_input)-7),
                        "Pahang" = rep(NA, nrow(rt_input)-7),
                        "Perak" = rep(NA, nrow(rt_input)-7),
                        "Perlis" = rep(NA, nrow(rt_input)-7),    # Create example data
                        "Pulau Pinang" = rep(NA, nrow(rt_input)-7),
                        "Sabah" = rep(NA, nrow(rt_input)-7),
                        "Sarawak" = rep(NA, nrow(rt_input)-7),
                        "Selangor" = rep(NA, nrow(rt_input)-7),
                        "Terengganu" = rep(NA, nrow(rt_input)-7),
                        "W.P. Kuala Lumpur" = rep(NA, nrow(rt_input)-7),
                        "W.P. Labuan" = rep(NA, nrow(rt_input)-7),
                        "W.P. Putrajaya" = rep(NA, nrow(rt_input)-7))

lower_output <- data.frame(time=seq(8,nrow(rt_input),1),
                           "Malaysia" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Johor" = rep(NA, nrow(rt_input)-7),
                           "Kedah" = rep(NA, nrow(rt_input)-7),
                           "Kelantan" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Melaka" = rep(NA, nrow(rt_input)-7),
                           "Negeri Sembilan" = rep(NA, nrow(rt_input)-7),
                           "Pahang" = rep(NA, nrow(rt_input)-7),
                           "Perak" = rep(NA, nrow(rt_input)-7),
                           "Perlis" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Pulau Pinang" = rep(NA, nrow(rt_input)-7),
                           "Sabah" = rep(NA, nrow(rt_input)-7),
                           "Sarawak" = rep(NA, nrow(rt_input)-7),
                           "Selangor" = rep(NA, nrow(rt_input)-7),
                           "Terengganu" = rep(NA, nrow(rt_input)-7),
                           "W.P. Kuala Lumpur" = rep(NA, nrow(rt_input)-7),
                           "W.P. Labuan" = rep(NA, nrow(rt_input)-7),
                           "W.P. Putrajaya" = rep(NA, nrow(rt_input)-7))

upper_output <- data.frame(time=seq(8,nrow(rt_input),1),
                           "Malaysia" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Johor" = rep(NA, nrow(rt_input)-7),
                           "Kedah" = rep(NA, nrow(rt_input)-7),
                           "Kelantan" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Melaka" = rep(NA, nrow(rt_input)-7),
                           "Negeri Sembilan" = rep(NA, nrow(rt_input)-7),
                           "Pahang" = rep(NA, nrow(rt_input)-7),
                           "Perak" = rep(NA, nrow(rt_input)-7),
                           "Perlis" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Pulau Pinang" = rep(NA, nrow(rt_input)-7),
                           "Sabah" = rep(NA, nrow(rt_input)-7),
                           "Sarawak" = rep(NA, nrow(rt_input)-7),
                           "Selangor" = rep(NA, nrow(rt_input)-7),
                           "Terengganu" = rep(NA, nrow(rt_input)-7),
                           "W.P. Kuala Lumpur" = rep(NA, nrow(rt_input)-7),
                           "W.P. Labuan" = rep(NA, nrow(rt_input)-7),
                           "W.P. Putrajaya" = rep(NA, nrow(rt_input)-7))

#loop the case counts for each state
count <- 2
for(i in 2:ncol(rt_input)) {# for-loop over columns
  rt <- estimate_R(rt_input[,i],
                   method="parametric_si",
                   config = make_config(list(
                     mean_si = 5.12, 
                     std_si = 1.86)))
  rt <- as.data.frame(rt$R[,c(3,5,11)])
  rt_output[,count] <- rt[,1]
  lower_output[,count] <- rt[,2]
  upper_output[,count] <- rt[,3]
  count <- count+1
}

#pivot rt
rt_output <- rt_output %>% pivot_longer(-time,
                                        names_to = "state",
                                        values_to = "rt") %>%
  mutate(state = replace(state, state=="Negeri.Sembilan", "Negeri Sembilan"),
         state = replace(state, state=="Pulau.Pinang", "Pulau Pinang"),
         state = replace(state, state=="W.P..Kuala.Lumpur", "W.P. Kuala Lumpur"),
         state = replace(state, state=="W.P..Labuan", "W.P. Labuan"),
         state = replace(state, state=="W.P..Putrajaya", "W.P. Putrajaya"))
  
lower_output <- lower_output %>% pivot_longer(-time,
                                              names_to = "state",
                                              values_to = "lower") %>%
  mutate(state = replace(state, state=="Negeri.Sembilan", "Negeri Sembilan"),
         state = replace(state, state=="Pulau.Pinang", "Pulau Pinang"),
         state = replace(state, state=="W.P..Kuala.Lumpur", "W.P. Kuala Lumpur"),
         state = replace(state, state=="W.P..Labuan", "W.P. Labuan"),
         state = replace(state, state=="W.P..Putrajaya", "W.P. Putrajaya"))
upper_output <- upper_output %>% pivot_longer(-time,
                                              names_to = "state",
                                              values_to = "upper") %>%
  mutate(state = replace(state, state=="Negeri.Sembilan", "Negeri Sembilan"),
         state = replace(state, state=="Pulau.Pinang", "Pulau Pinang"),
         state = replace(state, state=="W.P..Kuala.Lumpur", "W.P. Kuala Lumpur"),
         state = replace(state, state=="W.P..Labuan", "W.P. Labuan"),
         state = replace(state, state=="W.P..Putrajaya", "W.P. Putrajaya"))

#join up the rt to the transmission set
transmission <- transmission %>% left_join(rt_output, by=c("time", "state")) %>%
  left_join(lower_output, by=c("time", "state")) %>%
  left_join(upper_output, by=c("time", "state"))

#input population data
pop <- read_excel("P:/COVID-19/R0 Malaysia/data/population_healthcapacity/state_data_demographics_only_v2.xlsx", 
                  col_types = c("text", "numeric", "text", 
                                "text", "text", "text", "text", "text", 
                                "text", "text", "text", "text", "text", 
                                "text"))
pop <- pop %>% select (state, population_new)

#calaculate moving ir and mr
transmission <-  transmission %>% left_join(pop, by="state") %>%
  group_by(state) %>%
  mutate(ir_7day=(rollcases_7d/population_new)*100000,
         mr_7day=(rolldeaths_7d/population_new)*100000) 

#write to the data
write.csv(transmission, "data/transmission.csv")

#hospital capacity
#load data
hospital <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/hospital.csv")
icu <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/icu.csv")
pkrc <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/pkrc.csv")

#extract an occupancy rate for each level - state and national
bor_national <- hospital %>% dplyr::select(date, beds, hosp_covid, hosp_pui, hosp_noncovid) %>%
  group_by(date) %>%
  summarise(hosp_beds=sum(beds),
            hosp_covid=sum(hosp_covid),
            hosp_pui=sum(hosp_pui),
            hosp_noncovid=sum(hosp_noncovid),
            hosp=sum(hosp_covid+hosp_pui+hosp_noncovid),
  ) %>%
  mutate(bor=(hosp/hosp_beds)*100,
         state="Malaysia") %>%
  dplyr::select(date, state, bor, hosp_covid, hosp_pui, hosp_noncovid, hosp_beds) 

bor_state <- hospital %>% dplyr::select(date, state, beds, hosp_covid, hosp_pui, hosp_noncovid) %>%
  mutate(bor=((hosp_covid+hosp_pui+hosp_noncovid)/beds)*100) %>%
  dplyr::select(date, state, bor, hosp_covid, hosp_pui, hosp_noncovid, beds) %>%
  rename(date=date, state=state, bor=bor, hosp_covid=hosp_covid, 
         hosp_pui=hosp_pui, hosp_noncovid=hosp_noncovid, hosp_beds=beds)

icu_national <- icu %>% dplyr::select(date, beds_icu_total, icu_covid, icu_noncovid, icu_pui) %>%
  group_by(date) %>%
  summarise(icu_beds=sum(beds_icu_total),
            icu_covid=sum(icu_covid),
            icu_pui=sum(icu_pui),
            icu_noncovid=sum(icu_noncovid),
            icu=sum(icu_covid+icu_pui+icu_noncovid))%>%
  mutate(ior=(icu/icu_beds)*100,
         state="Malaysia") %>%
  dplyr::select(date, state, ior, icu_pui, icu_covid, icu_noncovid, icu_beds)

icu_state <- icu %>% dplyr::select(date, state, beds_icu_total, icu_covid, icu_noncovid, icu_pui) %>%
  mutate(ior=((icu_pui+icu_covid+icu_noncovid)/beds_icu_total)*100) %>%
  dplyr::select(date, state, ior, icu_pui, icu_covid, icu_noncovid, beds_icu_total) %>%
  rename(icu_beds=beds_icu_total)

vent_national <- icu %>% dplyr::select(date, vent, vent_port, vent_pui, vent_covid, vent_noncovid) %>%
  group_by(date) %>%
  summarise(vent_beds=sum(vent+vent_port),
            vent_covid=sum(vent_covid),
            vent_pui=sum(vent_pui),
            vent_noncovid=sum(vent_noncovid),
            ven=sum(vent_covid+vent_pui+vent_noncovid))%>%
  mutate(vor=(ven/vent_beds)*100,
         state="Malaysia") %>%
  dplyr::select(date, state, vor, vent_covid, vent_pui, vent_noncovid, vent_beds)

vent_state <- icu %>% dplyr::select(date, state, vent, vent_port, vent_pui, vent_covid,vent_noncovid) %>%
  mutate(vor=((vent_covid+vent_pui+vent_noncovid)/(vent+vent_port))*100) %>%
  dplyr::select(date, state, vor, vent_covid, vent_pui, vent_noncovid, vent, vent_port)%>%
  mutate(vent_beds=vent+vent_port)

pkrc_national <- pkrc %>% dplyr::select(date, pkrc_covid, pkrc_pui, pkrc_noncovid, beds) %>%
  group_by(date) %>%
  summarise(pkrc=sum(pkrc_covid+pkrc_pui+pkrc_noncovid),
            pkrc_covid=sum(pkrc_covid),
            pkrc_pui=sum(pkrc_pui),
            pkrc_noncovid=sum(pkrc_noncovid),
            pkrc_beds=sum(beds))%>%
  mutate(por=(pkrc/pkrc_beds)*100, 
         state="Malaysia") %>%
  dplyr::select(date, state, por, pkrc_covid, pkrc_pui, pkrc_noncovid, pkrc_beds) 

pkrc_state <- pkrc %>% dplyr::select(date, state, pkrc_covid, pkrc_pui, pkrc_noncovid, beds) %>%
  mutate(por=((pkrc_covid+pkrc_pui+pkrc_noncovid)/beds)*100) %>%
  dplyr::select(date, state, por, pkrc_covid, pkrc_pui, pkrc_noncovid, beds) %>%
  rename(date=date, state=state, por=por, pkrc_covid=pkrc_covid, pkrc_pui=pkrc_pui, 
         pkrc_noncovid=pkrc_noncovid, pkrc_beds=beds)

#merge the state and national rates
hosp_national <- bor_national %>% 
  left_join(icu_national, by=c("date", "state")) %>%
  left_join(vent_national, by=c("date", "state")) %>%
  left_join(pkrc_national, by=c("date", "state")) %>%
  mutate(date=as.Date(date))

hosp_state <- bor_state %>% 
  left_join(icu_state, by=c("date", "state")) %>%
  left_join(vent_state, by=c("date", "state")) %>%
  left_join(pkrc_state, by=c("date", "state"))  %>%
  mutate(date=as.Date(date)) 

#merge together
capacity <- bind_rows(hosp_national, hosp_state)

#get the absolute change from each day compared to 7-days prior
capacity <- capacity %>% group_by(state) %>%
  mutate(ma_7day_bor=rollmean(bor, k = 7, align = "right", fill = NA),
         ma_7day_ior=rollmean(ior, k = 7, align = "right", fill = NA),
         ma_7day_vor=rollmean(vor, k = 7, align = "right", fill = NA),
         change_bor=bor-lag(ma_7day_bor,1),
         change_ior=ior-lag(ma_7day_ior,1),
         change_vor=vor-lag(ma_7day_vor,1))

#write to the data
write.csv(capacity, "data/capacity.csv")

#testing
#load data
#testinf data
test_mas <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/tests_malaysia.csv")
test_state <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/tests_state.csv")

#edit data
test_mas <- test_mas %>% 
  mutate(tests_new=rtk.ag+pcr,
         state="Malaysia",
         date=as.Date(date)) %>%
  select(date, state, tests_new, rtk.ag, pcr)

test_state <- test_state %>% 
  mutate(tests_new=rtk.ag+pcr,
         date=as.Date(date)) %>% 
  select(date, state, tests_new, rtk.ag, pcr) 

#merge
test <- bind_rows(test_mas, test_state)

#add population data
pop <- read.csv("https://raw.githubusercontent.com/CITF-Malaysia/citf-public/main/static/population.csv")
pop <- pop %>% select(state, pop)
test <- left_join(test, pop, by="state")

#load case data- pull from above
test_cases <- transmission %>% select(date, state, cases_new) %>%
  mutate(date=as.Date(date))

#merge with test state
test <-  left_join(test, test_cases, by=c("date", "state"))

#add the new_cases, positivity rate and testing rate
test <- test %>% dplyr::group_by(state) %>%
  mutate(positivity_rate= round((cases_new/tests_new)*100, 2),
         testing_rate=round((tests_new/pop)*1000),2,
         ma_7day_tpr=rollmean(positivity_rate, k = 7, align = "right", fill = NA),
         ma_7day_tr=rollmean(testing_rate, k = 7, align = "right", fill = NA),
         change_tpr=ma_7day_tpr-lag(ma_7day_tpr,1),
         change_tr=ma_7day_tr-lag(ma_7day_tr,1)) %>% 
  dplyr::select(date, state, rtk.ag, pcr, tests_new, positivity_rate, testing_rate,
                ma_7day_tpr, ma_7day_tr, change_tpr, change_tr) 

#write to dataset
write.csv(test, "data/testing.csv")

#vaccinations
#load data
pop <- read.csv("https://raw.githubusercontent.com/CITF-Malaysia/citf-public/main/static/population.csv")
vax_state <- read.csv("https://raw.githubusercontent.com/CITF-Malaysia/citf-public/main/vaccination/vax_state.csv")
vax_malaysia <- read.csv("https://raw.githubusercontent.com/CITF-Malaysia/citf-public/main/vaccination/vax_malaysia.csv")
vaxreg_malaysia <- read.csv("https://raw.githubusercontent.com/CITF-Malaysia/citf-public/main/registration/vaxreg_malaysia.csv")
vaxreg_state <- read.csv("https://raw.githubusercontent.com/CITF-Malaysia/citf-public/main/registration/vaxreg_state.csv")

#add classifying variable
vaxreg_malaysia$state <- "Malaysia"
vax_malaysia$state <- "Malaysia"

#bind the state and national sets
vax <- bind_rows(vax_malaysia, vax_state)
vaxreg <- bind_rows(vaxreg_malaysia, vaxreg_state)
vax<- full_join(vax, vaxreg, by=c("date","state")) %>%
  full_join(pop, by="state") %>%
  mutate(state=ifelse(state=="Selangor", "Klang valley",
                      ifelse(state=="W.P. Putrajaya","Klang valley",
                             ifelse(state=="W.P. Kuala Lumpur", "Klang valley", state)))) %>%
  dplyr::select(date, state, cumul_partial, cumul_full, cumul_booster, 
                cumul_partial_adol,cumul_full_adol,
                cumul_partial_child,cumul_full_child, 
                pop, pop_18) %>%
  rename(pop_adult=pop_18) %>%
  mutate(pop_child=pop-pop_adult,
         cumul_partial_18=cumul_partial_adol+cumul_partial_child,
         cumul_full_18=cumul_full_adol+cumul_full_child,
         cumul_partial_adult=cumul_partial-cumul_partial_18,
         cumul_full_adult=cumul_full-cumul_full_18) %>%
  select(date, state, cumul_partial, cumul_full, cumul_booster,
         cumul_partial_adult, cumul_full_adult,
         cumul_partial_18, cumul_full_18,
         pop, pop_adult, pop_child) %>%
  rename(cumul_partial_child=cumul_partial_18,
         cumul_full_child=cumul_full_18) %>%
  group_by(state, date) %>% 
  summarise(cumul_partial=sum(cumul_partial),
            cumul_full=sum(cumul_full),
            cumul_booster=sum(cumul_booster),#all
            cumul_partial_adult=sum(cumul_partial_adult), 
            cumul_full_adult=sum(cumul_full_adult),#adult
            cumul_partial_child=sum(cumul_partial_child),
            cumul_full_child=sum(cumul_full_child),#child
            pop=sum(pop),
            pop_adult=sum(pop_adult),
            pop_child=sum(pop_child)) %>%
  mutate(perc_vax1=round((cumul_partial/pop)*100,1),
         perc_vax2=round((cumul_full/pop)*100,1),
         perc_booster=round((cumul_booster/pop)*100,1),
         perc_vax1_adult=round((cumul_partial_adult/pop_adult)*100,1),
         perc_vax2_adult=round((cumul_full_adult/pop_adult)*100,1),
         perc_booster_adult=round((cumul_booster/pop_adult)*100,1),
         perc_vax1_child=round((cumul_partial_child/pop_child)*100,1),
         perc_vax2_child=round((cumul_full_child/pop_child)*100,1),
         perc_total=100)

#write to the database
write.csv(vax, "data/vaccination.csv")

#cfr - delay adjusted
#merge datasets
cases <- read.csv("https://moh-malaysia-covid19.s3.ap-southeast-1.amazonaws.com/linelist_cases.csv")

#ic codes
ic_codes <- data.frame(state=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
                       states=c("Johor", "Kedah", "Kelantan", "Malacca", "Negeri Sembilan", "Pahang", "Penang", "Perak",
                                "Perlis", "Selangor", "Terengganu", "Sabah", "Sarawak", "W.P. Kuala Lumpur", "W.P. Labuan", "W.P. Putrajaya"))


#label the cases
cases <- left_join(cases, ic_codes, by="state")

cases_age <- cases %>%
  select(date,age) %>% 
  rename(age=age) %>%
  mutate(age=ifelse(age <18, 0,#0-17 years old
                    ifelse(age >59, 2, 1)), #>60
         date=as.Date(date),
         id=(as.numeric(date)-min(as.numeric(date)))+1) %>%
  group_by(date, age) %>% tally() %>%
  rename(case=n) %>%
  group_by(age) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), age) %>%
  mutate(ma_case = rollmean(case, k = 7, fill = "extend")) %>% ungroup() %>%
  group_by(date) %>%
  na.omit()  %>%
  mutate(ma_case=ifelse(is.na(ma_case), 0, ma_case)) %>%
  select(-case)

cases_total <- cases_age %>%
  select(date, ma_case) %>%
  group_by(date) %>%
  summarise(ma_case=sum(ma_case)) %>%
  mutate(age=3)

cases_cfr <- bind_rows(cases_age, cases_total)

#load deaths data
deaths <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/linelist/linelist_deaths.csv")

#tabulate the number od deaths by the date they turn positive 
deaths_age <- deaths %>% 
  select(date_positive,age) %>% 
  mutate(age=ifelse(age <18, 0,#0-17 years old
                    ifelse(age >59, 2, 1)), #>60
         age=round(age,0),
         date=as.Date(date_positive),
         id=(as.numeric(date)-min(as.numeric(date)))+1) %>%
  group_by(date, age) %>% tally() %>%
  rename(deaths=n) %>% ungroup() %>%
  group_by(age) %>%
  mutate(ma_death = rollmean(deaths, k = 7, fill = "extend")) %>%
  select(-deaths)

deaths_total <- deaths_age %>% ungroup() %>%
  select(date, ma_death) %>% 
  group_by(date) %>%
  summarise(ma_death=sum(ma_death)) %>% 
  mutate(age=3)

deaths_cfr=bind_rows(deaths_age, deaths_total) 

#join the 2 and save
cfr <- full_join(cases_cfr, deaths_cfr, by=c("date", "age")) %>%
  replace(is.na(.), 0) %>%
  group_by(age) %>%
  mutate(quarter=quarter(date, with_year = T, fiscal_start = 1)) %>% 
  ungroup() %>%
  group_by(age, date) %>%
  mutate(cfr=round((ma_death/ma_case)*100,2),
         cfr=ifelse(is.na(cfr), 0, 
                    ifelse(cfr==Inf, 0,
                           ifelse(cfr<0, 0, cfr)))) %>%
  ungroup() %>%
  group_by(age) %>%
  mutate(cfr=rollmean(cfr, k = 14, fill = "extend"),
         age=ifelse(age==0,"<18 years",
                    ifelse(age==1, "18-59 years",
                           ifelse(age==2, ">59 years", "All age groups"))))
  

#write to csv
write.csv(cfr, "data/cfr.csv")

#mobility
#load data
Global_Mobility_Report_3_ <- read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
# FIXME: update this daily 

#add an avergae mobility to the ts_mas dataset
mobility_mas <- Global_Mobility_Report_3_%>% filter(country_region=="Malaysia" & is.na(sub_region_1)) %>%
  select("date", "country_region", "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline",
         "retail_and_recreation_percent_change_from_baseline",
         "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", 
         "residential_percent_change_from_baseline") %>%
  mutate(date=as.Date(date),
         average=(parks_percent_change_from_baseline + grocery_and_pharmacy_percent_change_from_baseline +
                    retail_and_recreation_percent_change_from_baseline)/3) %>%
  rename(date=date, state=country_region,
         recreation=retail_and_recreation_percent_change_from_baseline,
         grocery=grocery_and_pharmacy_percent_change_from_baseline,
         parks=parks_percent_change_from_baseline,
         transit=transit_stations_percent_change_from_baseline,
         work=workplaces_percent_change_from_baseline,
         residence=residential_percent_change_from_baseline)

#clean up state
mobility_state <- Global_Mobility_Report_3_
mobility_state$sub_region_1[is.na(mobility_state$sub_region_1)] <- "country_level"
mobility_state <- mobility_state %>% filter(country_region=="Malaysia", sub_region_1!="country_level") %>%
  select("date", "sub_region_1", "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline",
         "retail_and_recreation_percent_change_from_baseline",
         "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", 
         "residential_percent_change_from_baseline") %>%
  mutate(date=as.Date(date),
         average=(parks_percent_change_from_baseline + grocery_and_pharmacy_percent_change_from_baseline +
                    retail_and_recreation_percent_change_from_baseline)/3) %>%
  rename(date=date, state=sub_region_1,
         recreation=retail_and_recreation_percent_change_from_baseline,
         grocery=grocery_and_pharmacy_percent_change_from_baseline,
         parks=parks_percent_change_from_baseline,
         transit=transit_stations_percent_change_from_baseline,
         work=workplaces_percent_change_from_baseline,
         residence=residential_percent_change_from_baseline) %>%
  as.data.frame()

#statndardise the states
mobility_state$state[mobility_state$state=="Penang"] <- 'Pulau Pinang'
mobility_state$state[mobility_state$state=="Malacca"] <- 'Melaka'
mobility_state$state[mobility_state$state=="Terengganu"] <- "Terengganu"
mobility_state$state[mobility_state$state=="Labuan Federal Territory"] <- 'W.P. Labuan'
mobility_state$state[mobility_state$state=="Federal Territory of Kuala Lumpur"] <- "W.P. Kuala Lumpur"
mobility_state$state[mobility_state$state=="Putrajaya"] <- 'W.P. Putrajaya'

#convert mobility into a dataframe
mobility <- bind_rows(mobility_mas, mobility_state)

#load case data- pull from above
mobility_cases <- transmission %>% select(date, state, cases_new) %>%
  mutate(date=as.Date(date))

#merge cases with mobility
mobility <-  left_join(mobility, mobility_cases, by=c("date", "state")) %>%
  group_by(state) %>%
  mutate(ma_7day_recreation=rollmean(recreation, k = 7, align = "right", fill = NA),
         ma_7day_grocery=rollmean(grocery, k = 7, align = "right", fill = NA),
         ma_7day_parks=rollmean(parks, k = 7, align = "right", fill = NA),
         ma_7day_transit=rollmean(transit, k = 7, align = "right", fill = NA),
         ma_7day_work=rollmean(work, k = 7, align = "right", fill = NA),
         ma_7day_residence=rollmean(residence, k = 7, align = "right", fill = NA),)

#index the mobility report
write.csv(mobility,  "data/mobility.csv")


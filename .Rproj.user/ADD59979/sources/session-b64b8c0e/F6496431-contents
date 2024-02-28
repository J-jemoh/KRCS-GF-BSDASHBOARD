library(shiny)
library(bs4Dash)
library(tidyr)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
#reading datasets
AYP<-readRDS("consolidated_AYP.RDS")
ebi_MHMC<-readRDS("ebi_consolidated_MHMC.RDS")
ebi_HBCF<-readRDS("ebi_consolidated_HBCF.RDS")
mentorship_data<-readRDS("menttorship_tracker.RDS")
fsw_consolidated<-readRDS("fsw_consolidated.RDS")
msm_consolidated<-readRDS("msm_consolidated.RDS")
pwid_consolidated<-readRDS("PWID_consolidated.RDS")
tg_consolidated<-readRDS("tg_consolidatd.RDS")

#numbers AYP
health_education<-nrow(filter(AYP,provided_health_education=="Yes"))
srh_info<-nrow(filter(AYP,provided_other_srh_information=="Yes"))
tested_for_hiv<-nrow(filter(AYP,tested_for_hiv=="Yes"))
screened_sti<-nrow(filter(AYP,sti_screening=="Yes"))
#HCBF AND MHMC
hbcf_complete_q9<-nrow(filter(ebi_HBCF,complete_sessions=="Complete sessions",age >=10 & age<=24,start_date>=as.Date("2023-07-01"),end_date<=as.Date("2023-09-30")))
hbcf_complete_10<-nrow(filter(ebi_HBCF,complete_sessions=="Complete sessions",age >=10 & age<=24,start_date>=as.Date("2023-10-01"),end_date<=as.Date("2023-12-31")))
hbcf_complete_sem<-nrow(filter(ebi_HBCF,complete_sessions=="Complete sessions",age >=10 & age<=24,start_date>=as.Date("2023-07-01"),end_date<=as.Date("2023-12-31")))
mhmc_complete_q9<-nrow(filter(ebi_MHMC,complete_sessions=="Complete sessions",age >=10 & age<=24,start_date>=as.Date("2023-07-01"),end_date<=as.Date("2023-09-30")))
mhmc_complete_q10<-nrow(filter(ebi_MHMC,complete_sessions=="Complete sessions",age >=10 & age<=24,start_date>=as.Date("2023-10-01"),end_date<=as.Date("2023-12-31")))
mhmc_complete_sem<-nrow(filter(ebi_MHMC,complete_sessions=="Complete sessions",age >=10 & age<=24,start_date>=as.Date("2023-07-01"),end_date<=as.Date("2023-12-31")))
mentorship_complete_q9<-nrow(filter(mentorship_data,complete_sessions=="Complete sessions",age >=14 & age<=24,start_date>=as.Date("2023-07-01"),end_date<=as.Date("2023-09-30")))
mentorship_complete_q10<-nrow(filter(mentorship_data,complete_sessions=="Complete sessions",age >=14 & age<=24,start_date>=as.Date("2023-10-01"),end_date<=as.Date("2023-12-31")))
mentorship_complete_sem<-nrow(filter(mentorship_data,complete_sessions=="Complete sessions",age >=14 & age<=24,start_date>=as.Date("2023-07-01"),end_date<=as.Date("2023-12-31")))
ayp_defined_9<-nrow(filter(AYP,provided_health_education=="Yes", counseling_on_safe_behaviour=="Yes",risk_assesment=="Yes",(provided_other_srh_information=="Yes"|family_planning_information=="Yes"),
                           date_of_outreach >=as.Date("2023-07-01"), date_of_outreach<=as.Date("2023-09-30")))
ayp_defined_10<-nrow(filter(AYP,provided_health_education=="Yes", counseling_on_safe_behaviour=="Yes",risk_assesment=="Yes",(provided_other_srh_information=="Yes"|family_planning_information=="Yes"),
                           date_of_outreach >=as.Date("2023-10-01"), date_of_outreach<=as.Date("2023-12-31")))
ayp_defined_sem<-nrow(filter(AYP,provided_health_education=="Yes", counseling_on_safe_behaviour=="Yes",risk_assesment=="Yes",(provided_other_srh_information=="Yes"|family_planning_information=="Yes"),
                            date_of_outreach >=as.Date("2023-07-01"), date_of_outreach<=as.Date("2023-12-31")))
#FSW CONSOLIDATED
fsw_consolidated$sex <- ifelse(fsw_consolidated$sex == "Male", "Female", fsw_consolidated$sex)
fsw_consolidated$sex <- ifelse(fsw_consolidated$sex == "FEMALE", "Female", fsw_consolidated$sex)
fsw_consolidated$hiv_status_at_enrollment<-ifelse(fsw_consolidated$hiv_status_at_enrollment == "positive", "Positive", fsw_consolidated$hiv_status_at_enrollment)
fsw_consolidated$hiv_status_at_enrollment<-ifelse(fsw_consolidated$hiv_status_at_enrollment == "negative", "Negative", fsw_consolidated$hiv_status_at_enrollment)
fsw_consolidated$hiv_status_at_enrollment<-ifelse(fsw_consolidated$hiv_status_at_enrollment == "Unknown", "Unknown ", fsw_consolidated$hiv_status_at_enrollment)
fsw_consolidated$hiv_status_at_enrollment<-ifelse(fsw_consolidated$hiv_status_at_enrollment == "Uknown", "Unknown ", fsw_consolidated$hiv_status_at_enrollment)
fsw_consolidated$hiv_status_at_enrollment<-ifelse(fsw_consolidated$hiv_status_at_enrollment == "Known positive", "Known Positive", fsw_consolidated$hiv_status_at_enrollment)
fsw_consolidated$hiv_status_at_enrollment<-ifelse(fsw_consolidated$hiv_status_at_enrollment == "known Positive", "Known Positive", fsw_consolidated$hiv_status_at_enrollment)
fsw_consolidated$hiv_status_at_enrollment<-ifelse(fsw_consolidated$hiv_status_at_enrollment == "unknown", "Unknown", fsw_consolidated$hiv_status_at_enrollment)
#total numbers FSW
total_fsw<-nrow(filter(fsw_consolidated))
total_counties <- length(unique(fsw_consolidated$county))
total_pees<-length(unique(fsw_consolidated$peer_educator))
total_15_14<-nrow(filter(fsw_consolidated,age >=14 & age <=24))
above_25<-nrow(filter(fsw_consolidated,age > 25))
sr_number<-length(unique(fsw_consolidated$sr_name))

#msm data checks
msm_consolidated$hiv_status_at_enrollment<-ifelse(msm_consolidated$hiv_status_at_enrollment == "positive", "Positive", msm_consolidated$hiv_status_at_enrollment)
msm_consolidated$hiv_status_at_enrollment<-ifelse(msm_consolidated$hiv_status_at_enrollment == "negative", "Negative", msm_consolidated$hiv_status_at_enrollment)
msm_consolidated$hiv_status_at_enrollment<-ifelse(msm_consolidated$hiv_status_at_enrollment == "Unknown", "Unknown ", msm_consolidated$hiv_status_at_enrollment)
msm_consolidated$hiv_status_at_enrollment<-ifelse(msm_consolidated$hiv_status_at_enrollment == "Uknown", "Unknown ", msm_consolidated$hiv_status_at_enrollment)
msm_consolidated$hiv_status_at_enrollment<-ifelse(msm_consolidated$hiv_status_at_enrollment == "Known positive", "Known Positive", msm_consolidated$hiv_status_at_enrollment)
msm_consolidated$hiv_status_at_enrollment<-ifelse(msm_consolidated$hiv_status_at_enrollment == "known Positive", "Known Positive", msm_consolidated$hiv_status_at_enrollment)
msm_consolidated$hiv_status_at_enrollment<-ifelse(msm_consolidated$hiv_status_at_enrollment == "unknown", "Unknown", msm_consolidated$hiv_status_at_enrollment)

#total numbers msm
total_msm<-nrow(filter(msm_consolidated))
total_counties_msm <- length(unique(msm_consolidated$county))
total_pees_msm<-length(unique(msm_consolidated$peer_educator))
sr_number_msm<-length(unique(msm_consolidated$sr_name))

#total numbers pwid
total_pwid<-nrow(filter(pwid_consolidated))
total_counties_pwid <- length(unique(pwid_consolidated$county))
total_pees_pwid<-length(unique(pwid_consolidated$peer_educator))
sr_number_pwid<-length(unique(pwid_consolidated$sr_name))

##total numbers TG
total_tg<-nrow(filter(tg_consolidated))
total_counties_tg <- length(unique(tg_consolidated$county))
total_pees_tg<-length(unique(tg_consolidated$peer_educator))
sr_number_tg<-length(unique(tg_consolidated$sr_name))

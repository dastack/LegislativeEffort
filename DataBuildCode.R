library(tidyverse)
library(haven)
library(lme4)
library(sjPlot)
library(ggthemes)



##Load Data
##Imports directly from dataverse using dataverse package
# ces <- dataverse::get_dataframe_by_name(
#   filename = "cumulative_2006-2023.dta",
#   dataset = "10.7910/DVN/II2DB6",
#   original = TRUE,
#   .f = haven::read_dta,
#   server = "dataverse.harvard.edu"
# )

cesIssue<- read_dta(file = "AnsoCCES/data/input/by-person_ansolabeherekuriwaki_issues-wide.dta")

cesIssue<- cesIssue %>%
  select(year, case_id, ideol_a_nominate, agrmt_issue_actl, agrmt_issue_prcp) %>%
  rename(IssueAgree = agrmt_issue_actl,
         PerIssueAgree = agrmt_issue_prcp,
         year2 = year)



ces <- arrow::read_feather("cumulative_2006-2023.feather")
ces$electionYear <- ifelse(ces$year %% 2 == 0, 1, 0) ##to remove non election year data. 

##Trim down
ces<- ces %>%
  filter(electionYear == 1) %>%
  select(year, case_id, weight, weight_cumulative, state, st, cong, state_post,
         st_post, dist, cd, pid3_leaner, pid7, ideo5, age, educ, newsint,
         approval_rep, intent_rep, rep_current, rep_icpsr) %>%
  filter(!is.na(rep_icpsr)) %>% ##Removes some NA icpsr to accomodate the merge.  5 rows removed
  rename(DistNum = dist) ##Renaming district number

ces$rep_icpsr<- as.character(ces$rep_icpsr)


##Merging in MC level data

LESDat<- read_dta("CELHouse93to117ReducedClassic.dta") %>%
  select(icpsr, congress, thomas_name, female, freshman, seniority, party_code,
         cbill1, sbill1, ssbill1, claw1, slaw1, sspass1) %>% ##, st_name, cd, dem. potentially useful IDentifiers
  rename(ComBills = cbill1,
         SubBills = sbill1,
         MajBills = ssbill1,
         LegSex = female,
         LegSeniority = seniority,
         rep_icpsr = icpsr,
         cong = congress,
         ComBillPass = claw1,
         SubBillPass = slaw1, 
         MajBillPass = sspass1) %>%
  mutate(BillsSpons = ComBills + SubBills + MajBills,
         BillsPassed = ComBillPass + SubBillPass + MajBillPass,
         DeadBillsSpons = BillsSpons - BillsPassed)
LESDat$rep_icpsr<- as.character(LESDat$rep_icpsr)

LESDat$LegParty = "Ind"
LESDat$LegParty[LESDat$party_code==100]<- "Dem"
LESDat$LegParty[LESDat$party_code==200]<- "Rep"


ces<- left_join(ces, LESDat, by = c("cong", "rep_icpsr"),
                relationship = "many-to-one")

ces<- left_join(ces, cesIssue, by = c("case_id"))



##Recoding Variables
##Treats Don't Knows/Not Sures as missing
ces$inc_approve<- NA
ces$inc_approve[ces$approval_rep == "Strongly Approve"]<-4
ces$inc_approve[ces$approval_rep == "Approve / Somewhat Approve"]<-3
ces$inc_approve[ces$approval_rep == "Disapprove / Somewhat Disapprove"]<-2
ces$inc_approve[ces$approval_rep == "Strongly Disapprove"]<- 1

##Recoding Variables
##Treats Don't Knows/Not Sures as middle category (similar to Ansolabhere)
ces$inc_approve5<- NA
ces$inc_approve5[ces$approval_rep == "Strongly Approve"]<-5
ces$inc_approve5[ces$approval_rep == "Approve / Somewhat Approve"]<-4

ces$inc_approve5[ces$approval_rep == "Never Heard of this Person"]<-3
ces$inc_approve5[ces$approval_rep == "Never Heard / Not Sure"]<-3 ##Other categories are listed but don't have observations.

ces$inc_approve5[ces$approval_rep == "Disapprove / Somewhat Disapprove"]<-2
ces$inc_approve5[ces$approval_rep == "Strongly Disapprove"]<- 1


##

ces$pid3<- "Ind"
ces$pid3[ces$pid3_leaner == 1]<- "Dem"
ces$pid3[ces$pid3_leaner == 2]<- "Rep"


ces$party_match <- ifelse(
  # For Democratic legislators
  (ces$LegParty == "Dem" & ces$pid3 == "Dem") |
    (ces$LegParty == "Rep" & ces$pid3 == "Rep"),
  "Same Party",
  "Different Party")
ces$party_match[ces$pid3 == "Ind"]<- "Ind"

ces$dist_year<- paste(as.character(ces$cong), as.character(ces$cd), sep = "_") 

#save(ces, file = "CES_SymbolicLegislation.rda")

# Packages & Setup -------------------------------------------------------------
library(readxl)     # excel files
library(tidyverse)  # data handling
options(dplyr.summarise.inform = FALSE)
library(magrittr)   # piping
library(psych)      # describe data
library(ggpubr)     # arranging plots

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script matches the 
# A: committee information of the climate change relevant hearings 
#     from 2003 to 2010 with 
# B: the witness information per hearing (n/%), 
# C: the committee assignments at the time based on the congressional 
#     Committees data compiled by Charles Stewart III and Jonathan Woon (MIT) 
#     (http://web.mit.edu/17.251/www/data_page.html#2), 
# D: the congress-legislators data (to obtain the OpenSecrets identification 
#     codes),
# E: the OpenSecrets Individual Contributions data for relevant industries, and
# F: the OpenSecrets Lobbying data for relevant industries.

# Note: Uncomment the lines filter(Issue == "Issue match") %>% in the section 
#       "Join the lobbying expenditures data to the MoC data" at the bottom of 
#       the script to filter the data by cap-and-trade related issue description
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getwd()
setwd("/Users/mn/Library/CloudStorage/OneDrive-UniversityofExeter/Projects/GitHub/defeating-cap-and-trade/Data")
theme_set(theme_minimal())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A: Load committee data ----
#     of the climate change relevant hearings from 2003 to 2010
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load hearings with committee information
hearings <- read.csv('Analysis/hearings.csv', 
                     sep = "\t", stringsAsFactors = F) %>% 
  mutate(date = as.Date(date, format = '%Y-%m-%d'),
         committee_short =
           ifelse(committee ==
                    "Committee on Agriculture, Nutrition, and Forestry",
                  "Agriculture, Nutrition, and Forestry", 
                  as.character(committee_short)),
         committee = paste(chamber, committee),
         committee_type = ifelse(
           committee %in% c("Senate Committee on Environment and Public Works",
                            "Senate Committee on Finance",
                            "House Committee on Agriculture",
                            "House Committee on Education and Labor", 
                            "House Committee on Energy and Commerce", 
                            "House Committee on Financial Services", 
                            "House Committee on Foreign Affairs", 
                            "House Committee on Judiciary", 
                            "House Committee on Natural Resources",
                            "House Committee on Oversight and Government Reform", 
                            "House Committee on Science",
                            "House Committee on Science and Technology", 
                            "House Committee on Transportation and Infrastructure",
                            "House Committee on Ways and Means"),
           'Key committee', 'Other committee'),
         hearing_id = factor(hearing_id),
         majority = factor(case_when(
           congress %in% c(108, 109) ~ 'R',
           congress %in% c(110, 111) ~ 'D')))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# B: Merge committee data with witness information ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load witness data
witnesses <- read.csv("Analysis/witnesses.csv", 
                      sep = "\t", stringsAsFactors = F) %>% 
  mutate(date = as.Date(date, format = '%Y-%m-%d'),
         hearing_id = factor(hearing_id),
         category2 = factor(
           ifelse(contrarian == "Contrarian", "Contrarian", 
                  ifelse(subcategory == 'Environment',
                         "Environmental Non-Profits", category))),
         category2_short = recode_factor(
           category2,
           `Alternative Energy`= "alternative",
           `Business & Services`= "business_services",
           `Carbon-intensive Industry` = "carbonintensive",
           `Contrarian` = "contrarian",
           `Environmental Non-Profits` = "environmental",
           `Fossil Fuel Industry`= "fossilfuel",
           `Non-Profit Organisations` = "nonprofit",
           `Scientists` = "science",
           `Government Officials` = "government",
           `Other` = "other"),
         category = factor(category),
         category_short = recode_factor(
           category,
           `Alternative Energy`= "alternative",
           `Business & Services`= "business_services",
           `Carbon-intensive Industry` = "carbonintensive",
           `Fossil Fuel Industry`= "fossilfuel",
           `Non-Profit Organisations` = "nonprofit",
           `Scientists` = "science",
           `Government Officials` = "government",
           `Other` = "other"))

table(witnesses$category, witnesses$category2) %>% addmargins()

# Aggregate the witnesses per category for each hearing
witnesses_per_hearing <- witnesses %>%
  group_by(hearing_id, category_short) %>% summarise(n = n()) %>%
  mutate(per = prop.table(n)*100) %>% group_by(hearing_id) %>%
  mutate(n_witnesses = sum(n)) %>%
  pivot_wider(names_from=category_short, values_from=c(n, per)) %>%
  setNames(make.names(names(.))) %>%
  replace(is.na(.), 0)

# Aggregate the witnesses per the alternative categories
# (with contrarians and environmental allies seperate)
contrarians_per_hearing <- witnesses %>%
  group_by(hearing_id, category2_short) %>% summarise(n2 = n()) %>%
  mutate(per2 = prop.table(n2)*100) %>% group_by(hearing_id) %>%
  mutate(n2_witnesses = sum(n2)) %>%
  pivot_wider(names_from=category2_short, values_from=c(n2, per2)) %>%
  setNames(make.names(names(.))) %>%
  replace(is.na(.), 0)

denialists_per_hearing <- witnesses %>%
  group_by(hearing_id, denialist) %>% summarise(n2 = n()) %>% 
  pivot_wider(names_from=denialist, values_from=c(n2)) %>%
  replace(is.na(.), 0) %>%
  select(Denialist) %>% 
  rename(n2_denialist = Denialist)

hearings <- merge(hearings, witnesses_per_hearing, by = 'hearing_id')
hearings <- merge(hearings, contrarians_per_hearing, by = c('hearing_id'))
hearings <- merge(hearings, denialists_per_hearing, by = c('hearing_id'))

rm(witnesses, witnesses_per_hearing, 
   contrarians_per_hearing, denialists_per_hearing)

plot(hearings$date, hearings$per_fossilfuel)
plot(hearings$date, hearings$per2_contrarian)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C: Match the committee assignments data----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Notes regarding the congressional Committees data: 
#
# 1) Two House committee names were coded wrongly and are adapted in the
#    code below. Namely:
#
#    House Committee on Government Reform for the 108th congress (2003/2004)
#    In the 106th congress, the panel was renamed the Committee on Government 
#    Reform. [...] On January 4, 2007, the 110th congress renamed it the 
#    Committee on Oversight and Government Reform.
#    https://en.wikipedia.org/wiki/United_States_House_Committee_on_Oversight_and_Reform
#
#    House Committee on Science for the 108th congress (2003/2004)
#    After the Republican Party gained a majority in congress in 1994, the name of
#    the committee was changed to the House Committee on Science. With the return 
#    of control to the Democrats in 2007, the committee's name was changed back to 
#    the House Committee on Science and Technology. 
#    https://en.wikipedia.org/wiki/United_States_House_Committee_on_Science,_Space,_and_Technology
#
# 2) Some variables are renamed to unify the House and Senate assignment csv 
#    files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load House committee data
house_assignments <- 
  read_xls("StewartWoon/house_assignments_103-115-3.xls", 
           col_types = c(rep("guess", 15), rep("text", 5))) %>% 
  setNames(tolower(str_replace_all(make.names(names(.)), '\\.', '_'))) %>% 
  rename(id = id__,
         rank_within_party = rank_within_party_status,
         party_code = party,
         state_icpsr = state,
         cd_code = cd,
         stab = state_name) %>% 
  mutate(committee_name = str_replace(str_to_title(committee_name), 
                                      " And ", " and "),
         committee_name = ifelse(
           committee_name ==
             "Energy Independence and Global Warming (Select)", 
           "Energy Independence and Global Warming",
           committee_name),
         committee_name = ifelse(
           committee_name %in% 
             c("Oversight and Government Reform",
               "Government Reform and Oversight"), 
           "Government Reform",
           committee_name),
         date_of_assignment = as.Date(date_of_assignment),
         date_of_termination = as.Date(date_of_termination)) %>%
  filter_all(any_vars(!is.na(.)))  %>%
  filter(congress %in% c(108, 109, 110, 111))

# Load Senate committee data
senate_assignments <- 
  read_xls("StewartWoon/senate_assignments_103-115-3.xls", 
           col_types = c(rep("guess", 16), rep("text", 5)))[,c(1:9, 11:21)] %>% 
  setNames(tolower(str_replace_all(make.names(names(.)), '\\.', '_'))) %>% 
  rename(id = id__,
         date_of_assignment = date_of_appointment,
         state_icpsr = state_code,
         cd_code = district, 
         stab = state_name) %>% 
  mutate(committee_name = str_replace(str_to_title(committee_name), 
                                      " And ", " and "),
         # committee_name = ifelse(committee_name ==
         #                           "Agriculture, Nutrition, and Forestry",
         #                         "Agriculture", committee_name),
         date_of_assignment = as.Date(date_of_assignment),
         date_of_termination = as.Date(date_of_termination)) %>%
  filter(congress %in% c(108, 109, 110, 111)) # subset correct timespan

# -------------------------------------------------------------------------



# Join the MoCs that were on the committees of the hearings

# House of Representatives
house_MoCs <- left_join(hearings[hearings$chamber == 'House',], 
                        house_assignments,
                        by = c('congress'='congress',
                               'committee_short'='committee_name'))

## Senate
senate_MoCs <- left_join(hearings[hearings$chamber == 'Senate',], 
                         senate_assignments,
                         by = c('congress'='congress',
                                'committee_short'='committee_name'))

## Merge House and Senate data
committee_members <- rbind(house_MoCs, senate_MoCs) %>% 
  mutate(Party = ifelse(party_code == 100, "D", 
                        ifelse(party_code == 200, "R", "I")))

## Remove unnecessary objects
rm(house_assignments, senate_assignments, 
   house_MoCs, senate_MoCs)


## Four hearings were held at the beginning of a new congress
# c("108shrg95341", "111shrg47252")
# These hearings were held with the new committee members even though they
# were not yet officially assigned to the committees
# https://www.govinfo.gov/app/details/CHRG-108shrg95341/CHRG-108shrg95341
# >> All correct
# https://www.govinfo.gov/app/details/CHRG-111shrg47252/CHRG-111shrg47252
# >> All but "Tester, Jon" correct >> Drop "Tester, Jon"
committee_members <- committee_members %>%
  subset(!(hearing_id == "111shrg47252" & name == "Tester, Jon"))

## For the remaining hearings:
# Was the committee member assigned when the hearing took place?
committee_members %>% 
  subset(date < date_of_assignment & 
           ! hearing_id %in% c("108shrg95341", "111shrg47252")) %>% 
  nrow()
# >> 101 MoCs had not yet joined the committee when the hearing took place
committee_members %>% 
  subset(date > date_of_termination) %>% 
  nrow()
# >> 70 MoCs had left the committee when the hearing took place

## Exclude all MoCs that were not on the committee when the hearing was held
committee_members <- committee_members %>% 
  subset(!(date < date_of_assignment &
             ! hearing_id %in% c("108shrg95341", "111shrg47252"))) %>% 
  subset(!(date > date_of_termination))

## Inspect the data
committee_members %>% select_if(is.numeric) %>% describe(skew = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# D: Match the congress-legislators data ----
#     to obtain OpenSecrets committee member identification codes (CID))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# congress-legislators data codebook:
#
# icpsr       The numeric ID for this legislator in Keith Poole's VoteView.com 
#             website, originally based on an ID system by the Interuniversity 
#             Consortium for Political and Social Research.
# opensecrets The alphanumeric ID for this legislator on OpenSecrets.org.
# last_name   The legislator's last name.
# first_name  The legislator's recognizable first name. Many people go by a 
#             different name than their legal first name, often their legal 
#             middle name, and our approach is to ensure that our first + last 
#             name fields combine to a recognizable name of the legislator.
# birthday    The legislator's birthday, in YYYY-MM-DD format.
# gender      The legislator's gender, either "M" or "F". (In historical data, 
#             we've worked backwards from history.house.gov's Women in congress 
#             feature.)
# govtrack    The numeric ID for this legislator on GovTrack.us.
# bioguide    The alphanumeric ID for this legislator in 
#             http://bioguide.congress.gov. Note that at one time some 
#             legislators (women who had changed their name when they got 
#             married) had two entries on the bioguide website. Only one 
#             bioguide ID is included here. This is the best field to use as a 
#             primary key.
# 
# Source: https://github.com/unitedstates/congress-legislators/
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CRP IDs codebook:
#
# CID         Unique identifier for each candidate. Every candidate should have 
#             one and only one CID throughout all cycles. CID for candidates 
#             is based on CID.
#             Note: If starting with N it refers to the MoCs time as a member, 
#             if starting with C it refers to them as a candidate.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

legislators <- rbind(
  read.csv("congress-legislators/legislators-historical.csv"),
  read.csv("congress-legislators/legislators-current.csv"))

committee_members$CID <- committee_members %$%
  legislators$opensecrets_id[match(id, legislators$icpsr_id)]

committee_members <- committee_members %>%
  mutate(# Replace missing CIDs
    CID = replace(CID, id == 70601, "N00009825"),
    CID = replace(CID, id == 70701, "N00024866"),
    CID = replace(CID, id == 70501, "N00000133"),
    CID = replace(CID, id == 70302, "N00007632"),
    CID = replace(CID, id == 94828, "N00005645"),
    CID = replace(CID, id == 14240, "N00000534"),
    CID = replace(CID, id == 70303, "N00001692"),
    CID = replace(CID, id == 29387, "N00003730"),
    CID = replace(CID, id == 94679, "N00005372"),
    CID = replace(CID, id == 70801, "N00026264"),
    CID = replace(CID, id == 20901, "N00029917"),
    CID = replace(CID, id == 90901, "N00029917"),
    CID = replace(CID, id == 70805, "N00029168"),
    CID = replace(CID, id == 70810, "N00030418"),
    CID = replace(CID, id == 70810, "N00030418"),
    # Correct wrongly assigned CID
    CID = replace(CID, name == "Deutch, Theodore E." & CID == "N00002839",
                  "N00031317"),
    # Correct wrongly assigned district
    cd_code = replace(cd_code, name == "Thomas, William M.", 22))

# Match identification codes to committee members
committee_members <- 
  left_join(committee_members, 
            legislators[,c("last_name", "first_name", "birthday", "gender", 
                           "opensecrets_id", "bioguide_id", "govtrack_id")],
            by = c("CID" = "opensecrets_id"))

# Drop legislators data
rm(legislators)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# E: Match the OpenSecrets Campaign Contributions data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CRP Categories Codebook:
# 
# Catcode	      OpenSecrets category code
# Catname       OpenSecrets category name	(Lv. 1)
# Catorder  	  OpenSecrets industry code
# Industry	    OpenSecrets industry name (Lv. 2)
# Sector        OpenSecrets sector name	(abbr.)
# Sector.Long   OpenSecrets sector name (Lv. 3)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CRP_Codes <- read.delim("OpenSecrets/BulkData/CRP_Categories.txt",
                        header = TRUE, skip = 7, sep = "\t")

# List of industries (Catorder) in a sector (Sector)
CRP_Codes %>% select(Sector, Catorder, Industry, Catcode, Catname) %>% 
  unique() %>% arrange(Sector, Catorder) %>% as_tibble %>% print(n = Inf)

CRP_Codes %>% filter(Sector == "Energy/Nat Resource") %>% 
  select(Catorder, Industry, Catcode, Catname) %>% unique() %>% 
  arrange(Catorder, Catcode)

USCAP <- read.csv("Analysis/USCAP_members_wayback.csv",
                            header = T, sep = ",") %>% 
  mutate(member_short = tolower(member),
         member_short = str_remove(member_short, ' inc\\.'),
         member_short = str_remove(member_short, ' corp\\.')); USCAP
USCAP$member_short %>% table() %>% sort()
USCAP_members <- USCAP$member_short %>% unique() %>% sort()
  
# Select the fossil fuel industry
fossilfuel <- 
  c("E1000",          #                         Energy production & distribution
    "E1100",          #                                                Oil & Gas
    "E1110",          #                Major (multinational) oil & gas producers
    "E1120",          #                          Independent oil & gas producers
    "E1140",          #                  Natural Gas transmission & distribution
    "E1150",          #                Oilfield service, equipment & exploration
    "E1160",          #                           Petroleum refining & marketing
    "E1170",          #                                Gasoline service stations
    "E1180",          #                                         Fuel oil dealers
    "E1190",          #                   LPG/Liquid Propane dealers & producers
    "E1210",          #                                              Coal mining
    "E1600",          #                                 Electric Power utilities
    "E1610",          #                              Rural electric cooperatives
    "E1620",          #                                 Gas & Electric Utilities
    "E1630",          #              Independent power generation & cogeneration
    "E1700"           #                     Power plant construction & equipment
  )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Center for Responsive Politics (CRP | OpenSecrets) Contributions Data
# 
# Individual Contributions (indivs):
# 
# RecipID	    The recipient's id number. If the contribution is to a candidate 
# (CID)       this will be the candidate's unique candidate id number (CID). 
#             Otherwise, it will be the FEC (campaign) committee id number.
# RealCode	  The standard five character code identifying the donor's industry 
#             or ideology.
# Date	      The reported date of the contribution.
# Amount	    The amount contributed. This will be negative for refunds.
# Type	      The transaction type code for the contribution. 15 is a 
#             contribution, 15e is an earmarked contribution (made through a 
#             group such as Club for Growth or Emily's List), 15j is a 
#             contribution through a joint fund raising committee and 22y is a 
#             refund. "10" indicates "soft" or nonfederal money for the 2002 
#             cycle and earlier. For the 2004 cycle and later type "10" 
#             indicates Levin funds.or outside spending.
# CmteID	    The committee id number for the recipient. Note that a candidate 
#             can have more than one committee — this field indicates the exact 
#             committee receiving the contribution.
# 
# PACs to Candidate Contributions (pacs):
# 
# RecipID     A unique identifier for candidates that is constant throughout 
# (CID)       cycles.
# Amount      The amount contributed. This will be negative for refunds. 
# Date 	      The reported date of the contribution. 
# RealCode	  The standard five character code identifying the donor's industry 
#             or ideology.
# Type       	The transaction type code for the contribution. 24A is an
#             Independent Expenditure against the candidate, 24C is a 
#             coordinated expenditure, 24E is an independent expenditure for the 
#             candidate, 24F is a communication cost for the candidate, 24K is a
#             direct contribution, 24N is a communication cost against the 
#             candidate and 24Z is an in kind contribution.
# DI 	        Whether the contribution is direct (“D”) or indirect (“I.”). 
#             Indirect contributions include independent expenditures and 
#             communications costs, are not subject to contribution limits and 
#             must be made completely independently of the candidate. Indirect 
#             contributions can also be against the candidate. 
# 
# PACs to PACs Contributions (pac_other):
# 
# Date 	      The reported date of the contribution.
# Amount      The amount contributed. This will be negative for refunds.
# RecipID     The recipient's id number. If the contribution is to a candidate 
# (CID)       this will be the candidate's unique candidate id number. 
# Type        The transaction type code for the contribution. 11 is a tribal
#             contribution, 22Z is a contribution refund to a candidate or 
#             committee, 24G is a Transfer to an affiliated committee, 24K is a 
#             direct contribution, 24R is a election recount disbursement and 
#             24Z is an in kind contribution
# RealCode	  The standard five character code identifying the donor's industry 
#             or ideology.
# 
# Source: OpenSectrets UserGuide.pdf
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define for which congresses data will be loaded
congresses <- c(108:111)
# Define the matching years as listed in the data names
years <- c("04", "06", "08", "10")
# Define directory
dir <- "OpenSecrets/BulkData/CampaignFin"
# Create empty data frames to save data
indivs <- data.frame()
pacs <- data.frame()
# pac_other <- data.frame()
campaign_contributions <- data.frame()

# Load contributions data
for (i in 1:length(congresses)) {
  path = paste0(dir, years[i], "/cmtes", years[i], ".txt")
  # Load FEC Committee table
  cmtes <- read.table(path, header = FALSE, quote = "|", sep = ",",
                      stringsAsFactors = F,
                      colClasses = c("NULL", NA, rep("NULL", 5), NA,
                                     "NULL", NA, rep("NULL", 3))) %>% 
    rename(CmteID = V2, FeCCandID = V8, PrimCode = V10)
  # Load Individual Contributions and merge with FEC data for "FeCCandID"
  path = paste0(dir, years[i], "/indivs", years[i], ".txt")
  indivs_temp <- left_join(
    read.table(path, header = FALSE, quote = "|", sep = ",",
               colClasses = c(rep("NULL", 3), rep(NA, 7), rep("NULL", 5),
                              NA, NA, rep("NULL", 3), NA, NA, "NULL"), 
               encoding = 'windows-1252') %>% 
      rename(Contrib = V4, CID = V5, Orgname = V6, UltOrg = V7, RealCode = V8, 
             Date = V9, Amount = V10, Type = V16, CmteID = V17, 
             Occupation = V21, Employer = V22) %>% 
      mutate(Orgname = Orgname %>% str_replace('\x82', 'e') %>% str_squish(),
             Employer = Employer %>% str_replace('\x82', 'e') %>% str_squish()),
    cmtes, by = c("CmteID")) %>%
    mutate(RealCode = toupper(RealCode)) %>% 
    filter(CID %in% committee_members$CID[committee_members$congress == 
                                            congresses[i]]) %>%
    filter(!startsWith(RealCode, "Z9")) %>% 
    filter(str_trim(Type) %in% c("11", "15", "15E", "15J", "22Y")) %>%
    filter(!startsWith(FeCCandID, "P")) %>%
    filter(!startsWith(toupper(PrimCode), "Z4")) %>% 
    mutate(Catcode = CRP_Codes$Catcode[match(RealCode, CRP_Codes$Catcode)],
           Catorder = CRP_Codes$Catorder[match(RealCode, CRP_Codes$Catcode)],
           SectorCode = str_extract(Catorder, "[:alpha:]+"),
           year = as.numeric(substr(Date, 7, 10)),
           congress = case_when(
             year %in% 2003:2004 ~ 108, year %in% 2005:2006 ~ 109,
             year %in% 2007:2008 ~ 110, year %in% 2009:2010 ~ 111)) 
  # Load Pac Contributions  
  path = paste0(dir, years[i], "/pacs", years[i], ".txt")
  pacs_temp <- read.table(path, header = FALSE, quote = "|", sep = ",",
                          colClasses = c(rep("NULL", 2), rep(NA, 7), 
                                         "NULL")) %>% 
    rename(PACID = V3, CID = V4, Amount = V5, Date = V6,
           RealCode = V7, Type = V8, DI = V9) %>%
    mutate(RealCode = toupper(RealCode)) %>% 
    filter(CID %in% committee_members$CID[committee_members$congress == 
                                            congresses[i]]) %>%
    filter(!startsWith(RealCode, "Z4") & !startsWith(RealCode, "Z9")) %>%
    filter(DI == "D") %>% 
    mutate(Catcode = CRP_Codes$Catcode[match(RealCode, CRP_Codes$Catcode)],
           Catorder = CRP_Codes$Catorder[match(RealCode, CRP_Codes$Catcode)],
           SectorCode = str_extract(Catorder, "[:alpha:]+"),
           year = as.numeric(substr(Date, 7, 10)),
           congress = case_when(
             year %in% 2003:2004 ~ 108, year %in% 2005:2006 ~ 109,
             year %in% 2007:2008 ~ 110, year %in% 2009:2010 ~ 111))
  # Save contributions
  indivs <- rbind(indivs, indivs_temp) %>% 
    mutate(Orgname = stringr::str_trim(Orgname),
           UltOrg = stringr::str_trim(UltOrg),
           UltOrg = str_replace_all(UltOrg, "[^[:graph:]]", " "))
  pacs <- rbind(pacs, pacs_temp)
  print(paste0('Campaign contributions for ', congresses[i], 
               "th congress loaded (", i, "/", length(congresses), ")"))
}

# Drop temporary objects
rm(CRP_Codes, dir, path, congresses, years, i, cmtes, indivs_temp, pacs_temp)

## Save data
# write_csv(indivs, "Analysis/indivs.csv")
# write_csv(pacs, "Analysis/pacs.csv")

# 
# indivsOrgname <-
#   indivs %>% filter(!Catcode %in% fossilfuel) %>%
#   select(Orgname) %>% filter(!Orgname == "") %>% drop_na(Orgname) %>%
#   unique() %>% arrange(Orgname) %>% pull(Orgname)
# 
# indivsUltOrg <-
# indivs %>% filter(!Catcode %in% fossilfuel) %>%
#   select(UltOrg) %>% filter(!UltOrg == "") %>% drop_na(UltOrg) %>%
#   unique() %>% arrange(UltOrg) %>% pull(UltOrg)
# 
# write.csv(indivsOrgname, "Analysis/indivsOrgname.csv", row.names=F)
# write.csv(indivsUltOrg, "Analysis/indivsUltOrg.csv", row.names=F)
# 
# 
# indivs %>% filter(!Catcode %in% fossilfuel) %>%
#   select(UltOrg) %>% filter(!UltOrg == "")
# 
# 
# # indivs_names[stringr::str_detect(indivs_names, "koch")] %>%
# #   unique() %>% dput()
# 
# 
# indivs %>% filter(!Catcode %in% fossilfuel) %>% 
#   filter(tolower(Orgname) %in% c("exxon mobil"))
# 
# indivs %>% filter(!Catcode %in% fossilfuel) %>% 
#   filter(UltOrg %in% c("Exxon Mobil")) #%>% #as.tibble() %>% 
#   # print(n=200)

## Join and aggregate the contributions by MoC and congress --------------------

USCAP_indivs <- c("aes corp", "alstom power", "bp", "bp capital", "bp america",
                  "bp energy", "bp alaska", "bp exploration & oil",
                  "british petroleum", "cinergy corp", "conocophillips",
                  "conocophillips alaska", "duke energy", 
                  "exelon business services", "exelon corp",
                  "exelon energy","exelon generation", "fpl energy",
                  "fpl group", "florida power & light","ge energy",
                  "nrg energy", "pg&e national energy group", "pg&e corp",
                  "pg&e national energy", "pacific gas & electric", 
                  "pacific gas transmission", "pnm electric & gas service",
                  "pnm electric & gas services", "pnm resources",
                  "public service co of new mexico", "royal dutch shell",
                  "shell exploration & production", "shell global solutions us",
                  "shell oil", "siemens ag")

USCAP_pacs <- c("C00265595", "C00060103", "C00148031", "C00112896", "C00144824", 
                "C00091884", "C00083535", "C00385849", "C00141218", "C00024869", 
                "C00366559", "C00177469", "C00039503")

# CCCM_indivs_Orgname <-
#   c('american assn of blacks in e', 'american chemistry council',
#     'american council for capital f', 'american council for capital format',
#     'american enterprise institute', 'american legislative exchange council', 
#     'american legislative exchange council', 'american petroleum institute',
#     'americans for prosperity', 'apco worldwide', 'black chamber of commerce', 
#     'cato institute', 'center for strategic & intl studies',
#     'chamber of commerce', 'charles g koch charitable foundation', 'dci group',
#     'discovery institute', 'exxon mobil', 'federalist society', 'freedomworks',
#     'fti consulting', 'george c marshall institute', 
#     'george c. marshall institute', 'george mason university', 
#     'george mason university law school', 'global climate coalition',
#     'heartland institute', 'heritage foundation', 'hudson institute',
#     'illinois coal assn', 'insitute for energy research', 
#     'institute for energy research', 'institute for liberty',
#     'intermountain rural elec assoc', 'international republican institute',
#     'leadership institute', 'lindenwood university', 'linderwood university',
#     'media research center', 'national assn of manufacturers',
#     'national assn of manufacturers', 'national black chamber of commerce', 
#     'national center for policy ana/publ', 'national center for policy analysis',
#     'national chicken council', 'national mining assn',
#     'national pork prod council', 'national pork producers', 
#     'national rural electic cooperative ass', 'nera economic consulting', 
#     'pacific legal foundation', 'pacific research institute', 
#     'philanthropy roundtable', 'shook, hardy & bacon',
#     'thomas jefferson public policy institu', 'turning point', 
#     'us chamber of commerce', 'us grains council',
#     'us russian business council', 'western fuels assn',
#     'world affairs council & c', 'world affairs council of n ca')
# 
# CCCM_indivs_UltOrg <- c('apco worldwide', 'exxon mobil', 'fti consulting',
#                         'george mason university', 'koch industries',
#                         'national pork producers council')
  
# indivsUltOrg %>% tolower()
# indivsfossilorgnames <- indivs %>% filter(Catcode %in% fossilfuel) %>%
#   mutate(Orgname = tolower(Orgname)) %>%
#   unique() %>% arrange(Orgname) %>% pull(Orgname)
# 
# indivsfossilorgnames <- indivs %>% filter(Catcode %in% fossilfuel) %>%
#   mutate(UltOrg = tolower(UltOrg)) %>%
#   unique() %>% arrange(UltOrg) %>% pull(UltOrg)
# 
# USCAP_members %in% indivsfossilorgnames
# 
# indivsfossilorgnames[stringr::str_detect(indivsfossilorgnames, "xerox")] %>%
#   unique() %>% dput()
# 
# indivs[tolower(indivs$Orgname) %in%
#          c('siemens ag'),] %>% filter(Catcode %in% fossilfuel)
# 
# indivs[(indivs$UltOrg) %in%
#          c("Royal Dutch Shell"),] %>% filter(Catcode %in% fossilfuel) %>% 
#   select(Orgname) %>% unique()

# Summarise and bind contributions data
campaign_contributions <- rbind(
  # Individual contributions total
  indivs %>% mutate(Catcode = "total") %>%
    group_by(CID, Catcode, congress) %>%
    summarise(campaign_contributions = sum(Amount)),
  # Individual FFI contributions
  indivs %>% filter(Catcode %in% fossilfuel) %>%
    mutate(Catcode = "fossilfuel") %>% 
    group_by(CID, Catcode, congress) %>%
    summarise(campaign_contributions = sum(Amount)),
  # Individual FFI contributions without USCAP companies
  indivs %>% filter(Catcode %in% fossilfuel) %>%
    filter(!Orgname %in% USCAP_indivs) %>% 
    mutate(Catcode = "fossilfuel_noUSCAP") %>% 
    group_by(CID, Catcode, congress) %>%
    summarise(campaign_contributions = sum(Amount)),
  # # Individual CCCM contributions (excluding FFI)
  # indivs %>% filter(!Catcode %in% fossilfuel) %>%
  #   filter(tolower(Orgname) %in% CCCM_indivs_Orgname |
  #            tolower(UltOrg) %in% CCCM_indivs_UltOrg) %>% 
  #   mutate(Catcode = "CCCM") %>% 
  #   group_by(CID, Catcode, congress) %>%
  #   summarise(campaign_contributions = sum(Amount)),
  # PACs contributions total
  pacs %>% mutate(Catcode = "total") %>%
    group_by(CID, Catcode, congress) %>%
    summarise(campaign_contributions = sum(Amount)),
  # PACs contributions per category
  pacs %>% filter(Catcode %in% fossilfuel) %>%
    mutate(Catcode = "fossilfuel") %>% 
    group_by(CID, Catcode, congress) %>%
    summarise(campaign_contributions = sum(Amount)),
  # PACs contributions per category
  pacs %>% filter(Catcode %in% fossilfuel) %>%
    filter(!PACID %in% USCAP_pacs) %>% 
    mutate(Catcode = "fossilfuel_noUSCAP") %>% 
    group_by(CID, Catcode, congress) %>%
    summarise(campaign_contributions = sum(Amount))) %>% 
  # Summarise contributions by category and congress and reshape data
  group_by(CID, Catcode, congress) %>%
  summarise(campaign_contributions = sum(campaign_contributions)) %>%
  arrange(CID, congress, Catcode) %>%
  pivot_wider(names_from = Catcode, names_prefix = "cc_",
              values_from = campaign_contributions,
              values_fill = 0) %>% 
  filter(!is.na(congress))

campaign_contributions %>% 
  group_by(congress) %>% 
  summarise(across(c(cc_fossilfuel:cc_total), sum))

# Check result
campaign_contributions %>% describe()
# Check campaign_contributions per category
campaign_contributions[, 3:5] %>% colSums() %>% as.data.frame()
# cc_fossilfuel        64 010 962
# cc_total          1 487 876 001

# Calculate campaign_contributions proportions
campaign_contributions <- campaign_contributions %>% 
  mutate(per_cc_fossilfuel = cc_fossilfuel/cc_total*100,
         per_cc_fossilfuel_noUSCAP = cc_fossilfuel_noUSCAP/cc_total*100)

## Join campaign_contributions data to committee member data -------------------
committee_members <- left_join(committee_members, campaign_contributions,
                               by = c("CID", "congress")) %>% 
  mutate(across(cc_fossilfuel:per_cc_fossilfuel_noUSCAP, ~replace_na(.,0)))

# Inspect the data
committee_members %>% select_if(is.numeric) %>% describe(skew = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# F: Match the OpenSecrets Lobbying data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the raw lobbying data (quarterly lobbying reports)
lob_lobbying <- 
  read.table("OpenSecrets/BulkData/Lobby/lob_lobbying.txt", 
             sep = ",", quote = "|",
             col.names = c("Uniqid", "Registrant_raw", "Registrant", "Isfirm",
                           "Client_raw", "Client", "Ultorg", "Amount",
                           "Catcode", "Source", "Self", "IncludeNSFS", "Use",
                           "Ind", "year", "Type", "Typelong", "Affiliate")) %>% 
  filter(year %in% 2003:2010) %>% arrange(year, Catcode) %>%
  filter(Use == "y" & Ind == "y") %>%
  mutate(congress = case_when(
    year %in% 2003:2004 ~ 108, year %in% 2005:2006 ~ 109,
    year %in% 2007:2008 ~ 110, year %in% 2009:2010 ~ 111), 
    halfyear = case_when(
      Typelong %>% str_extract('\\w+-?\\w+') %in% 
        c("FIRST", "SECOND", "MID-YEAR") ~ "H1",
      Typelong %>% str_extract('\\w+-?\\w+') %in%  
        c("THIRD", "FOURTH", "YEAR-END") ~ "H2"))


## Join and investigate the lobbying issue data --------------------------------
# Load the lobbying issue description data
lob_issue <- 
  read.table("OpenSecrets/BulkData/Lobby/lob_issue.txt", 
             sep = ",", quote = "|", encoding="latin1")

# Subset lobbying data by the FFI
lob_fossil <- lob_lobbying %>% filter(Catcode %in% fossilfuel)

# Merge the issues to the lobbying reports data
lob_fossil_issue <- merge(lob_fossil, lob_issue, by.x = "Uniqid", by.y = "V2")

# Match all reports related to climate change and cap-and-trade policy
climate_regex = "climate|global warming|greenhouse|ghg|emissions|co2|carbon"
capandtrade_regex = "cap(-| )?and(-| )?(trade|auction|dividend|tax)|cap on (the amount of )?(carbon|co2|emission)|carbon limits|climate income|emissions?(-| )?(cap|limit|restriction|standard|trading)"
policy_regex = "(4067|759|620|1590|4226|1862|2454|2998)\\D|(139|2028|843|150|342|1151|3698|280|317|485|1201|1766|2191|3036|1733|2877)\\D|waxman|lieberman(-| )?warner|clean (air|energy|power)|energy (\\w+ ){0,3}(policy|policies|legislation|bill)|regulatory (and legislative )?(matters|issues|policy)"
lob_fossil_issue_cc <- lob_fossil_issue %>%
  filter(
    grepl(climate_regex, tolower(V5), perl = T) |
      grepl(capandtrade_regex, tolower(V5), perl = T) |
      grepl(policy_regex, tolower(V5), perl = T)
  ) %>%
  mutate(Issue = "Issue match")

length(unique(lob_fossil_issue_cc$Uniqid))
# 4102 reports containing one or more of the keywords were matched
length(unique(lob_fossil_issue_cc$Uniqid)) /nrow(lob_fossil)
# That is around 35% of all FFI lobbying reports in the data

# other_regex = "keystone"
# lob_fossil_issue_cc <- lob_fossil_issue %>% 
#   filter(grepl(other_regex, tolower(V5), perl=T))
# nrow(lob_fossil_issue_cc) 
# lob_fossil_issue_cc$V5

# Check if the remaining rows don't match or have missing issue data
lob_fossil_issue_not_cc <- lob_fossil_issue %>% 
  filter(!Uniqid %in% lob_fossil_issue_cc$Uniqid) %>% 
  mutate(Issue = ifelse(V5=="", "Missing", "No issue match")) %>% 
  arrange(Uniqid, desc(Issue == 'Not matched')) %>% 
  distinct(Uniqid, .keep_all = TRUE)

table(lob_fossil_issue_not_cc$Issue)

lob_fossil <- lob_fossil %>%
  mutate(
    Issue = ifelse(
      Uniqid %in% lob_fossil_issue_cc$Uniqid,
      "Issue match",
      ifelse(Uniqid %in%
          lob_fossil_issue_not_cc$Uniqid[lob_fossil_issue_not_cc$Issue ==
                                           "No issue match"],
        "No issue match",
        ifelse(!Uniqid %in% lob_issue$V2,
               "NA: no matching Uniqid",
               "NA: no issue description"))),
    Issue = factor(Issue, levels = c( "NA: no matching Uniqid",
                                      "NA: no issue description",
                                      "No issue match", 
                                      "Issue match")),
    Missing = ifelse(Issue %in% c("NA: no matching Uniqid",
                   "NA: no issue description"), "Yes", "No"))

rm(lob_fossil_issue,
   lob_fossil_issue_cc,
   lob_fossil_issue_not_cc)

# Investigate 
table(lob_fossil$Issue)
round(prop.table(table(lob_fossil$Issue)), 2)

# Summary of matching the issue data to the 11855 rows of lobbying data
# 929   rows can't be matched to the issue data by Uniqid
# 2623  rows can be matched by Uniqid, but have missing data on the issue
# 4201  rows can be matched by Uniqid, but not by keyword
# 4102  rows can be matched by Uniqid and by keyword

### Investigate systematic pattern of issue missingness over time --------------

round(prop.table(table(lob_fossil$congress, lob_fossil$Missing), 1), 2)

lob_fossil %>%
  subset(Missing == "No") %>% 
  group_by(Issue) %>% 
  summarise(Amount = sum(Amount),
            Reports = n()) %>% 
  ungroup() %>% 
  mutate(Amount_prop = Amount/sum(Amount),
         Reports_prop = Reports/sum(Reports))

ggarrange(
  ggplot(lob_fossil, aes(as.factor(year), fill = Issue)) +
    geom_bar(stat = "count") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_fill_manual(
      values = c("#2E9FDF", "#E7B800", "#FC4E07", '#8B0000'),
      breaks = c( "Issue match", "No issue match",
                  "NA: no issue description", "NA: no matching Uniqid")) +
    labs(x = "Year", y = "Nr. of Reports"),
  
  lob_fossil %>% group_by(Issue, year) %>% 
    summarise(Amount = sum(Amount)) %>% 
    ggplot(aes(as.factor(year), Amount, fill = Issue)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_fill_manual(
      values = c("#2E9FDF", "#E7B800", "#FC4E07", '#8B0000'),
      breaks = c( "Issue match", "No issue match",
                  "NA: no issue description", "NA: no matching Uniqid")) +
    labs(x = "Year", y = "Total lobbying expenditures"),
  ncol = 1)

## Subset and investigate the USCAP lobbying expenditures data -----------------

USCAP_lob_lobbying <- c("aes corp", "alstom", "bp", "cinergy corp",
                        "conocophillips", "duke energy", "exelon corp",
                        "fpl group", "nrg energy", "pg&e corp", "pnm resources",
                        "royal dutch shell", 
                        "royal dutch/shell group of companies", "siemens ag")

lob_fossil <- lob_fossil %>%
  mutate(USCAP = ifelse(tolower(Ultorg) %in% USCAP_lob_lobbying, "Yes", "No"))
  
### Investigate USCAP vs not USCAP top contributions over time -----------------

lob_fossil %>%
  group_by(year, congress, USCAP) %>%
  summarise(lobbying_fossilfuel = sum(Amount)) %>% 
  pivot_wider(names_from = USCAP, values_from = lobbying_fossilfuel) %>% 
  rename("FFIlobbying_notUSCAP" = "No", "FFIlobbying_USCAP" = "Yes") %>% 
  mutate(FFIlobbying_total = FFIlobbying_notUSCAP + FFIlobbying_USCAP,
         FFIlobbying_USCAP_prop = FFIlobbying_USCAP/FFIlobbying_total)

ggarrange(
  lob_fossil %>%
    group_by(year, congress, USCAP) %>%
    summarise(lobbying_fossilfuel = sum(Amount)) %>% 
    ggplot(aes(year, lobbying_fossilfuel, color = USCAP)) + geom_line(lwd=1) +
    theme_bw() +
    labs(x = "Year", y = "Total lobbying expenditures", color = "USCAP members"),
  lob_fossil %>% 
    filter(USCAP == "Yes") %>%
    group_by(Ultorg) %>% 
    mutate(total = sum(Amount)) %>% 
    ungroup() %>%
    mutate(Ultorg = ifelse(total<10000000, "Other", Ultorg)) %>% 
    group_by(year, congress, Ultorg) %>%
    summarise(lobbying_fossilfuel = sum(Amount)) %>%
    filter(!Ultorg=="Other") %>% 
    ggplot(aes(year, lobbying_fossilfuel, color = Ultorg)) +
    geom_line(lwd=1) + 
    theme_bw() +
    labs(x = "Year", y = "Total lobbying expenditures",
         color = "TOP 7 USCAP members"),
  lob_fossil %>%
    filter(USCAP == "No") %>% 
    group_by(Ultorg) %>% 
    mutate(total = sum(Amount)) %>% 
    ungroup() %>%
    mutate(Ultorg = ifelse(total<35000000, "Other", Ultorg),
           Ultorg = ifelse(Ultorg == "American Petroleum Institute",
                           "American Petroleum\nInstitute", Ultorg),
           Ultorg = ifelse(Ultorg == "Edison Electric Institute",
                           "Edison Electric\nInstitute", Ultorg)) %>% 
    group_by(year, congress, Ultorg) %>%
    summarise(lobbying_fossilfuel = sum(Amount)) %>% 
    filter(!Ultorg=="Other") %>% 
    ggplot(aes(year, lobbying_fossilfuel, color = Ultorg)) + 
    geom_line(lwd=1) + 
    theme_bw() +
    labs(x = "Year", y = "Total lobbying expenditures", 
         color = "TOP 7 other FFI companies"),
  ncol = 1, align = "v")

## Join the lobbying expenditures data to the MoC data -------------------------
  
lobbying <-
  merge(lob_fossil %>%
          ### Uncomment line below to subset issue matched data
          # filter(Issue == "Issue match") %>%
          group_by(halfyear, year, congress) %>%
          summarise(lobbying_fossilfuel = sum(Amount)),
        lob_lobbying %>%
          group_by(halfyear, year, congress) %>%
          summarise(lobbying_total = sum(Amount))) %>%
  mutate(per_lobbying_fossilfuel = lobbying_fossilfuel/lobbying_total*100)


lobbying_noUSCAP <-
  merge(lob_fossil %>%
          ### Uncomment line below to subset issue matched data
          # filter(Issue == "Issue match") %>%
          filter(USCAP == "Yes") %>% 
          group_by(halfyear, year, congress) %>%
          summarise(lobbying_fossilfuel_noUSCAP = sum(Amount)),
        lob_lobbying %>%
          group_by(halfyear, year, congress) %>%
          summarise(lobbying_total = sum(Amount))) %>%  
  mutate(per_lobbying_fossilfuel_noUSCAP = lobbying_fossilfuel_noUSCAP/lobbying_total*100)

# Match the lobbying expenditures with the committee member data
committee_members$halfyear <- ifelse(
  as.numeric(format(committee_members$date,"%m")) <= 6, "H1", "H2")

committee_members <- left_join(committee_members, 
                               lobbying,
                               by = c("halfyear", "year", "congress"))
committee_members <- left_join(committee_members, 
                               lobbying_noUSCAP,
                               by = c("halfyear", "year", "congress", "lobbying_total"))

# Plot fossil fuel and alternative lobbying over time
ggpubr::ggarrange(
  committee_members %>% group_by(congress) %>%
    summarise(lobbying_fossilfuel = sum(lobbying_fossilfuel)) %>%
    ggplot(aes(congress, lobbying_fossilfuel)) + geom_point(),
  committee_members %>% group_by(year) %>%
    summarise(lobbying_fossilfuel = sum(lobbying_fossilfuel)) %>%
    ggplot(aes(year, lobbying_fossilfuel)) + geom_point(),
  committee_members %>% mutate(term = paste0(year, halfyear)) %>%
    group_by(term) %>%
    summarise(lobbying_fossilfuel = sum(lobbying_fossilfuel)) %>%
    ggplot(aes(term, lobbying_fossilfuel)) + geom_point(),
  nrow = 3)

# Check final data -------------------------------------------------------------
committee_members %>% select_if(is.numeric) %>% describe(skew = F)

# Save data --------------------------------------------------------------------
write_csv(committee_members, 
          "Analysis/hearings_committee_member_level.csv")



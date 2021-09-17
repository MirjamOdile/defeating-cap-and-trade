library(readxl)     # excel files
library(tidyverse)  # data handling
options(dplyr.summarise.inform = FALSE)
library(magrittr)   # piping
library(stringr)    # text data handling
library(stringdist) # matching
library(psych)      # describe data
library(beepr)      # play notification sound
library(GGally)     # pairs plots

#-------------------------------------------------------------------------------
# This script matches the 
# (A) committee information of the climate change relevant hearings 
#     from 2003 to 2010 with 
# (B) the witness information per hearing (n/%), 
# (C) the committee assignments at the time based on the Congressional 
#     Committees data compiled by Charles Stewart III and Jonathan Woon (MIT) 
#     (http://web.mit.edu/17.251/www/data_page.html#2), 
# (D) the OpenSecrets committee member names and identification codes,
# (E) the OpenSecrets Individual Contributions data for relevant industries, and
# (F) the district level employment rates in relevant sectors according to the
#     North American Industry Classification System (NAICS)
#-------------------------------------------------------------------------------
setwd("~/Dropbox/Article_Analysis/")

#-------------------------------------------------------------------------------
# (A) Committees of the climate change relevant hearings from 2003 to 2010 
#-------------------------------------------------------------------------------

# Load hearings with committee information
committees <- read.csv('Data/06_hearings.csv', 
               sep = "\t") %>% 
  mutate(date = as.Date(date, format = '%B %d, %Y'),
         hearing.id = factor(hearing.id),
         chamber = factor(ifelse(grepl('shrg', hearing.id), 'Senate', 'House')),
         committee_short = tolower(str_replace(committee, 
                                               '.+? on (?:the )?', '')),
         committee_short = ifelse(committee_short == 'joint economic committee',
                                  "economic", committee_short),
         majority = factor(case_when(
           congress %in% c(108, 109) ~ 'R',
           congress %in% c(110, 111) ~ 'D')))

#-------------------------------------------------------------------------------
# (B) Merge committee data with witness information for the respective hearing
#-------------------------------------------------------------------------------

# Load witness data
witnesses <- read.csv("Data/06_witnesses_aggregated.csv", 
                      sep = "\t") %>% 
  mutate(date = as.Date(date, format = '%B %d, %Y'),
         hearing.id = factor(hearing.id),
         chamber = factor(ifelse(grepl('shrg', hearing.id), 'Senate', 'House')))

# Remove nuclear electric utilities from the electric utilities industry and
# add them to alternate energy production instead:
# nuclear, NRG Energy
table(witnesses$subcategory)
witnesses$subcategory <- ifelse(witnesses$industry == "Electric Utilities" &
  str_detect(tolower(witnesses$affiliation), 'nuclear|nrg energy') == T, 
         "Alternate Energy Production & Services", witnesses$subcategory)
table(witnesses$subcategory)
table(witnesses$industry)
witnesses$industry <- ifelse(witnesses$industry == "Electric Utilities" &
  str_detect(tolower(witnesses$affiliation), 'nuclear|nrg energy') == T, 
         "Alternate energy production & services", witnesses$industry)
table(witnesses$industry)

# 2do >> change in witness matching script

# witnesses_per_hearing <- witnesses %>% 
#   group_by(hearing.id, category) %>% summarise(n = n()) %>% 
#   mutate(per = prop.table(n)) %>% group_by(hearing.id) %>% 
#   mutate(n_witnesses = sum(n)) %>% 
#   pivot_wider(names_from=category, values_from=c(n, per)) %>%
#   setNames(make.names(names(.))) %>% 
#   replace(is.na(.), 0)

witnesses$subcategory.contrarians <- ifelse(witnesses$category == "Contrarians", 
                                            witnesses$category, 
                                            witnesses$subcategory)
witnesses_per_hearing <- witnesses %>%
  group_by(hearing.id, subcategory.contrarians) %>% summarise(n = n()) %>%
  mutate(per = prop.table(n)) %>% group_by(hearing.id) %>%
  mutate(n_witnesses = sum(n)) %>%
  pivot_wider(names_from=subcategory.contrarians, values_from=c(n, per)) %>%
  setNames(make.names(names(.))) %>%
  rename(n_Alternate.Energy = n_Alternate.Energy.Production...Services,
         per_Alternate.Energy = per_Alternate.Energy.Production...Services) %>% 
  replace(is.na(.), 0)

committees <- merge(committees, witnesses_per_hearing, by = 'hearing.id')
rm(witnesses, witnesses_per_hearing)

# Save the data
# write_csv(committees, 'Data/07_committees.csv')


#------------------------------------------------------------------------------
# (C) Match the committee assignments of each committee
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Notes regarding the Congressional Committees data: 
#
# 1) Two House committee names were coded wrongly and are adapted in the
#    code below. Namely:
#
#    House Committee on Government Reform for the 108th Congress (2003/2004)
#    In the 106th Congress, the panel was renamed the Committee on Government 
#    Reform. [...] On January 4, 2007, the 110th Congress renamed it the 
#    Committee on Oversight and Government Reform.
#    https://en.wikipedia.org/wiki/United_States_House_Committee_on_Oversight_and_Reform
#
#    House Committee on Science for the 108th Congress (2003/2004)
#    After the Republican Party gained a majority in Congress in 1994, the name of
#    the committee was changed to the House Committee on Science. With the return 
#    of control to the Democrats in 2007, the committee's name was changed back to 
#    the House Committee on Science and Technology. 
#    https://en.wikipedia.org/wiki/United_States_House_Committee_on_Science,_Space,_and_Technology
#
# 2) Some variables are renamed to unify the House and Senate assignment csv 
#    files
#-------------------------------------------------------------------------------

# Load House comittee data
house_assignments <- 
  read_xls("Data/StewartWoon/house_assignments_103-115-3.xls", 
           col_types = c(rep("guess", 15), rep("text", 5))) %>% 
  setNames(make.names(names(.))) %>% 
  rename(ID = ID..,
         Committee.Code = Committee.code,
         Party.Code = Party,
         state.icpsr = State,
         cd_code = CD,
         stab = State.Name) %>% 
  mutate(Committee.Name = str_to_title(Committee.Name),
         Committee.Name = ifelse(Congress == 108 & Committee.Name == 
                                   "Government Reform And Oversight",
                                 "Government Reform", Committee.Name),
         Committee.Name = ifelse(Congress == 108 & Committee.Name == 
                                   "Science And Technology",
                                 "Science", Committee.Name), 
         committee_short = tolower(str_replace(Committee.Name, 
                                               ' \\([^)]*\\)', '')),
         Date.of.Assignment = as.Date(Date.of.Assignment),
         Date.of.Termination = as.Date(Date.of.Termination)) %>% 
  filter_all(any_vars(!is.na(.)))  %>%
  filter(Congress %in% c(108, 109, 110, 111))

# Load Senate comittee data
senate_assignments <- 
  read_xls("Data/StewartWoon/senate_assignments_103-115-3.xls", 
           col_types = c(rep("guess", 16), rep("text", 5)))[,c(1:9, 11:21)] %>% 
  setNames(make.names(names(.))) %>% 
  rename(ID = ID..,
         Rank.Within.Party.Status = Rank.Within.Party,
         Date.of.Assignment = Date.of.Appointment,
         state.icpsr = State.Code,
         cd_code = District, 
         stab = State.Name) %>% 
  mutate(Committee.Name = str_to_title(Committee.Name),
         committee_short = tolower(str_replace(Committee.Name, 
                                               ' \\([^)]*\\)', '')),
         Date.of.Assignment = as.Date(Date.of.Assignment),
         Date.of.Termination = as.Date(Date.of.Termination)) %>% 
  filter(Congress %in% c(108, 109, 110, 111)) # subset correct timespan

# Join the House of Representatives
house_members <- left_join(committees[committees$chamber == 'House',], 
                           house_assignments,
                       by = c('congress' = 'Congress',
                              'committee_short' = 'committee_short'))

## Senate
senate_members <- left_join(committees[committees$chamber == 'Senate',], 
                            senate_assignments,
                            by = c('congress' = 'Congress',
                                   'committee_short' = 'committee_short'))

## Merge House and Senate data
committee_members <- rbind(house_members, senate_members)
glimpse(committee_members)

# Remove unneccesary objetcs
rm(house_assignments, senate_assignments, house_members, senate_members)

## Was the committee member assigned when the hearing took place?
nrow(committee_members[committee_members$date < 
                         committee_members$Date.of.Assignment,])
# >> 323 MoCs had not yet joined the committee when the hearing took place

## Mark if a MoC was member at time of hearing or not
committee_members$not_member_yet <- 
  ifelse(committee_members$date < committee_members$Date.of.Assignment, 1, 0)

## Inspect the data
describe(committee_members[c(3:28, 31:43)], skew = F)
committee_members %>% select_if(is.numeric) %>% describe(skew = F)


#-------------------------------------------------------------------------------
# (D) Match the OpenSecrets committee member identification codes (CID)
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
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
#             we've worked backwards from history.house.gov's Women in Congress 
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
# ------------------------------------------------------------------------------
# CRP IDs codebook:
#
# CID         Unique identifier for each candidate. Every candidate should have 
#             one and only one CID throughout all cycles. CID for candidates 
#             is based on CID.
#             Note: If starting with N it refers to the MoCs time as a member, 
#             if starting with C it refers to them as a candidate.
# ------------------------------------------------------------------------------

legislators <- rbind(
  read.csv("Data/congress-legislators/legislators-historical.csv"),
  read.csv("Data/congress-legislators/legislators-current.csv"))

committee_members$CID <- committee_members %$%
         legislators$opensecrets_id[match(ID, legislators$icpsr_id)]
committee_members <- committee_members %>%
  mutate(# Replace missing CIDs
         CID = replace(CID, ID == 70601, "N00009825"),
         CID = replace(CID, ID == 70701, "N00024866"),
         CID = replace(CID, ID == 70501, "N00000133"),
         CID = replace(CID, ID == 70302, "N00007632"),
         CID = replace(CID, ID == 94828, "N00005645"),
         CID = replace(CID, ID == 14240, "N00000534"),
         CID = replace(CID, ID == 70303, "N00001692"),
         CID = replace(CID, ID == 29387, "N00003730"),
         CID = replace(CID, ID == 94679, "N00005372"),
         CID = replace(CID, ID == 70801, "N00026264"),
         CID = replace(CID, ID == 20901, "N00029917"),
         CID = replace(CID, ID == 90901, "N00029917"),
         CID = replace(CID, ID == 70805, "N00029168"),
         CID = replace(CID, ID == 70810, "N00030418"),
         # Correct wrongly assigned CID
         CID = replace(CID, Name == "Deutch, Theodore E." & CID == "N00002839",
                        "N00031317"),
         # Correct wrongly assigned district
         cd_code = replace(cd_code, Name == "Thomas, William M.", 22))

# Match identification codes to committee members
committee_members <- 
  left_join(committee_members, 
            legislators[,c("last_name", "first_name", "birthday", "gender", 
                           "opensecrets_id", "bioguide_id", "govtrack_id")],
            by = c("CID" = "opensecrets_id"))

# Drop legislators data
rm(legislators)

#-------------------------------------------------------------------------------
# (E) Match the OpenSecrets Individual Contributions data 
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# CRP Categories Codebook:
# 
# Catcode	      OpenSecrets category code
# Catname       OpenSecrets category name	(Lv. 1)
# Catorder  	  OpenSecrets industry code
# Industry	    OpenSecrets industry name (Lv. 2)
# Sector        OpenSecrets sector name	(abbr.)
# Sector.Long   OpenSecrets sector name (Lv. 3)
# ------------------------------------------------------------------------------

CRP_Codes <- read.delim("Data/OpenSecrets/CRP_Categories.txt",
           header = TRUE, skip = 8, sep = "\t")
CTTs <- read.csv("Data/Brulle2014/Brulle_S3_matched.csv", header = F) %>% 
  rename(Name = V1) %>% 
  mutate(Name = str_trim(Name))
foundations <- read.csv("Data/Brulle2014/Brulle_S6_matched.csv", header = F) %>% 
  rename(Name = V1) %>% 
  mutate(Name = str_trim(Name))

# List of industries (Catorder) in a sector (Sector)
CRP_Codes %>% select(Sector, Catorder, Industry, Catcode, Catname) %>% 
  unique() %>% arrange(Sector, Catorder) %>% as_tibble %>% print(n = Inf)

CRP_Codes %>% filter(Sector == "Energy/Nat Resource") %>% 
  select(Catorder, Industry, Catcode, Catname) %>% unique() %>% 
  arrange(Catorder, Catcode)
CRP_Codes %>% filter(Sector == "Ideology/Single-Issue") %>% 
  select(Catorder, Industry, Catcode, Catname) %>% unique() %>% 
  arrange(Catorder, Catcode)

# Select relevant industries
# Fossil fuel industry
fossil.fuel.industry <- 
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
# Alternate energy industry
alternate.energy <- 
  c("E1500",          #                   Alternate energy production & services
    "E1300",          #                                           Nuclear energy
    "E1320",          #             Nuclear plant construction, equipment & svcs
    "E5000"           #                                          Water Utilities
    )
# Environmental movement
# E0000   Energy, Natural Resources and Environment ??
environmental <- 
  c("JE300"           #                                     Environmental policy
    ) 
# Republican/conservative
CCCM <- unique(append(CTTs$Name, foundations$Name))

## Catcodes for all relevant industries
relevant.industries <- c(fossil.fuel.industry, alternate.energy, 
                         environmental)#, republican.conservative)



# ------------------------------------------------------------------------------
# Center for Responsive Politics (CRP | OpenSecrets) Contributions Data
# 
# Individual Contributions (indivs):
# 
# RecipID	    The recipient's id number. If the contribution is to a candidate 
# (CID)       this will be the candidate's unique candidate id number (CID). 
#             Otherwise, it will be the FEC committee id number.
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
# ------------------------------------------------------------------------------


# Define for which Congresses data will be loaded
congresses <- c(108:111)
# Define the matching years as listed in the data names
years <- c("04", "06", "08", "10")
# Define directory
dir <- "Data/OpenSecrets/CampaignFin"
# Create empty data frames to save data
indivs <- data.frame()
pacs <- data.frame()
pac_other <- data.frame()
income <- data.frame()
# Load contributions data
for (i in 1:length(congresses)) {
  path = paste0(dir, years[i], "/cmtes", years[i], ".txt")
  # Load FEC Committee table
  cmtes <- read.table(path, header = FALSE, quote = "|", sep = ",",
                      colClasses = c("NULL", NA, rep("NULL", 5), NA,
                                     "NULL", NA, rep("NULL", 3))) %>% 
    rename(CmteID = V2, FeCCandID = V8, PrimCode = V10)
  # Load Individual Contributions
  path = paste0(dir, years[i], "/indivs", years[i], ".txt")
  indivs.temp <- left_join(
    read.table(path, header = FALSE, quote = "|", sep = ",",
               colClasses = c(rep("NULL", 3), rep(NA, 7), rep("NULL", 5),
                              NA, NA, rep("NULL", 3), NA, NA, "NULL")) %>% 
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
           SectorCode = str_extract(Catorder, "[:alpha:]+")) 
  # Load Pac Contributions  
  path = paste0(dir, years[i], "/pacs", years[i], ".txt")
  pacs.temp <- read.table(path, header = FALSE, quote = "|", sep = ",",
                   colClasses = c(rep("NULL", 3), rep(NA, 6), "NULL")) %>% 
  rename(CID = V4, Amount = V5, Date = V6, RealCode = V7, Type = V8, DI = V9) %>%
  mutate(RealCode = toupper(RealCode)) %>% 
  filter(CID %in% committee_members$CID[committee_members$congress == 
                                          congresses[i]]) %>%
  filter(!startsWith(RealCode, "Z4") & !startsWith(RealCode, "Z9")) %>%
  filter(DI == "D") %>% 
  mutate(Catcode = CRP_Codes$Catcode[match(RealCode, CRP_Codes$Catcode)],
         Catorder = CRP_Codes$Catorder[match(RealCode, CRP_Codes$Catcode)],
         SectorCode = str_extract(Catorder, "[:alpha:]+"))
  # Load Pac Other Contributions
  path = paste0(dir, years[i], "/pac_other", years[i], ".txt")
  pac_other.temp <- read.table(path, header = FALSE, quote = "|", sep = ",",
                          colClasses = c(rep("NULL", 10), rep(NA, 3),
                                         rep("NULL", 8), rep(NA, 2), "NULL")) %>% 
  rename(Date = V11, Amount = V12, CID = V13, Type = V22, RealCode = V23) %>% 
  filter(CID %in% committee_members$CID[committee_members$congress == 
                                          congresses[i]]) %>%
  filter(Type %in% c("22Z", "24K", "24R", "24Z")) %>%
  filter(!startsWith(RealCode, "Z4") & !startsWith(RealCode, "Z9")) %>%
  mutate(Catcode = CRP_Codes$Catcode[match(RealCode, CRP_Codes$Catcode)],
         Catorder = CRP_Codes$Catorder[match(RealCode, CRP_Codes$Catcode)],
         SectorCode = str_extract(Catorder, "[:alpha:]+"))
  # Save contributions
  indivs <- rbind(indivs, indivs.temp)
  pacs <- rbind(pacs, pacs.temp)
  pac_other <- rbind(pac_other, pac_other.temp)
  # Summarise and bind contributions data 
  income.temp <- rbind(
      # Individual contributions total
      indivs.temp %>% mutate(Catcode = "_total") %>% group_by(CID, Catcode) %>%
        summarise(income = sum(Amount)),
      # Individual contributions per category
      indivs.temp %>% filter(Catcode %in% relevant.industries) %>%  
        group_by(CID, Catcode) %>% summarise(income = sum(Amount)),
      # Individual contributions by CCCM
      indivs.temp %>% filter(! Catcode %in% fossil.fuel.industry) %>% 
        filter(tolower(Orgname) %in% tolower(CCCM) |
               tolower(Employer) %in% tolower(CCCM)) %>%  
        group_by(CID) %>% summarise(income = sum(Amount), Catcode = "CCCM"),
      # PACs contributions total
      pacs.temp %>% mutate(Catcode = "_total") %>% group_by(CID, Catcode) %>%
        summarise(income = sum(Amount)),
      # PACs contributions per category
      pacs.temp %>% filter(Catcode %in% relevant.industries) %>%  
        group_by(CID, Catcode) %>% summarise(income = sum(Amount)),
      # PACs other contributions income
      pac_other.temp %>% mutate(Catcode = "_total") %>% 
        group_by(CID, Catcode) %>% summarise(income = sum(Amount)),
      # PACs other contributions per category
      pac_other.temp %>% 
        filter(Catcode %in% relevant.industries) %>% group_by(CID, Catcode) %>% 
        summarise(income = sum(Amount))) %>%
  # Summarise contributions by parent category and reshape data
  replace(is.na(.), 0) %>% 
  group_by(CID, Catcode) %>%
  mutate(Catcode = replace(Catcode, Catcode %in% fossil.fuel.industry, 
                            "fossil.fuel.industry"),
         Catcode = replace(Catcode, Catcode %in% alternate.energy, 
                            "alternate.energy"),
         Catcode = replace(Catcode, Catcode %in% environmental, 
                            "environmental")) %>% 
         # Catcode = replace(Catcode, Catcode %in% republican.conservative, 
         #                    "republican.conservative")) %>% 
  summarise(income = sum(income)) %>% 
  arrange(Catcode) %>% 
  pivot_wider(names_from = Catcode, names_prefix = "inc_",
              values_from = income,
              values_fill = 0) %>%
  mutate(congress = congresses[i])
  income = rbind(income, income.temp)
  print(paste0('Income for ', congresses[i], "th Congress processed. (",
               i, "/", length(congresses), ")"))
}
beep()
rm(dir, path, congresses, years, i, cmtes, indivs.temp, pacs.temp, pac_other.temp, income.temp)

# Check income per catgory
income[, 2:7] %>% colSums()
      
# Calculate income proportions
income <- income %>% 
  rename(inc_total = inc__total,
         inc_fossil.fuel = inc_fossil.fuel.industry) %>% 
  mutate(across(c(2:5), .fns = ~./inc_total*100, .names = "per_{col}"))

glimpse(income)

# Join income data to committe member data
committee_members <- left_join(committee_members, income,
                               by = c("CID", "congress"))
# Drop individual income data frames
# rm(income108, income109, income110, income111)
# Check the new data
describe(committee_members[51:66], skew = F)
# Save the data
# write_csv(committee_members, 'Data/07_committees_members.csv')


# income[income$CID == "N00000019",] # Clinton, Hillary
# sum(income$inc_CCCM[income$CID == "N00000019"][1:3])
# # 3000 >> correct
# 
# income[income$CID == "N00006424",] # McCain, John
# sum(income$inc_CCCM[income$CID == "N00006424"][1:3])
# # without presidential funding: 60,155
# #   - too much for Campaign Committee & Leadership PAC Combined (16,500)
# #   - too much for only Campaign Committee (3,000)
# indivs[indivs$CID == "N00006424" & indivs$RealCode %in% republican.conservative,]
# pacs[pacs$CID == "N00006424" & pacs$RealCode %in% republican.conservative,] %>% select(Amount) %>% sum()
# pac_other[pac_other$CID == "N00006424" & pac_other$RealCode %in% republican.conservative,]
# 
# income[income$CID == "N00009888",] # Alexander, Lamar 
# sum(income$inc_CCCM[income$CID == "N00009888"][1:3])
# # without presidential funding: 18,850
# #   - too little for Campaign Committee & Leadership PAC Combined (26,500)
# #   - too little for only Campaign Committee (26,500)
# 
# # Ehlers, Vernon
# income[income$CID == "N00004166",] 
# sum(income$inc_CCCM[income$CID == "N00004166"][1]) # 250 >> correct 
# sum(income$inc_CCCM[income$CID == "N00004166"][2]) # 4,650 >> correct
# sum(income$inc_CCCM[income$CID == "N00004166"][3]) # 14,000 >> too little!
# # Jones, Walter B.: 
# income[income$CID == "N00002299",] # 1,750 >> correct
# # Everett, Terry
# income[income$CID == "N00003083",] # 0 >> correct
# # Carson, Brad
# income[income$CID == "N00009704",] # 0 >> correct


#-------------------------------------------------------------------------------
# Match House district information
#-------------------------------------------------------------------------------

# Load district data
districts <- read.csv("~/Data/QCEW/QCEW_congressional_districts_employment.csv")[,2:16] %>%
  mutate(emp_fossil.fuel = select(., emp.211:emp.221112) %>% rowSums()) %>% 
  group_by(state, stab, congress, cd_code, congressionaldistrict) %>% 
  summarise(emp_total = sum(emp.10),
            emp_fossil.fuel = sum(emp_fossil.fuel)) %>% 
  mutate(per_emp_fossil.fuel = emp_fossil.fuel/emp_total*100)
# Replace single congressional district states cd_code with 1 instead of 0 to match committee data
districts$cd_code[districts$stab %in% c("MT", "ND", "SD", "VT", "WY")] <- 1
# Replace non-voting member states' cd_code with 79 (Delegate) to match committee data
districts$cd_code[districts$stab %in% c("DC", "VI")] <- 79
# Replace non-voting member states' cd_code with 80 (Resident Commissioner) committee data
districts$cd_code[districts$stab %in% c("PR")] <- 80

# Subset House data
house_members_districts <- subset(committee_members, chamber == "House") %>% 
  mutate(state.icpsr = as.numeric(state.icpsr),
         cd_code = as.numeric(cd_code))
# Replace non-voting member states' cd_code with 79 (Delegate) where wrongly coded as 1
house_members_districts$cd_code[house_members_districts$stab %in% c("VI")] <- 79
# Changes congressional district information due to redistricting
house_members_districts$cd_code[281] <- 14 # Steve LaTourette: Redistricted to the 14th district. https://en.wikipedia.org/wiki/Steve_LaTourette

# Join House and district data
house_members_districts <- left_join(house_members_districts, districts,
          by = c('congress' = 'congress',
                 'stab' = 'stab',
                 'cd_code' = 'cd_code')) %>%
  rename(area_title = congressionaldistrict)

# Check data
describe(house_members_districts, skew = F)
# Missing employment data (39 observations):
house_members_districts$stab[is.na(house_members_districts$emp_total)] %>% unique() %>% sort()
# All are delegates from US territories (American Samoa, Guam, the Northern Mariana Islands) and
# non-voting members of the United States House of Representatives
districts[districts$stab %in% c("AS","GU","MP"),] 
# >> No employment information available for these territories. Leave as NA.

#-------------------------------------------------------------------------------
# Match Senate state information
#-------------------------------------------------------------------------------

# Load stata data
states <- read.csv("~/Data/QCEW/QCEW_states_employment.csv")[,2:15]  %>% 
  mutate(emp_fossil.fuel = select(., emp.211:emp.221112) %>% rowSums()) %>% 
  group_by(state, area_title, congress) %>%
  summarise(emp_total = sum(emp.10),
            emp_fossil.fuel = sum(emp_fossil.fuel)) %>% 
  mutate(per_emp_fossil.fuel = emp_fossil.fuel/emp_total*100)

# Join state 2-digit state abbreviations (stab)
state.icpsrs <- read.table("Data/StewartWoon/ICPSR_state_codes.txt", sep = ",", header = T) %>% 
  mutate(state.name = str_trim(tolower(state.name)))
states$stab <- str_trim(state.icpsrs$state.abb[match(tolower(states$area_title), state.icpsrs$state.name)])
states$stab[states$area_title == "Puerto Rico"] <- "PR"
states$stab[states$area_title == "Virgin Islands"] <- "VI"
rm(state.icpsrs)

# Join state data
senate_members_states <- subset(committee_members, chamber == "Senate") %>% 
  mutate(state.icpsr = as.numeric(state.icpsr))
senate_members_states <- left_join(senate_members_states, states,
          by = c('congress' = 'congress',
                 'stab' = 'stab'))

# Check data
describe(senate_members_states, skew = F)

# write_csv(senate_members_states,
#           'Data/07_senate_members_states_relevant.csv')


#-------------------------------------------------------------------------------
# Recombine data
#-------------------------------------------------------------------------------

committee_members_matched <- rbind(house_members_districts, senate_members_states)

# Select relevant committee members (exclude all MoCs that joined the committe 
# after the hearing was held)
committee_members_matched <- committee_members_matched %>% 
  filter(not_member_yet == 0)

describe(committee_members_matched, skew = F)

write_csv(committee_members_matched, 
          "Data/climatehearings0310_for_modelling_20210709.csv")

table(committee_members$year, committee_members$majority)

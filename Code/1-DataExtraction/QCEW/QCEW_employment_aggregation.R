### ----------------------------------
###                                                                          
### Aggregation of annual US county level employment data from the Quarterly 
### Census of Employment and Wages (QCEW) by industry (NAICS) to the 
### Congressional district and state level for the 108th to 115th Congress 
### (2003 - 2018)                                                            
###
### Author: Mirjam Nanko
### Email: m.nanko@exeter.ac.uk
### Date: July 1, 2021
###
### ----------------------------------

library(readxl)     # read excel files
library(haven)      # read .dta files
library(tidyverse)  # data handling
library(magrittr)   # advanced piping
library(stringr)    # text data handling
library(psych)      # describe data

#-------------------------------------------------------------------------------
# This script loads and aggregates the Quarterly Census of Employment and Wages 
# (QCEW) annual US county level employment data by NAICS industry to the 
# Congressional district level for the 108th to 115th Congress (2003 - 2018).
# Data Source: CSVs By Industry - Annual Averages: 
# https://www.bls.gov/cew/downloadable-data-files.htm)
# 
# Specifically, the script
# (A) loads the county level annual average employment numbers for a specified 
#     range of years (in this example: 2003 to 2010) and industries classified
#     by NAICS industry codes (e.g. 10 Total, all industries; 21 Mining, 
#     quarrying, and oil and gas extraction; 211 Oil and gas extraction) ,
#     NAICS codes: 
#     https://www.bls.gov/cew/classifications/industry/industry-titles.htm
# (B) aggregates the data to the 108th Congress congressional district level 
#     with help of the Missouri Census Data Center Geographic Correspondence 
#     Engine Geocorr2000,
#     Source: https://mcdc.missouri.edu/applications/geocorr2000.html
# (C) and adjusts the data according to the resdistricting crosswalk files
#     provided by Autor, Dorn, Hanson and Majlesi (2020: Political Geography 
#     Crosswalk files G1, G2, G3 & G4).
#     Source: https://www.ddorn.net/data.htm
# 
# Additionally, it loads and aggregates the state level employment data for the 
# same years and industries.
# 
# Namely, it
# (D) loads Quarterly Census of Employment and Wages (QCEW) annual US state 
#     level employment and 
# (E) aggregates it by ignoring the ownership level.
#-------------------------------------------------------------------------------
# 
# NOTE: To use this script, it is only neccessary to specify the correct 
#       information or to verify the output at the six steps that are marked
#       with "### >> Step X". All the remaining code should run automatically. 
# 
#-------------------------------------------------------------------------------


### >> Step 1: Specify the path to data parent directory ###
# 
# Set to the folder containing the data folders used in this script. The folder 
# should contain:
# 
# - One folder called QCEW that contains all the extracted zip files
#   "CSVs By Industry - Annual Averages" for each required year
#   (e.g. 2009.annual.by_industry, 2010.annual.by_industry). These can be
#   downloaded here:
#   https://www.bls.gov/cew/downloadable-data-files.htm
# 
# - One folder called geocorr2000 containing the county to 108th Congress 
#   districts crosswalk file geocorr2000_county_108cd.csv provided on github.
# 
# - One folder called PoliticalGeography that contains the Political Geography 
#   Crosswalk extracted G1, G2, G3 & G4 zip files (e.g. cw_cd108_cd109).
#   These can be downloaded here: https://www.ddorn.net/data.htm
# 
setwd("/home/mirjam/OneDrive/Projects/GitHub/defeating-cap-and-trade/Data")
### <<

#-------------------------------------------------------------------------------
# (A) Load Quarterly Census of Employment and Wages (QCEW) annual US county 
#     level employment
#-------------------------------------------------------------------------------

### >> Step 2: Specify path to QCEW data folder ###
# 
# Set to directory where the individual Annual Averages by Industry folders 
# are saved (absolute filepath or relative to working directory)
# 
directory = "QCEW/"
### <<

### >> Step 3: Define relevant years (between 2003 and 2018) ###
# 
years <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
### <<

### >> Step 4: Define relevant industry NAICS codes ###
# 
# "NAICS is a 2- through 6-digit hierarchical classification system, 
# offering five levels of detail. Each digit in the code is part of a series 
# of progressively narrower categories, and the more digits in the code 
# signify greater classification detail. The first two digits designate the 
# economic sector, the third digit designates the subsector, the fourth 
# digit designates the industry group, the fifth digit designates the NAICS 
# industry, and the sixth digit designates the national industry."
# (Source: https://www.census.gov/naics/)
# 
# See list of "QCEW Industry Codes and Titles (For NAICS Coded Data)":
# https://www.bls.gov/cew/classifications/industry/industry-titles.htm
# 
# NOTE: There have been changes to some of the NAICS industry codes, i.e. NAICS 
# code with 4 or more digits. This means multiple codes may be required to load
# the same industry. Make sure to provide ALL required codes for the years 
# specified. Step 4 will provide an extra check to make sure that data has been 
# loaded for all years. A hierarchy crosswalk file to check the codes for all 
# years can be downloaded as a csv file here:
# https://www.bls.gov/cew/classifications/industry/2017-naics-hierarchy-crosswalk.htm
# 
NAICS <- 
  c("10",                          # Total
    "211", "2121",                 # Fossil fuel mining
    "213111", "213112", "213113",  # Support activities for fossil fuel mining
    "221112",                      # Fossil fuel power generation
    "2212",                        # Natural gas distribution
    "486"                          # Pipeline transportation
    )
### <<

# NAICS <- 
#   c("10",     # Total
#     "211",    # Oil and gas extraction
#     "2121"    # Coal mining
#     "213111", # Drilling oil and gas wells
#     "213112", # Support activities for oil and gas operations
#     "213113", # Support activities for coal mining
#     "221112", # Fossil fuel power generation
#     "2212",   # Natural gas distribution
#     "486"     # Pipeline transportation
#     )
# ### <<


# Load the data 
# Create empty data frame
QCEW.counties = data.frame()
# Extract folder names for the specified years
folder.names <- dir(directory)[str_extract(dir(directory), "[:digit:]*") 
                               %in% years]
# Specify column types to save the FIPS county code as a character
# (preserves leading zeros) and to select relevant columns
cols <- c("character", "NULL", NA, "NULL", "NULL", NA, "NULL", "NULL",
          rep(NA, 4), "NULL", "NULL", NA, rep("NULL", 28))
# Append the csv data files depending on the selected years and NAICS codes
for (folder.name in folder.names) {  
  print(paste0("Processing ", folder.name, "."))
  folder.path = paste0(directory, folder.name)
  file.names = dir(folder.path)[str_extract(dir(folder.path), "[:digit:]*")
                                %in% years &
                                str_trim(str_extract(dir(folder.path), 
                                                     "\\s[:digit:]*\\s"))
                                %in% NAICS]
  for (file.name in file.names) {
    file = paste0(folder.path, "/", file.name)
    temp = read.csv(file, colClasses = cols)
    QCEW.counties = rbind(QCEW.counties, temp)
    }
  rm(temp)
}

# Filter the relevant observations
QCEW.counties <- QCEW.counties %>% 
  # Subset county level data 
  # See list of "QCEW Aggregation Level Codes":
  # https://www.bls.gov/cew/classifications/aggregation/agg-level-titles.htm
  filter(agglvl_title %in% 
           c("County, Total Covered", 
             "County, by Domain -- by ownership sector",
             "County, by Supersector -- by ownership sector",
             "County, NAICS Sector -- by ownership sector",
             "County, NAICS 3-digit -- by ownership sector",
             "County, NAICS 4-digit -- by ownership sector",
             "County, NAICS 5-digit -- by ownership sector",
             "County, NAICS 6-digit -- by ownership sector")) %>%
  # Remove observations that have "unknown or undefined" county information
  filter(str_detect(area_fips, "999$") != T) 

# Save NAICS industry codes and titles
NAICS.codebook <- QCEW.counties[,c("industry_code", "industry_title")] %>% 
  unique() %>% 
  mutate(industry_code_long = str_pad(industry_code, 6, "right", pad = "0")) %>%
  arrange(industry_code_long)


### >> Step 5: Check result and filter correct observations ###
# 
# Check that the required industries have been loaded
NAICS.codebook
# In case the NAICS code has changed for any of the required industries, some 
# years will have zero observations for the affected industry. The following
# table displays the number of loaded observations for each industry and year.
# If there are zero observations for some year, this is a strong indicator that 
# the NAICS code for this industry may have changed at some point. 
QCEW.counties %>% 
  select(year, industry_code, industry_title, annual_avg_emplvl) %$% 
  table(year, industry_title)
### <<

#-------------------------------------------------------------------------------
# (B) Match the geocorr congressional district crosswalk file to data and
#     aggregate the data at the 108th Congress congressional district level
#-------------------------------------------------------------------------------

# Load the Census 2000 county to 108 congressional district crosswalk file 
geocorr.108 <- read.csv("geocorr2000/geocorr2000_county_108cd.csv",
                  header = F, skip = 2,
                  colClasses = c(c(rep("character", 5)), c(rep("numeric", 2))),
                  col.names = names(
                  read.csv("geocorr2000/geocorr2000_county_108cd.csv"))
                  )[, c(1:5, 7)]

# Join the district information with the QCEW employment data
QCEW.counties <- right_join(geocorr.108, QCEW.counties, 
                            by = c("county" = "area_fips")) %>% 
  arrange(county, year, industry_code) %>% 
  rename(cd_code = cd108)


### >> Step 6: Check result and manually replace missing information ###
# 
# Check missing district level information
QCEW.counties[is.na(QCEW.counties$state), 1:9] %>% 
  select(state, stab, cd_code, cntyname, afact, area_title) %>% 
  unique()
# Manually replace missing information
# Alaska
# https://en.wikipedia.org/wiki/Colorado%27s_2nd_congressional_district
QCEW.counties[str_detect(QCEW.counties$county, "^02"), ] <- QCEW.counties %>% 
  filter(str_detect(county, "^02")) %>% 
  mutate(state = "02", cd_code = "01", stab = "AK", afact = 1,
         cntyname = str_replace(area_title, ", Alaska", " AK"))
# Broomfield County, Colorado
# https://en.wikipedia.org/wiki/Colorado%27s_2nd_congressional_district
QCEW.counties[QCEW.counties$county == "08014", ] <- QCEW.counties %>% 
  filter(county  == "08014") %>% 
  mutate(state = "08", cd_code = "02", stab = "CO", afact = 1,
         cntyname = str_replace(area_title, ", Colorado", " CO"))
# Puerto Rico
QCEW.counties[str_detect(QCEW.counties$county, "^72"), ] <- QCEW.counties %>% 
  filter(str_detect(county, "^72")) %>% 
  mutate(state = "72", cd_code = "01", stab = "PR", afact = 1,
         cntyname = str_replace(area_title, ", Puerto Rico", " PS"))
# Virgin Islands
QCEW.counties[str_detect(QCEW.counties$county, "^78"), ] <- QCEW.counties %>% 
  filter(str_detect(county, "^78")) %>% 
  mutate(state = "78", cd_code = "01", stab = "VI", afact = 1,
         cntyname = str_replace(area_title, ", Virgin Islands", " VI"))
# Check that all missing district level information has been replaced
QCEW.counties[is.na(QCEW.counties$state), 1:9] %>% 
  select(state, stab, cd_code, cntyname, afact, area_title) %>% 
  unique()
### <<

# Inspect data
describe(QCEW.counties, skew = F)

# Complete data (make implicit missing values explicit)
QCEW.counties.complete <- QCEW.counties[c(1:8,13)] %>% 
  complete(industry_code, year,
           nesting(cd_code, state, stab, county, cntyname, afact),
           fill = list(annual_avg_emplvl = 0)) %>%
  arrange(county, year, industry_code)

# Aggregate by the geocorr congressional districts (variable "afact")
QCEW.congressional.districts <- QCEW.counties.complete[] %>%
  mutate(employment.108 = afact*annual_avg_emplvl,
         congress = case_when(
         year %in% c(2003, 2004) ~ 108,
         year %in% c(2005, 2006) ~ 109,
         year %in% c(2007, 2008) ~ 110,
         year %in% c(2009, 2010) ~ 111,
         year %in% c(2011, 2012) ~ 112,
         year %in% c(2013, 2014) ~ 113,
         year %in% c(2015, 2016) ~ 114,
         year %in% c(2017, 2018) ~ 115)) %>%
  group_by(state, stab, cd_code, industry_code, year, congress) %>%
  summarise(employment = sum(employment.108))

head(QCEW.congressional.districts, 20)


#-------------------------------------------------------------------------------
# (C) Recalculate district employment information using the Congress districts
#     crosswalk files by Autor et al. (2020)
#-------------------------------------------------------------------------------

# afact_cdXXX_cd108: population share of a district of the XXXth Congress who 
# live in Census blocks which form a given district of the 108th Congress 

cw_cd_cd <- rbind(
  read_dta("PoliticalGeography/cw_cd108_cd109/cw_cd108_cd109.dta") %>% 
  crossing(year = c(2005:2006)) %>% 
  mutate(afact.redist = afact_cd109_cd108,
         congressionaldistrict = congressionaldistrict109) %>% 
  select(congressionaldistrict108, congressionaldistrict,
         afact.redist, year),
  read_dta("PoliticalGeography/cw_cd108_cd110/cw_cd108_cd110.dta") %>% 
  crossing(year = c(2007:2012)) %>% 
  mutate(afact.redist = afact_cd110_cd108,
         congressionaldistrict = congressionaldistrict110) %>% 
  select(congressionaldistrict108, congressionaldistrict, 
         afact.redist, year),
  read_dta("PoliticalGeography/cw_cd108_cd113/cw_cd108_cd113.dta") %>% 
  crossing(year = c(2013:2016)) %>% 
  mutate(afact.redist = afact_cd113_cd108,
         congressionaldistrict = congressionaldistrict113) %>% 
  select(congressionaldistrict108, congressionaldistrict,
         afact.redist, year),
  read_dta("PoliticalGeography/cw_cd108_cd115/cw_cd108_cd115.dta") %>% 
  crossing(year = c(2017:2018)) %>% 
  mutate(afact.redist = afact_cd115_cd108,
         congressionaldistrict = congressionaldistrict115) %>% 
  select(congressionaldistrict108, congressionaldistrict, 
         afact.redist, year))

# Merge the redistricting crosswalk file (multiplication factor: afact.redist) 
# to the data
QCEW.congressional.districts.redist <- 
  left_join(QCEW.congressional.districts %>% 
            mutate(congressionaldistrict = paste(stab, as.numeric(cd_code))),
            cw_cd_cd,
            by = c("year", "congressionaldistrict")) %>% 
  mutate(congressionaldistrict108 = ifelse(is.na(congressionaldistrict108), 
                                           congressionaldistrict,
                                           congressionaldistrict108),
         afact.redist = ifelse(is.na(afact.redist), 1, afact.redist))
  
# Merge the employment to multiply (employment.redsit) to the data and 
# recalculate employment level depending on the redistricting multiplication
# factor afact.redist
QCEW.congressional.districts.redist <-
  left_join(
    QCEW.congressional.districts.redist,
      unique(QCEW.congressional.districts.redist[
      c("industry_code", "year", "congressionaldistrict", "employment")]),
      by = c("industry_code", "year",
             "congressionaldistrict108" = "congressionaldistrict"),
      suffix = c("108", ".redist")) %>% 
  mutate(employment = employment.redist * afact.redist) 

# Collapse the data back to the Congress district level
QCEW.congressional.districts.redist <- QCEW.congressional.districts.redist %>% 
  group_by(state, stab, cd_code, industry_code, year, congress, 
           congressionaldistrict) %>% 
  summarise(employment = sum(employment))

# Pivot the data to wide format
QCEW.congressional.districts.redist <- QCEW.congressional.districts.redist %>% 
  pivot_wider(names_from = industry_code, values_from = employment,
              names_prefix = "emp.") %>% 
  relocate(c(cd_code, congressionaldistrict), .after = last_col())

head(QCEW.congressional.districts.redist)

# Save the Congress districts data in the QCEW data directory
write.csv(QCEW.congressional.districts.redist, 
          paste0(directory, "QCEW_congressional_districts_employment.csv"))

# Save the NAICS codebook in the QCEW data directory
write.csv(NAICS.codebook, 
          paste0(directory, "QCEW_employment_NAICS_codebook.csv"))


#-------------------------------------------------------------------------------
# (D) Load Quarterly Census of Employment and Wages (QCEW) annual US state 
#     level employment
#-------------------------------------------------------------------------------

QCEW.states = data.frame()
# Append the csv data files depending on the selected years and industries
for (folder.name in folder.names) {
  folder.path = paste0(directory, folder.name)
  file.names = dir(folder.path)[str_extract(dir(folder.path), "[:digit:]*")
                                %in% years &
                                str_trim(str_extract(dir(folder.path), 
                                                     "\\s[:digit:]*\\s"))
                                %in% NAICS]
  for (file.name in file.names) {
    file = paste0(folder.path, "/", file.name)
    temp = read.csv(file, colClasses = cols)
    QCEW.states = rbind(QCEW.states, temp)
    }
  rm(temp)
}

# Filter the relevant observations
QCEW.states <- QCEW.states %>% 
  # Subset state level data 
  # See list of "QCEW Aggregation Level Codes":
  # https://www.bls.gov/cew/classifications/aggregation/agg-level-titles.htm
  filter(agglvl_title %in% 
           c("State, Total Covered", 
             "State, by Domain -- by ownership sector",
             "State, by Supersector -- by ownership sector",
             "State, NAICS Sector -- by ownership sector",
             "State, NAICS 3-digit -- by ownership sector",
             "State, NAICS 4-digit -- by ownership sector",
             "State, NAICS 5-digit -- by ownership sector",
             "State, NAICS 6-digit -- by ownership sector")) %>%
  # Remove " -- Statewide" from state name
  mutate(area_title = str_replace(area_title, " -- Statewide", ""))


# Save NAICS industry codes and titles
NAICS.codebook <- QCEW.states[,c("industry_code", "industry_title")] %>% 
  unique() %>% 
  mutate(industry_code_long = str_pad(industry_code, 6, "right", pad = "0")) %>%
  arrange(industry_code_long)


### >> Step 5: Check result and filter correct observations ###
# 
# Check that the required industries have been loaded
NAICS.codebook
# In case the NAICS code has changed for any of the required industries, some 
# years will have zero observations for the affected industry. The following
# table displays the number of loaded observations for each industry and year.
# If there are zero observations for some year, this is a strong indicator that 
# the NAICS code for this industry may have changed at some point. 
QCEW.states %>% 
  select(year, industry_code, industry_title, annual_avg_emplvl) %$% 
  table(year, industry_title)
### <<


#-------------------------------------------------------------------------------
# (E) Aggregate the state employment data by ignoring the ownership level and 
#     pivot it to wide format
#-------------------------------------------------------------------------------

# Aggregate the state data
QCEW.states <- QCEW.states %>% 
  select(area_fips, area_title, year, industry_code, annual_avg_emplvl) %>%
  group_by(area_fips, area_title, year, industry_code) %>% 
  summarise(annual_avg_emplvl = sum(annual_avg_emplvl)) %>%
  # Pivot the data to wide format
  pivot_wider(names_from = industry_code, values_from = annual_avg_emplvl,
              names_prefix = "emp.", values_fill = 0) %>% 
  arrange(area_fips, area_title, year) %>% 
  mutate(state = substr(area_fips, 1, 2),
         congress = case_when(
         year %in% c(2003, 2004) ~ 108,
         year %in% c(2005, 2006) ~ 109,
         year %in% c(2007, 2008) ~ 110,
         year %in% c(2009, 2010) ~ 111,
         year %in% c(2011, 2012) ~ 112,
         year %in% c(2013, 2014) ~ 113,
         year %in% c(2015, 2016) ~ 114,
         year %in% c(2017, 2018) ~ 115)) %>% 
  relocate(c(area_fips, area_title), .after = last_col()) %>% 
  relocate(c(state), .before = year) %>% 
  relocate(c(congress), .after = year)

# Inspect data
head(QCEW.states, 10)

# Save the Congress districts data in the QCEW data directory
write.csv(QCEW.states,
          paste0(directory, "QCEW_states_employment.csv"))



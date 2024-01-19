# Packages & Setup -------------------------------------------------------------
library(readxl)     # excel files
library(tidyverse)  # data handling
options(dplyr.summarise.inform = FALSE)
library(magrittr)   # piping
library(psych)      # describe data
library(ggpubr)     # ggarrange() multiple plots
options(pillar.sigfig = 7) # tibble decimal places


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script loads the lobbying and campaign contributions money from the
# Fossil Fuel Industry (FFI) from 2003 to 2010 and plots the absolute and 
# relative contributions 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getwd()
setwd("/Users/mn/Library/CloudStorage/OneDrive-UniversityofExeter/Projects/GitHub/defeating-cap-and-trade/Data")
theme_set(theme_minimal())

# Functions---------------------------------------------------------------------
# Function to rescale second axis
second_axis = function(y1, y2) {
  ylim1 <- range(y1, na.rm = TRUE)
  ylim2 <- range(y2, na.rm = TRUE)
  mult <- (ylim1[2] - ylim1[1])/(ylim2[2] - ylim2[1])
  miny1 <- ylim1[1]
  miny2 <- ylim2[1]
  cast_to_y1 = function(x) {
    (mult * (x - miny2)) + miny1
  }
  yf <- cast_to_y1(y2)
  labelsyf <- pretty(y2)
  return(
    list(
      yf = yf,
      labels = labelsyf,
      breaks = cast_to_y1(labelsyf),
      zero = cast_to_y1(0)
    ))
}

# 1: Load data -----------------------------------------------------------------

## Load the FFI codes to identify FFI money ------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CRP Categories Codebook:
# 
# Catcode	      OpenSecrets category code
# Catname       OpenSecrets category name	(Lv. 1)
# Catorder  	  OpenSecrets industry code
# Industry	    OpenSecrets industry name (Lv. 2)
# Sector        OpenSecrets sector name	(abbr.)
# Sector.Long   OpenSecrets sector name (Lv. 3)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CRP_Codes <- read.delim("OpenSecrets/BulkData/CRP_Categories.txt",
                        header = TRUE, skip = 7, sep = "\t")

# List of industries (Catorder) in a sector (Sector)
CRP_Codes %>% select(Sector, Catorder, Industry, Catcode, Catname) %>% 
  unique() %>% arrange(Sector, Catorder) %>% as_tibble %>% print(n = Inf)

CRP_Codes %>% filter(Sector == "Energy/Nat Resource") %>% 
  select(Catorder, Industry, Catcode, Catname) %>% unique() %>% 
  arrange(Catorder, Catcode)

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

## Load the MoC identifiers for committee members on cap-and-trade hearings ----

CIDs <- 
  read.csv("Analysis/hearings_committee_member_level.csv") %>% 
  select(congress, CID) %>% 
  distinct() %>% 
  mutate(capandtradeMoC = "capandtrade")

## Load the OpenSecrets Campaign Contributions data (indivs and pacs) ----------

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
  # Load current Members of Congress
  path = paste0(dir, years[i], "/cands", years[i], ".txt")
  
  currentMoCs <- read.table(path, quote = "|", sep = ",") %>% 
    filter(V7!="    ") %>% 
    rename(CID = V3)
  # Load Individual Contributions and merge with FEC data for "FeCCandID"
  path = paste0(dir, years[i], "/indivs", years[i], ".txt")
  indivs_temp <- left_join(
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
    filter(!startsWith(RealCode, "Z9")) %>% 
    filter(str_trim(Type) %in% c("11", "15", "15E", "15J", "22Y")) %>%
    filter(!startsWith(FeCCandID, "P")) %>%
    filter(!startsWith(toupper(PrimCode), "Z4")) %>%
    filter(startsWith(CID, "N")) %>% 
    filter(CID %in% currentMoCs$CID) %>%
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
    rename(PACID = V3, CID = V4, Amount = V5, Date = V6, RealCode = V7, Type = V8, 
           DI = V9) %>%
    mutate(RealCode = toupper(RealCode)) %>%
    filter(!startsWith(RealCode, "Z4") & !startsWith(RealCode, "Z9")) %>%
    filter(DI == "D") %>% 
    filter(startsWith(CID, "N")) %>% 
    filter(CID %in% currentMoCs$CID) %>% 
    mutate(Catcode = CRP_Codes$Catcode[match(RealCode, CRP_Codes$Catcode)],
           Catorder = CRP_Codes$Catorder[match(RealCode, CRP_Codes$Catcode)],
           SectorCode = str_extract(Catorder, "[:alpha:]+"),
           year = as.numeric(substr(Date, 7, 10)),
           congress = case_when(
             year %in% 2003:2004 ~ 108, year %in% 2005:2006 ~ 109,
             year %in% 2007:2008 ~ 110, year %in% 2009:2010 ~ 111))
  # Save contributions
  indivs <- rbind(indivs, indivs_temp)
  pacs <- rbind(pacs, pacs_temp)
  print(paste0('Campaign contributions for ', congresses[i], 
               "th congress loaded (", i, "/", length(congresses), ")"))
}

# Drop temporary objects
rm(CRP_Codes, dir, path, congresses, years, i, cmtes, 
   currentMoCs, indivs_temp, pacs_temp)

pacs <- merge(pacs, CIDs, by = c("CID", "congress"), all.x = TRUE) %>% 
  mutate(capandtradeMoC = replace_na(capandtradeMoC, "other")) %>%
  filter(!is.na(congress))

indivs <- merge(indivs, CIDs, by = c("CID", "congress"), all.x = TRUE) %>% 
  mutate(capandtradeMoC = replace_na(capandtradeMoC, "other")) %>%
  filter(!is.na(congress))

# Summarise and bind contributions data
campaign_contributions <- rbind(
  # Individual contributions total
  indivs %>% mutate(Catcode = "total") %>%
    group_by(CID, Catcode, congress, capandtradeMoC) %>%
    summarise(campaign_contributions = sum(Amount)),
  # Individual contributions per category
  indivs %>% filter(Catcode %in% fossilfuel) %>%
    mutate(Catcode = "fossilfuel") %>% 
    group_by(CID, Catcode, congress, capandtradeMoC) %>%
    summarise(campaign_contributions = sum(Amount)),
  # PACs contributions total
  pacs %>% mutate(Catcode = "total") %>%
    group_by(CID, Catcode, congress, capandtradeMoC) %>%
    summarise(campaign_contributions = sum(Amount)),
  # PACs contributions per category
  pacs %>% filter(Catcode %in% fossilfuel) %>%
    mutate(Catcode = "fossilfuel") %>% 
    group_by(CID, Catcode, congress, capandtradeMoC) %>%
    summarise(campaign_contributions = sum(Amount))) %>% 
  # Summarise contributions by category and congress and reshape data
  group_by(CID, Catcode, congress, capandtradeMoC) %>%
  summarise(campaign_contributions = sum(campaign_contributions)) %>%
  arrange(CID, congress, Catcode) %>%
  pivot_wider(names_from = Catcode, names_prefix = "cc_",
              values_from = campaign_contributions,
              values_fill = 0)


# Summarise and bind contributions data
campaign_contributions_year <- rbind(
  # Individual contributions total
  indivs %>% mutate(Catcode = "total") %>%
    group_by(CID, Catcode, congress, capandtradeMoC, year) %>%
    summarise(campaign_contributions = sum(Amount)),
  # Individual contributions per category
  indivs %>% filter(Catcode %in% fossilfuel) %>%
    mutate(Catcode = "fossilfuel") %>% 
    group_by(CID, Catcode, congress, capandtradeMoC, year) %>%
    summarise(campaign_contributions = sum(Amount)),
  # PACs contributions total
  pacs %>% mutate(Catcode = "total") %>%
    group_by(CID, Catcode, congress, capandtradeMoC, year) %>%
    summarise(campaign_contributions = sum(Amount)),
  # PACs contributions per category
  pacs %>% filter(Catcode %in% fossilfuel) %>%
    mutate(Catcode = "fossilfuel") %>% 
    group_by(CID, Catcode, congress, capandtradeMoC, year) %>%
    summarise(campaign_contributions = sum(Amount))) %>% 
  # Summarise contributions by category and congress and reshape data
  group_by(CID, Catcode, congress, capandtradeMoC, year) %>%
  summarise(campaign_contributions = sum(campaign_contributions)) %>%
  arrange(CID, year, congress, Catcode) %>%
  pivot_wider(names_from = Catcode, names_prefix = "cc_",
              values_from = campaign_contributions,
              values_fill = 0)

## Load the OpenSecrets Lobbying data (lob_lobbying) ---------------------------

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

# Summarise the lobbying expenditures by 6-month-period
lobbying <-
  merge(lob_lobbying %>%
          filter(Catcode %in% fossilfuel) %>%  
          group_by(halfyear, year, congress) %>%
          summarise(lobbying_fossilfuel = sum(Amount)),
        lob_lobbying %>%
          group_by(halfyear, year, congress) %>%
          summarise(lobbying_total = sum(Amount))) %>%  
  mutate(per_lobbying_fossilfuel = lobbying_fossilfuel/lobbying_total*100)
 
# 2: Aggregate the data --------------------------------------------------------

# Lobbying by year
lobbying_year <-
lobbying %>%
  group_by(year) %>%
  summarise(
    per_lobbying_fossilfuel =
      sum(lobbying_fossilfuel)/sum(lobbying_total) * 100,
    lobbying_fossilfuel =
      sum(lobbying_fossilfuel)/1000000) %>%
  ungroup() %>%
  mutate(per_lobbying_fossilfuel_rescaled =
           second_axis(lobbying_fossilfuel, per_lobbying_fossilfuel)$yf)

lobbying_year_rescaled <- 
  lobbying_year %$% 
  second_axis(lobbying_fossilfuel, per_lobbying_fossilfuel)

# Campaign contributions by congress
cc_year <-
  campaign_contributions_year %>%
  group_by(year) %>%
  summarise(per_cc_fossilfuel =
              sum(cc_fossilfuel)/sum(cc_total)*100,
            cc_fossilfuel =
              sum(cc_fossilfuel)/1000000) %>%
  ungroup() %>%
  mutate(cc_fossilfuel_rescaled =
           second_axis(cc_fossilfuel, per_cc_fossilfuel)$yf)

cc_year_rescaled <- 
  cc_year %$% 
  second_axis(cc_fossilfuel, per_cc_fossilfuel)

# Campaign contributions by congress
cc_congress <-
  campaign_contributions %>%
  group_by(congress) %>%
  summarise(per_cc_fossilfuel =
              sum(cc_fossilfuel)/sum(cc_total)*100,
            cc_fossilfuel =
              sum(cc_fossilfuel)/1000000) %>%
  ungroup() %>%
  mutate(cc_fossilfuel_rescaled =
           second_axis(cc_fossilfuel, per_cc_fossilfuel)$yf)

cc_congress_rescaled <- 
  cc_congress %$% 
  second_axis(cc_fossilfuel, per_cc_fossilfuel)


# 3: Generate plots ------------------------------------------------------------

p.lobbying_year <-
  lobbying_year %>% select(c(1,3,4)) %>% 
  pivot_longer(2:3,
               names_to = "group", values_to = "value") %>%
  mutate(Type = ifelse(group == "lobbying_fossilfuel", 
                       "In million $", "In percent")) %>% 
  ggplot() +
  geom_line(aes(x=year, y = value, linetype = Type)) +
  scale_linetype_manual(name = " ",
                        values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(2003,2010,1)) +
  scale_y_continuous(limits = c(144,371),
                     breaks = seq(150,350,50),
                     expand = c(.01, 0),
                     sec.axis = 
                       dup_axis(breaks = lobbying_year_rescaled$breaks, 
                                labels = lobbying_year_rescaled$labels,
                                name = "FFI lobbying in %")) +
  labs(x = "Year", y = "FFI lobbying in $mil.") +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica', size = 9),
        legend.title = element_text(face = "bold"),
        legend.position = 'top',
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 3)),
        axis.title.y.right = element_text(margin = margin(l = 6))) +
  guides(fill = guide_legend(nrow = 1))

p.cc_year <-
  cc_year %>% select(c(1,3,4)) %>% 
  pivot_longer(2:3,
               names_to = "group", values_to = "value") %>%
  mutate(Type = ifelse(group == "cc_fossilfuel", 
                       "In million $", "In percent")) %>% 
  ggplot() +
  geom_line(aes(x=year, y = value, linetype = Type)) +
  scale_linetype_manual(name = " ",
                        values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(2003,2010,1)) +
  scale_y_continuous(limits = c(9,17),
                     breaks = c(9,11,13,15,17),
                     expand = c(.03, 0),
    sec.axis = 
      dup_axis(breaks = cc_year_rescaled$breaks, 
               labels = cc_year_rescaled$labels,
               name = "FFI campaign contributions in %")) +
  labs(x = "Year", y = "FFI campaign contributions in $mil.", 
       fill = "Category") +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica', size = 9),
        legend.title = element_text(face = "bold"),
        legend.position = 'top',
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 3)),
        axis.title.y.right = element_text(margin = margin(l = 6))) +
  guides(fill = guide_legend(nrow = 1))

plot <- ggarrange(p.lobbying_year, NULL,  p.cc_year, heights = c(1,0.15, 1),
                  ncol = 1, common.legend = TRUE); plot

ggsave("../Plots/timeline_FFImoney.pdf", plot,
       device = "pdf", dpi = 1000, width = 90, height = 120, units = "mm")

p.cc_congress <-
  cc_congress %>% select(c(1,3,4)) %>% 
  pivot_longer(2:3,
               names_to = "group", values_to = "value") %>%
  mutate(Type = ifelse(group == "cc_fossilfuel", 
                       "In million $", "In percent")) %>% 
  ggplot() +
  geom_line(aes(x=congress, y = value, linetype = Type)) +
  scale_linetype_manual(name = " ",
                        values = c("solid", "dashed")) +
  scale_y_continuous(limits = c(22,33),
                     sec.axis = 
                       dup_axis(breaks = cc_congress_rescaled$breaks, 
                                labels = cc_congress_rescaled$labels,
                                name = "FFI campaign contributions in %")) +
  labs(x = "Congress", y = "FFI campaign contributions in $mil.", 
       fill = "Category") +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica', size = 9),
        legend.title = element_text(face = "bold"),
        legend.position = 'top',
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 3)),
        axis.title.y.right = element_text(margin = margin(l = 6)),
        ) +
  guides(fill = guide_legend(nrow = 1))

plot_congress <- ggarrange(p.lobbying_year, p.cc_congress, heights = c(1.2,1),
                  ncol = 1, common.legend = TRUE); plot_congress

# 4: FFI campaign contributions by MoC type  -----------------------------------
# Compare mean FFI campaign contributions to MoCs on committees holding a 
# cap-and-trade hearing vs all other MoCs

campaign_contributions %>% 
  group_by(capandtradeMoC) %>%
  summarise(sum = sum(cc_fossilfuel),
            mean = mean(cc_fossilfuel),
            SD = sd(cc_fossilfuel),
            N = n())

t.test(cc_fossilfuel~capandtradeMoC, 
       data = campaign_contributions)

campaign_contributions %>% 
  ggplot(aes(cc_fossilfuel, capandtradeMoC)) + 
  geom_jitter(alpha = .1) + 
  geom_boxplot()

# 5: TOP FFI contributors ------------------------------------------------------
# Investigate the top FFI companies lobbying and giving campaign contributions 
# from 2003 to 2010 (note that these are only campaign contributions to MoCs
# on committees holding one of the hearings in the cap-and-trade analysis)

# Subset campaign contributions to MoCs on committees holding one of the 
# hearings in the cap-and-trade analysis

pacs_subset <- pacs %>% filter(capandtradeMoC == "capandtrade")
indivs_subset <- indivs %>% filter(capandtradeMoC == "capandtrade")

campaign_contributions  %>% filter(capandtradeMoC == "capandtrade") %$% 
  sum(cc_fossilfuel)

pacs_subset %>%  filter(Catcode %in% fossilfuel) %$%  sum(Amount) 
indivs_subset %>%  filter(Catcode %in% fossilfuel) %$%  sum(Amount)

## Top 50 FFI lobbying firms from 2003 to 2010 ---------------------------------
top50lobbying <- lob_lobbying %>%
  filter(Catcode %in% fossilfuel) %>%  
  group_by(Ultorg) %>%
  summarise(lobbying_fossilfuel = sum(Amount)) %>%
  arrange(desc(lobbying_fossilfuel)) %>% 
  head(50) #%>% print(n=50) #%>% kableExtra::kable(format = 'latex')

# Check USCAP lobbying
lob_lobbying %>%
  filter(Catcode %in% fossilfuel) %>%  
  group_by(Ultorg) %>%
  summarise(lobbying_fossilfuel = sum(Amount)) %>%
  arrange(desc(lobbying_fossilfuel)) %>% 
  filter(Ultorg %in% c("BP", "Duke Energy", "FPL Group", "PG&E Corp",
                       "PNM Resources", "ConocoPhillips", "Royal Dutch Shell",
                       "Royal Dutch/Shell Group of Companies", "Siemens AG")) %>% 
  summarise(TOTAL = sum(lobbying_fossilfuel))

# The 14 founding members of USCAP are:
#   
#   Alcoa, BP America, Caterpillar Inc, Duke Energy, DuPont
# Environmental Defense, FPL Group, General Electric, Lehman Brothers
# Natural Resources Defense Council, Pew Center on Global Climate Change
# PG&E Corporation, PNM Resources, and World Resources Institute
# 
# In April, 2007 oil giant ConocoPhillips and insurer AIG joined USCAP.[7]
# 
# The following groups and companies joined in June 2007:[8]
# 
# American International Group (AIG), Alcan, Boston Scientific, ConocoPhillips
# Deere & Company, The Dow Chemical Company, General Motors
# Johnson & Johnson, Marsh, PepsiCo, Shell
# Siemens, The Nature Conservancy, The National Wildlife Federation
# 
# In July, 2007, two major U.S. automakers joined:[8]
# 
# Chrysler, Ford Motor Company 

## Top 50 FFI PAC campaign contributions ---------------------------------------
# to MoCs on committee holding cap-and-trade hearings
top50pacs <- pacs_subset %>% filter(Catcode %in% fossilfuel) %>%
  mutate(Catcode = "fossilfuel") %>% 
  # mutate(UltOrg = ifelse(UltOrg == "", Orgname, UltOrg)) %>% 
  group_by(PACID) %>%
  summarise(cc_fossilfuel = sum(Amount)) %>%
  arrange(desc(cc_fossilfuel)) %>% 
  head(50) # %>% print(n=50)  %>% kableExtra::kable(format = 'latex')

## Top 50 FFI individual campaign contributions --------------------------------
# to MoCs on committee holding cap-and-trade hearings
top50indivs <- indivs_subset %>% filter(Catcode %in% fossilfuel) %>%
  mutate(Catcode = "fossilfuel") %>% 
  mutate(UltOrg = ifelse(UltOrg == "", Orgname, UltOrg)) %>% 
  group_by(UltOrg) %>%
  summarise(cc_fossilfuel = sum(Amount)) %>%
  arrange(desc(cc_fossilfuel)) %>% 
  head(50) # %>% print(n=50) %>% kableExtra::kable(format = 'latex')

## FFI money overview table ----------------------------------------------------
money <- rbind(
  # Lobbying
  cbind(top50lobbying %>% summarise(TOPxTOTAL = sum(lobbying_fossilfuel),
                                    PROPORTION = TOPxTOTAL/
                                      (lob_lobbying %>% 
                                         filter(Catcode %in% fossilfuel) %>% 
                                         summarise(TOTAL = sum(Amount)))[1,1]),
        lob_lobbying %>% filter(Catcode %in% fossilfuel) %>% 
          summarise(TOTAL = sum(Amount))),
  # PAC CC
  cbind(top50pacs %>% summarise(TOPxTOTAL = sum(cc_fossilfuel),
                                PROPORTION = TOPxTOTAL/
                                  (pacs_subset %>%
                                     filter(Catcode %in% fossilfuel) %>% 
                                     summarise(TOTAL = sum(Amount)))[1,1]),
        pacs_subset %>% filter(Catcode %in% fossilfuel) %>% 
          summarise(TOTAL = sum(Amount))),
  # Individual CC
  cbind(top50indivs %>% summarise(TOPxTOTAL = sum(cc_fossilfuel),
                                  PROPORTION = TOPxTOTAL/
                                    (indivs_subset %>% 
                                       filter(Catcode %in% fossilfuel) %>% 
                                       summarise(TOTAL = sum(Amount)))[1,1]),
        indivs_subset %>% filter(Catcode %in% fossilfuel) %>% 
          summarise(TOTAL = sum(Amount)))
)

rownames(money) <- c("Lobbying", "PAC CC", "Individual CC")
colnames(money) <- c("Top50 $", "Top 50 %", "Total $")
money %>% round(2)

#                   Top50 $   Top 50 %      Total $
# Lobbying      1326453428   0.7182663   1846743176
# PAC CC          31220868   0.7320453     42648817
# Individual CC    7205691   0.3444702     20918181

## Absolute lobbying and campaign contributions over time ----------------------
cbind(lob_lobbying %>% 
  filter(Catcode %in% fossilfuel) %>% 
  group_by(year) %>% 
  summarise(FFI = sum(Amount),
            inMIO = FFI/1000000),
  (lob_lobbying %>% 
     group_by(year) %>%
     summarise(ALL = sum(Amount)))[,2]) %>% 
  mutate(per = FFI/ALL*100) %>% 
  select(c(1,2,3,5)) %>% 
  round(2)

campaign_contributions_year %>% 
  group_by(year) %>% 
  summarise(TOTAL = sum(cc_fossilfuel),
            inMIO = TOTAL/1000000, 
            per = TOTAL/sum(cc_total)*100) %>% 
  round(2)


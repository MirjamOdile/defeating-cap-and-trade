# Defeating cap-and-trade

This repository makes available the code to replicate the analysis of climate change contrarian testimonies and fossil-fuel industry lobbying at US congressional hearings on cap-and-trade legislation in the years 2003 to 2010 described in the following [article]():

    "Defeating Cap-and-Trade: How the Fossil Fuel Industry and Climate Change Counter Movement
    obstruct U.S. Climate Change Legislation" by Mirjam O. Nanko and Travis G. Coan (2024).

 ## Analysis
 
The descriptive and inferential analysis can be replicated by executing the scripts `09 Descriptive analysis.R` and `10 Inferential analysis.R` in the `Code/3-Analysis` folder. These scripts solely rely on the pre-compiled datasets `hearings_committee_member_level.csv` and `witnesses.csv` provided in the `Data/Analysis` folder.

For a deeper understanding of the data, the remaining scripts in the `Code/3-Analysis` folder can be executed, which illustrate how the raw witness data was combined with external datasets and how this data was subset and aggregated. However, these rely on additional data not provided in this GitHub directory which needs to be added manually (instructions in the **Additional Data** section below).

The code in the folders `Code/1-DataExtraction` and `Code/2-CommitteeHearings` is not required for replicating the analysis, but rather serves as a reference for the generation of the raw data at the core of this analysis. <br> <br>
 

## Additional Data

The data used in this analysis is partially available in the `/Data` folder, specifically all data generated or compiled by the authors. In order to replicate the data compilation, this data needs to be complemented with other publicly available datasets. Specifically, the following datasets need to be added:

* Committee assignment data based on the congressional Committees data compiled by Charles Stewart III and Jonathan Woon (MIT) saved into a folder called `Data/StewartWoon`.  <br>
    Available [here](http://web.mit.edu/17.251/www/data_page.html#2). Required files:
  * `house_assignments_103-115-3.xls`
  * `senate_assignments_103-115-3.xls`. <br>
    
* Congressional committee data from the `unitedstates` github repository saved into a folder called `Data/congress-legislators`. <br>
  Available [here](https://github.com/unitedstates/congress-legislators). Required files:
  * `legislators-current.csv`
  * `legislators-historical.csv`. <br>

* OpenSecrets bulk data saved into a folder called `Data/OpenSecrets/BulkData`. <br>
    Available [here](https://www.opensecrets.org/open-data/bulk-data). Required files: <br>
  * Industry Codes: `CRP_Categories.txt`
  * Campaign Finance Data: the folders `CampaignFin04`,  `CampaignFin06`,  `CampaignFin08`, and  `CampaignFin10` listed as 2004, 2006, 2006, and 2010 Cycle Tables
  * Lobbying Data: the folder `Lobby` listed as Lobbying Tables <br> <br>
 
## Notes

:heavy_exclamation_mark: The working directory needs to be changed to the `/Data` parent folder at the top of each script by replacing the existing filepath in the `setwd()` function.<br>
:bangbang: The code was written using Unix-based filepath notation with forward slashes. This needs to be changed to backslashes if executed in Windows. <br> <br>


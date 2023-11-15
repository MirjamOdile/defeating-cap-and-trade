# Defeating cap-and-trade

This repository makes available the code to replicate the analysis of climate change contrarian testimonies and fossil-fuel industry lobbying at US congressional hearings on cap-and-trade legislation in the years 2003 to 2010 described in the following [article]()

    "Defeating Cap-and-Trade: How the Fossil Fuel Industry and Climate Change Counter Movement obstruct 
    U.S. Climate Change Legislation" by Mirjam O. nanko and Travis G. Coan (2023).


## Data

The data used in this analysis is partially available in the `\Data` folder, specifically all data generated or compiled by the authors. In order to replicate the analysis, this data needs to be complemented with other publicly available datasets. Specifically, the following datasets need to be added:

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
  * Lobbying Data: the folder `Lobby` listed as Lobbying Tables
 
 

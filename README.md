---
title: "Estimate PPP distribution in AAPI Communities"
output: html_notebook
---

# Estimate PPP distribution in AAPI Communities

Created by Julie Christie, Data & Impact Editor at Resolve Philly.

## Goal of analysis

This methodology can be used/applied with other racial groups and combinations.

The PPP Loan data is extremely lacking when it comes to racial information for businesses. This means that the analysis cannot prove or disprove that these loans went to any or only a specific group of people. Instead, this analysis enables us to look at the number of loans given to businesses in census tracts with a high density of people within a specific demographic while controlling for deeply residential versus commercial areas.

## Glossary

-   **ACS** --- American Community Survey; An annual survey the U.S. Census bureau conducts to collect an annual snapshot of demographic, housing and other information on United States residents. The presentation of this area is always aggregated by a geographic location.

-   **PPP** --- Payment Protection Program; A federal economic relief fund created in response to COVID-19. There were several rounds of funding, and this analysis looks at all.

-   **Census Tract** --- A geographic designation set by the U.S. Census Bureau. These geographies fit within county boundaries and are akin to looking at "neighborhood" level information.

## Resources used

### Data

-   [2020 Decennial Census (in BLN: Census 2020 Data Co-op)](https://biglocalnews.org/#/open_projects) - August 2021 "redistricting" data release, which include detailed figures about population, race and ethnicity, and housing. The Associated Press processed the data into more usable files and integrated comparative data from the 2010 Census.

-   [SBA PPP Data](https://data.sba.gov/dataset/ppp-foia) --- All PPP loans given (the data is split between loans more and less than \$150,000)

-   [Philadelphia Properties and Assessment History](https://www.opendataphilly.org/dataset/opa-property-assessments) --- Property characteristic and assessment history from the Office of Property Assessment for all properties in Philadelphia. Keep in mind we don't want the historical flavor of the data because we're looking for the most recent point in time we can get.

-   [Census Tracts](https://www.opendataphilly.org/dataset/census-tracts) --- For matching and analyzing demographic data collected and compiled by the U.S. Census Bureau & American Community Survey (ACS) to the geography of Census Block Group boundaries within the City of Philadelphia.

### Tools

-   [R and RStudio](rstudio.cloud/) --- To create tables of summary information using microdata.

-   [DB Browser for SQLite](https://sqlitebrowser.org/) --- To combine PPP data sets, clean it, and minimize the data table to Philadelphia.

-   [Geocod.io](https://www.geocod.io/) --- To geocode PPP loans after cleaning.

-   [QGIS](https://www.qgis.org/en/site/) --- To fix the census tracts in the property data.

## CLEANING

### Clean PPP Data

1.  Combine all PPP data tables downloaded in DB Browser for SQLite. This is possible after an update that made the columns and data types uniform across loans above and below \$150,000. This is done by uploading all the .csv files to the database at the same time.

2.  Create a new table that focuses on the loans from the state you want to focus on.

    ```{sql connection=}
     CREATE TABLE PA_PPPLoans AS
       SELECT * FROM All_PPPLoans WHERE BorrowerState = 'PA';
    ```

3.  See what misspellings are possible for the towns you need by creating a table that examines the spelling.

    ```{sql connection=}
    SELECT BorrowerCity, Count(BorrowerCity) FROM PA_PPPLoans
       GROUP BY BorrowerCity;
    ```

4.  Create a table that isolates the loans given in Philadelphia. The previous table will give what spelling parameters to give for this narrowing down process.

    ```{sql connection=}
    CREATE TABLE PHL_PPPLoans AS
       SELECT * FROM PA_PPPLoans WHERE BorrowerCity like 'ph%' OR BorrowerCity like 'pi%l%';
    ```

5.  Delete cities that were incorrectly captured in the previous table.

    ```{sql connection=}
     DELETE FROM PHL_PPPLoans WHERE
       BorrowerCity = 'Philipsburg' OR BorrowerCity = 'PHOENIXVILLE' OR 
       BorrowerCity = 'PHONIXVILLE' OR BorrowerCity = 'PILLOW' OR
       BorrowerCity = 'PINE GROVE MILLS' OR BorrowerCity = 'PINEVILLE' OR 
       BorrowerCity = 'PIPERSVILLE' OR BorrowerCity = 'PIPERVILLE' OR 
       BorrowerCity = 'PITTSFIELD' OR BorrowerCity = 'Pheonixville' OR
       BorrowerCity = 'Phoenixville' OR BorrowerCity = 'Phoenxiville' OR 
       BorrowerCity = 'Phone' OR BorrowerCity = 'Pine Grove Mills' OR 
       BorrowerCity = 'Pipersville' OR BorrowerCity = 'Pittsfield' OR
       BorrowerCity = 'phoenixville' OR BorrowerCity = 'pipersville';
    ```

6.  Rename the misspellings of Philadelphia to the correct ones.

    ```{sql connection=}
     UPDATE PHL_PPPLoans SET BorrowerCity = 'PHILADELPHIA';
    ```

7.  Export this resulting table as a `.csv` file.

8.  Geocode the addresses to get the census tract for each loan. In our work, we used Geocod.io which is a paid service.

9.  Save the resulting `.csv` file to your computer. This will be used in R later.

### Clean ODP Property Data

This data is pretty messed up -- we mostly want to explore summary info on these data at the census tract level, and guess what: the data use 2000 census tracts (sort of -- they're grouped in ways that turn out to align to 2000 CTs but the FIPS numbers do not match existing 2000 census tracts).

If we were to use Geocod.io to fix this, which would definitely be the easiest method, it would cost about \$300. That's not necessarily in our budget, so we're going to use QGIS to fix this problem.

1.  Open QGIS and save a new project.

2.  Add a layer with the Loans as a Delimited Text layer. Then style them to be colored by the census tract assigned by Geocod.io.

    1.  Set the **X field** to `lat` and **Y field** to `lng` because this dataset has them mixed up. Change the layer name to `Properties`.

    2.  In `Symbology`, we need to change it so that the color changes based on the assigned census tract.

        1.  Change the first dropdown from **Single Symbol** to **Categorized**

        2.  Set the **Value** to `Census_Tract`

        3.  Click **Classify**

        4.  Change the symbol to 45% **opacity** and remove the **border**. Reduce the **size** to 1.

3.  Add a layer with the 2020 Census Tracts as a vector layer. Then Style them to be just borders and labeled with the tract numbers

    1.  In `Symbology`, set the **Simple fill** \> **Fill style** to "No Brush"

    2.  In `Labels`, change the dropdown to select `Single Labels`. Set the **Value** to `GEOID20`, a string that will have the State, County, and census tract FIPS numbers combined. Change the **size** to 8.

    3.  Right click and rename the layer to `CensusTract_2020`

4.  Go to **View** \> **Panels** \> **Processing Toolbox** and then click on **Vector general** \> **Join attributes by location**. In the panel that pops up, only change the following fields to be:

    1.  **Base**: Properties

    2.  **Join**: Census

    3.  **Fields**: `GEOID20`

    4.  **Join Type**: Create Separate feature for each matching feature (one-to-many)

5.  Go get a snack, this is going to take a long time. When it's finally done, you can export the combined layer into a .csv file.

## ANALYSIS

### Get P-R-epared (haha)

1.  Load Tidyverse and Tidycensus. [Don't forget to get a Census API key.](http://api.census.gov/data/key_signup.html) *\*This setup was written for use with the 2019 5Y ACS. However, the rest of the census-related work is done with the 2020 Decennial census, which at the time was not do-able with* **tidycensus***.*

    ```{r}
    options(scipen = 999)
    library(tidyverse)
    library(tidycensus)
    library(readr)
    library(RSQLite)
    library(pollster)
    census_api_key("75007795b5e9fa3e79f0c006845eb1c62c0a92c9")
    ```

### Determine Census Tracts with High AAPI Populations

We're going to use Census data to determine what census tracts in Philadelphia can be considered ones with a "high" percentage of AAPI people. We'll do this by combining the number of people who identify as `Asian Only` and `Native Hawaiian and Other Pacific Islander Alone` *(which we'll reference as NHPI in shorthand.)*

1.  Download decennial census data from Big Local News, which was already cleaned for use by news outlets. Don't duplicate base work if you don't have to! I used the file labeled `05_tract_pl94171_standard_compare_2010_2020.csv`. You will need to make the `.pl` file into a .csv file, which you can do by importing it into Excel.

2.  Upload this census data into your environment through your preferred method (either manual code or through your files). Either way, here's the code that should be generated:

    ```{r}
    library(readr)
    DATA_Census1 <- read_csv("data/2020Census_PADecennial/05_tract_pl94171_standard_compare_2010_2020.csv")
    ```

3.  Narrow it down to just show Philadelphia County. This can overwrite the table, since I don't want to look at any other places.

    ```{r}
    DATA_Census1 <- DATA_Census1 %>%
      filter(county_name == 'Philadelphia County') 
    ## I deliberately ignored the parsing error. I'll revisit it if it becomes an issue later.
    ```

4.  Now we're going to set this data frame up to tell us the percentages rather than the actual numbers for the different races we pulled in. We're also going to be combining `AsianAlone` with `NHOPIAlone` to get a more inclusive number to count as AAPI.

    ```{r}
    DATA_Census2 <- DATA_Census1 %>%
      mutate(WhitePct = `2020_pop_white_non_hisp`/`2020_pop`) %>%
      mutate(BlackPct = `2020_pop_black_non_hisp`/`2020_pop`) %>%
      mutate(AAPIPct = (`2020_pop_asian_non_hisp` + `2020_pop_nhpi_non_hisp`)/`2020_pop`) %>%
      select(GEOID_11, state_code, county_code, tract_code, tract_name, `2020_pop`, WhitePct, BlackPct, AAPIPct)
    ```

5.  It's time to find out at what point in each population is its high density. In this, I've decided that any census tract in the fourth quantile of the percentages will be considered "High". Let's calculate what those numbers are.

    ```{r}
    SUM_Census2020 <- DATA_Census2 %>%
      summarise(WhiteQ4 = quantile(WhitePct, 0.75, na.rm = TRUE),
                BlackQ4 = quantile(BlackPct, 0.75, na.rm = TRUE),
                AAPIQ4 = quantile(AAPIPct, 0.75, na.rm = TRUE))

    tibble(SUM_Census2020)
    ```

    Great! Now we know that we can label whether a census tract has a high white, Black, or AAPI population and what those thresholds are.

6.  Our next step is to label these. We know that any census tract with an `AAPIPct` population **more than 10% is what we'll consider "high".** We're going to use the `WhitePct` and `BlackPct` to identify the largest demographic in the tract.\
    \
    *Note: In Philly, if you sort through `DATA_Census2`, you'll see that there is only one census tract more than 50% made up of an AAPI population. That's why we're going to add the info on what the largest race is for each tract.*

    ```{r}
    DATA_Census3 <- DATA_Census2 %>%
      # Add the column to indicate that the Census Tract is "high"
      mutate(AAPIHigh = case_when(AAPIPct >= 0.10 ~ "Y",
                                      TRUE ~ "N")) %>%
      # Add the column to indicate what the majority race is in that census tract.
      mutate(LargeRace = case_when(WhitePct > BlackPct & WhitePct > AAPIPct ~ "White",
                                 BlackPct > WhitePct & BlackPct > AAPIPct ~ "Black",
                                 AAPIPct > WhitePct & AAPIPct > BlackPct ~ "AAPI",
                                  TRUE ~ "No majority")) %>%
      select(GEOID_11, AAPIHigh, AAPIPct, WhitePct, BlackPct, LargeRace)
    ```

7.  For this data to be ready to merge we'll need a universal matching code for each census tract. We're going to use what's called the **GEOID** which combines the state and county FIPS numbers with the Census Tract number. Our Census tables already have this because we chose to keep the column `GEOID_11` from step 4, so we don't have to do anything. But this is a column title we'll use in other data sets as well.

### Upload the PPP loan data into your environment and do some more cleaning

After cleaning and before uploading to R, I geocoded the loans using [Geocod.io](https://www.geocod.io/) so I can connect information at the Census Tract level. This code uses the appended columns from the process.

1.  Load in the cleaned PPP loan data as `DATA_Loans1`.

    ```{r}
    library(readr)
    DATA_Loans1 <- read_csv("data/New_Geocoded.csv")
    ```

2.  Our next step will be to make another table (`DATA_Loans2`) that filters out all the bad geocodes and removes columns that we won't need. We'll have to rename the `County FIPS` column to not have any spaces because for some reason the filter doesn't work otherwise.

    ```{r}
    # Rename a columnt to make your next step a bit easier
    DATA_Loans1 <- rename(DATA_Loans1, CountyFIPS = "County FIPS")

    DATA_Loans2 <- DATA_Loans1 %>%
      filter(CountyFIPS == 42101) %>%
      select("LoanNumber", "DateApproved", "SBAOfficeCode", "ProcessingMethod", "BorrowerName", "BorrowerAddress", "BorrowerCity", "BorrowerState", "BorrowerZip", "LoanStatusDate", "LoanStatus", "Term", "SBAGuarantyPercentage", "InitialApprovalAmount", "CurrentApprovalAmount", "UndisbursedAmount", "FranchiseName", "ServicingLenderLocationID", "ServicingLenderName", "BusinessAgeDescription", "JobsReported", "NAICSCode", "Race", "Ethnicity", "UTILITIES_PROCEED", "PAYROLL_PROCEED", "MORTGAGE_INTEREST_PROCEED", "RENT_PROCEED", "REFINANCE_EIDL_PROCEED", "HEALTH_CARE_PROCEED", "DEBT_INTEREST_PROCEED", "BusinessType", "Gender", "Veteran", "NonProfit", "Latitude", "Longitude", "Accuracy Score", "State FIPS", "CountyFIPS", "Place Name", "Place FIPS", "Census Tract Code", "Full FIPS (tract)")
    ```

3.  Great! Now we have a cleaner, more narrowed down set of loans to work with. We're now going to merge `DATA_Loans2` with `DATA_Census3` so we can see some neighborhood characteristics for the loans. To do this we'll need that identical `GEOID_11` column. The value exists already under `Full FIPS (tract)` so we just need to rename it.

    ```{r}
    DATA_Loans2 <- rename(DATA_Loans2, GEOID_11 = "Full FIPS (tract)")

    DATA_Loans3 <- merge(DATA_Loans2, DATA_Census3)
    ```

    You should now be able to see a data frame with the same number of observations, and about 5 more variables.

### Determine the commercial/residential rates of census tracts

This next phase we're doing will determine whether the census tracts we identify are ones with a lot or very few commercial properties. What this will tell us is whether areas with low loans are also more residential (which would make sense)

1.  Upload the fixed Property Assessment Property data and name it something like `DATA_Properties1`. We'll need to change the type of a few different columns to make sure that we don't have any parsing errors.

    ```{r}
    DATA_Properties1 <- read_csv("data/Geocoding/PropertiesCoded.csv", 
        col_types = cols(assessment_date = col_date(),
                         date_exterior_condition = col_character(),
                         other_building = col_character(),
                         unfinished = col_character()))
    ```

2.  Next we're going to create summaries that are grouped by the Census Tract. We're going to calculate the total number of different property types for each census tract, but I can't figure out the best code to do this. The method below makes a new table for each category type that will later get merged together so that we have them all in one table.

    ```{r}
    DATA_Properties2 <- DATA_Properties1 %>%
      group_by(GEOID20) %>%
      summarise(TotalProps = n())

    DATA_Properties2a <- DATA_Properties1 %>%
      filter(category_code == 1) %>%
      group_by(GEOID20) %>%
      summarise(Residential = n())

    DATA_Properties2b <- DATA_Properties1 %>%
      filter(category_code == 2) %>%
      group_by(GEOID20) %>%
      summarise(HotelsApts = n())

    DATA_Properties2c <- DATA_Properties1 %>%
      filter(category_code == 3) %>%
      group_by(GEOID20) %>%
      summarise(StoreDwell = n())
      
    DATA_Properties2d <- DATA_Properties1 %>%
      filter(category_code == 4) %>%
      group_by(GEOID20) %>%
      summarise(Commercial = n())

    DATA_Properties2e <- DATA_Properties1 %>%
      filter(category_code == 5) %>%
      group_by(GEOID20) %>%
      summarise(Industrial = n())

    DATA_Properties2f <- DATA_Properties1 %>%
      filter(category_code == 6) %>%
      group_by(GEOID20) %>%
      summarise(VacantLand = n())
    ```

3.  Now we're merging those tables.

    ```{r}
    DATA_Properties3 <- merge(DATA_Properties2, DATA_Properties2a, all = TRUE)
    DATA_Properties3 <- merge(DATA_Properties3, DATA_Properties2b, all = TRUE)
    DATA_Properties3 <- merge(DATA_Properties3, DATA_Properties2c, all = TRUE)
    DATA_Properties3 <- merge(DATA_Properties3, DATA_Properties2d, all = TRUE)
    DATA_Properties3 <- merge(DATA_Properties3, DATA_Properties2e, all = TRUE)
    DATA_Properties3 <- merge(DATA_Properties3, DATA_Properties2f, all = TRUE)
    ```

4.  The final step is to change up the values so that we're looking at percent rather than raw numbers. But the percent we want to use is a much more general "Residential" versus "Commercial" rather than 6 different categories. So, we're going to group all the properties that could have some place of business within them in the same column and everything else will be labeled for residential.

    ```{r}
    DATA_Properties4 <- DATA_Properties3 %>%
      mutate_all(funs(ifelse(is.na(.), 0, .)))%>%
      mutate(ResidentialPCT = (Residential + VacantLand)/TotalProps) %>%
      mutate(CommercialPCT = (HotelsApts + StoreDwell + Commercial + Industrial)/TotalProps) %>%
      mutate(GEOID_11 = GEOID20) %>%
      select(GEOID_11, TotalProps, ResidentialPCT, CommercialPCT)

    # To keep our environment a bit more manageable, we're going to delete all the sub-Properties2 tables.
    rm(DATA_Properties2a)
    rm(DATA_Properties2b)
    rm(DATA_Properties2c)
    rm(DATA_Properties2d)
    rm(DATA_Properties2e)
    rm(DATA_Properties2f)
    ```

5.  Now we want to do something very similar with what we did with the census data to see the distribution of residential/commercial properties. The goal is to be able to identify places that are very high in residential properties, so that we can see where there are census tracts that have few loans because there are just no businesses there.

    ```{r}
    SUM_Properties <- DATA_Properties4 %>%
      summarise(Residential = quantile(ResidentialPCT, 0.25, na.rm = TRUE),
                Commercial = quantile(CommercialPCT, 0.25, na.rm = TRUE))

    tibble(SUM_Properties)
    ```

6.  The results show us that a census tract with less than 8.2% commercial properties are "Low commercial" so now we're going to make a new table that has that label so we can merge it with the combined loan and census data.

    ```{r}
    DATA_Properties5 <- DATA_Properties4 %>%
      mutate(CommercialDensity = case_when(CommercialPCT <= 0.08188406 ~ "Low Commercial",
                                           TRUE ~ "Commercial"))

    DATA_All <- merge(DATA_Loans3, DATA_Properties5)
    ```

7.  Finally, let's export the large data set that we'll use for some visualization.

    ```{r}
    write.csv(DATA_All, file = "data/exports/DATA_AllLoans.csv")
    ```

# Getting those numbers

We're going to put together some numbers about this data that will be explorable and determine where the reporting goes next. Here are some of the main things we'll put together:

-   A map of the census tracts with the highest AAPI populations, extruded by number and \$\$ of loans

-   Top 20 census tracts with most loans and \$\$ of high AAPI

-   Bottom 20 census tracts with least loans and \$\$ of high AAPI

-   median loans and \$\$ for city

-   difference in median for loans w/ Black vs white majority

-   top industries x10 for \# of loans

-   top industries x10 for \$ of loans

**Prep the data for the map**

```{r}
VIS_TractTots <- DATA_All %>%
  group_by(GEOID_11) %>%
  summarise(AAPIPct = median(AAPIPct, na.rm = FALSE),
            WhitePct = median(WhitePct, na.rm = FALSE),
            BlackPct = median(BlackPct, na.rm = FALSE),
            NumberLoans = n(),
            AmountLoans = sum(InitialApprovalAmount),
            MedianLoans = median(InitialApprovalAmount))

VIS_TractTots <- VIS_TractTots %>%
  mutate(AAPIHigh = case_when(AAPIPct >= 0.099 ~ "Y",
                                  TRUE ~ "N")) %>%
  mutate(MajRace = case_when(WhitePct > BlackPct & WhitePct > AAPIPct ~ "White",
                             BlackPct > WhitePct & BlackPct > AAPIPct ~ "Black",
                             AAPIPct > WhitePct & AAPIPct > BlackPct ~ "AAPI",
                              TRUE ~ "No majority"))

write.csv(VIS_TractTots, file = "data/exports/VIS_TractTots.csv")
```

**Get top/bottom 20 CTs for loan \#s and \$s**

```{r}
VIS_CTSortByNum <- VIS_TractTots %>%
  filter(AAPIHigh == "Y") %>%
  arrange(NumberLoans)

VIS_CTBotNum <- VIS_CTSortByNum[c(1:20),c(1:9)]

VIS_CTTopNum <- VIS_CTSortByNum[c(71:90),c(1:9)]

write.csv(VIS_CTBotNum, file = "data/exports/VIS_CTBotNum.csv")
write.csv(VIS_CTTopNum, file = "data/exports/VIS_CTTopNum.csv")
```

**Get total city medians**

```{r}
VIS_CityLevels <- DATA_All %>%
  summarise(MedianLoan = median(InitialApprovalAmount),
            TotalLoans = n(),
            MaxLoan = max(InitialApprovalAmount),
            MinLoan = min(InitialApprovalAmount))

write.csv(VIS_CityLevels, file = "data/exports/VIS_CityLevels.csv")
```

**Median difference in Black vs white majority neighborhoods**

```{r}
VIS_MajorityDiff <- DATA_All %>%
  group_by(LargeRace) %>%
  summarise(TotalLoans = n(),
            MedianLoan = median(InitialApprovalAmount),
            TotalDollars = sum(InitialApprovalAmount))

write.csv(VIS_MajorityDiff, file = "data/exports/VIS_MajorityDiff.csv")
```

**Get those individual loans**

```{r}
# Best 5 census tracts
RAW_TopFiveCTs <- DATA_All %>%
  filter(GEOID_11 %in% c(42101031000, 42101019100, 42101003901, 42101035500, 42101003600))

write.csv(RAW_TopFiveCTs, file = "data/exports/RAW_TopFiveCTs.csv")

# Bottom 5 census tracts
RAW_BottomFiveCTs <- DATA_All %>%
  filter(GEOID_11 %in% c(42101980002, 42101008702, 42101012501, 42101036600, 42101008801))

write.csv(RAW_BottomFiveCTs, file = "data/exports/RAW_BottomFiveCTs.csv")
```

```{r}
Example <- DATA_All %>%
  filter(GEOID_11 %in% c(42101004103, 42101004104))

write.csv(Example, "data/exports/example.csv")
```

```{r}
# All loans in Chinatown
VIS_ChinatownOnly <- DATA_All %>%
  filter(GEOID_11 == 42101000200)

write.csv(VIS_ChinatownOnly, "data/exports/VIS_ChinatownOnly.csv")
```

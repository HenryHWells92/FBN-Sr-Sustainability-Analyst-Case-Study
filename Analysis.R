## FBN Sr Sustainability Analyst - Case Study Exercise
## Henry Wells; 3 June 2022

library(ggplot2)
library(gridExtra)
library(corrplot)
library(visdat)
library(sp)
library(spdep)
library(sf)
library(terra)
library(leaflet)
library(reshape2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(usdarnass)
library(config)
library(geojsonio)
library(GADMTools)
library(readxl)

setwd("/Users/henrywells/Desktop/FBN Sr Sustainability Analyst Case Study/")


## ASSUMPTIONS
# Focus only on COMET data rows where class=="Cropland Management" 
    # (given that FBN programs are for grain producers & grain buyers)

# Given the exercise's emphasis on "carbon credits" (bottom of the document),
# focus this analysis on Carbon/CO2 sequestration potential of each practice,
# rather than other outcomes such as Nitrous Oxide or Methane.

# Also, limit the practices in focus from the COMET data set focused on FBN's 
# list of promoted practices, per the exercise:
  # "The regenerative practices we promote include reducing tillage, 
  # planting cover crops, optimizing nitrogen use, and maximizing yields 
  # through seed selection."

# Given the specific inclusion of corn_acres in the Case Study data set, assume
# that grain buyers in the audience for this analysis are looking specifically to
# buy corn, not other crops, and that the farmer audience for this analysis would
# be made up primarily of corn growers.

# NOTE: Data are unavailable from either the USDA Census of Ag or USDA Surveys for:
  # yield info specific to corn for grain
  # yield info specific to irrigated vs non-irrigated corn


## Exploring COMET data

# read COMET data provided:
cometdata <- read_xlsx("./US_COMET-Planner_Download.xlsx", sheet=2, col_names=TRUE)
sdcomet <- cometdata[which(cometdata$state == "SD"), ]

# subset to focus on cropland only:
sdcomet_cropland <- sdcomet[which(str_detect(sdcomet$class, "^Cropland.")), ]

# Consider "Cropland Management" and "Cropland to Herbaceous Cover" separately...
# which practices do these each include?
sdcomet_cropmgnt <- sdcomet_cropland[which(sdcomet_cropland$class=="Cropland Management"), ]
dim(sdcomet_cropmgnt); unique(sdcomet_cropmgnt$cps_name)
sdcomet_croptoherbcover <- sdcomet_cropland[which(sdcomet_cropland$class=="Cropland to Herbaceous Cover"), ]
dim(sdcomet_croptoherbcover); unique(sdcomet_croptoherbcover$cps_name)

# Most, if not all, of the COMET practices bucketed under "Cropland to Herbaceous Cover"
# are geared toward 'permanent' shifts from grain production to some alternative land use.
# Exclude this subset from here forward and focus specifically on "Cropland Management" practices.

# Within the "Cropland Management" class, there are a few practices that don't fit within 
# FBN's promoted list. Exclude these from here forward:
exclusion <- which(sdcomet_cropmgnt$cps_name == "Combustion System Improvement (CPS 372)" | 
                     sdcomet_cropmgnt$cps_name == "Mulching (CPS 484)" |
                     sdcomet_cropmgnt$cps_name == "Stripcropping (CPS 585)")
sdcomet_fbn <- sdcomet_cropmgnt[-exclusion, ]
dim(sdcomet_fbn); unique(sdcomet_fbn$cps_name)

# Examine what specific practices are included under cps_name=="Multiple Conservation Practices":
multipractice <- sdcomet_fbn[which(sdcomet_fbn$cps_name=="Multiple Conservation Practices"), ]
dim(multipractice); unique(multipractice$planner_implementation)
# All of these reflect practice change regimes within what FBN promotes - keep these rows.

## Finally, a note that a manual 'unit test' of the COMET data set I conducted in excel revealed
## that the practices in question are not merely additive in nature. For example, the total CO2
## sequestered on non-irrigated cropland in Moody county, SD from a combination (0.88) of converting  
## Intensive Till to No-Till (by itself: 0.57) along with adding a legume cover crop (by itself: 0.40)
## is not equal to the sum of the mean CO2 sequestered if both practices are chosen separately.

## Therefore - the 'grain' of the data for this analysis is defined by: 
      # county x planner_implementation


# -----------------------------------------------------------------------------

## Reading, cleaning, and enriching Case Study & additional USDA NASS data sets

# read case study data provided:
Case_data <- read.csv("./Sustainability_Analyst_Case_Study.csv", header = TRUE)

# Retrieve data on cropland acres, operations, and irrigation from USDA NASS api, 
# selecting from the 2017 census.
# (for consistency with the source of the rest of the case study data set):
key <- config::get(value="api_key", config="NASS_QuickStatsAPI")
nass_set_key(key)
aglanddata <- nass_data(source_desc = "CENSUS", group_desc = "FARMS & LAND & ASSETS", 
                             commodity_desc = "AG LAND", agg_level_desc = "COUNTY",
                             state_name = "SOUTH DAKOTA",
                             freq_desc = "ANNUAL", year = "2017")
# conserve total acres & number of operations for all cropland and irrigated cropland:
dataitems <- unique(aglanddata$short_desc)[c(20,21,30,31)]
nass_sub <- aglanddata[which(aglanddata$short_desc %in% dataitems), ]
# keep only relevant columns:
nass_sub <- nass_sub[,c(22,10,38)]
# xtabs(formula = ~., data = nass_sub[,c(1,2)]) # used to check for duplicates
# reshape nass_sub such that each measure is it's own column:
nass_sub <- dcast(nass_sub, formula = county_name ~ short_desc, value.var = "Value")
names(nass_sub) <- c("county_name", "total_cropland_acres_NASS", "total_cropland_ops_NASS",
                     "total_irrigated_cropland_acres_NASS", "total_irrigated_cropland_ops_NASS")

# clean up unwanted characters in NASS output data and convert measure columns to numeric:
numcols <- c(2,3,4,5)
for(i in numcols){
  nass_sub[,i] <- gsub("\\(", "", nass_sub[,i])
  nass_sub[,i] <- gsub("\\)", "", nass_sub[,i])
  nass_sub[,i] <- gsub("\\,", "", nass_sub[,i])
  nass_sub[,i] <- gsub("\\D", "0", nass_sub[,i])
  nass_sub[,i] <- as.numeric(nass_sub[,i])
}

# Repeat this to retrieve specific acreage and production info for irrigated corn for grain:
cornacdata <- nass_data(source_desc = "CENSUS", group_desc = "FIELD CROPS", 
                        commodity_desc = "CORN", agg_level_desc = "COUNTY",
                        state_name = "SOUTH DAKOTA", domain_desc = "TOTAL",
                        freq_desc = "ANNUAL", year = "2017")
dataitems2 <- unique(cornacdata$short_desc)[c(3,5,9)]
nass_sub2 <- cornacdata[which(cornacdata$short_desc %in% dataitems2), ]
nass_sub2 <- nass_sub2[,c(22,10,38)]
nass_sub2 <- dcast(nass_sub2, formula = county_name ~ short_desc, value.var = "Value")
names(nass_sub2) <- c("county_name", "corn_forgrain_ac_harvested_NASS", 
                      "corn_forgrain_production_bu_NASS_2017", 
                      "corn_forgrain_irrigated_ac_harvested_NASS")
numcols <- c(2,3,4)
for(i in numcols){
  nass_sub2[,i] <- gsub("\\(", "", nass_sub2[,i])
  nass_sub2[,i] <- gsub("\\)", "", nass_sub2[,i])
  nass_sub2[,i] <- gsub("\\,", "", nass_sub2[,i])
  nass_sub2[,i] <- gsub("\\D", "0", nass_sub2[,i])
  nass_sub2[,i] <- as.numeric(nass_sub2[,i])
}


# join nass_sub and nass_sub2 to Case_data:
nass_sub <- dplyr::mutate(nass_sub, "joinkey" = tolower(county_name))
nass_sub2 <- dplyr::mutate(nass_sub2, "joinkey" = tolower(county_name))
Case_data <- dplyr::mutate(Case_data, "joinkey" = tolower(county))
joined <- base::merge(Case_data, nass_sub, by="joinkey", all = TRUE)
joined <- base::merge(joined, nass_sub2, by="joinkey", all = TRUE)
# eliminate duplicate columns from 'joined'
joined <- joined[,-c(1,16,17,21)]


# Check for missing data (NAs) before proceeding to enrich the data with calculated fields:
vis_miss(joined)
# various columns do have NA values present, with NAs overall representing 2.7% of the data set
# and occurring by far most frequently in the column "corn_forgrain_irrigated_ac_harvested_NASS"

# Four options for how to deal with NAs:
    # 1. Assume NA indicates a zero value (implies meaning from source)
    # 2. Compare NAs against NASS data query results & fill in with an observed value
    # 3. Interpolate a non-zero value using average values of all neighboring polygons
    # 4. Interpolate a non-zero value using a regression model

# the spatial dimension might be useful here so let's convert 'joined' to a spatial object:
crdref <- "+proj=longlat +datum=WGS84"
tmp_sv <- vect(joined, crs=crdref, geom="geometry")
tmp_sf <- sf::st_as_sf(tmp_sv)


# both yield and n_needs_lbs_acre have only 1 missing value each - interpolate with avg value
    # from all neighboring polygons (option 3)
ggplot(data = tmp_sf) + geom_sf(data = tmp_sf, aes(fill = yield)) +
  theme(legend.position = "bottom")
ggplot(data = tmp_sf) + geom_sf(data = tmp_sf, aes(fill = n_needs_lbs_acre)) +
  theme(legend.position = "bottom")

# create neighbors list:
nb <- poly2nb(tmp_sf, queen=TRUE) # throws error
sf::sf_use_s2(FALSE) # switching off spherical geometry 
# (see this link for explanation: 
# https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data )
nb <- poly2nb(tmp_sf, queen=TRUE) # trying again - working!
# identify neighbors for each missing value:
missingyield <- which(is.na(joined$yield)); joined$county[missingyield]; nb[missingyield]
missingNneeds <- which(is.na(joined$n_needs_lbs_acre)); joined$county[missingNneeds]; nb[missingNneeds]
# examine neighboring values:
joined$yield[nb[missingyield][[1]]]
joined$n_needs_lbs_acre[nb[missingNneeds][[1]]]
# interpolate using mean of neighboring values
joined$yield[missingyield] <- mean(joined$yield[nb[missingyield][[1]]])
joined$n_needs_lbs_acre[missingNneeds] <- mean(joined$n_needs_lbs_acre[nb[missingNneeds][[1]]])
# re-incorporate to tmp_sf and plot result:
tmp_sv <- vect(joined, crs=crdref, geom="geometry")
tmp_sf <- sf::st_as_sf(tmp_sv)
ggplot(data = tmp_sf) + geom_sf(data = tmp_sf, aes(fill = yield)) +
  theme(legend.position = "bottom")
ggplot(data = tmp_sf) + geom_sf(data = tmp_sf, aes(fill = n_needs_lbs_acre)) +
  theme(legend.position = "bottom")


vis_miss(joined)
# corn_acres has 5 missing values, and none of these are from neighboring polygons:
ggplot(data = tmp_sf) + geom_sf(data = tmp_sf, aes(fill = corn_acres)) +
  theme(legend.position = "bottom")
# to avoid introducing extra autocorrelation, and given the ease of retrieving an alternate
# observed value from the same data source, we can instead use the 5-year average value for
# these 5 counties from the Ag survey data set available from USDA NASS:
missingcornacres <- joined$county[which(is.na(joined$corn_acres))]; missingcornacres

# query corn acreage from USDA NASS survey data sets from 2017-2021 per SD county:
cornac_surveys <- nass_data(source_desc = "SURVEY", group_desc = "FIELD CROPS", 
                        commodity_desc = "CORN", agg_level_desc = "COUNTY",
                        state_name = "SOUTH DAKOTA", domain_desc = "TOTAL",
                        freq_desc = "ANNUAL", year = ">2016")
dataitems3 <- unique(cornac_surveys$short_desc)[1]
nass_sub3 <- cornac_surveys[which(cornac_surveys$short_desc %in% dataitems3), ]
nass_sub3 <- nass_sub3[,c(22,10,31,38)]
nass_sub3 <- nass_sub3[which(tolower(nass_sub3$county_name) %in% tolower(missingcornacres)), ]
unique(nass_sub3$county_name) # only two of the counties with missing values show up!
table(nass_sub3$county_name, nass_sub3$year); dim(nass_sub3) # many years have zero values anyway...
nass_sub3 # view all relevant data returned from this query...
joined[which(is.na(joined$corn_acres)), c(2,5,18)] # compare to values for "corn_forgrain_ac_harvested_NASS" from 2017 Ag Census...

# Based on the above query results and the limited number of data points available for these
# counties from the USDA surveys, I opted to simply use the "corn_forgrain_ac_harvested_NASS"
# values, which were originally queried from the 2017 Ag Census earlier in this script. 
# This isn't a perfect solution but I felt more comfortable using these values to fill NAs in
# "corn_acres" than I did either inserting a zero value in those rows, or interpolating with an
# average from neighboring polygons, given the higher share (5/66) of rows with an NA value.

# Fill in NAs in corn_acres with matching values from "corn_forgrain_ac_harvested_NASS":
joined[which(is.na(joined$corn_acres)), 5] <- joined[which(is.na(joined$corn_acres)), 18]
vis_miss(joined)
tmp_sv <- vect(joined, crs=crdref, geom="geometry")
tmp_sf <- sf::st_as_sf(tmp_sv)
ggplot(data = tmp_sf) + geom_sf(data = tmp_sf, aes(fill = corn_acres)) +
  theme(legend.position = "bottom")


# Next - look at missing values in "conventional_tillage_acres" and "cover_crop_acres"
joined$county[which(is.na(joined$conventional_tillage_acres))]
joined$county[which(is.na(joined$cover_crop_acres))]
tp1 <- ggplot(data = tmp_sf) + 
  geom_sf(data = tmp_sf, aes(fill = conventional_tillage_acres)) +
  theme(legend.position = "right")
tp2 <- ggplot(data = tmp_sf) + 
  geom_sf(data = tmp_sf, aes(fill = cover_crop_acres)) +
  theme(legend.position = "right")
grid.arrange(tp1,tp2, ncol=1) # plot seems to be rendering NAs as zeros...interesting...
# View(joined) << definitely NA's, not zero values

# Manual investigation revealed that the three missing counties for "conventional_tillage_acres"
# are contiguous - all located in central SD - and that the missing county for "cover_crop_acres"
# is Oglala Lakota county, which is the same county that was originally missing a value for 
# "n_needs_lbs_acre". 

# Though there are inevitably some issues with this approach, I felt comfortable filling each of
# these remaining missing values with zeros after all - the fact that this data is missing may
# simply indicate that these practices aren't in use. The three counties missing conventional
# tillage data are among the highest in terms of usage of no-till management, which may mean
# that no-till is simply the prevailing tillage practice in those counties. Similarly, Oglala
# Lakota county is located in a region (southwest corner) of SD where cover crop acreage in 
# neighboring counties is among the lowest in the state - perhaps also indicating that using
# cover crops simply isn't common in that county. 

# To view data for these rows:
# View(joined[which(is.na(joined$conventional_tillage_acres)), ])
# View(joined[which(is.na(joined$cover_crop_acres)), ])
# Fill missing values with 0s:
joined[which(is.na(joined$conventional_tillage_acres)), 8] <- 0
joined[which(is.na(joined$cover_crop_acres)), 9] <- 0
# plot results:
vis_miss(joined)
tmp_sv <- vect(joined, crs=crdref, geom="geometry")
tmp_sf <- sf::st_as_sf(tmp_sv)
ggplot(data = tmp_sf) + geom_sf(data = tmp_sf, aes(fill = conventional_tillage_acres)) +
  theme(legend.position = "bottom")


# Finally - need to deal with missing values in irrigation-related columns.
tp3 <- ggplot(data = tmp_sf) + geom_sf(data = tmp_sf, aes(fill = total_irrigated_cropland_acres_NASS)) +
  theme(legend.position = "right")
tp4 <- ggplot(data = tmp_sf) + geom_sf(data = tmp_sf, aes(fill = corn_acres)) +
  theme(legend.position = "right")
grid.arrange(tp3,tp4, ncol=1) # irrigation seems to vary a bit more randomly, 
  # but each of the missing states also seems to have a comparatively lower corn acreage
  # than much of the state.

# For simplicity, I've chosen to simply fill values for these columns with zeros, taking
# the same point of view as I did for the use of tillage and cover cropping practices - i.e.
# a missing value would indicate that those practices aren't in use in those counties.

# This feels dubious, considering the heterogeneity of irrigation practices observed in
# other SD counties and the nature of the data source (USDA Census), so this may be something
# to unpack with additional data and time. 

# Fill missing values with 0s:
joined[which(is.na(joined$total_irrigated_cropland_acres_NASS)), 16] <- 0
joined[which(is.na(joined$total_irrigated_cropland_ops_NASS)), 17] <- 0
# plot results:
vis_miss(joined)
tmp_sv <- vect(joined, crs=crdref, geom="geometry")
tmp_sf <- sf::st_as_sf(tmp_sv)
ggplot(data = tmp_sf) + geom_sf(data = tmp_sf, aes(fill = total_irrigated_cropland_acres_NASS)) +
  theme(legend.position = "bottom")

# Finally - the column "corn_forgrain_irrigated_ac_harvested_NASS" has a total of 16 values 
# missing, or about 24% of the data in that column. This is too high a rate of missing-ness to
# recouperate for analysis, so I will simply eliminate this column from here forward.
length(which(is.na(joined$corn_forgrain_irrigated_ac_harvested_NASS))) / nrow(joined)
joined <- joined[,-20]


# Calculate relative shares of all cropland acreage represented by:
    # corn vs "not corn"
    # corn for grain as % of all cropland acres
    # irrigated acres as % of all cropland acres
    # each of the 3 tillage types
    # share of acres with a cover crop
# Additionally, calculate the following relative metrics for nutrient mgnt:
    # total N applied (manure + conventioinal)
    # total N applied as a % of corn's N requirement
        # manure N applied as a % of total N applied
        # conv N applied as a % of total N applied
    # also calculate difference: ("TotalN" - N req) as "N Surplus"
        # could also create logical field indicating N surplus vs N deficit

enriched <- mutate(joined, "corn_perc_of_all_ac" = corn_acres/cropland_acres,
                 "corn_grain_perc_of_all_ac" = corn_forgrain_ac_harvested_NASS/cropland_acres,
                 "irrigated_cropland_perc_all" = total_irrigated_cropland_acres_NASS/cropland_acres,
                 "notill_perc_all_ac" = conservation_tillage_notill_acres/cropland_acres,
                 "reducedtill_perc_all_ac" = conservation_tillage_excl_notill_acres/cropland_acres,
                 "convtill_perc_all_ac" = conventional_tillage_acres/cropland_acres,
                 "covercrop_perc_all_ac" = cover_crop_acres/cropland_acres,
                 "total_N_applied_lbs_ac" = fert_n_rate_lbs_acre + manure_n_rate_lbs_acre,
                 "ratio_total_N_applied_relative_to_need" = (fert_n_rate_lbs_acre + manure_n_rate_lbs_acre)/n_needs_lbs_acre,
                 "ratio_conv_N_applied_relative_to_need" = fert_n_rate_lbs_acre/n_needs_lbs_acre,
                 "ratio_manure_N_applied_relative_to_need" = manure_n_rate_lbs_acre/n_needs_lbs_acre,
                 "N_surplus_lbs_ac" = (fert_n_rate_lbs_acre + manure_n_rate_lbs_acre) - n_needs_lbs_acre)


# convert enriched data set to spatial format(s):
crdref <- "+proj=longlat +datum=WGS84"
case_sv <- vect(enriched, crs=crdref, geom="geometry")
case_sf <- sf::st_as_sf(case_sv)


# -----------------------------------------------------------------------------

## Exploratory Plots

# Fertilizer use:
p1 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = total_N_applied_lbs_ac)) +
  theme(legend.position = "bottom")
p2 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = fert_n_rate_lbs_acre)) +
  theme(legend.position = "bottom")
p3 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = manure_n_rate_lbs_acre)) +
  theme(legend.position = "bottom")
p4 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = N_surplus_lbs_ac)) +
  theme(legend.position = "bottom")
grid.arrange(p1,p2,p3,p4, ncol=2)
# takeaways: Higher N use in East, higher N surplus in East, 
# most of overall N use and N surplus seems due to N from fertilizers rather than manure;
# seems to be an inverse relationship between fertilizer N use and manure N use;
# also seems that only a small number of counties are at an N "deficit" - everywhere else
  # seems to be over-applying nitrogen

# corn acreage:
p5 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = corn_acres)) +
  theme(legend.position = "bottom")
p6 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = corn_forgrain_ac_harvested_NASS)) +
  theme(legend.position = "bottom")
p7 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = corn_perc_of_all_ac)) +
  theme(legend.position = "bottom")
p8 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = corn_grain_perc_of_all_ac)) +
  theme(legend.position = "bottom")
grid.arrange(p5,p6,p7,p8, ncol=2)
# takeaways: Corn acreage heavily concentrated in the East as well, and quite correlated with N use;
# Corn for grain acreage is also very closely correlated with overall corn acreage;
# A cluster of counties in the North-East corner of the state dominates overall acreage, but the 
  # counties in the South-East of the state seem to have a higher share of their overall cropland 
  # acres planted to corn. 

# acreage double-click: corn for grain & irrigation use:
p9 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = total_irrigated_cropland_acres_NASS)) +
  theme(legend.position = "bottom")
p10 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = irrigated_cropland_perc_all)) +
  theme(legend.position = "bottom")
p11 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = corn_forgrain_ac_harvested_NASS)) +
  theme(legend.position = "bottom")
p12 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = corn_grain_perc_of_all_ac)) +
  theme(legend.position = "bottom")
grid.arrange(p9,p10,p11,p12, ncol=2)
# takeaways: most corn seems to be sold for grain, especially in the East; 
# meanwhile, the counties with the most corn acreage also seem to use the least irrigation;
# irrigated land has a fairly low share of cropland overall, and most of the counties with a 
# higher percentage of irrigated cropland are in the West, on the opposite side of the state
# from where most of the corn is grown.

# conservation ag practices: tillage & cover crops
p13 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = notill_perc_all_ac)) +
  theme(legend.position = "bottom") + labs(fill="No-Till Percentage of\nCropland Acres")
p14 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = reducedtill_perc_all_ac)) +
  theme(legend.position = "bottom") + labs(fill="Reduced Till Percentage of\nCropland Acres")
p15 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = convtill_perc_all_ac)) +
  theme(legend.position = "bottom")
p16 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = covercrop_perc_all_ac)) +
  theme(legend.position = "bottom")
grid.arrange(p13,p14,p15,p16, ncol=2)
# takeaways: perhaps the most interesting plots so far - no-till is most common in central SD,
# while reduced till is most common in the far Eastern counties, as is conventional tillage;
# most counties that produce a large amount of corn appear to also heavily utilize EITHER 
  # no-till or reduced till, at least compared to the areas of SD with lower corn production...
# also, shares of cropland by tillage type do not all sum to 1 - take note for further analysis...;
# cover cropping meanwhile seems to have the least spatial correlation among all practice 
  # variables - and might therefore have broadest applicability across the state?
  # (what explains the lack of apparent spatial correlation with cover cropping usage)?


# -----------------------------------------------------------------------------

# Exploring spatial and non-spatial correlations and doing some initial hypothesis tests:


# compute Moran's I for column "covercrop_perc_all_ac" to test for spatial autocorrelation:

# calculate weights matrix:
wm <-  adjacent(case_sv, "queen", pairs=FALSE)
# compute moran's I stat:
ac <- autocor(case_sv$covercrop_perc_all_ac, wm, "moran")
ac # very low Moran's I stat - 0.09 - test for significance...
# conduct monte carlo simulation for significance test:
m <- sapply(1:999, function(i) {
  autocor(sample(case_sv$covercrop_perc_all_ac), wm, "moran")
})
hist(m)
# and determine the p-value:
pval <- sum(m >= ac) / 1000
pval # p=0.09 - significant result at p<0.1 level (90% confidence)


# plotting a non-geospatial correlation matrix to examine original co-variates:
vars.cor <- cor(joined[,c(4:13,16)])
corrplot.mixed(vars.cor, 
               lower = "ellipse", 
               upper = "number",
               tl.pos = "lt",
               diag = "l",
               tl.col = "black")

# plot another correlation matrix, this time using enriched vars for 'share of cropland':
vars2.cor <- cor(enriched[,c(10:13,20,22:26,31)])
corrplot.mixed(vars2.cor, 
               lower = "ellipse", 
               upper = "number",
               tl.pos = "lt",
               diag = "l",
               tl.col = "black")

# A few initial observations from the 2nd corrplot:
  # - fertilizer N rate has a very weak positive correlation to the crop's actual N needs, but 
  #   a much stronger positive correlation to N surplus.
  # - the percentage of cropland in a given county that is irrigated has only very weak
  #   correlations to tillage and cover crop practices, as well as a weak correlation to N surplus.
  # - the share of all acres in a given county that is cover-cropped is the worst (lowest Rsq.)
  #   predictor of any of the rest of the variables, followed closely by N needs **

# ** this last observation is interesting given the lack of spatial autocorrelation as well in
# the use of cover crops. Let's take a look at the choropleth maps for these two variables:
p17 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = n_needs_lbs_acre)) +
  theme(legend.position = "bottom")
p18 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = ratio_conv_N_applied_relative_to_need)) +
  theme(legend.position = "bottom")
grid.arrange(p17,p18,p4,p16, ncol=2)

# there does appear to be spatial patterning in fertilizer use and fertilizer needs - but, 
# interestingly, it's also clear that the areas of highest corn production are also the areas
# of highest fertilizer over-application - the N requirement by county is much greater in the
# west of the state, where the least corn is grown.


# Calculate spatial autocorrelation among each variable in the enriched data set
# build function to output Moran's I stat and P-value for significance for each variable:

ac_table <- function(data_sv, data_sf, cols){
  wm <-  adjacent(data_sv, "queen", pairs=FALSE)
  Var_name <- names(data_sv[,cols])
  Morans_I <- numeric(length=length(Var_name))
  P_val <- numeric(length=length(Var_name))
  # var names might not be in the same order between data_sv and data_sf:
  cols_sf <- integer()
  for(i in 1:length(Var_name)){
    colnumsf <- which(names(data_sf) %in% Var_name[i])
    cols_sf = append(cols_sf,colnumsf,after=length(cols_sf))
  }
  # calculate ac vals and p vals
  for(n in 1:length(cols_sf)){
    colnum <- cols_sf[n]
    ac <- autocor(as.numeric(unlist(data_sf[,colnum]))[1:nrow(data_sf)], wm, "moran")
    Morans_I[n] <- ac
    m <- sapply(1:999, function(i) {
      autocor(sample(as.numeric(unlist(data_sf[,colnum]))[1:nrow(data_sf)]), wm, "moran") } )
    p <- sum(m >= ac) / 1000
    P_val[n] <- p
  }
  return(as.data.frame(cbind(Var_name, Morans_I, P_val)))
}

# try the function for each appropriate variable in the case_sv object:
# names(case_sv)
ac_results <- ac_table(case_sv, case_sf, c(4:13,16:31))
View(ac_results) # slow...but working!

# Interpretation: each of covercrop_perc_all_ac, irrigated_cropland_perc_all, and 
  # and n_needs_lbs_acre has a Moran's I value of less than 0.16 (weak positive correlation),
  # with a p-value of 0.1 or less (significant at 90% confidence) - implying that there is not 
  # a strong spatial autocorrelation in these values across counties in SD.


# -----------------------------------------------------------------------------

# Getting back to the COMET data set: sdcomet_fbn

View(sdcomet_fbn)
unique(sdcomet_fbn$county)
unique(case_sv$county) %in% unique(sdcomet_fbn$county) # all counties represented in both data sets


# To understand the potential relevance of the conservation ag practices in this case study 
# to a corn grain buyer looking to do business in SD, we need to understand these practices
# in the specific contexts of counties where corn is grown widely.

p19 <- ggplot(data = case_sf, aes(x=corn_perc_of_all_ac)) + 
  geom_histogram(bins=10, color="black", fill="lightgreen") +
  geom_vline(aes(xintercept=mean(corn_perc_of_all_ac)), color="blue",linetype="dashed",size=1) +
  geom_text(mapping = aes(x = mean(corn_perc_of_all_ac), y = 13, 
                          label = paste("mean = ", 
                                        100*round(mean(corn_perc_of_all_ac), digits=4), "%"), 
                          hjust = -0.2, vjust = -1)) +
  ggtitle("Corn's share of all cropland acres by county in\n South Dakota is bi-modally distributed") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  xlab("Bins: Corn as a % of a county's total cropland acreage") + ylab("Counties per bin")
p20 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = corn_acres)) +
  ggtitle("SD Counties with greatest corn acreage") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face="bold")); p20
p21 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = corn_perc_of_all_ac)) +
  ggtitle("Counties with larger shares cropland occupied by corn are \n concentrated in southeastern South Dakota") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face="bold")); p21


# To help with analysis, I'll sort the counties in both data sets into two groups:
# "High Corn Acreage" and "Low Corn Acreage", setting the threshold at corn_perc_of_all_ac > 0.20
case_sf <- mutate(case_sf, "greaterthan_20perc_corn" = corn_perc_of_all_ac > 0.20)
case_sv <- vect(case_sf)
p22 <- ggplot(data = case_sf) + geom_sf(data = case_sf, aes(fill = greaterthan_20perc_corn)) +
  theme(legend.position = "bottom"); p22

# group size:
table(case_sf$greaterthan_20perc_corn)
# based on observations from earlier plots, do these two groups show significantly different 
# levels of N surplus? (hypothesis: yes they should...)
# paired t-test to find out, with histogram to view result:
High_Corn_Counties <- case_sf$N_surplus_lbs_ac[which(case_sf$greaterthan_20perc_corn)]
Low_Corn_Counties <- case_sf$N_surplus_lbs_ac[which(!(case_sf$greaterthan_20perc_corn))]
t.test(x=High_Corn_Counties, y=Low_Corn_Counties, paired = FALSE)
# result - extremely significant (p-value = 1.887e-10)



# With this in mind, I'll take a look at the projected impact of practice changes from COMET
# on cropland within each of these groups of counties...

# First though, COMET data seems to have lots of co2_mean values that are within +/-
# standard errors. I'll identify and subset these out of the data set so that the analysis
# focuses only on practice changes with a higher confidence of impact.
within2sterr <- which(abs(sdcomet_fbn$co2_mean) < (2*sdcomet_fbn$co2_sterr))
sdcomet_fbn <- sdcomet_fbn[-within2sterr, ]; nrow(sdcomet_fbn)

# Next, this analysis will probably be simplest if I leave aside the rows indicating a change
# of multiple conservation practices at once - I'll subset those rows out for now as well.
multiplepractices <- which(sdcomet_fbn$cps_name=="Multiple Conservation Practices")
sdcomet_fbn_1 <- sdcomet_fbn[-multiplepractices, ]

# how many rows are there for each county x each cps_name (high-level practice change)?
table(sdcomet_fbn_1$cps_name, sdcomet_fbn_1$county) # between 1 and 4 options for everything
  # except nutrient management, which shows 23-24 practice options per county.

# what about for type of cropland (irrigated vs non-) x each cps_name?
sdcomet_fbn_1 <- mutate(sdcomet_fbn_1, 
                        "for_irrigated_land" = (!(grepl("Non-Irrigated", 
                                                        sdcomet_fbn_1$planner_implementation, 
                                                        ignore.case = TRUE))))
table(sdcomet_fbn_1$cps_name, sdcomet_fbn_1$for_irrigated_land)
# interestingly - there are no options for conservation crop rotations on non-irrigated cropland,
# according to COMET. Everything else has at least roughly equal number of options.
# This is probably OK, since I don't have any acreage data to indicate crop rotations in the 
# case study data set, so these won't be in focus. 

# In a final subsetting step, I'll remove the rows where:
# cps_name = "Conservation Crop Rotation (CPS 328)"
sdcomet_fbn_1 <- sdcomet_fbn_1[-(which(sdcomet_fbn_1$cps_name=="Conservation Crop Rotation (CPS 328)")), ]
unique(sdcomet_fbn_1$cps_name)


# Subset columns for analysis:
SDcomet_practices <- sdcomet_fbn_1[,c(2,8,9,12,13,37)]
# Compute summary statistics for each category (cps_name) within each county and type of cropland:
SDcomet_groups_all <- dplyr::group_by(SDcomet_practices, county, cps_name)
SDcomet_summary_all <- as.data.frame(dplyr::summarise(SDcomet_groups_all, mean(co2_mean), min(co2_mean), max(co2_mean)))
SDcomet_groups_irr <- dplyr::group_by(SDcomet_practices, county, cps_name, for_irrigated_land)
SDcomet_summary_irr <- as.data.frame(dplyr::summarise(SDcomet_groups_irr, mean(co2_mean), min(co2_mean), max(co2_mean)))
# manipulate these summaries into one data frame:
SDcomet_summary_all <- mutate(SDcomet_summary_all, "CroplandType" = "all")
SDcomet_summary_irr <- mutate(SDcomet_summary_irr, "CroplandType" = "")
for(i in 1:nrow(SDcomet_summary_irr)){
  if(SDcomet_summary_irr$for_irrigated_land[i]){
    SDcomet_summary_irr$CroplandType[i] = "irrigated"
  } else {SDcomet_summary_irr$CroplandType[i] = "non-irrigated"}
}
SDcomet_summary_irr <- SDcomet_summary_irr[,c(1,2,4:7)]
names(SDcomet_summary_all) <- names(SDcomet_summary_irr) <- 
  c("county", "cps_name", "mean_co2", "min_co2", "max_co2", "croplandtype")
SDcomet_summary <- as.data.frame(rbind(SDcomet_summary_all, SDcomet_summary_irr))
View(SDcomet_summary)

# Bring in the 'greaterthan_20perc_corn' group variable we created earlier in case_sf:
greaterthan_20perc_corn <- logical()
for(i in 1:nrow(SDcomet_summary)){
  val <- case_sf$greaterthan_20perc_corn[which(case_sf$county == SDcomet_summary[i,1])]
  greaterthan_20perc_corn <- append(greaterthan_20perc_corn, val, 
                                    after=length(greaterthan_20perc_corn))
}
SDcomet_summary <- as.data.frame(cbind(SDcomet_summary, greaterthan_20perc_corn))


# To get a feel for the variation in the data, conduct an ANOVA test among categories,
# using the mean_co2 as the dependent variable and each of the categorical vars as predictors:
mod_all <- lm(mean_co2~county+cps_name+greaterthan_20perc_corn, 
              data=SDcomet_summary[which(SDcomet_summary$croplandtype=="all"), ])
anova(mod_all)
mod_irr <- lm(mean_co2~county+cps_name+croplandtype+greaterthan_20perc_corn, 
              data=SDcomet_summary[-(which(SDcomet_summary$croplandtype=="all")), ])
anova(mod_irr)
# extremely significant test results on both runs....

# now see if a t-test results in a significant difference in mean_co2 for each croplandtype:
allland <- SDcomet_summary[which(SDcomet_summary$croplandtype=="all"), ]
co2_HighCorn_all <- allland[which(allland$greaterthan_20perc_corn),3]
co2_LowCorn_all <- allland[-(which(allland$greaterthan_20perc_corn)),3]
t.test(x=co2_HighCorn_all, y=co2_LowCorn_all, paired=FALSE)
# result - extremely significant (p-value = 7.481e-11)
# High-corn avg: 0.3171443; Low-corn avg: 0.2166377

# do the same for irrigated land only:
irrland <- SDcomet_summary[which(SDcomet_summary$croplandtype=="irrigated"), ]
co2_HighCorn_irr <- irrland[which(irrland$greaterthan_20perc_corn),3]
co2_LowCorn_irr <- irrland[-(which(irrland$greaterthan_20perc_corn)),3]
t.test(x=co2_HighCorn_irr, y=co2_LowCorn_irr, paired=FALSE)
# result - extremely significant (p-value = 2.838e-07)
# High-corn avg: 0.3634455; Low-corn avg: 0.2702864 

# do the same for non-irrigated land only:
nonland <- SDcomet_summary[which(SDcomet_summary$croplandtype=="non-irrigated"), ]
co2_HighCorn_non <- nonland[which(nonland$greaterthan_20perc_corn),3]
co2_LowCorn_non <- nonland[-(which(nonland$greaterthan_20perc_corn)),3]
t.test(x=co2_HighCorn_non, y=co2_LowCorn_non, paired=FALSE)
# result - extremely significant (p-value = 6.241e-16)
# High-corn avg: 0.2712688; Low-corn avg: 0.1626817 

# ALL THREE of these tests showed that simply which group (high- or low-corn) a county
# belongs to is a significant determinant of the average carbon sequestration potential across
# all conservation ag single-practice changes in that county.

# It also appears that irrigation is a substantial determinant of average co2 sequestration,
# at least within the comet data. I'll do another test below to see if that's true:
mod_irr_simp <- lm(mean_co2~croplandtype+greaterthan_20perc_corn, 
              data=SDcomet_summary[-(which(SDcomet_summary$croplandtype=="all")), ])
anova(mod_irr_simp) # highly significant on both croplandtype and irrigation...
# try a t-test:
irrvals <- SDcomet_summary[which(SDcomet_summary$croplandtype=="irrigated"), 3]
nonirrvals <- SDcomet_summary[which(SDcomet_summary$croplandtype=="non-irrigated"), 3]
t.test(x=irrvals, y=nonirrvals, paired=FALSE)
# result - extremely significant (p-value = 5.494e-16)
# High-corn avg: 0.3323925; Low-corn avg: 0.2350731 


# Based on the above, I can conclude both that counties in the "high-corn-production" group
# (greater than 20% total cropland acreage in corn) tend to have higher average mean_co2 
# sequestration values (tons/ac) than "low-corn" counties do, with statistical significance.

# I can also conclude that mean_co2 (tons/ac) sequestration potential tends to be higher on 
# irrigated vs non-irrigated cropland in South Dakota, also with statistical significance.

# BUT: Do these two groups substantially overlap?
# looking back at the case_sf data set - does whether a county is in the high-corn group
# significantly predict its share of irrigated cropland?
mod_irrgroup <- lm(irrigated_cropland_perc_all~greaterthan_20perc_corn, data = case_sf)
anova(mod_irrgroup) # It does NOT! P-value of result is 0.147 - not a significant result.
# In other words, the prevalence of irrigation in a given county doesn't seem to predict whether
# that county is a significant corn producer. These two variables can probably be used 
# together predictively, in that case.


# Try some exploratory maps that help capture spatial variation in mean_co2 values...
# clean up cps_name values such that they're usable as components of column names:
SDcomet_summary[which(SDcomet_summary$cps_name=="Cover Crop (CPS 340)"), 2] = "cover_crop_co2"
SDcomet_summary[which(SDcomet_summary$cps_name=="Nutrient Management (CPS 590)"), 2] = "nutrients_co2"
SDcomet_summary[which(SDcomet_summary$cps_name=="Residue and Tillage Management - No-Till (CPS 329)"), 2] = "no_till_co2"
SDcomet_summary[which(SDcomet_summary$cps_name=="Residue and Tillage Management - Reduced Till (CPS 345)"), 2] = "reduced_till_co2"
# cast variables into columns in comet data set:
joinmeans <- dcast(SDcomet_summary, formula = county~cps_name+croplandtype, value.var = "mean_co2")
names(joinmeans) <- c("county", paste0(names(joinmeans)[2:13], "_mean"))
joinmins <- dcast(SDcomet_summary, formula = county~cps_name+croplandtype, value.var = "min_co2")
names(joinmins) <- c("county", paste0(names(joinmins)[2:13], "_min"))
joinmaxes <- dcast(SDcomet_summary, formula = county~cps_name+croplandtype, value.var = "max_co2")
names(joinmaxes) <- c("county", paste0(names(joinmaxes)[2:13], "_max"))
# merge the results back into 1 data frame
mer1 <- base::merge(case_sf, joinmeans, by="county", all = TRUE)
mer2 <- base::merge(mer1, joinmins, by="county", all=TRUE)
mer3 <- base::merge(mer2, joinmaxes, by="county", all=TRUE)
names(mer3) <- gsub("-", "_", names(mer3))
# rename & convert to spatial objects
case_sf_comet <- mer3
case_sv_comet <- vect(case_sf_comet)



# some exploratory plots...

# N surplus and nutrient co2 capture potential:
p23 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = n_needs_lbs_acre)) +
  theme(legend.position = "bottom")
p24 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = N_surplus_lbs_ac)) +
  theme(legend.position = "bottom")
p25 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = nutrients_co2_irrigated_mean)) +
  theme(legend.position = "bottom")
p26 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = nutrients_co2_non_irrigated_mean)) +
  theme(legend.position = "bottom")
grid.arrange(p23,p24,p25,p26, ncol=2)
# highest capture potential in lbs/ac is in irrigated cropland in central SD, not necessarily
# in the counties with highest N surplus or highest corn acreage.

# double-click on nutrient management & corn acreage:
p27 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = fert_n_rate_lbs_acre)) +
  theme(legend.position = "bottom")
p28 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = manure_n_rate_lbs_acre)) +
  theme(legend.position = "bottom")
p29 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = ratio_conv_N_applied_relative_to_need)) +
  theme(legend.position = "bottom")
p30 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = corn_perc_of_all_ac)) +
  theme(legend.position = "bottom")
grid.arrange(p27,p28,p29,p30, ncol=2)
# rates of N application through fertilizers are highly coincident with areas of significant
  # corn acreage, as well as with a high rate of over-application of N from fertilizers. 
# In other words, all the corn growers are using too much conventional nitrogen!
# Meanwhile, manure N application rate appears largely unrelated to the other three variables
  # shown here. Recall that the manure N rate also has a significant result on a very weak
  # moran's I statistic from our earlier test:
ac_results[ac_results$Var_name=="manure_n_rate_lbs_acre", ]


# Another pattern - coincidence between areas of greatest mean_co2 from N reduction
# and tillage patterns among counties:
p31 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = nutrients_co2_all_mean)) +
  theme(legend.position = "bottom") + labs(fill="N Mgnt - mean soil CO2\nCapture Rate (tons/ac/yr)")
p53 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = no_till_co2_all_mean)) +
  theme(legend.position = "bottom") + labs(fill="No-Till - mean soil CO2\nCapture Rate (tons/ac/yr)")
grid.arrange(p13,p14,p31,p53, ncol=2)

# extra test - going back to using the SDcomet_practices table with each "planner implementation"
# still recorded - do nutrient management single-practice changes seem to make a bigger impact on
# mean_co2 than other cps_names? Does irrigation matter?
nutri_mod_data <- mutate(SDcomet_practices, 
                         "is_nutr" = grepl("Nutrient Management", cps_name, ignore.case = TRUE))
t.test(nutri_mod_data[which(nutri_mod_data$is_nutr), 4], 
       nutri_mod_data[which(!(nutri_mod_data$is_nutr)), 4], paired = FALSE)
## ^^ shows a very significant inverse relationship here - non-nutrient management practices
# tend to make a bigger difference to co2 sequestration than nutrient management practices do...
  # nutrition mgnt practices average 0.2443351 co2 tons/ac; 
  # non-nutrition practices average 0.3215923 co2 tons/ac
  # -->> what about for other ghgs in the comet data set?
t.test(SDcomet_practices[which(grepl("No-Till", SDcomet_practices$cps_name)), 4], 
       +        SDcomet_practices[which(!(grepl("No-Till", SDcomet_practices$cps_name))), 4], paired = FALSE)
# T-test for no-till vs non-no-till shows that no-till produces the most significant difference
# to soil CO2 sequestration.

mean(SDcomet_practices$co2_mean[which(grepl("Nutrient Management", SDcomet_practices$cps_name))])
mean(SDcomet_practices$co2_mean[which(grepl("Cover Crop", SDcomet_practices$cps_name))])
mean(SDcomet_practices$co2_mean[which(grepl("No-Till", SDcomet_practices$cps_name))])
mean(SDcomet_practices$co2_mean[which(grepl("Reduced Till", SDcomet_practices$cps_name))])
# nutrient management has the second-lowest mean co2 value of each of these groups.
# explore these distributions in a boxplot:
p32 <- ggplot(SDcomet_practices, aes(x=cps_name, y=co2_mean, fill=cps_name)) + 
  geom_boxplot(outlier.colour="blue", outlier.size=2) +
  ggtitle("Of all practices, switching from conventional tillage to no-till tends to \n have the greatest impact on co2 capture in soils.") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), legend.position = "none") +
  xlab("") + ylab("Mean soil CO2 sequestration (tons/ac)")
p32 + scale_x_discrete(labels=c("Cover Crop (CPS 340)" = "Cover Crop",
                                "Nutrient Management (CPS 590)" = "Nutrient \nManagement",
                                "Residue and Tillage Management - No-Till (CPS 329)" = "No-Till",
                                "Residue and Tillage Management - Reduced Till (CPS 345)" = "Reduced Till"))

p33 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = yield)) +
  theme(legend.position = "bottom")
p34 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = n_needs_lbs_acre)) +
  theme(legend.position = "bottom")

# explore relationship between corn acres and fertilizer N surplus:
case_sf_comet <- mutate(case_sf_comet, 
                        "Fert_N_Surplus_Ratio" = (fert_n_rate_lbs_acre-n_needs_lbs_acre)/n_needs_lbs_acre,
                        "Total_N_Surplus_Ratio" = N_surplus_lbs_ac/n_needs_lbs_acre)
p35 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = Fert_N_Surplus_Ratio)) +
  theme(legend.position = "bottom") + labs(fill="Fertilizer N\nSurplus Ratio")
p36 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = corn_perc_of_all_ac)) +
  theme(legend.position = "bottom") + labs(fill="Corn's Share of\nAll Cropland")
grid.arrange(p35,p36,ncol=2)
# try out a model:
fertmod <- lm(Fert_N_Surplus_Ratio~corn_perc_of_all_ac, data=case_sf_comet)
summary(fertmod) # very significant! 
# what if we include tillage as well...

# determine the most common type of tillage by county in a categorical variable:
most_common_tillage_type <- character()
for(i in 1:nrow(case_sf_comet)){
  answer <- character()
  sub <- as.numeric(case_sf_comet[i, c(22,23,24)])[1:3]
  maxind <- which(sub==max(sub))
  if(maxind==1){
    answer <- "No-Till"
  } else if (maxind==2){
    answer <- "Reduced Till"
  } else { answer <- "Conventional Till" }
  most_common_tillage_type <- append(most_common_tillage_type, answer, 
                                     after=length(most_common_tillage_type))
}
head(most_common_tillage_type); length(most_common_tillage_type)
case_sf_comet <- cbind(case_sf_comet, most_common_tillage_type)
# another try at a model:
fertmod2 <- lm(Fert_N_Surplus_Ratio~corn_perc_of_all_ac+most_common_tillage_type,
              data=case_sf_comet)
summary(fertmod2) # not additionally significant

# try calculating the total 'conservation tillage' share of acreage (no-till & reduced till)
case_sf_comet <- mutate(case_sf_comet, 
                        "conservation_tillage_perc" = ((conservation_tillage_notill_acres + 
                                                         conservation_tillage_excl_notill_acres) /
                                                        cropland_acres))
# one more try at a model:
fertmod3 <- lm(Fert_N_Surplus_Ratio~corn_perc_of_all_ac+conservation_tillage_perc,
               data=case_sf_comet)
summary(fertmod3) # both very significant!!
# are residuals spatially autocorrelated?
fertmod3_residuals <- fertmod3$residuals
case_sf_fertmod3 <- cbind(case_sf_comet, fertmod3_residuals)
case_sv_fertmod3 <- vect(case_sf_fertmod3)
wm <-  adjacent(case_sv_fertmod3, "queen", pairs=FALSE)
# compute moran's I stat:
ac <- autocor(case_sv_fertmod3$fertmod3_residuals, wm, "moran")
ac # very low Moran's I stat - 0.09 - test for significance...
# conduct monte carlo simulation for significance test:
m <- sapply(1:999, function(i) {
  autocor(sample(case_sv_fertmod3$fertmod3_residuals), wm, "moran")
})
hist(m)
# and determine the p-value:
pval <- sum(m >= ac) / 1000
pval # Moran's I of -0.02843399, with a p-value of 0.561 - not autocorrelated!
# This model can be trusted after all.
p37 <- ggplot(data = case_sf_fertmod3) + geom_sf(data = case_sf_fertmod3, 
                                                 aes(fill = fertmod3_residuals)) +
  theme(legend.position = "bottom") + labs(fill="Residuals"); p37

# scatterplot of the predictor variables in this model:
p38 <- ggplot(case_sf_fertmod3, aes(x = corn_perc_of_all_ac, 
                                    y = Fert_N_Surplus_Ratio)) + 
  geom_point() + geom_smooth(method='lm') + theme(legend.position = "none") + 
  xlab("Corn % Share\nof All Cropland") + ylab("Fertilizer N Surplus Ratio")
p39 <- ggplot(case_sf_fertmod3, aes(x = conservation_tillage_perc, 
                                    y = Fert_N_Surplus_Ratio)) + 
  geom_point() + geom_smooth(method='lm') + theme(legend.position = "none") + 
  xlab("Cropland % Under\nConservation Tillage") + ylab("Fertilizer N Surplus Ratio")
p40 <- ggplot(case_sf_fertmod3, aes(x = corn_perc_of_all_ac, 
                                    y = conservation_tillage_perc)) + 
  geom_point() + geom_smooth(method='lm') + theme(legend.position = "none") + 
  xlab("Corn % Share\nof All Cropland") + ylab("Cropland % Under Conservation Tillage")
grid.arrange(p38,p39,p40, ncol=3)

# model output plots:
plot(fertmod3)

# Surrounding observations for this model: 
# Most "high-corn" counties are already widely using conservation tillage:
table(case_sf_comet$greaterthan_20perc_corn, case_sf_comet$most_common_tillage_type)
# indicative exploratory plots:
p41 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = corn_perc_of_all_ac)) +
  theme(legend.position = "bottom") + labs(fill="Corn % Share\nof All Cropland")
p42 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = conservation_tillage_perc)) +
  theme(legend.position = "bottom") + labs(fill="Cropland % Under\nConservation Tillage")
p43 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = fert_n_rate_lbs_acre)) +
  theme(legend.position = "bottom") + labs(fill="Fertilizer N Application\nRate (lbs per acre)")
p44 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = Fert_N_Surplus_Ratio)) +
  theme(legend.position = "bottom") + labs(fill="Fertilizer N Surplus\nRatio")
p49 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = notill_perc_all_ac)) +
  theme(legend.position = "bottom") + labs(fill="Cropland % Under\nNo-Till Mgnt")
p50 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = covercrop_perc_all_ac)) +
  theme(legend.position = "bottom") + labs(fill="Cropland % Under\nCover Cropping")
p51 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = reducedtill_perc_all_ac)) +
  theme(legend.position = "bottom") + labs(fill="Cropland % Under\nReduced-Till Mgnt")
p52 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = manure_n_rate_lbs_acre)) +
  theme(legend.position = "bottom") + labs(fill="Manure N Application\nRate (lbs per acre)")
p53 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = yield)) +
  theme(legend.position = "bottom") + labs(fill="5-yr average Corn Yield\n (bu per acre)")
p54 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = n_needs_lbs_acre)) +
  theme(legend.position = "bottom") + labs(fill="Average N Requirement\nfor Corn (lbs per acre)")
grid.arrange(p41,p42,p43,p44, ncol=2)
grid.arrange(p50,p54,p52,p53, ncol=2)

# descriptive categorical plots showing H/L Corn Acreage and Most Common Tillage Types
p45 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = greaterthan_20perc_corn)) +
  theme(legend.position = "bottom") + labs(fill="") +
  ggtitle("Counties where corn represents more than 20% of Cropland")
p46 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = most_common_tillage_type)) +
  theme(legend.position = "bottom") + labs(fill="") + 
  ggtitle("Most Common Type of Tillage Used")
grid.arrange(p45,p46,ncol=2)
# which counties have greater than 20% corn by cropland and have no-till listed as their most
# common tillage practice?
hcorn_ntill <- case_sf_comet[which(case_sf_comet$greaterthan_20perc_corn), ]
hcorn_ntill <- hcorn_ntill[which(hcorn_ntill$most_common_tillage_type=="No-Till"), ]
other <- case_sf_comet[which(!(case_sf_comet$county %in% unique(hcorn_ntill$county))), ]
mean(hcorn_ntill$notill_perc_all_ac); mean(other$notill_perc_all_ac)
sum(hcorn_ntill$corn_acres); sum(hcorn_ntill$corn_forgrain_production_bu_NASS_2017)
sum(hcorn_ntill$corn_acres)/sum(case_sf_comet$corn_acres)
sum(hcorn_ntill$corn_forgrain_production_bu_NASS_2017)/sum(case_sf_comet$corn_forgrain_production_bu_NASS_2017)

# ranking values in each column of interest:
df <- case_sf_comet[,c(1,70,31,5,10,18,19,21:25,72,68)]
df2 <- df %>% st_drop_geometry()
df2[paste0("v", 1:11, "rank")] <- apply(-df2[4:14], 2, rank)
names(df2) <- c(names(df2)[1:14], "rank_corn_acres", "rank_yield", "rank_production",
                "rank_corn_perc_of_all", "rank_irrigated_cropland_perc",
                "rank_notill_perc_of_all", "rank_reducedtill_perc_of_all", 
                "rank_convtill_perc_of_all", "rank_covercrop_perc_of_all",
                "rank_conservation_tillage_perc", "rank_Fert_N_Surplus_Ratio")
write.csv(df2, "./RankOutputData.csv")
View(df2)
# Of the top 10 counties in terms of highest Fert_N_Surplus_Ratio, only one of them 
# is among the top 10 counties in South Dakota in terms of average corn yields.
# 3 of them, meanwhile, are in the bottom half by yield rank.
# Additionally, of the top 10 counties in terms of highest Fert_N_Surplus_Ratio, 5 of
# these counties rank among the bottom half across SD in terms of cover crop acreage.
# COMETfarm's cover crop practices, as prescribed, often include a fertilizer reduction 
# aspect to them - this might be interesting to investigate further. 

# one last exploratory plot on this issue: incorporate corn harvested for grain, and corn yields:
p47 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = corn_forgrain_production_bu_NASS_2017)) +
  theme(legend.position = "bottom") + labs(fill="Total Corn Harvested\nfor Grain (2017, bu)")
p48 <- ggplot(data = case_sf_comet) + geom_sf(data = case_sf_comet, aes(fill = yield)) +
  theme(legend.position = "bottom") + labs(fill="Average Corn Yields\n(bu/ac, 2017-2021)")
grid.arrange(p47,p48,p43,p44, ncol=2)

# get list of top 10 counties by Fert_N_Surplus_Ratio, and see which practices COMETfarm 
# suggests as the biggest difference-makers. How many of those are related to nutrient management?
top10 <- df2$county[which(df2$rank_Fert_N_Surplus_Ratio <= 10)]
top10_cometpractices <- SDcomet_practices[which(SDcomet_practices$county %in% top10), ]
top10_cometpractices[which(top10_cometpractices$co2_mean==max(top10_cometpractices$co2_mean)), ]
# take a look at the list of possible cover cropping practices in the COMET data set - 
# many of them include a fertility management component (N reduction amount):
cover_practices <- top10_cometpractices[which(grepl("Cover",top10_cometpractices$cps_name)), ]
unique(cover_practices$planner_implementation) 
# requires fertilizer N reduction of 25% and 50% respectively to implement these - 
# what's the average fertilizer N surplus ratio on these 10 counties? (if greater than 0.5, golden!)
mean(unique(df2[which(df2$rank_Fert_N_Surplus_Ratio <= 10), 14])) # 1.276247! Wow.
sd(unique(df2[which(df2$rank_Fert_N_Surplus_Ratio <= 10), 14])) # std dev of slightly more than 0.1
# According to this data set, the average Fertilizer N surplus is 127% of the N requirement per acre
# among these top 10 counties - more than enough to accommodate that type of reduction. 

# But what about cover cropping? 
# Why doesn't it seem correlated to corn acreage, fertilizer n rates, or tillage?

# try lm just to make sure...
covermod <- lm(covercrop_perc_all_ac~Fert_N_Surplus_Ratio + 
                 corn_perc_of_all_ac + conservation_tillage_perc, data=case_sf_comet)
summary(covermod) # none of these three are significant predictors. check.
# try some other variables:
covermod2 <- lm(covercrop_perc_all_ac~irrigated_cropland_perc_all + 
                  ratio_manure_N_applied_relative_to_need, data=case_sf_comet)
summary(covermod2) # no significance again.

# questions left over on why cover cropping isn't more prevalent and what might determine
# the spatial distribution of its popularity - this could be an area for grain buyers to
# incentivize in the future.

p55 <- ggplot(data = case_sf_comet, aes(x=covercrop_perc_all_ac)) + 
  geom_histogram(bins=11, color="black", fill="lightgreen") +
  geom_vline(aes(xintercept=mean(covercrop_perc_all_ac)), color="blue",linetype="dashed",size=1) +
  geom_text(mapping = aes(x = mean(covercrop_perc_all_ac), y = 14, 
                          label = paste("mean = ", 
                                        100*round(mean(covercrop_perc_all_ac), digits=4), "%"), 
                          hjust = -0.2, vjust = -1)) +
  ggtitle("Histogram: Cover-cropped acres as a % share\nof a county's total cropland acreage") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  xlab("Bins: Cover-cropped acres as a % of all cropland") + ylab("Counties per bin"); p55

p56 <- ggplot(data = case_sf_comet, aes(x=manure_n_rate_lbs_acre)) + 
  geom_histogram(bins=11, color="black", fill="lightblue") +
  geom_vline(aes(xintercept=mean(manure_n_rate_lbs_acre)), color="purple",linetype="dashed",size=1) +
  geom_text(mapping = aes(x = mean(manure_n_rate_lbs_acre), y = 14, 
                          label = paste("mean = ", round(mean(manure_n_rate_lbs_acre), digits=4)), 
                          hjust = -0.2, vjust = -1)) +
  ggtitle("Histogram: County-level average manure N application rates") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  xlab("Bins: Manure N rate (lbs/acre)") + ylab("Counties per bin"); p56


# -----------------------------------------------------------------------------

## SCRATCH / GENERAL NOTES:

# plotting options:

# BASIC: 
# plot(st_geometry(case_sf))

# using ggplot2:
# ggplot(data = case_sf) + geom_sf()

# using leaflet:
# leaflet(case_sf) %>%
#   addTiles() %>%
#   addPolygons(fillOpacity=0.1)

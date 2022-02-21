##################################################################
##       AIM: Transform raw-data for RShiny visualisation       ##
##################################################################
# CHARTS

##################################################################
##                        Concentrations                        ## done
##################################################################
# Read exposures data
load("raw-data/Concentrations/UnweightedCountryExposures.RData")
load("raw-data/Concentrations/UnweightedGBDRegionExposures.RData")
load("raw-data/Concentrations/UnweightedGlobalExposures.RData")
load("raw-data/Concentrations/UnweightedSDGRegionExposures.RData")
load("raw-data/Concentrations/UnweightedWHOIncomeRegionExposures.RData")
load("raw-data/Concentrations/UnweightedWHORegionExposures.RData")
# Bind exposures data
concentrations_unweighted_exposures = rbind(
  UnweightedCountry %>% mutate (name = "Unweighted Country"),
  UnweightedGBDRegion %>% mutate (name = "Unweighted GBD Region"),
  UnweightedGlobal %>% mutate (name = "Unweighted Global"),
  UnweightedSDGRegion %>% mutate (name = "Unweighted SDG Region"),
  UnweightedWHOIncomeRegion %>% mutate (name = "Unweighted WHO Income Region"),
  UnweightedWHORegion %>% mutate (name = "Unweighted WHO Region")
) %>% select(-c(Type))
# Clear environment
rm(
  UnweightedCountry,
  UnweightedGBDRegion,
  UnweightedGlobal,
  UnweightedSDGRegion,
  UnweightedWHOIncomeRegion,
  UnweightedWHORegion
)

# Read exposure change data
load("raw-data/Concentrations/UnweightedCountryExposures_Changes.RData")
load("raw-data/Concentrations/UnweightedGBDRegionExposures_Changes.RData")
load("raw-data/Concentrations/UnweightedGlobalExposures_Changes.RData")
load("raw-data/Concentrations/UnweightedSDGRegionExposures_Changes.RData")
load("raw-data/Concentrations/UnweightedWHOIncomeRegionExposures_Changes.RData")
load("raw-data/Concentrations/UnweightedWHORegionExposures_Changes.RData")

concentrations_unweighted_exposures_diff = rbind(
  UnweightedCountry_diff       %>% mutate (name = "Unweighted Country"),
  UnweightedGBDRegion_diff      %>% mutate (name = "Unweighted GBD Region"),
  UnweightedGlobal_diff  %>% mutate (name = "Unweighted Global"),
  UnweightedSDGRegion_diff       %>% mutate (name = "Unweighted SDG Region"),
  UnweightedWHOIncomeRegion_diff    %>% mutate (name = "Unweighted WHO Income Region"),
  UnweightedWHORegion_diff           %>% mutate (name = "Unweighted WHO Region")
)

rm(
  UnweightedGBDRegion_diff,
  UnweightedCountry_diff,
  UnweightedGlobal_diff,
  UnweightedWHOIncomeRegion_diff,
  UnweightedWHORegion_diff,
  UnweightedSDGRegion_diff
)

concentrations_unweighted_exposures = rbind(concentrations_unweighted_exposures,concentrations_unweighted_exposures_diff)
saveRDS(concentrations_unweighted_exposures, "data/concentrations_exposure.RDS")

#################################################################
##                         Exceedances                         ## done
#################################################################
#10
load("raw-data/Exceedances/10/Country.RData")
load("raw-data/Exceedances/10/GBDRegion.RData")
load("raw-data/Exceedances/10/GBDSuperRegion.RData")
load("raw-data/Exceedances/10/Global.RData")
load("raw-data/Exceedances/10/SDGRegion.RData")
load("raw-data/Exceedances/10/WHOIncomeRegion.RData")
load("raw-data/Exceedances/10/WHORegion.RData")

Exceed_10 = rbind(
  Exceed_Country %>% mutate (Category = "Country") %>% rename_at(1,  ~ "ID"),
  Exceed_GBDRegion %>% mutate (Category = "GBD Region") %>% rename_at(1,  ~
                                                                        "ID"),
  Exceed_Global %>% mutate (Category = "Global") %>% rename_at(1,  ~ "ID"),
  Exceed_SDGRegion %>% mutate (Category = "SDG Region") %>% rename_at(1,  ~
                                                                        "ID"),
  Exceed_GBDSuperRegion %>% mutate (Category = "GBD Super Region") %>% rename_at(1,  ~
                                                                                   "ID"),
  Exceed_WHOIncomeRegion %>% mutate (Category = "WHO Income Region") %>% rename_at(1,  ~
                                                                                     "ID"),
  Exceed_WHORegion %>% mutate (Category = "WHO Region") %>% rename_at(1,  ~
                                                                        "ID")
) %>% gather("Year", "Value", Perc2016:Perc2010) %>%
  mutate(Year = as.numeric(sub("....", "", Year))) %>%
  mutate(Scale = 10)

rm(
  Exceed_GBDRegion,
  Exceed_Country,
  Exceed_Global,
  Exceed_GBDSuperRegion,
  Exceed_WHOIncomeRegion,
  Exceed_WHORegion,
  Exceed_SDGRegion
)

# 15
load("raw-data/Exceedances/15/Country.RData")
load("raw-data/Exceedances/15/GBDRegion.RData")
load("raw-data/Exceedances/15/GBDSuperRegion.RData")
load("raw-data/Exceedances/15/Global.RData")
load("raw-data/Exceedances/15/SDGRegion.RData")
load("raw-data/Exceedances/15/WHOIncomeRegion.RData")
load("raw-data/Exceedances/15/WHORegion.RData")

Exceed_15 = rbind(
  Exceed_Country %>% mutate (Category = "Country") %>% rename_at(1,  ~ "ID"),
  Exceed_GBDRegion %>% mutate (Category = "GBD Region") %>% rename_at(1,  ~
                                                                        "ID"),
  Exceed_Global %>% mutate (Category = "Global") %>% rename_at(1,  ~ "ID"),
  Exceed_SDGRegion %>% mutate (Category = "SDG Region") %>% rename_at(1,  ~
                                                                        "ID"),
  Exceed_GBDSuperRegion %>% mutate (Category = "GBD Super Region") %>% rename_at(1,  ~
                                                                                   "ID"),
  Exceed_WHOIncomeRegion %>% mutate (Category = "WHO Income Region") %>% rename_at(1,  ~
                                                                                     "ID"),
  Exceed_WHORegion %>% mutate (Category = "WHO Region") %>% rename_at(1,  ~
                                                                        "ID")
) %>% gather("Year", "Value", Perc2016:Perc2010) %>%
  mutate(Year = as.numeric(sub("....", "", Year))) %>%
  mutate(Scale = 15)

rm(
  Exceed_GBDRegion,
  Exceed_Country,
  Exceed_Global,
  Exceed_WHOIncomeRegion,
  Exceed_GBDSuperRegion,
  Exceed_WHORegion,
  Exceed_SDGRegion
)


# 25
load("raw-data/Exceedances/25/Country.RData")
load("raw-data/Exceedances/25/GBDRegion.RData")
load("raw-data/Exceedances/25/GBDSuperRegion.RData")
load("raw-data/Exceedances/25/Global.RData")
load("raw-data/Exceedances/25/SDGRegion.RData")
load("raw-data/Exceedances/25/WHOIncomeRegion.RData")
load("raw-data/Exceedances/25/WHORegion.RData")

Exceed_25 = rbind(
  Exceed_Country %>% mutate (Category = "Country") %>% rename_at(1,  ~ "ID"),
  Exceed_GBDRegion %>% mutate (Category = "GBD Region") %>% rename_at(1,  ~
                                                                        "ID"),
  Exceed_Global %>% mutate (Category = "Global") %>% rename_at(1,  ~ "ID"),
  Exceed_GBDSuperRegion %>% mutate (Category = "GBD Super Region") %>% rename_at(1,  ~
                                                                                   "ID"),
  Exceed_SDGRegion %>% mutate (Category = "SDG Region") %>% rename_at(1,  ~
                                                                        "ID"),
  Exceed_WHOIncomeRegion %>% mutate (Category = "WHO Income Region") %>% rename_at(1,  ~
                                                                                     "ID"),
  Exceed_WHORegion %>% mutate (Category = "WHO Region") %>% rename_at(1,  ~
                                                                        "ID")
) %>% gather("Year", "Value", Perc2016:Perc2010) %>%
  mutate(Year = as.numeric(sub("....", "", Year))) %>%
  mutate(Scale = 25)

rm(
  Exceed_GBDRegion,
  Exceed_Country,
  Exceed_Global,
  Exceed_WHOIncomeRegion,
  Exceed_GBDSuperRegion,
  Exceed_WHORegion,
  Exceed_SDGRegion
)


# 35
load("raw-data/Exceedances/35/Country.RData")
load("raw-data/Exceedances/35/GBDRegion.RData")
load("raw-data/Exceedances/35/GBDSuperRegion.RData")
load("raw-data/Exceedances/35/Global.RData")
load("raw-data/Exceedances/35/SDGRegion.RData")
load("raw-data/Exceedances/35/WHOIncomeRegion.RData")
load("raw-data/Exceedances/35/WHORegion.RData")

Exceed_35 = rbind(
  Exceed_Country %>% mutate (Category = "Country") %>% rename_at(1,  ~ "ID"),
  Exceed_GBDRegion %>% mutate (Category = "GBD Region") %>% rename_at(1,  ~
                                                                        "ID"),
  Exceed_Global %>% mutate (Category = "Global") %>% rename_at(1,  ~ "ID"),
  Exceed_GBDSuperRegion %>% mutate (Category = "GBD Super Region") %>% rename_at(1,  ~
                                                                                   "ID"),
  Exceed_SDGRegion %>% mutate (Category = "SDG Region") %>% rename_at(1,  ~
                                                                        "ID"),
  Exceed_WHOIncomeRegion %>% mutate (Category = "WHO Income Region") %>% rename_at(1,  ~
                                                                                     "ID"),
  Exceed_WHORegion %>% mutate (Category = "WHO Region") %>% rename_at(1,  ~
                                                                        "ID")
) %>% gather("Year", "Value", Perc2016:Perc2010) %>%
  mutate(Year = as.numeric(sub("....", "", Year))) %>%
  mutate(Scale = 35)

rm(
  Exceed_GBDRegion,
  Exceed_Country,
  Exceed_Global,
  Exceed_WHOIncomeRegion,
  Exceed_GBDSuperRegion,
  Exceed_WHORegion,
  Exceed_SDGRegion
)

Exceed = rbind(Exceed_10, Exceed_15, Exceed_25, Exceed_35)
rm(Exceed_10, Exceed_15, Exceed_25, Exceed_35)

write_rds(Exceed, "data/exceed_10_15_25_35.RDS")
#################################################################
##                          Exposures                          ## todo
#################################################################
load("raw-data/Exposures/WeightedCountryExposures.RData")
load("raw-data/Exposures/WeightedCountryExposures_Changes.RData")
load("raw-data/Exposures/WeightedGBDRegionExposures.RData")
load("raw-data/Exposures/WeightedGBDRegionExposures_Changes.RData")
load("raw-data/Exposures/WeightedGBDSuperRegionExposures.RData")
load("raw-data/Exposures/WeightedGBDSuperRegionExposures_Changes.RData")
load("raw-data/Exposures/WeightedGlobalExposures.RData")
load("raw-data/Exposures/WeightedGlobalExposures_Changes.RData")
load("raw-data/Exposures/WeightedSDGRegionExposures.RData")
load("raw-data/Exposures/WeightedSDGRegionExposures_Changes.RData")
load("raw-data/Exposures/WeightedWHOIncomeRegionExposures.RData")
load("raw-data/Exposures/WeightedWHOIncomeRegionExposures_Changes.RData")
load("raw-data/Exposures/WeightedWHORegionExposures.RData")
load("raw-data/Exposures/WeightedWHORegionExposures_Changes.RData")




#################################################################
##                       Ground monitors                       ## done
#################################################################

#load("raw-data/Ground monitors/GM_dat.RData")

# MAPS

#################################################################
##                   Read Gridded prediction                   ## done
#################################################################

# Read data
load("raw-data/Gridded predictions/pred_2016.RData")
load("raw-data/Gridded predictions/pred_2015.RData")
load("raw-data/Gridded predictions/pred_2014.RData")
load("raw-data/Gridded predictions/pred_2013.RData")
load("raw-data/Gridded predictions/pred_2012.RData")
load("raw-data/Gridded predictions/pred_2011.RData")
# Bind data
grid_prediction = rbind(pred_2011,
                        pred_2012,
                        pred_2013,
                        pred_2014,
                        pred_2015,
                        pred_2016)
# Clear environment
rm(pred_2011,
   pred_2012,
   pred_2013,
   pred_2014,
   pred_2015,
   pred_2016)

saveRDS(grid_prediction, "data/grid_prediction.RDS")

##################################################################
##                          Shapefiles                          ## done
##################################################################

#load("raw-data/Shapefiles/shapefiles.RData")

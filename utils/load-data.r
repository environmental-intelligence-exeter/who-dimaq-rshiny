##################################################################
##                        Concentrations                        ##
##################################################################
# Read exposures data
load("raw-data/Concentrations/UnweightedCountryExposures.RData")
load("raw-data/Concentrations/UnweightedGBDRegionExposures.RData")
load("raw-data/Concentrations/UnweightedGlobalExposures.RData")
load("raw-data/Concentrations/UnweightedSDGRegionExposures.RData")
load("raw-data/Concentrations/UnweightedWHOIncomeRegionExposures.RData")
load("raw-data/Concentrations/UnweightedWHORegionExposures.RData")
# Bind exposures data
exposures = rbind(
  UnweightedCountry,
  UnweightedGBDRegion,
  UnweightedGlobal,
  UnweightedSDGRegion,
  UnweightedWHOIncomeRegion,
  UnweightedWHORegion
)
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

exposures_diff = rbind(
  UnweightedGBDRegion_diff,
  UnweightedCountry_diff,
  UnweightedGlobal_diff,
  UnweightedWHOIncomeRegion_diff,
  UnweightedWHORegion_diff,
  UnweightedSDGRegion_diff
)

rm(
  UnweightedGBDRegion_diff,
  UnweightedCountry_diff,
  UnweightedGlobal_diff,
  UnweightedWHOIncomeRegion_diff,
  UnweightedWHORegion_diff,
  UnweightedSDGRegion_diff
)

#################################################################
##                         Exceedances                         ##
#################################################################
load("raw-data/Exceedances/10/Country.RData")
load("raw-data/Exceedances/10/GBDRegion.RData")
load("raw-data/Exceedances/10/GBDSuperRegion.RData")
load("raw-data/Exceedances/10/Global.RData")
load("raw-data/Exceedances/10/SDGRegion.RData")
load("raw-data/Exceedances/10/WHOIncomeRegion.RData")
load("raw-data/Exceedances/10/WHORegion.RData")

load("raw-data/Exceedances/15/Country.RData")
load("raw-data/Exceedances/15/GBDRegion.RData")
load("raw-data/Exceedances/15/GBDSuperRegion.RData")
load("raw-data/Exceedances/15/Global.RData")
load("raw-data/Exceedances/15/SDGRegion.RData")
load("raw-data/Exceedances/15/WHOIncomeRegion.RData")
load("raw-data/Exceedances/15/WHORegion.RData")

load("raw-data/Exceedances/25/Country.RData")
load("raw-data/Exceedances/25/GBDRegion.RData")
load("raw-data/Exceedances/25/GBDSuperRegion.RData")
load("raw-data/Exceedances/25/Global.RData")
load("raw-data/Exceedances/25/SDGRegion.RData")
load("raw-data/Exceedances/25/WHOIncomeRegion.RData")
load("raw-data/Exceedances/25/WHORegion.RData")

load("raw-data/Exceedances/35/Country.RData")
load("raw-data/Exceedances/35/GBDRegion.RData")
load("raw-data/Exceedances/35/GBDSuperRegion.RData")
load("raw-data/Exceedances/35/Global.RData")
load("raw-data/Exceedances/35/SDGRegion.RData")
load("raw-data/Exceedances/35/WHOIncomeRegion.RData")
load("raw-data/Exceedances/35/WHORegion.RData")

#################################################################
##                          Exposures                          ##
#################################################################
load("raw-data/Exposures/")


#################################################################
##                       Ground monitors                       ##
#################################################################

#load("raw-data/Ground monitors/GM_dat.RData")

#################################################################
##                   Read Gridded prediction                   ##
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

##################################################################
##                          Shapefiles                          ##
##################################################################

#load("raw-data/Shapefiles/shapefiles.RData")

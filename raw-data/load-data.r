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

                              #TODO

#################################################################
##                       Ground monitors                       ##
#################################################################


##################################################################
##                        Concentrations                        ##
##################################################################


#################################################################
##                          Exposures                          ##
#################################################################


#################################################################
##                         Exceedances                         ##
#################################################################



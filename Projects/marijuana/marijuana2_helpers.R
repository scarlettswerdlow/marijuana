###########################################################################
#                                                                         #
#  Marijuana arrests in Chicago Part II                                   #
#  Helpers                                                                #
#  Coded by Scarlett Swerdlow                                             #
#  scarlett.swerdlow@gmail.com                                            #
#  February 2, 2015                                                       #
#                                                                         #
###########################################################################

library( RCurl )
library( XML )
library( plyr )

#####################################################
#  Function to get FIPS code for given lat and lon  #
#####################################################

getFIPS <- function( record ) {
  
  geoURL <- "http://data.fcc.gov/api/block/find?latitude="
  
  lat <- record["Latitude"]
  lon <- record["Longitude"]
  
  # Check to make sure lat and lon exist
  if ( is.na( lat ) | is.na( lon ) ) {
    return( NULL )
  }
  
  url <- paste( geoURL, lat, "&longitude=", lon, sep = "")
  request <- getURL( url )
  
  FIPS <- xmlGetAttr( 
    xmlRoot( xmlTreeParse( request ) )[[ "Block" ]], "FIPS" )
  
  return( FIPS )
  
}

#######################################################################
#  Function to format FIPS code so it can be merged with Census data  #
#######################################################################

formatFIPS <- function( record ) {
  
  tract <- record["Tract"]
  first <- substr( tract, 1, nchar(tract)-2 )
  second <- substr( tract, nchar(tract)-1, nchar(tract) )
  
  if (second == "00") {
    tract <- first
  }
  
  else {
    tract <- as.numeric( paste( first, second, sep = "." ) )
  }
  
  return( tract )
  
}

#############################################################
#  Function to calculate population for given Census tract  #
#############################################################

getPopByTract <- function( record ) {
  
  tract <- record["Tract.F"]
  pop <- sum( pop$Estimate..Total[ pop$Tract == tract ] )
  
  return( pop )
  
}


###########################################################################
#                                                                         #
#  Marijuana arrests in Chicago Part II                                   #
#  Coded by Scarlett Swerdlow                                             #
#  scarlett.swerdlow@gmail.com                                            #
#  February 2, 2015                                                       #
#                                                                         #
###########################################################################

library( RCurl )
library( XML )
library( plyr )
source('~/marijuana/marijuana1.R')
source('~/marijuana/marijuana2_helpers.R')

#############################################
#  Get FIPS code for each marijuana arrest  #
#############################################

mj_arrests$FIPS <- apply( mj_arrests, 1, getFIPS )
mj_arrests$Tract <- as.numeric( substr( mj_arrests$FIPS, 6, nchar( mj_arrests$FIPS ) - 4 ) )

#################################################
#  Aggregate marijuana arrests by Census tract  #
#################################################

arrestsByTract <- count( mj_arrests[ !is.na(mj_arrests$Tract), ], "Tract" )
arrestsByTract$Tract.F <- apply( arrestsByTract, 1, formatFIPS )

############################################
#  Merge arrest data with population data  #
############################################

# 2013 5-year American Community Survey population estimates
pop <- read.csv( "~/marijuana/pop.csv", skip = 1 )
pop$Tract <- substr( pop$Geography, 14, nchar( as.character(pop$Geography) )-23 )

# Add population data to arrests by tract data
arrestsByTract$pop <- apply( arrestsByTract, 1, getPopByTract )

####################################################
#  Merge arrest data with Chicago geographic data  #
####################################################

# Chicago Census Tract-Community Area equivalency via Rob Paral
geo <- read.csv( "~/marijuana/geo.csv" )
geo <- geo[ geo$COUNTY == 31, ]
geo <- rename( geo, c( "TRACT" = "Tract" ) )
geo$Tract.F <- apply( geo, 1, formatFIPS )

arrestsByTractPop <- merge( arrestsByTract, geo, by.x = "Tract", by.y = "TRACT")
arrestsByTractPop <- arrestsByTractPop[ ,c( 1:4,11 ) ]
arrestsByTractPop <- rename( arrestsByTractPop, c( 
  "Tract.F.x" = "Tract.F", "freq" = "Arrests", "pop" = "Pop", "CHGOCA" = "Community" ) )

# Chicago Community Area names
commNames <- read.csv( "~/marijuana/commNames.csv" )

arrestsByTractPop <- merge( arrestsByTractPop, commNames, by.x = "Community", by.y = "Community.Area" )
arrestsByTractPop <- arrestsByTractPop[ ,c( 1:6 ) ]

#####################################################
#  Calculate marijuana arrest rate by Census tract  #
#####################################################

# Calculate 4-year arrest rate per 1000 residents
arrestsByTractPop$Rate <- arrestsByTractPop$Arrests*1000/arrestsByTractPop$Pop
arrestsByTractPop$Rate[ arrestsByTractPop$Rate == Inf ] <- 0

write.csv( arrestsByTractPop, "marijuana2_map.csv" )

####################################################################
#  Calculate African-American share of population by Census tract  #
####################################################################

# 2013 5-year American Community Survey population estimates by race
race <- read.csv( "~/marijuana/race.csv", skip = 1 )
race$Tract <- as.integer( substr( race$Id2, 6, 11 ) )
race <- merge( race, geo, by.x = "Tract", by.y = "Tract")
race <- merge( race, commNames, by.x = "CHGOCA", by.y = "Community.Area" )
race <- race[ ,c( 1, 2, 6, 10, 33, 34 ) ]
race <- rename( race, c(
  "CHGOCA" = "Community.Area",
  "Estimate..Total." = "Pop", 
  "Estimate..Total....Black.or.African.American.alone" = "Black",
  "Name" = "Community") )
race$Black.Share <- race$Black/race$Pop

write.csv( race, "race_map.csv" )

#########################################################################
#  Evaluate correlation between arrest rate and African-American share  #
#########################################################################

arrestsByTractPopRace <- merge( arrestsByTractPop, race, by.x = "Tract", by.y = "Tract" )

top100 <- head( arrestsByTractPopRace[ order( -arrestsByTractPopRace$Rate ), ], 100 )
top100[ top100$Black.Share < .5, ]

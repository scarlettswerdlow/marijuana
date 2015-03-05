###########################################################################
#                                                                         #
#  Marijuana arrests in Chicago Part III                                  #
#  Coded by Scarlett Swerdlow                                             #
#  scarlett.swerdlow@gmail.com                                            #
#  February 8, 2015                                                       #
#                                                                         #
###########################################################################

library( plyr )
library( ggplot2 )
source('~/marijuana/marijuana3_helpers.R')

##################################
#  Read in and clean crime data  #
##################################

# Data includes arrests for possession of 30 grams or less of marijuana
# between June 1, 2013 and December 31, 2013 and between June 1, 2014 and
# December 31, 2014.

mj_arrests_recent <- read.csv( "~/marijuana/crimes_recent.csv" )
mj_arrests_recent <- splitDate( mj_arrests_recent )

# Drop observations where no arrest was made
mj_arrests_recent <- mj_arrests_recent[ mj_arrests_recent$Arrest == "true", ]

######################################################################
#  Calculate number of marijuana arrests by community area and year  #
######################################################################

# Count 2013 and 2014 arrests by community area
arrestsByCommYear <- count( mj_arrests_recent, c( "Community.Area", "Year" ) )
arrestsByComm <- collapseDFByComm( arrestsByCommYear )

# Add community area data
# 2012 5-year American Community Survey estimates via Rob Paral
commData <- read.csv( "~/marijuana/comm.csv" )
commData <- rename( commData, c( "Community" = "Community_name" ) )

arrestsByComm <- merge( arrestsByComm, commData, by.x = "Community", by.y = "Community_area" )
arrestsByComm$Majority <- apply( arrestsByComm, 1, idMajorityRace )
arrestsByComm$Rate <- arrestsByComm$Arrests2014*1000/arrestsByComm$Pop_total

###########################################
#  Collapse communities by majority race  #
###########################################

# Count 2013 and 2014 arrests by majority community race
arrestsByRace <- collapseDFByRace( arrestsByComm )

# Calculate population and 2013 and 2014 arrest shares for each race
arrestsByRace$PopShare <- arrestsByRace$RacePop/sum( arrestsByRace$RacePop )
arrestsByRace$Arrests2013Share <- arrestsByRace$RaceArrests2013/sum( 
  arrestsByRace$RaceArrests2013 )
arrestsByRace$Arrests2014Share <- arrestsByRace$RaceArrests2014/sum( 
  arrestsByRace$RaceArrests2014 )

###########
#  Plots  #
###########

# Total arrests in majority Asian, Black, Latino, and White communities
levelsPlt <- ggplot( melt( arrestsByRace[ c(1,3:4) ] ), 
                     aes( x = variable, y = value, fill = RaceGroup ) ) + 
  geom_bar( stat = "identity" ) +
  ggtitle( "Total marijuana arrests in majority\nAsian, Black, Latino, and White communities" ) +
  theme( axis.title.x = element_blank() ) +
  ylab( "Arrests" ) +
  scale_x_discrete( labels = c( "2013 arrests\n(June 1 - Dec 31)", "2014 arrests\n(June 1 - Dec 31)" ) ) +
  guides( fill = guide_legend( title=NULL ) ) +
  theme( text = element_text( size = 16 ) )

# Share of total arrests in majority Asian, Black, Latino, and White communities
sharesPlt <- ggplot( melt( arrestsByRace[ c(1,7:9) ] ), 
                     aes( x = variable, y = value*100, fill = RaceGroup ) ) + 
  geom_bar( stat = "identity" ) +
  ggtitle( "Share of total marijuana arrests in majority\nAsian, Black, Latino, and White communities" ) +
  theme( axis.title.x = element_blank() ) +
  ylab( "Percent" ) +
  scale_x_discrete( limits = c( "Arrests2013Share", "Arrests2014Share", "PopShare" ), 
                    labels = c( "2013 arrests\n(June 1 - Dec 31)", 
                                "2014 arrests\n(June 1 - Dec 31)", 
                                "City population" ) ) +
  guides( fill = guide_legend( title=NULL ) ) +
  theme( text = element_text( size = 16 ) )

# Combine levelsPlt and sharesPlt in one image
multiplot( levelsPlt, sharesPlt, cols=2 )

# Percent change in marijuana arrests by community
changePltByComm <- ggplot( arrestsByComm, 
                           aes( x = reorder( factor( Community_name ), Change ), y = Change*100 ) ) + 
  geom_point( aes( colour = factor( Majority ), size = Rate ) ) +
  coord_flip( ) + 
  theme( axis.title.y = element_blank() ) +  
  ggtitle( "Change in marijuana arrests\nby community, 2013-2014" ) +
  ylab( "Percent change" ) +
  theme( axis.text.y = element_text( vjust = .5, hjust = 1 ) ) +
  ylim( -120, 120 ) +
  scale_colour_discrete( name = "Majority race" ) +
  scale_size_continuous( name = "2014 arrests per\n1000 residents" ) +
  geom_hline( aes( yintercept = 0 ), width = .1, colour = "grey" ) +
  theme( text = element_text( size = 16 ) )






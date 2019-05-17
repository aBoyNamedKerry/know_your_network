install.packages("lubridate", repos = "https://cran.r-project.org/"); library(lubridate)

flow = read.csv( 'Data/M6_traffic.csv', skip = 3 )
head( flow )

monday = flow[flow$Day.Type.ID == 0, ]
by_time = aggregate( monday$Total.Carriageway.Flow, list( monday$Local.Time ), mean )
colnames( by_time ) = c( 'time_of_day', 'no_vehicles' )
head( by_time )

attach( by_time )
# plot( time_of_day, no_vehicles, type = 'l' )
barplot( no_vehicles )

library(tidyverse)
library(nycflights13)

#13.2.1-------------------------------------------------------------------

#Imagine you wanted to draw (approximately) the route each plane flies from its origin to its destination. What variables would you need? What tables would you need to combine?
#The flights table has the origin (origin) and destination (dest) airport of each flight.
#The airports table has the longitude (lon) and latitude (lat) of each airport. 
flights_latlon <- flights %>%
  inner_join(select(airports, origin = faa, origin_lat = lat, origin_lon = lon),
             by = "origin"
  ) %>%
  inner_join(select(airports, dest = faa, dest_lat = lat, dest_lon = lon),
             by = "dest"
  )

#I forgot to draw the relationship between weather and airports. What is the relationship and how should it appear in the diagram?
#The column airports$faa is a foreign key of weather$origin. 
#The line representing the new relation between weather and airports is colored black. The lines representing the old relations are gray and thinner.

#weather only contains information for the origin (NYC) airports. If it contained weather records for all airports in the USA, what additional relation would it define with flights?
#provide the weather for the destination of each flight.

#We know that some days of the year are “special”, and fewer people than usual fly on them. How might you represent that data as a data frame?
special_days <- tribble(
  ~year, ~month, ~day, ~holiday,
  2013, 01, 01, "New Years Day",
  2013, 07, 04, "Independence Day",
  2013, 11, 29, "Thanksgiving Day",
  2013, 12, 25, "Christmas Day"
)
#What would be the primary keys of that table? How would it connect to the existing tables?
#The primary key of the table would be the (year, month, day) columns. The (year, month, day) columns could be used to join special_days with other tables.

#13.3.1------------------------------------------------------------

#Add a surrogate key to flights.
flights %>%
  arrange(year, month, day, sched_dep_time, carrier, flight) %>%
  mutate(flight_id = row_number()) %>%
  glimpse()

Identify the keys in the following datasets

Lahman::Batting #(playerID, yearID, stint).
babynames::babynames #(year, sex, name).
nasaweather::atmos #(lat, long, year, month). 
fueleconomy::vehicles #The column id, the unique EPA identifier of the vehicle.
ggplot2::diamonds #There is no primary key

#13.4.6----------------------------------------------------------------------

#Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
#Answer
avg_dest_delays <-
  flights %>%
  group_by(dest) %>%
  # arrival delay NA's are cancelled flights
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c(dest = "faa"))
avg_dest_delays %>%
  ggplot(aes(lon, lat, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

#Add the location of the origin and destination (i.e. the lat and lon) to flights.
airport_locations <- airports %>%
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa"),
    suffix = c("_origin", "_dest")

#Is there a relationship between the age of a plane and its delays?
#To compare the age of the plane to flights delay, I merge flights with the planes, which contains a variable plane_year, with the year in which the plane was built. 
plane_cohorts <- inner_join(flights,
                            select(planes, tailnum, plane_year = year),
                            by = "tailnum"
) %>%
  mutate(age = year - plane_year) %>%
  filter(!is.na(age)) %>%
  mutate(age = if_else(age > 25, 25L, age)) %>%
  group_by(age) %>%
  summarise(
    dep_delay_mean = mean(dep_delay, na.rm = TRUE),
    dep_delay_sd = sd(dep_delay, na.rm = TRUE),
    arr_delay_mean = mean(arr_delay, na.rm = TRUE),
    arr_delay_sd = sd(arr_delay, na.rm = TRUE),
    n_arr_delay = sum(!is.na(arr_delay)),
    n_dep_delay = sum(!is.na(dep_delay))
  )

#What weather conditions make it more likely to see a delay?
#Almost any amount of precipitation is associated with a delay. 
flight_weather <-
  flights %>%
  inner_join(weather, by = c(
    "origin" = "origin",
    "year" = "year",
    "month" = "month",
    "day" = "day",
    "hour" = "hour"
  ))

flight_weather %>%
  group_by(precip) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = precip, y = delay)) +
  geom_line() + geom_point()
#And there is a relationship between visibility and delay.
flight_weather %>%
  ungroup() %>%
  mutate(visib_cat = cut_interval(visib, n = 10)) %>%
  group_by(visib_cat) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = visib_cat, y = dep_delay)) +
  geom_point()

#What happened on June 13 2013? Display the spatial pattern of delays, and then use Google to cross-reference with the weather.
#There was a large series of storms in the southeastern US . 
#flights %>%
filter( year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  ggplot(aes(y = lat, x = lon, size = delay, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis()
























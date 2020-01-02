##1990
#population weighted mean of the tan of the latitudes for each region

population.weighted.latitude.1990 <- location_population_1990 %>%
  group_by(location_population_1990$REGION)%>%
  summarise(weighted.mean(tan.of.latitude, `1990 population`))

population.weighted.latitude.1990$representative.latitude.region <- (atan(population.weighted.latitude.1990[1:4,2]))*57.2958


##2finding representative coordinates:longitude
#population weighted mean of the longitudes for each region
location_population_1990$representative.longitude <- as.numeric(location_population_1990$representative.longitude)

population.weighted.longitude.1990 <- location_population_1990 %>%
  group_by(location_population_1990$REGION)%>%
  summarise(weighted.mean(representative.longitude, `1990 population`))


#3population weighted mean of the latitude for each region (for datasets outside of the light dataset)
location_population_1990$representative.latitude <- as.numeric(location_population_1990$representative.latitude)

population.weighted.latitude.1990 <- location_population_1990 %>%
  group_by(location_population_1990$REGION)%>%
  summarise(weighted.mean(representative.latitude, `1990 population`))


#Merge into one table
population.weighted.corrdinates.1990 <- merge(population.weighted.latitude.1990, population.weighted.longitude.1990, by= 'location_population_1990$REGION')
names(population.weighted.corrdinates.1990)[1] <- "REGION"
names(population.weighted.corrdinates.1990)[2] <- "Latitude"
names(population.weighted.corrdinates.1990)[3] <- "Longitude"













##2000
##population weighted mean of the tan of the latitudes for each region

population.weighted.latitude.2000 <- location_population_2000 %>%
  group_by(Location_population_2000$REGION)%>%
  summarise(weighted.mean(tan.of.latitude, `2000 population`))

population.weighted.latitude.2000$representative.latitude.region <- (atan(population.weighted.latitude.2000[2]))*57.2958


##2finding representative coordinates:longitude
#population weighted mean of the longitudes for each region
location_population_2000$representative.longitude <- as.numeric(location_population_2000$representative.longitude)

population.weighted.longitude.2000 <- location_population_2000 %>%
  group_by(location_population_2000$REGION)%>%
  summarise(weighted.mean(representative.longitude, `2000 population`))


#3population weighted mean of the latitude for each region (for datasets outside of the light dataset)
location_population_2000$representative.latitude <- as.numeric(location_population_2000$representative.latitude)

population.weighted.latitude.2000 <- location_population_2000 %>%
  group_by(location_population_2000$REGION)%>%
  summarise(weighted.mean(representative.latitude, `2000 population`))


#Merge into one table
population.weighted.corrdinates.2000 <- merge(population.weighted.latitude.2000, population.weighted.longitude.2000, by= 'location_population_2000$REGION')
names(population.weighted.corrdinates.2000)[1] <- "REGION"
names(population.weighted.corrdinates.2000)[2] <- "Latitude"
names(population.weighted.corrdinates.2000)[3] <- "Longitude"
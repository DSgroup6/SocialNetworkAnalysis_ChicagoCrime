comm_crimes <- readr::read_csv("D:/Datasets/sna/Comm_crimes.csv")
demo_data <- readr::read_csv("D:/Datasets/sna/ReferenceCCAProfiles20162020.csv")
demo_data <- demo_data[order(demo_data$GEOID),] #order by area

############################# Data preparation #############################

areas <- 1:77
sub <- comm_crimes

names <- demo_data$GEOG
node_info = data.frame(areas, names)
tot_pop <- demo_data$TOT_POP

# get center points of each district

get_crime_data <- function(is_preprocessed = TRUE){
  if (is_preprocessed == FALSE){
    crimes <- readr::read_csv("D:/Datasets/sna/crime_data/chicago_crimes.csv")
    crimes <- crimes[,c('Latitude', "Longitude", 'Community Area')]
    crimes <- crimes[complete.cases(crimes),]
    crimes$`Community Area` <- as.numeric(crimes$`Community Area`)
    
    # store for future use
    write.csv(crimes,'D:/Datasets/sna/crime_data/crimes_latlon.csv')
    
    return(crimes)
  }
  else {
    crimes <- readr::read_csv('D:/Datasets/sna/crime_data/crimes_latlon.csv')
    return(crimes)
  }
}

crimes <- get_crime_data()

lat <- vector()
lon <- vector()
for(area in areas){
  crimes_in_area <- crimes[crimes$`Community Area` == area,]
  area1_latitude <- mean(crimes_in_area$Latitude)
  area1_longitude <- mean(crimes_in_area$Longitude)
  lat <- append(lat, area1_latitude)
  lon <- append(lon, area1_longitude)
}
node_info$Latitude = lat
node_info$Longitude = lon


# Extracting top crimes per area
crimes_of_interest = list("THEFT", "BATTERY","CRIMINAL DAMAGE",
                          "DECEPTIVE PRACTICE","ASSAULT",
                          "NARCOTICS", "BURGLARY","MOTOR VEHICLE THEFT",
                          "ROBBERY","CRIMINAL TRESPASS")

j = 1
top_crime_list = rep(NA, length(areas))
for (area in areas) {
  # All the unique crimes in district
  crimes_in_area <- data.frame(subset(sub, `Community Area`== area))$Primary.Type
  counts = rep(NA, length(crimes_of_interest))
  i = 1
  for (crime in crimes_of_interest){
    # Number of occurrences of crime in district
    num_occurrences <- sum(crimes_in_area == crime) / tot_pop[j]
    counts[i] = num_occurrences
    i = i + 1
  }
  count_crimes_of_area = data.frame(crimes_of_interest, counts)
  perc_occurences = count_crimes_of_area$counts
  top_crime_list[j] = list(perc_occurences)
  print(j)
  j = j + 1
  
}

df_crime <- as.data.frame(do.call(rbind, top_crime_list))
df_crime$areas <- areas
node_info <- merge(node_info, df_crime, by = 'areas')
colnames(node_info)[5:14] <- crimes_of_interest


# Adding additional node information
work_force <- demo_data$TOT_HH
perc_unemp <- demo_data$UNEMP / tot_pop
perc_black <- demo_data$BLACK / tot_pop
perc_asian <- demo_data$ASIAN / tot_pop
perc_white <- demo_data$WHITE / tot_pop
perc_hispanic <- demo_data$HISP / tot_pop

node_info$perc_unemp <- perc_unemp
node_info$perc_black <- perc_black
node_info$perc_asian <- perc_asian
node_info$perc_white <- perc_white
node_info$perc_hispanic <- perc_hispanic
node_info$med_age <- demo_data$MED_AGE

# Income levels, measured on household level
node_info$perc_inc_under_25 <- demo_data$INC_LT_25K / work_force
node_info$perc_inc_25_50 <- demo_data$INC_25_50K / work_force
node_info$perc_inc_50_75 <- demo_data$INC_50_75K / work_force
node_info$perc_inc_75_100 <- demo_data$INC_75_100K / work_force
node_info$perc_inc_100_150 <- demo_data$INC_100_150K / work_force
node_info$perc_inc_over_150 <- demo_data$INC_GT_150 / work_force

# Education Level, Measured for 25+ in age
edu_pop <- demo_data$POP_25OV
node_info$LT_HS <- demo_data$LT_HS / edu_pop
node_info$HS <- demo_data$HS/ edu_pop
node_info$SOME_COLL <- demo_data$SOME_COLL/ edu_pop
node_info$ASSOC  <- demo_data$ASSOC/ edu_pop
node_info$BACH <- demo_data$BACH/ edu_pop
node_info$GRAD_PROF <- demo_data$GRAD_PROF/ edu_pop

as.matrix(node_info[node_info$areas == 3,c('Latitude','Longitude')])

##################### Building fully connected geo Network ###############################
df_fully <- expand.grid(areas, areas)
colnames(df_fully) <- c('area1', 'area2')

# remove duplicage edges
df_fully <- df_fully[df_fully$area2 > df_fully$area1,]

distance_coords <- function(lat1,lon1,lat2,lon2) {
  3963.0 * acos( (sin(lat1) * sin(lat2)) + cos(lat1) * cos(lat2) * cos(lon2-lon1))
}

distances = vector()
for (i in 1:nrow(df_fully)){
  area_start <- df_fully[i,]$area1
  area_start_loc <- c(node_info[node_info$areas == area_start,]$Latitude, node_info[node_info$areas == area_start,]$Longitude)
  
  
  area_finish <- df_fully[i,]$area2
  area_finish_loc <- c(node_info[node_info$areas == area_finish,]$Latitude, node_info[node_info$areas == area_finish,]$Longitude)
  
  distance <- distance_coords(node_info[node_info$areas == area_start,]$Latitude, node_info[node_info$areas == area_start,]$Longitude, node_info[node_info$areas == area_finish,]$Latitude, node_info[node_info$areas == area_finish,]$Longitude)
  
  distances <- append(distances,distance)
}

df_fully$weight <- distances
colnames(df_fully) <- c('area1', 'area2', 'weight')

network <- network::network(df_fully, loops = TRUE, multiple = FALSE) # need to set directed false?
network_i <- snafun::to_igraph(network)
network_i <- igraph::as.undirected(network_i)

weights <- df_fully$weight
weights.inverted <- 1/df_fully$weight
weights.zscore_normalized <- (df_fully$weight - mean(df_fully$weight, na.rm = TRUE)) / sd(df_fully$weight, na.rm = TRUE)
weights.inverted.zscore_normalized <- (weights.inverted - mean(weights.inverted, na.rm = TRUE)) / sd(weights.inverted, na.rm = TRUE)
weights.exp <- 10000/(df_fully$weight ^ 1.25)

# plot weight distribution
hist(weights.exp,breaks = 30, xlab = 'distance between 2 neighborhoods')

network_i <- snafun::add_edge_attributes(network_i, c('weight'), weights.inverted) # 1/weight because the closer, the stronger the connection
network_i <- snafun::remove_loops(network_i)

####################### Adding vertex attributes ###############################
data_in_network <-matrix(, nrow = 0, ncol = 31)

#remove names
node_info <- node_info[!names(node_info) %in% c('names')]


for (area in areas){
  data_for_area <- node_info[areas == area,]
  data_in_network <- rbind(data_in_network, as.matrix(data_for_area))
}

network_i <- snafun::add_vertex_attributes(network_i, value = data_in_network)                           

# creating weight matrix
net <- network_i

streq2 <- snafun::to_matrix(net)
diag(streq2) <- 0
W_equiv <-  streq2 / rowSums(streq2)

attrs <- snafun::extract_all_vertex_attributes(net)
attr_names <- c("perc_unemp", "med_age", "LT_HS","HS", "SOME_COLL", "ASSOC", "BACH", "GRAD_PROF", "perc_black", "perc_asian", "perc_white","perc_hispanic", 
                "perc_inc_under_25", "perc_inc_25_50", "perc_inc_50_75", "perc_inc_75_100","perc_inc_100_150") 

model1 <- sna::lnam(y = as.numeric(attrs[,  'THEFT']),
                    x = as.matrix(attrs[, attr_names]),
                    W1 = W_equiv, null.model='meanstd')
model2 <- sna::lnam(y = as.numeric(attrs[, 'ASSAULT']),
                    x = as.matrix(attrs[, attr_names]),
                    W1 = W_equiv, null.model='meanstd')
model3 <- sna::lnam(y = as.numeric(attrs[, 'BATTERY']),
                    x = as.matrix(attrs[, attr_names]),
                    W1 = W_equiv, null.model='meanstd')
model4 <- sna::lnam(y = as.numeric(attrs[, 'CRIMINAL DAMAGE']),
                    x = as.matrix(attrs[, attr_names]),
                    W1 = W_equiv, null.model='meanstd')
model5 <- sna::lnam(y = as.numeric(attrs[, 'DECEPTIVE PRACTICE']),
                    x = as.matrix(attrs[, attr_names]),
                    W1 = W_equiv, null.model='meanstd')
model6 <- sna::lnam(y = as.numeric(attrs[, 'NARCOTICS']),
                    x = as.matrix(attrs[, attr_names]),
                    W1 = W_equiv, null.model='meanstd')
model7 <- sna::lnam(y = as.numeric(attrs[, 'BURGLARY']),
                    x = as.matrix(attrs[, attr_names]),
                    W1 = W_equiv, null.model='meanstd')
model8 <- sna::lnam(y = as.numeric(attrs[, 'MOTOR VEHICLE THEFT']),
                    x = as.matrix(attrs[, attr_names]),
                    W1 = W_equiv, null.model='meanstd')
model9 <- sna::lnam(y = as.numeric(attrs[, 'ROBBERY']),
                    x = as.matrix(attrs[, attr_names]),
                    W1 = W_equiv, null.model='meanstd')
model10 <- sna::lnam(y = as.numeric(attrs[, "CRIMINAL TRESPASS"]),
                    x = as.matrix(attrs[, attr_names]),
                    W1 = W_equiv, null.model='meanstd')

crime_names = c("THEFT", "BATTERY","CRIMINAL DAMAGE",
                "DECEPTIVE PRACTICE","ASSAULT",
                "NARCOTICS", "BURGLARY","MOTOR VEHICLE THEFT",
                "ROBBERY")

texreg::screenreg(list(model1,model2,model3,model4,model5,model6,model7,model8,model9), custom.model.names = crime_names)

summary(model6)
plot(model1)

# WHY is the R2 so high?
# what is the net influence plot?
# the RHO is really low and not significant, so this means there is no social influence?
# What is the effect of the weights on the model? do we need to square, log, invert them, or what to do with them

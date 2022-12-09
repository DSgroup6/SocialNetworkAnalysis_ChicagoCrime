dataset_dir <- "D:/Datasets/sna"

comm_crimes <- readr::read_csv("D:/Datasets/sna/Comm_crimes.csv")
demo_data <- readr::read_csv("D:/Datasets/sna/ReferenceCCAProfiles20162020.csv")
demo_data <- demo_data[order(demo_data$GEOID),] #order by area

sub <- comm_crimes

tab <- table(sub$`Primary Type`)
tab2 <- tab[order(tab, decreasing = TRUE)]
qcc::pareto.chart(tab2)

areas <- 1:77
names <- demo_data$GEOG
node_info = data.frame(areas, names)
tot_pop <- demo_data$TOT_POP

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
colnames(node_info)[3:12] <- crimes_of_interest

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


################################################################################
##################### Building Bipartite Network ###############################

bipartite_network <- data.frame(0,0)

for (idx in 1:10){
  crime <- crimes_of_interest[idx]
  mean_of_crime <- sum(node_info[2 + idx]) / length(areas)
  larger_than <- node_info[[2 + idx]] >= mean_of_crime + 0.2
  areas_for_which <- node_info[larger_than,]$areas
  
  crime_lst <- rep(as.character(crime), length(areas_for_which))
  data_frame_crime <- data.frame( crime_lst, areas_for_which)
  bipartite_network <- data.frame(rbind(as.matrix(bipartite_network), as.matrix(data_frame_crime)))
}


bipartite_network <- bipartite_network[-1,]

network <- network::network(bipartite_network, bipartite = TRUE, bipartite_col = "X0.1")
network_i <- snafun::to_igraph(network)

col = c('green','red')

plot(network_i,
     vertex.color = col[as.numeric(igraph::V(network_i)$type)+1],
     layout = igraph::layout.fruchterman.reingold(network_i)) #igraph::layout.bipartite(network_i))


################################################################################
####################### Adding vertex attributes ###############################
areas_in_network <- as.numeric(snafun::extract_all_vertex_attributes(network)$vertex.names[-c(1:10)])
data_in_network <-matrix(, nrow = 10, ncol = 30)

for (area in areas_in_network){
  data_for_area <- node_info[areas == area,]
  data_in_network <- rbind(data_in_network, as.matrix(data_for_area))
}

network_i <- snafun::add_vertex_attributes(network_i, value = data_in_network)                           
proj_i <- igraph::bipartite.projection(network_i)$proj2
snafun::g_density(proj_i)


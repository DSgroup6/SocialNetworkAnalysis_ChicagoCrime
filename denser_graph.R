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
    num_occurrences <- sum(crimes_in_area == crime) / tot_pop[j]# rate of crime per population
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
nrow(node_info)
node_info[30:77,]
################################################################################
##################### Building Bipartite Network ###############################

bipartite_network <- data.frame(0,0)

for (idx in 1:10){
  crime <- crimes_of_interest[idx]
  mean_of_crime <- sum(node_info[2 + idx]) / length(areas)
  larger_than <- node_info[[2 + idx]] >= mean_of_crime 
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
     layout = igraph::layout.bipartite(network_i))

##################### unimodal network #####################
proj_i <- igraph::bipartite.projection(network_i)$proj2

#### adding isolated nodes to the network ###############

areas_in_network <- as.numeric(snafun::extract_all_vertex_attributes(network)$vertex.names[-c(1:10)])
isolates <- areas[!(areas %in% as.numeric(areas_in_network))]# isolated nodes
new_networkd <- proj_i + igraph::vertices(isolates)# add isolated nodes to the network

######## adding attributes to the nodes 
df_isolated <- subset(node_info, rownames(node_info) %in% isolates)
df_conected <- subset(node_info, rownames(node_info) %in% areas_in_network)

df_crimenet<-rbind(df_conected , df_isolated )
network_newd<- snafun::add_vertex_attributes(new_networkd, value = df_crimenet)

#check consistency
snafun::extract_all_vertex_attributes(network_newd)[2,]
node_info[8,]

snafun::g_density(network_newd)
### store the network 
save('network_new', file="crime_netd.rda")
crime_net <- snafun::to_network(network_newd)
## try ERGM
crime_ERGM <- ergm::ergm(crime_net ~ edges + nodecov("perc_unemp") + 
                           nodecov("perc_inc_under_25") + nodecov("LT_HS")+ 
                           gwesp(decay= 0.1,fixed=TRUE )
                          , 
                         control = ergm::control.ergm(MCMC.burnin = 5000,
                                                      MCMC.samplesize = 10000,
                                                      seed = 1234,
                                                      MCMLE.maxit = 20,
                                                      parallel = 6,
                                                      parallel.type = "PSOCK"))




quartz()
ergm::mcmc.diagnostics(crime_ERGM)
quartz()
crime_ERGM.gof <- ergm::gof(crime_ERGM)
snafun::plot(crime_ERGM.gof)


dev.off()
par(mfrow=c(1,1))

while (!is.null(dev.list()))  dev.off()

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

par("mfrow")
par(resetPar())     


# by Mathieu
degree_dist <- snafun::g_degree_distribution(crime_net)

crime_net = snafun::to_network(network_new)
crime_ERGM <- ergm::ergm(crime_net ~ edges + nodecov("perc_unemp") + 
                           nodecov("perc_inc_under_25") + nodecov("LT_HS")+ 
                           nodecov("med_age") + nodecov("perc_black") + gwesp(decay= 0.1,fixed=TRUE )
                         + degree(34) + degree(36) + degree(37) + degree(38) , 
                         control = ergm::control.ergm(MCMC.burnin = 5000,
                                                      MCMC.samplesize = 10000,
                                                      seed = 1234,
                                                      MCMLE.maxit = 20,
                                                      parallel = 4,
                                                      parallel.type = "PSOCK"))




crime_ERGM <- ergm::ergm(crime_net ~ edges + gwdsp(decay= 0.01,fixed=TRUE ), 
                         control = ergm::control.ergm(MCMC.burnin = 4000,
                                                      MCMC.samplesize = 6000,
                                                      seed = 1234,
                                                      MCMLE.maxit = 20,
                                                      parallel = 6,
                                                      parallel.type = "PSOCK"))
ergm::mcmc.diagnostics(crime_ERGM)
crime_ERGM.gof <- ergm::gof(crime_ERGM)
plot(crime_ERGM.gof)



# check for NA in attributes
crime_ERGM1  <- ergm::ergm(crime_net ~ edges + gwesp(decay= 0.1,fixed=TRUE )
                         + nodecov("perc_unemp") + 
                           nodecov("perc_inc_under_25") , 
                         control = ergm::control.ergm(MCMC.burnin = 4000,
                                                      MCMC.samplesize = 6000,
                                                      seed = 1234,
                                                      MCMLE.maxit = 20,
                                                      parallel = 6,
                                                                                                        parallel.type = "PSOCK"))
ergm::mcmc.diagnostics(crime_ERGM)

 
crime_ERGM1.gof <- ergm::gof(crime_ERGM1)
snafun::stat_plot_gof_as_btergm(crime_ERGM1)


summary(crime_ERGM1)
### a term to add betweenness 

plot(crime_ERGM.gof)

### try gwesp not work 
### gwdsp doesn't work the The log-likelihood improved by 0.4997 decrease

# to test degree(40) based on the outcome of 
degree_dist <- snafun::g_degree_distribution(crime_net)

crime_ERGM2 <- ergm::ergm(crime_net ~ edges + nodecov("perc_unemp") + 
                            nodecov("perc_inc_under_25") + degree(40)
                          + gwesp(decay= 0.1,fixed=TRUE )
                         , 
                         control = ergm::control.ergm(MCMC.burnin = 5000,
                                                      MCMC.samplesize = 10000,
                                                      seed = 1234,
                                                      MCMLE.maxit = 20,
                                                      parallel = 4,
                                                      parallel.type = "PSOCK"))

quartz()
ergm::mcmc.diagnostics(crime_ERGM2)

crime_ERGM.gof <- ergm::gof(crime_ERGM)
snafun::stat_plot_gof_as_btergm(crime_ERGM)
snafun::g_summary(crime_net)


unem <-snafun::extract_vertex_attribute(crime_net, "perc_unemp")
perc_inc_under_25 <- snafun::extract_vertex_attribute(crime_net, "perc_inc_under_25")

sum(is.na(unem))

crime_ERGM3<- ergm::ergm(crime_net ~ edges + nodecov("perc_unemp") + 
                           nodecov("perc_inc_under_25") + gwesp(decay= 0.1,fixed=TRUE )
                         + degree(35) + degree(38)  , 
                         control = ergm::control.ergm(MCMC.burnin = 5000,
                                                      MCMC.samplesize = 10000,
                                                      seed = 1234,
                                                      MCMLE.maxit = 20,
                                                      parallel = 4,
                                                      parallel.type = "PSOCK"))

quartz()
ergm::mcmc.diagnostics(crime_ERGM3)

crime_ERGM3.gof <- ergm::gof(crime_ERGM3)


snafun::stat_plot_gof_as_btergm(crime_ERGM3)
summary(crime_ERGM3)


crime_ERGM3<- ergm::ergm(crime_net ~ edges + nodecov("perc_unemp") + 
                           nodecov("perc_inc_under_25") + gwesp(decay= 0.1,fixed=TRUE )
                         + gwdegree(0.08, fixed= TRUE)  , 
                         control = ergm::control.ergm(MCMC.burnin = 5000,
                                                      MCMC.samplesize = 6000,
                                                      seed = 1234,
                                                      MCMLE.maxit = 20,
                                                      parallel = 7,
                                                      parallel.type = "PSOCK"))


#### adding gwdegree doesn't converge
snafun::g_degree_distribution(crime_net)

crime_net = snafun::to_network(network_new)
snafun::g_density(crime_net)
crime_ERGM36_7 <- ergm::ergm(crime_net ~ edges + nodecov("perc_unemp") + 
                           nodecov("perc_inc_under_25") + nodecov("LT_HS")+ 
                           nodecov("med_age") + nodecov("perc_black")
                          + gwesp(decay= 0.05,fixed=TRUE )
                         + degree(36) , 
                         control = ergm::control.ergm(MCMC.burnin = 5000,
                                                      MCMC.samplesize = 6000,
                                                      seed = 1234,
                                                      MCMLE.maxit = 20,
                                                      parallel = 7,
                                                      parallel.type = "PSOCK"))




quartz()
ergm::mcmc.diagnostics(crime_ERGM36)

crime_ERGM36.gof <- ergm::gof(crime_ERGM36)
snafun::stat_plot_gof(crime_ERGM36.gof)

summary(crime_ERGM36)



snafun::g_transitivity(crime_net)


snafun::stat_plot_gof_as_btergm(crime_ERGM36)
 

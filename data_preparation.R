setwd("D:/Datasets/")

install.packages('qcc')

ds <- read.csv('sna/crime_data/Chicago Crimes_-_2001_to_Present.csv')
length(ds)

summary(ds)
cols_to_drop <- c("X.Coordinate","Y.Coordinate", "IUCR",  "Location.Description", "Description", "Ward", "Beat", "Community.Area", "Location", "Updated.On", "Date")
ds <- ds[,!(names(ds) %in% cols_to_drop)]

ds <- ds[!is.na(ds$Latitude),]
ds <- ds[!is.na(ds$District),]

summary(ds)

subset <- ds[sample(1:nrow(ds),1000000),]

tab <- table(subset$Primary.Type)
tab2 <- tab[order(tab, decreasing = TRUE)]

ten_most_common_categories <- names(tab2[1:10])

ds$District <- as.factor(ds$District)
ds$Block <- as.factor(ds$Block)


subset <- subset[subset$District != '21',]

num_of_rows <- function(x) {
  c(x,nrow(ds[ds$District == x,]))
}

sapply(1:25,num_of_rows)


head(subset[,]$Block)
#21 = district 02
qcc::pareto.chart(tab2)

?sample

levels(ds$District)

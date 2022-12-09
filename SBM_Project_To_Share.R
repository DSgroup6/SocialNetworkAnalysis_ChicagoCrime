# SBM Preprocessing

info_week32 <- readRDS("~/Year_2_Semester_1/SBM/Kickstarter/Kickstarter/2019_week32/info.rds")

unix_timestamp_last_week_day <- 1565536269

info_week32_with_projects_that_reached_deadline <- info_week32[info_week32$Deadline < unix_timestamp_last_week_day,] # 73219 cases

summary(info_week32_with_projects_that_reached_deadline)


info_week32_with_projects_that_reached_deadline
new_df <- info_week32_with_projects_that_reached_deadline[!is.na(info_week32_with_projects_that_reached_deadline$Goal_USD),]
new_df_without_na <- info_week32_with_projects_that_reached_deadline[!is.na(info_week32_with_projects_that_reached_deadline$Pledge_USD),]


new_df_without_na$Goal_USD <- as.integer(as.character(new_df_without_na$Goal_USD))
new_df_without_na$Pledge_USD <- as.integer(as.character(new_df_without_na$Pledge_USD))



# dit klopt nog niet met de outliers
new_df_without_outliers <- subset(new_df_without_na, new_df_without_na["Goal_USD"] <= 1000000)
new_df_without_outliers <- subset(new_df_without_outliers, new_df_without_outliers["Goal_USD"] >= 100)
# Unnecessary statements
failed_projects <-  subset(new_df_without_outliers, new_df_without_outliers["Goal_USD"] > new_df_without_outliers["Pledge_USD"])  # 20399 cases

succeeded_projects <- subset(new_df_without_outliers, new_df_without_outliers["Goal_USD"] <= new_df_without_outliers["Pledge_USD"])  # 52440 cases



# 0 == Unsuccesful
# 1 == successful

new_df_without_outliers$successful = ifelse(new_df_without_outliers$Goal_USD > new_df_without_outliers$Pledge_USD, '0',
                      ifelse(new_df_without_outliers$Goal_USD < new_df_without_outliers$Pledge_USD, '1', '1'))

new_df_without_outliers$successful <- as.integer(as.character(new_df_without_outliers$successful))
summary(new_df_without_outliers["successful"])


final_subset <- new_df_without_outliers[c("my_id", "creator_slug", "Category", "Project_description", "successful")]


# list of the variables 


# Word2vec | N-gram (Try out)

# Look at whether making the project description more personal: e.g. use of the words: We and You, or Hi!, Hey! whether it makes a project more successful
# 
# 
# 


install.packages("word2vec")


write.csv(final_subset,"~/Year_2_Semester_1/SBM/final_dataframe_goodone.csv", row.names = FALSE)











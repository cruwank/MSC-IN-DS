#List files in the working directory
path = "D:/Download-15-03-2023/MsC in DS - ICBT-CIS7017/data/"
list.files (path)

#Set file path (Working Directory)
setwd(path)  

#Import required libraries
#############################################
#Install dplyr library for data manipulation
#install.packages("dplyr")
library(dplyr)
#Install caret library for data normalization
#install.packages("caret")
library(caret)
#Install VIM library to see the pattern of my missing data
#install.packages("VIM")
library(VIM)
#install.packages("class")
library(class)
#install.packages("gmodels")
library(gmodels)
#install.packages("neuralnet")
library(neuralnet)
#install.packages("ggplot2")
library(ggplot2)

#Read csv file and create data frames
rawdata_survay_data <- read.csv("Survay_Data.csv")

#View data summery of rawdata_survay_data dataframe
head(rawdata_survay_data)
#Identfy the rawdata_survay_data dataframe structure
nrow(rawdata_survay_data)
ncol(rawdata_survay_data)
str(rawdata_survay_data)

#Create duplicate data set from the original dataframes
temp_rawdata_survay_data <- rawdata_survay_data  

#Convert “NULL” string values and empty values to NA values get NA value count  
temp_rawdata_survay_data[temp_rawdata_survay_data == " "] <- NA
temp_rawdata_survay_data[temp_rawdata_survay_data == ""] <- NA
temp_rawdata_survay_data[temp_rawdata_survay_data == "NULL"] <- NA
#View number of NA records in each column
lapply(temp_rawdata_survay_data,function(x) { length(which(is.na(x)))})


##########################################################################
#####                                                                #####
##### Cleaning outliers, Exceptional Values, Normalizations, Scaling #####
#####                      <- Start ->                               #####
#####        Treatment to Patient dataframe                          #####
#####                                                                #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Check data type of each column 
sapply(temp_rawdata_survay_data, class)

#View dataframe summery and understand the structure and datatypes
summary(temp_rawdata_survay_data)
#Convert important columns to a suitable data format
temp_rawdata_survay_data$Accept_future <- as.factor(temp_rawdata_survay_data$Accept_future)
temp_rawdata_survay_data$Gender <- as.factor(temp_rawdata_survay_data$Gender) 

#View dataframe summery after conversion 
summary(temp_rawdata_survay_data)

#Remove if any duplicate records in the dataframe by considering columns "PatientId ,Title ,NIC ,Mobile ,Age ,BirthYear ,UniqueId ,DateOfBirth ,Gender"
temp_rawdata_survay_data <- temp_rawdata_survay_data %>% group_by(temp_rawdata_survay_data[1:9]) %>% filter(!n() >1)

#Check dataframe record count before and after make unique records 
dim(temp_rawdata_survay_data)

#Get the most occurrence Gender from the unique records
table(temp_rawdata_survay_data$Gender)
max_gender<- names(which.max(table(temp_rawdata_survay_data$Gender)))

# Create a new column called gender_value and assign Gender binary values
#temp_rawdata_survay_data$gender_value <- ifelse(temp_rawdata_survay_data$Gender == "M", 1, 0)


#View number of NA records in each column
lapply(temp_rawdata_survay_data,function(x) { length(which(is.na(x)))})

#Remove unwanted data columans
clean1_temp_rawdata_survay_data<- temp_rawdata_survay_data[,!names(temp_rawdata_survay_data) %in% c("BloodGroup","EmergencyContact","MaritalStatus","UserModified","DateModified","DateModified","MOH")]

#Visualize the missing data (NA values) distribution 
missing_clean1_temp_rawdata_survay_data <- aggr(clean1_temp_rawdata_survay_data, col=c('navyblue','yellow'), 
                                            numbers=TRUE, sortVars=TRUE, labels=names(clean1_temp_rawdata_survay_data), 
                                            cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"), oma = c(6,5,2,4))
print(missing_clean1_temp_rawdata_survay_data)

#Remove age not specified (Zero values and NA)
clean2_temp_rawdata_survay_data <- clean1_temp_rawdata_survay_data %>% filter(!is.na(Age))

#Visualize the missing data (NA values) distribution after removing NA value records from Age
missing_clean2_clean2_temp_rawdata_survay_data <- aggr(clean2_temp_rawdata_survay_data, col=c('darkgreen','yellow'), 
                                                   numbers=TRUE, sortVars=TRUE, labels=names(clean2_temp_rawdata_survay_data), 
                                                   cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"), oma = c(6,5,2,4))
print(missing_clean2_clean2_temp_rawdata_survay_data)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                    Visualize the Age and Gender                             #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#BAR chart to view age information
# Group the data frame by 10-year blocks of age and get the count of each group
age_count <- clean2_temp_rawdata_survay_data %>%
  mutate(age_group = cut(Age, breaks = seq(10, 110, 10))) %>%
  group_by(age_group) %>%
  summarise(count = n())

# Print the age count
print(age_count)
# Create a bar chart of the age groups and their respective counts
ggplot(age_count, aes(x = age_group, y = count, fill = age_group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  labs(title = "Count of people by age group",
       x = "Age group",
       y = "Count")


#Bar chart to view age and gender information
# Group the data frame by 10-year blocks of age and get the count of each group
age_count <- clean2_temp_rawdata_survay_data %>%
  mutate(age_group = cut(Age, breaks = seq(10, 110, 10))) %>%
  group_by(age_group, Gender) %>%
  summarise(count = n())

ggplot(age_count, aes(x = age_group, y = count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(title = "Count of people by age group and gender",
       x = "Age group",
       y = "Count",
       fill = "Gender")

#Bar chart to view age and gender information
# Group the data frame by 10-year blocks of age and get the count of each group
age_count <- clean2_temp_rawdata_survay_data %>%
  mutate(age_group = cut(Age, breaks = seq(10, 110, 10))) %>%
  group_by(age_group, Gender) %>%
  summarise(count = n())

ggplot(age_count, aes(x = age_group, y = count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(title = "Count of people by age group and gender",
       x = "Age group",
       y = "Count",
       fill = "Gender")


#scatter plot view for gender
# Create a scatter plot of age versus gender, colored by gender
scatter <- ggplot(clean2_temp_rawdata_survay_data, aes(x = Age, y = Gender, color = Gender)) +
  geom_point() +
  labs(title = "Age by Gender",
       x = "Age",
       y = "Gender",
       color = "Gender")

#histogram view for gender
# Create a histogram of age grouped by gender
histogram <- ggplot(clean2_temp_rawdata_survay_data, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) +
  labs(title = "Distribution of Age by Gender",
       x = "Age",
       y = "Count",
       fill = "Gender")

# Combine the scatter plot and histogram using the patchwork package
#install.packages("patchwork")
library(patchwork)
scatter + histogram + plot_layout(ncol = 2, widths = c(4, 3))

#Pie chart for gender
# Calculate the number of observations in each gender group
counts <- table(clean2_temp_rawdata_survay_data$Gender)

# Create a data frame with the counts and labels
pie_data <- data.frame(counts)
pie_data$labels <- rownames(pie_data)

# Create a pie chart
pie_chart <-
  ggplot(pie_data, aes(x = "", y = counts, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Gender Distribution",
       x = "Male",
       y = "Female",
       fill = "Gender") +
  theme_void()

# Show the pie chart
pie_chart


#Line chart to view Age and Gender Relationship
# Create a line chart
line_chart <- ggplot(clean2_temp_rawdata_survay_data, aes(x = Age, color = Gender)) +
  geom_density() +
  labs(title = "Age and Gender Relationship",
       x = "Age",
       y = "Density",
       color = "Gender")

# Show the line chart
line_chart

###
# Create a scatter plot
scatter_plot <- ggplot(clean2_temp_rawdata_survay_data, aes(x = Age, y = No_of_Doses , color = Gender, size = No_of_Doses )) +
  geom_point() +
  labs(title = "Age, Gender, and Vaccine Dose Relationship",
       x = "Age",
       y = "Dose",
       color = "Gender",
       size = "Dose")

# Show the scatter plot
scatter_plot

# Group the data by age, gender, and Dose and count the number of records
df_summary <- clean2_temp_rawdata_survay_data %>% group_by(Age, Gender, No_of_Doses) %>% summarize(count = n())

# Create a bar chart
bar_chart <- ggplot(df_summary, aes(x = Age, y = count, fill = factor(No_of_Doses))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Gender, ncol = 2) +
  labs(title = "Age, Gender, and Dose Distribution",
       x = "Age",
       y = "Count",
       fill = "Dose") +
  scale_fill_discrete(name = "Dose", labels = c("Dose 1", "Dose 2", "Dose 3", "Dose 4"))

# Show the bar chart
bar_chart






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
rawdata_patient_data <- read.csv("P_Data_Research.csv")
rawdata_vaccine_data <- read.csv("V_Data_Research.csv")

#View data summery of rawdata_patient_data dataframe
head(rawdata_patient_data)
#Identfy the rawdata_patient_data dataframe structure
nrow(rawdata_patient_data)
ncol(rawdata_patient_data)
str(rawdata_patient_data)

#View data summery of rawdata_vaccine_data dataframe
head(rawdata_vaccine_data)
#Identfy the rawdata_vaccine_data dataframe structure
nrow(rawdata_vaccine_data)
ncol(rawdata_vaccine_data)
str(rawdata_vaccine_data)


#Create duplicate data set from the original dataframes
temp_rawdata_patient_data <- rawdata_patient_data  
temp_rawdata_vaccine_data <- rawdata_vaccine_data


#Convert “NULL” string values and empty values to NA values get NA value count  
temp_rawdata_patient_data[temp_rawdata_patient_data == " "] <- NA
temp_rawdata_patient_data[temp_rawdata_patient_data == ""] <- NA
temp_rawdata_patient_data[temp_rawdata_patient_data == "NULL"] <- NA
#View number of NA records in each column
lapply(temp_rawdata_patient_data,function(x) { length(which(is.na(x)))})

temp_rawdata_vaccine_data[temp_rawdata_vaccine_data == " "] <- NA
temp_rawdata_vaccine_data[temp_rawdata_vaccine_data == ""] <- NA
temp_rawdata_vaccine_data[temp_rawdata_vaccine_data == "NULL"] <- NA
#View number of NA records in each column
lapply(temp_rawdata_vaccine_data,function(x) { length(which(is.na(x)))})



##########################################################################
#####                                                                #####
##### Cleaning outliers, Exceptional Values, Normalizations, Scaling #####
#####                      <- Start ->                               #####
#####        Treatment to Patient dataframe                          #####
#####                                                                #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Check data type of each column 
sapply(temp_rawdata_patient_data, class)

#View dataframe summery and understand the structure and datatypes
summary(temp_rawdata_patient_data)
#Convert important columns to a suitable data format
temp_rawdata_patient_data$PatientId <- as.character(temp_rawdata_patient_data$PatientId)
temp_rawdata_patient_data$Title <- as.character(temp_rawdata_patient_data$Title)
temp_rawdata_patient_data$NIC <- as.character(temp_rawdata_patient_data$NIC)
temp_rawdata_patient_data$Mobile <- as.character(temp_rawdata_patient_data$Mobile)
temp_rawdata_patient_data$UniqueId <- as.character(temp_rawdata_patient_data$UniqueId)
temp_rawdata_patient_data$ParentId <- as.character(temp_rawdata_patient_data$ParentId)
temp_rawdata_patient_data$PatientTypeId <- as.character(temp_rawdata_patient_data$PatientTypeId)
temp_rawdata_patient_data$AddressId <- as.character(temp_rawdata_patient_data$AddressId)
temp_rawdata_patient_data$Occupation <- as.character(temp_rawdata_patient_data$Occupation)
temp_rawdata_patient_data$Status <- as.character(temp_rawdata_patient_data$Status)
temp_rawdata_patient_data$UserCreated <- as.character(temp_rawdata_patient_data$UserCreated)
temp_rawdata_patient_data$PHIArea <- as.factor(temp_rawdata_patient_data$PHIArea)
temp_rawdata_patient_data$GNDivision <- as.factor(temp_rawdata_patient_data$GNDivision)
temp_rawdata_patient_data$DateOfBirth <- as.Date(temp_rawdata_patient_data$DateOfBirth) 
temp_rawdata_patient_data$Gender <- as.factor(temp_rawdata_patient_data$Gender) 
temp_rawdata_patient_data$Age <- as.integer(temp_rawdata_patient_data$Age)
temp_rawdata_patient_data$BirthYear <- as.integer(temp_rawdata_patient_data$BirthYear)
#View dataframe summery after conversion 
summary(temp_rawdata_patient_data)

#Remove if any duplicate records in the dataframe by considering columns "PatientId ,Title ,NIC ,Mobile ,Age ,BirthYear ,UniqueId ,DateOfBirth ,Gender"
unique_rawdata_patient_data <- temp_rawdata_patient_data %>% group_by(temp_rawdata_patient_data[1:9]) %>% filter(!n() >1)

#Check dataframe record count before and after make unique records 
dim(temp_rawdata_patient_data)
dim(unique_rawdata_patient_data)

print(unique_rawdata_patient_data)

#Get the most occurrence Gender from the unique records
table(unique_rawdata_patient_data$Gender)
max_gender<- names(which.max(table(unique_rawdata_patient_data$Gender)))

# Create a new column called gender_value and assign Gender binary values
unique_rawdata_patient_data$gender_value <- ifelse(unique_rawdata_patient_data$Gender == "M", 1, 0)


#View number of NA records in each column
lapply(unique_rawdata_patient_data,function(x) { length(which(is.na(x)))})

#Remove unwanted data columans
clean1_unique_rawdata_patient_data <- unique_rawdata_patient_data[,!names(unique_rawdata_patient_data) %in% c("BloodGroup","EmergencyContact","MaritalStatus","UserModified","DateModified","DateModified","MOH")]

#Visualize the missing data (NA values) distribution 
missing_clean1_unique_rawdata_patient_data <- aggr(clean1_unique_rawdata_patient_data, col=c('navyblue','yellow'), 
                                            numbers=TRUE, sortVars=TRUE, labels=names(clean1_unique_rawdata_patient_data), 
                                            cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"), oma = c(6,5,2,4))
print(missing_clean1_unique_rawdata_patient_data)

#Remove age not specified (Zero values and NA)
clean2_unique_rawdata_patient_data <- clean1_unique_rawdata_patient_data %>% filter(!is.na(Age))

#Visualize the missing data (NA values) distribution after removing NA value records from Age
missing_clean2_unique_rawdata_patient_data <- aggr(clean2_unique_rawdata_patient_data, col=c('darkgreen','yellow'), 
                                                   numbers=TRUE, sortVars=TRUE, labels=names(clean2_unique_rawdata_patient_data), 
                                                   cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"), oma = c(6,5,2,4))
print(missing_clean2_unique_rawdata_patient_data)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                    Visualize the Age and Gender                             #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#BAR chart to view age information
# Group the data frame by 10-year blocks of age and get the count of each group
age_count <- clean2_unique_rawdata_patient_data %>%
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
age_count <- clean2_unique_rawdata_patient_data %>%
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
scatter <- ggplot(clean2_unique_rawdata_patient_data, aes(x = Age, y = Gender, color = Gender)) +
  geom_point() +
  labs(title = "Age by Gender",
       x = "Age",
       y = "Gender",
       color = "Gender")

#histogram view for gender
# Create a histogram of age grouped by gender
histogram <- ggplot(clean2_unique_rawdata_patient_data, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) +
  labs(title = "Distribution of Age by Gender",
       x = "Age",
       y = "Count",
       fill = "Gender")

# Combine the scatter plot and histogram using the patchwork package
install.packages("patchwork")
library(patchwork)
scatter + histogram + plot_layout(ncol = 2, widths = c(4, 3))

#Pie chart for gender
# Calculate the number of observations in each gender group
counts <- table(clean2_unique_rawdata_patient_data$Gender)

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
line_chart <- ggplot(clean2_unique_rawdata_patient_data, aes(x = Age, color = Gender)) +
  geom_density() +
  labs(title = "Age and Gender Relationship",
       x = "Age",
       y = "Density",
       color = "Gender")

# Show the line chart
line_chart

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#Check dataframe record count
dim(clean2_unique_rawdata_patient_data)
#Visualize outliers using boxplot
boxplot(clean2_unique_rawdata_patient_data$Age, col="skyblue2", pch=20, main="Age data distribution")

#View the quantile values
quantile(clean2_unique_rawdata_patient_data$Age)

# Remove outliers from using 3rd Quantile
Q3 <- quantile(clean2_unique_rawdata_patient_data$Age, probs = , na.rm = TRUE, 0.75)
IQR <- IQR(clean2_unique_rawdata_patient_data$Age)
# Apply 1.5 IQR to the 3rd quantile
IQR_patient_data <- subset(clean2_unique_rawdata_patient_data, clean2_unique_rawdata_patient_data$Age < Q3 + 1*IQR)

#View the quantile values after removing outliers
quantile(IQR_patient_data$Age)
#Check dataframe record count
dim(IQR_patient_data)
#Visualize outliers using boxplot
boxplot(IQR_patient_data$Age, col="skyblue2", pch=20, main="Age data distribution")

#Normalize Data with Min-Max Scaling 
process <- preProcess(as.data.frame(IQR_patient_data), method=c("range"))
norm_scale_patient_data <- predict(process, as.data.frame(IQR_patient_data))
summary(norm_scale_patient_data)

#Change treated dataframe name 
treated_patient_data <- IQR_patient_data
normalied_patient_vaccine_data <- norm_scale_patient_data


#Remove unwanted dtaframes from memory
rm(rawdata_patient_data)
rm(temp_rawdata_patient_data)
rm(unique_rawdata_patient_data)
rm(clean1_unique_rawdata_patient_data)
rm(missing_clean1_unique_rawdata_patient_data)
rm(clean2_unique_rawdata_patient_data)
rm(missing_clean2_unique_rawdata_patient_data)
rm(IQR_patient_data)
rm(norm_scale_patient_data)

#View treated patient dataframe
View(treated_patient_data)
#View treated and normalized patient dataframe
View(normalied_patient_vaccine_data)


##########################################################################
#####                                                                #####
##### Cleaning outliers, Exceptional Values, Normalizations, Scaling #####
#####                         <- Start ->                            #####
#####            Treatment to Vaccine_Data datafram                  #####
#####                                                                #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Check data type of each column 
sapply(temp_rawdata_vaccine_data, class)

#View dataframe summery and understand the structure and datatypes
summary(temp_rawdata_vaccine_data)

#Convert important columns to a suitable data format
temp_rawdata_vaccine_data$Id <- as.character(temp_rawdata_vaccine_data$Id)
temp_rawdata_vaccine_data$PatientId <- as.character(temp_rawdata_vaccine_data$PatientId)
temp_rawdata_vaccine_data$VaccineName <- as.factor(temp_rawdata_vaccine_data$VaccineName)
temp_rawdata_vaccine_data$VaccineBatchNumber <- as.factor(temp_rawdata_vaccine_data$VaccineBatchNumber)
temp_rawdata_vaccine_data$Dose <- as.factor(temp_rawdata_vaccine_data$Dose)
temp_rawdata_vaccine_data$Center <- as.factor(temp_rawdata_vaccine_data$Center)
temp_rawdata_vaccine_data$GNDivision <- as.factor(temp_rawdata_vaccine_data$GNDivision)
temp_rawdata_vaccine_data$Remark <- as.character(temp_rawdata_vaccine_data$Remark)
temp_rawdata_vaccine_data$UserCreated <- as.character(temp_rawdata_vaccine_data$UserCreated)
temp_rawdata_vaccine_data$VaccinationDate <- as.Date(temp_rawdata_vaccine_data$VaccinationDate)
temp_rawdata_vaccine_data$DateCreated <- as.Date(temp_rawdata_vaccine_data$DateCreated) 
#View table summery after conversion 
summary(temp_rawdata_vaccine_data)

#View number of NA records in each column
lapply(temp_rawdata_vaccine_data,function(x) { length(which(is.na(x)))})

#Remove unwanted data columans
clean1_rawdata_vaccine_data <- temp_rawdata_vaccine_data[,!names(temp_rawdata_vaccine_data) %in% c("VaccineScheduleId","VaccineId","Latitude","Longitude","MOHArea","Type","UserModified","DateModified")]


#Visualize the missing data
missing_clean1_rawdata_vaccine_data <- aggr(clean1_rawdata_vaccine_data, col=c('navyblue','yellow'), 
                           numbers=TRUE, sortVars=TRUE, labels=names(clean1_rawdata_vaccine_data), 
                           cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"), oma = c(6,5,2,4))
print(missing_clean1_rawdata_vaccine_data)

#Remove dose not specified (Zero values and NA)
clean2_unique_rawdata_vaccine_data <- clean1_rawdata_vaccine_data %>% filter(!is.na(Dose))

#Visualize the missing data (NA values) distribution after removing NA value records from Dose
missing_clean2_rawdata_vaccine_data <- aggr(clean2_unique_rawdata_vaccine_data, col=c('darkgreen','yellow'), 
                                            numbers=TRUE, sortVars=TRUE, labels=names(clean2_unique_rawdata_vaccine_data), 
                                            cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"), oma = c(6,5,2,4))
print(missing_clean2_rawdata_vaccine_data)


VaccineNames<-sample(c(clean2_unique_rawdata_vaccine_data$VaccineName),20,replace=TRUE)
VaccineDose<-sample(c(clean2_unique_rawdata_vaccine_data$Dose),20,replace=TRUE)
df<-data.frame(VaccineNames,VaccineDose)
Table<-with(df,table(VaccineNames,VaccineDose))
barplot(Table,beside=TRUE,legend=TRUE)
barplot(Table, beside = TRUE, main = "Vaccine usage by doses", 
        legend=TRUE)

#Pie chart for gender
# Calculate the number of observations in each gender group
counts <- table(clean2_unique_rawdata_vaccine_data$Dose)

# Create a data frame with the counts and labels
pie_data <- data.frame(counts)
pie_data$labels <- rownames(pie_data)

# Create a pie chart
pie_chart <-
  ggplot(pie_data, aes(x = "", y = counts, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Vaccine dosage Distribution",
       fill = "Dose") +
  theme_void()

# Show the pie chart
pie_chart

#Get patient wise vaccine count in to a table
unique_patient_vaccine_data <- clean2_unique_rawdata_vaccine_data %>% group_by(PatientId) %>% 
  summarise(total_count=n(),.groups = 'drop')
unique_patient_vaccine_data

# Convert table in to a dataframe
patient_vaccine_data <- unique_patient_vaccine_data %>% as.data.frame()
patient_vaccine_data

#change total_count field name to no_of_doses
colnames(patient_vaccine_data)[colnames(patient_vaccine_data) == "total_count"] ="no_of_doses"

#Check dataframe record count
dim(patient_vaccine_data)
#Visualize outliers using boxplot
boxplot(patient_vaccine_data$no_of_doses, col="skyblue2", pch=20, main="Vaccine Dose distribution")

#View the quantile values
quantile(patient_vaccine_data$no_of_doses)

# Remove outliers from using 3rd Quantile
Q3 <- quantile(patient_vaccine_data$no_of_doses, probs = , na.rm = TRUE, 0.75)
IQR <- IQR(patient_vaccine_data$no_of_doses)
# Apply 1.5 IQR to the 3rd quantile
IQR_vaccine_data <- subset(patient_vaccine_data, patient_vaccine_data$no_of_doses < Q3 + 1.5*IQR)

#Check dataframe record count
dim(IQR_vaccine_data)
#Visualize outliers using boxplot
boxplot(IQR_vaccine_data$no_of_doses, col="skyblue2", pch=20, main="Vaccine Dose distribution")

#Normalize Data with Min-Max Scaling 
process <- preProcess(as.data.frame(IQR_vaccine_data), method=c("range"))
norm_scale_vaccine_data <- predict(process, as.data.frame(IQR_vaccine_data))
summary(norm_scale_vaccine_data)

#Change treated dataframe name 
treated_patient_vaccine_data <- IQR_vaccine_data
normalied_patient_vaccine_data <- norm_scale_vaccine_data

rm(temp_rawdata_vaccine_data)
rm(clean1_rawdata_vaccine_data)
rm(missing_clean1_rawdata_vaccine_data)
rm(clean2_unique_rawdata_patient_data)
rm(missing_clean2_rawdata_vaccine_data)
rm(unique_patient_vaccine_data)
rm(patient_vaccine_data)
rm(IQR_vaccine_data)
rm(norm_scale_vaccine_data)

#View treated Vaccine dataframe
View(treated_patient_vaccine_data)
#View treated and normalized Vaccine dataframe
View(normalied_patient_vaccine_data)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                             Merging Dataframes                              #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

summary(treated_patient_data)
summary(treated_patient_vaccine_data)

#Meriging datasets
merged_dataframe_vaccine_based = merge(x = clean2_unique_rawdata_vaccine_data, y = treated_patient_data, by = "PatientId", all.x = TRUE)
merged_dataframe = merge(x = treated_patient_data, y = treated_patient_vaccine_data, by = "PatientId", all.x = TRUE)
merged_normalized_dataframe = merge(x = normalied_patient_vaccine_data, y = treated_patient_data, by = "PatientId", all.x = TRUE)

summary(merged_dataframe_vaccine_based)
summary(merged_dataframe)
summary(merged_normalized_dataframe)

View(merged_dataframe_vaccine_based)
View(merged_dataframe)
View(merged_normalized_dataframe)

# Create a scatter plot
scatter_plot <- ggplot(merged_dataframe, aes(x = Age, y = no_of_doses , color = Gender, size = no_of_doses )) +
  geom_point() +
  labs(title = "Age, Gender, and Vaccine Dose Relationship",
       x = "Age",
       y = "Dose",
       color = "Gender",
       size = "Dose")

# Show the scatter plot
scatter_plot

# Group the data by age, gender, and Dose and count the number of records
df_summary <- merged_dataframe %>% group_by(Age, Gender, no_of_doses) %>% summarize(count = n())

# Create a bar chart
bar_chart <- ggplot(df_summary, aes(x = Age, y = count, fill = factor(no_of_doses))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Gender, ncol = 2) +
  labs(title = "Age, Gender, and Dose Distribution",
       x = "Age",
       y = "Count",
       fill = "Dose") +
  scale_fill_discrete(name = "Dose", labels = c("Dose 1", "Dose 2", "Dose 3", "Dose 4"))

# Show the bar chart
bar_chart

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#        Create training and test dataset using merged_dataframe              #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#Select require columns
merged_dataframe_ml <- merged_dataframe[c('no_of_doses','Gender','Age')]

#Visualize and see missing data distrubution in the dataframe
missing_values_in_merged_dataframe_ml <- aggr(merged_dataframe_ml, col=c('tomato3','deepskyblue'), 
                      numbers=TRUE, sortVars=TRUE, labels=names(merged_dataframe_ml), 
                      cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"), oma = c(6,5,2,4))
print(missing_values_in_merged_dataframe_ml)

#Remove dose not specified (Zero values and NA)
clean1_missing_values_in_merged_dataframe_ml <- merged_dataframe_ml %>% filter(!is.na(Gender))
clean1_missing_values_in_merged_dataframe_ml <- clean1_missing_values_in_merged_dataframe_ml %>% filter(!is.na(Age))

#Visualize and see missing data distrubution in the dataframe
missing_clean1_missing_values_in_merged_dataframe_ml <- aggr(clean1_missing_values_in_merged_dataframe_ml, col=c('orange','deepskyblue'), 
                                     numbers=TRUE, sortVars=TRUE, labels=names(clean1_missing_values_in_merged_dataframe_ml), 
                                     cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"), oma = c(6,5,2,4))
print(missing_clean1_missing_values_in_merged_dataframe_ml)

#Create sample dataset for training and testing - select randomly 10% of the total records
sample_data_n <- sample_n(clean1_missing_values_in_merged_dataframe_ml, (nrow(clean1_missing_values_in_merged_dataframe_ml)*0.1))
summary(sample_data_n)

#Visualize the dataset
plot(sample_data_n)

#Get total sample size
total_records = round(nrow(sample_data_n))
#Mark 80% from total sample
max_training = round(total_records*0.8)

#Create training and test data values
#80% from the sample
vaccine_data_train <- sample_data_n[1:max_training, ]
#20% from the sample
vaccine_data_test <- sample_data_n[(max_training+1):total_records, ]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#       Create training and test dataset merged_normalized_dataframe          #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#Select require columns
merged_normalized_dataframe_ml <- merged_normalized_dataframe[c('no_of_doses','gender_value','Age')]

#Visualize and see missing data distrubution in the dataframe
missing_values_in_normalized <- aggr(merged_normalized_dataframe_ml, col=c('tomato3','deepskyblue'), 
                                     numbers=TRUE, sortVars=TRUE, labels=names(merged_normalized_dataframe_ml), 
                                     cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"), oma = c(6,5,2,4))
print(missing_values_in_normalized)

#Remove dose not specified (Zero values and NA)
clean1_missing_values_in_normalized <- merged_normalized_dataframe_ml %>% filter(!is.na(gender_value))
clean1_missing_values_in_normalized <- clean1_missing_values_in_normalized %>% filter(!is.na(Age))

#Visualize and see missing data distrubution in the dataframe
missing_clean1_missing_values_in_normalized <- aggr(clean1_missing_values_in_normalized, col=c('orange','deepskyblue'), 
                                                    numbers=TRUE, sortVars=TRUE, labels=names(clean1_missing_values_in_normalized), 
                                                    cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"), oma = c(6,5,2,4))
print(missing_clean1_missing_values_in_normalized)

#Create sample dataset for training and testing - select randomly 10% of the total records
sample_data_n <- sample_n(clean1_missing_values_in_normalized, (nrow(clean1_missing_values_in_normalized)*0.1))
summary(sample_data_n)

#Visualize the dataset
plot(sample_data_n)

#Get total sample size
total_records = round(nrow(sample_data_n))
#Mark 80% from total sample
max_training = round(total_records*0.8)

#Create training and test data values
#80% from the sample
vaccine_data_train <- sample_data_n[1:max_training, ]
#20% from the sample
vaccine_data_test <- sample_data_n[(max_training+1):total_records, ]

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                        Decision Tree Algorithm                              #
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

#Import Rpart library to apply decision tree algorithem
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#View no of doses details
prop.table(table(vaccine_data_train$no_of_doses))

#Apply Rpart decision tree function
fit <- rpart(no_of_doses~., data = vaccine_data_train, method = 'class')

#Plot the output
rpart.plot(fit, extra = "auto")

#Make a prediction
predict_unseen <-predict(fit, vaccine_data_test, type = 'class')

#Test it with no_of_doses
table_mat <- table(vaccine_data_test$no_of_doses, predict_unseen)
#View the model prediction on no_of_doses
table_mat

#Measure performance
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
#Accuracy of the test set
print(paste('Accuracy for test', accuracy_Test))



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                 Support Vector Machines Algorithm                           #
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

#install.packages("psych")
library(psych)
pairs.panels(vaccine_data_train)

#Training a model on the data
ins_model <- lm(no_of_doses ~ ., data = vaccine_data_train) # this is equivalent to above
#See the estimated beta coefficients
ins_model

#Evaluating model performance
#See more detail about the estimated beta coefficients
summary(ins_model)

#Improving model performance
#Add a higher-order "age" term
vaccine_data_train$Age2 <- vaccine_data_train$Age^2

#Create final model
ins_model2 <- lm(no_of_doses ~ Age + Age2+gender_value , data = vaccine_data_train)

summary(ins_model2)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#                        Neural Network Algorithm                             #
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

#install.packages("neuralnet")
library(neuralnet)

# simple ANN with only a single hidden neuron
Doses_model <- neuralnet(formula = no_of_doses ~ Age+gender_value, data = vaccine_data_train)
# visualize the network topology
plot(Doses_model)

#Evaluating model performance
#Obtain model results
model_results <- neuralnet::compute(Doses_model, vaccine_data_test)
#Obtain predicted strength values
predicted_strength <- model_results$net.result
#Examine the correlation between predicted and actual values
cor(predicted_strength, vaccine_data_test$no_of_doses)

#Improving model performance ----
#More complex neural network topology with 5 hidden neurons
Doses_model2 <- neuralnet(formula = no_of_doses ~ Age+gender_value, data = vaccine_data_train, hidden = 5)
#Plot the network
plot(Doses_model2)

#Evaluate the results as we did before
model_results2 <- compute(Doses_model2, vaccine_data_test)
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, vaccine_data_test$no_of_doses)


###########################################################################################
###########################################################################################







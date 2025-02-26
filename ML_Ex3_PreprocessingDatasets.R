##### load libraries and set wd #####
library(ggplot2)
library(dplyr)
library(caTools)
library(corrplot)
library(vcd)
library(reshape2)
library(tidyverse)

setwd("C:/Users/ameli/OneDrive/Studium/TU Wien/WS2024/ML/Exercise 3")


table(KDD$income)
KDD$income[which(KDD$income == "-50000")] = "<=50K"
KDD$income[which(KDD$income == " 50000+.")] <- ">50K"
ggplot(KDD, aes(income)) + geom_bar() +
  geom_bar(fill = "skyblue", color = "black") +
  ggtitle("Barplot of Income") +
  xlab("Incomce") +
  ylab("Frequency") +
  theme_minimal()

table(PID$Outcome)
PID$Outcome[which(PID$Outcome == 1)] = "Diabetes"
PID$Outcome[which(PID$Outcome == 0)] <- "No Diabetes"
ggplot(PID, aes(Outcome)) + geom_bar() +
  geom_bar(fill = "skyblue", color = "black") +
  ggtitle("Barplot of Diabetes incidence") +
  xlab("Incidence") +
  ylab("Frequency") +
  theme_minimal()

table(CCD$default_payment_next_month)
ggplot(CCD, aes(default_payment_next_month)) + geom_bar() +
  geom_bar(fill = "skyblue", color = "black") +
  ggtitle("Barplot of Payment next month") +
  xlab("Incidence") +
  ylab("Frequency") +
  theme_minimal()


##### functions #####
### Summary for the whole dataset ###
summary_data <- function(dataset, target) {
  cat("Dimensions: ", dim(dataset), "\n")
  cat("Datatypes: ", sapply(dataset, class), "\n")
  cat("Missing values: ", apply(dataset, 2, function(x) {sum(is.na(x))}), "\n")
  cat("Number of rows with missing values: ", sum(apply(dataset, 2, function(x) {sum(is.na(x))}) > 0), "\n")
  cat("Missing values total: ", sum(is.na(dataset)), "\n")
  
  if (sum(is.na(dataset[[target]])) > 0) {
    dataset <- dataset %>% drop_na(all_of(target))
    cat("Target had missing values. These rows were removed.\n")
    cat("New dimensions: ", dim(dataset), "\n")
  }
  
  cat("Target table:\n")
  print(table(dataset[[target]], useNA = "ifany"))
  print(prop.table(table(dataset[[target]], useNA = "ifany")))

  }

### preprocessing of the training dataset ###
preprocessing <- function(dataset, target, numeric_vars, categorical_vars, name) {
  cat("Target table:\n")
  print(table(dataset[[target]], useNA = "ifany"))
  print(prop.table(table(dataset[[target]], useNA = "ifany")))
  
  # Barplot for target variable #
  ggplot(dataset, aes(x = .data[[target]])) +
    geom_bar(fill = "skyblue", color = "black") +
    ggtitle("Barplot of Target Variable") +
    xlab("Target Categories") +
    ylab("Frequency") +
    theme_minimal()
  

  
  # Barplot for categorical variables #
  for (var in categorical_vars) {
    p <- ggplot(dataset, aes(x = .data[[var]])) +  # Use `!!sym()` for tidy evaluation
      geom_bar(fill = "skyblue", color = "black") +
      ggtitle(paste("Bar Plot of", var)) +
      xlab(var) +
      ylab("Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate axis labels if needed
    ggsave(paste0("plots/", name, var, "_barplot.png"), plot = p)
  }
  
  # Histogram for numerical variables #
  for (var in numeric_vars) {
    p <- ggplot(dataset, aes(x = .data[[var]])) +  # Use `!!sym()` for tidy evaluation
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +  # You can adjust the `binwidth`
      ggtitle(paste("Histogram of", var)) +
      xlab(var) +
      ylab("Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate axis labels if needed
    ggsave(paste0("plots/", name, var, "_histogram.png"), plot = p)
  }
  
  # Correlation plot for numerical variables #
  numeric_data <- dataset[, numeric_vars]
  cor_matrix <- cor(numeric_data, use = "complete.obs")  # "complete.obs" ignores missing values
  corrplot(cor_matrix, method = "square", type = "upper", 
           tl.col = "black", tl.cex = 0.8, addCoef.col = "black")
  
  # Cramers V for all the variables #
  cramers_v_matrix <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars))
  rownames(cramers_v_matrix) <- categorical_vars; colnames(cramers_v_matrix) <- categorical_vars
  for (i in 1:(length(categorical_vars) - 1)) {
    for (j in (i + 1):length(categorical_vars)) {
      var1 <- categorical_vars[i]
      var2 <- categorical_vars[j]
      contingency_table <- table(dataset[[var1]], dataset[[var2]])
      cramers_v <- assocstats(contingency_table)$cramer
      cramers_v_matrix[i, j] <- cramers_v
      cramers_v_matrix[j, i] <- cramers_v
    }
  }
  print(cramers_v_matrix)
  # cramers_v_long <- melt(cramers_v_matrix, na.rm = TRUE)
  # ggplot(cramers_v_long, aes(Var1, Var2, fill = value)) +
  #  geom_tile() +
  #  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5) +
  #  theme_minimal() +
  #  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #  labs(title = "Cramér's V Correlation Matrix", x = "Variables", y = "Variables")
}



##### Load datasets #####
setwd("C:/Users/ameli/OneDrive/Studium/TU Wien/WS2024/ML/Exercise 3")
na_values <- c("NA", "", "NULL", "unknown", "Unknown", "na", "?")

DIA <- read.csv("Datasets/diabetes.csv", na.strings = na_values, sep = ",")
stroke <- read.csv("Datasets/stroke.csv", na.strings = na_values, sep = ",")
AIDS <- read.csv("Datasets/AIDS_Classification.csv", na.strings = na_values, sep = ",")

set.seed(42) 

PID_split <- sample.split(DIA, SplitRatio = 0.8)
PID_train <- DIA[PID_split, ]
PID_test <- DIA[!PID_split, ]

PID_train$BloodPressure[which(PID_train$BloodPressure == 0)] <- mean(PID_train$BloodPressur[which(PID_train$BloodPressure != 0)])
PID_train$Glucose[which(PID_train$Glucose == 0)] <- mean(PID_train$Glucose[which(PID_train$Glucose != 0)])
PID_train$BMI[which(PID_train$BMI == 0)] <- mean(PID_train$BMI[which(PID_train$BMI != 0)])
dim(PID_train)
PID_test$BloodPressure[which(PID_test$BloodPressure == 0)] <- mean(PID_train$BloodPressur[which(PID_train$BloodPressure != 0)])
PID_test$Glucose[which(PID_test$Glucose == 0)] <- mean(PID_train$Glucose[which(PID_train$Glucose != 0)])
PID_test$BMI[which(PID_test$BMI == 0)] <- mean(PID_train$BMI[which(PID_train$BMI != 0)])

hist(PID_train$BloodPressure)
write.csv(PID_train,"./Datasets/PID_train.csv", row.names = FALSE)
write.csv(PID_test,"./Datasets/PID_test.csv", row.names = FALSE)
stroke$stroke
AIDS$in

CCD <- read.csv("Datasets/defaultOfCreditCardClient.csv", na.strings = na_values, sep = ";")
CMC <- read.csv("Datasets/contraceptiveMethodChoice.csv", na.strings = na_values, sep = ";")
KDD <- read.csv("Datasets/censusIncomeKDD.csv", na.strings = na_values, sep = ";")
BMT <- read.csv("Datasets/boneMarrowTransplant.csv", na.strings = na_values, sep = ";")

##### Split datasets into train and test sets #####
set.seed(42) 

CCD <- CCD[,-1]
CCD_split <- sample.split(CCD, SplitRatio = 0.8)
CCD_train <- CCD[CCD_split, ]
CCD_test <- CCD[!CCD_split, ]

CMC_split <- sample.split(CMC, SplitRatio = 0.8)
CMC_train <- CMC[CMC_split, ]
CMC_test <- CMC[!CMC_split, ]

KDD_preprocessed = KDD[, apply(KDD, 2, function(x) sum(is.na(x)))/nrow(KDD) < 0.34]
KDD_preprocessed <- KDD_preprocessed[which(KDD_preprocessed$AAGE > 18), ]
KDD_preprocessed$education <- sub("\\(.*", "", KDD_preprocessed$AHGA)
KDD_preprocessed <- KDD_preprocessed[, !colnames(KDD_preprocessed) %in% c("AHGA", "year", "PENATVTY", "AMJOCC", "AMJIND", "GRINREG", "GRINST", "AHSCOL", "AUNMEM", "AUNTYPE", "HHDFMX", "MARSUPWRT", "PEFNTVTY")]
KDD <- KDD_preprocessed
colnames(KDD) <- c("age", "class_of_worker", "industry_code", "occup_code", "marital_stat", "race", "hispanic", "sex",
                   "full_or_part_time", "cap_gain", "cap_loss", "dividends", "filing_stat", "household_summary",
                   "live_in_house_1_year_ago", "num_persons_worked", "fam_members_under18", "birth_country_mother",
                   "citizenship", "own_business", "questionnaire", "vet_benefits", "weeks_per_year", "wage_per_hour", "income", "education")
KDD_split <- sample.split(KDD, SplitRatio = 0.8)
KDD_train <- KDD[KDD_split, ]
KDD_test <- KDD[!KDD_split, ]

BMT_preprocessed = BMT[, apply(BMT, 2, function(x) sum(is.na(x)))/nrow(BMT) < 0.34]
BMT_preprocessed = BMT_preprocessed[-188, ]
BMT_preprocessed <- BMT_preprocessed[, !colnames(BMT_preprocessed) %in% c("survival_time", "Recipientage10")]
BMT <- BMT_preprocessed
BMT_split <- sample.split(BMT, SplitRatio = 0.8)
BMT_train <- BMT[BMT_split, ]
BMT_test <- BMT[!BMT_split, ]

##### Preprosessing #####
colnames()
numeric_vars = c("LIMIT_BAL",
                 "BILL_AMT1", "BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5", "BILL_AMT6",
                 "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6")
categorical_vars = setdiff(setdiff(colnames(CCD), numeric_vars), "default_payment_next_month")
print("Dataset: CCD")
summary_data(CCD, "default_payment_next_month")
preprocessing(CCD_train, "default_payment_next_month", numeric_vars, categorical_vars, "CCD")

colnames(CMC)
numeric_vars = c("wife_age", "nr_children")
categorical_vars = setdiff(setdiff(colnames(CMC), numeric_vars), "contraceptive_method")
print("Dataset: CMC")
summary_data(CMC, "contraceptive_method")
preprocessing(CMC_train, "contraceptive_method", numeric_vars, categorical_vars, "CMC")

colnames(KDD)
numeric_vars = c("age", "cap_gain", "cap_loss", "dividends", "wage_per_hour", "weeks_per_year")
categorical_vars = setdiff(setdiff(colnames(KDD), numerical_vars), "income")
print("Dataset: KDD")
summary_data(KDD, "income")
preprocessing(KDD_train, "income", numeric_vars, categorical_vars, "KDD")
KDD_train$birth_country_mother = ifelse(
  is.na(KDD_train$birth_country_mother),  # Check for missing values
  ifelse(
    KDD_train$citizenship == "Native- Born in the United States", 
    "United-States", 
    "Puerto-Rico"
  ), 
  KDD_train$birth_country_mother  # Keep the original value if it's not missing
)

colnames(BMT)
numeric_vars = c("DonorAge", "CD34kgx10d6", "CD3dCD34", "CD3dkgx10d8", "Rbodymass", "ANCrecovery", "PLTrecovery", "time_to_aGvHD_III_IV")
categorical_vars = setdiff(setdiff(colnames(BMT), numeric_vars), "survival_status")
print("Dataset: BMT")
summary_data(BMT, "survival_status")
preprocessing(BMT_train, "survival_status", numeric_vars, categorical_vars, "BMT")
impute_missing_values <- function(dataset, numerical_vars, categorical_vars) {
  for (var in numerical_vars) {
    dataset[[var]][is.na(dataset[[var]])] = mean(dataset[[var]], na.rm = TRUE)
  }
  for (var in categorical_vars) {
    mode_value <- names(sort(table(dataset[[var]]), decreasing = TRUE))[1]  # Get the most frequent value
    dataset[[var]][is.na(dataset[[var]])] <- mode_value
  }
  return(dataset)
}
BMT_train <- impute_missing_values(BMT_train, numeric_vars, categorical_vars)

apply(BMT, 2, function(x) sum(is.na(x)))


##### Save new datasets ###
write.csv(CCD_train,"./Datasets/CCD_train.csv", row.names = FALSE)
write.csv(CCD_test,"./Datasets/CCD_test.csv", row.names = FALSE)
write.csv(CMC_train,"./Datasets/CMC_train.csv", row.names = FALSE)
write.csv(CMC_test,"./Datasets/CMC_test.csv", row.names = FALSE)
write.csv(KDD_train,"./Datasets/KDD_train.csv", row.names = FALSE)
write.csv(KDD_test,"./Datasets/KDD_test.csv", row.names = FALSE)
write.csv(BMT_train,"./Datasets/BMT_train.csv", row.names = FALSE)
write.csv(BMT_test,"./Datasets/BMT_test.csv", row.names = FALSE)

table(KDD$income)
############################################################################################
head(KDD)
summary(KDD)
KDD$ve
KDD_preprocessed$year

table(KDD_preprocessed$target)
dim(KDD)
dim(KDD_preprocessed)
KDD$pea
sum(apply(KDD, 1, function(x) sum(is.na(x)))>10)
colnames(KDD$income)

print(table(BMT$survival_status, useNA = "ifany"))
dim(BMT)

contingency_table <- table(KDD$PRCITSHP, KDD$PEMNTVTY)
cramers_v <- assocstats(contingency_table)$cramer
cramers_v

contingency_table <- table(KDD$birth_country_mother, KDD$citizenship)
cramers_v <- assocstats(contingency_table)$cramer
cramers_v

summary(KDD_preprocessed)

sum(KDD_preprocessed$AUNMEM == 0)/nrow(KDD_preprocessed)

prop.table(table(KDD_preprocessed$GRINST))
K <- KDD_preprocessed[which(KDD_preprocessed$AHRSPAY == 0), ]
table(K$target)

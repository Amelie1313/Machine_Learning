library(gridExtra)
library(ggplot2)
library(sdcMicro)

setwd("C:/Users/ameli/OneDrive/Studium/TU Wien/WS2024/ML/Exercise 3")

CCD <- read.csv("Datasets/CCD_train.csv", sep = ",")
KDD <- read.csv("Datasets/KDD_train.csv", sep = ",")
PID <- read.csv("Datasets/PID_train.csv", sep = ",")

CCD_test <- read.csv("Datasets/CCD_test.csv", sep = ",")
KDD_test <- read.csv("Datasets/KDD_test.csv", sep = ",")
PID_test <- read.csv("Datasets/PID_test.csv", sep = ",")


############ Pima Indians Diabetes ##############
##### Overview #####
# We will use the variables Age, BloodPressure, Insulin
p1<- ggplot(PID, aes(Age)) + geom_histogram(binwidth = 5)
p2<- ggplot(PID, aes(BloodPressure)) + geom_histogram(binwidth = 5)
p3<- ggplot(PID, aes(Insulin)) + geom_histogram(binwidth = 40)
grid.arrange(p1, p2, p3, ncol=3)

### We will try to use k=2 - k=20 ###
min = 2
max = 20

global_risk_onedims_3 <- rep(NA, length(min:max)); global_risk_onedims_2 <- rep(NA, length(min:max))
global_risk_mdav_3 <- rep(NA, length(min:max)); global_risk_mdav_2 <- rep(NA, length(min:max))
global_risk_pca_3 <- rep(NA, length(min:max)); global_risk_pca_2 <- rep(NA, length(min:max))
utility_risk_onedims_3 <- rep(NA, length(min:max)); utility_risk_onedims_2 <- rep(NA, length(min:max))
utility_risk_mdav_3 <- rep(NA, length(min:max)); utility_risk_mdav_2 <- rep(NA, length(min:max))
utility_risk_pca_3 <- rep(NA, length(min:max)); utility_risk_pca_2 <- rep(NA, length(min:max))

for(k in 2:20) {
  m_onedims_3 = microaggregation(PID[,c("Age", "BloodPressure", "Insulin")], method="onedims", aggr=k)
  m_mdav_3 = microaggregation(PID[,c("Age", "BloodPressure", "Insulin")], method="mdav", aggr=k)
  m_pca_3 = microaggregation(PID[,c("Age", "BloodPressure", "Insulin")], method="pca", aggr=k)
  global_risk_onedims_3[k-1] <- dRisk(obj=PID[,c("Age", "BloodPressure", "Insulin")], xm=m_onedims_3$mx)
  global_risk_mdav_3[k-1] <- dRisk(obj=PID[,c("Age", "BloodPressure", "Insulin")], xm=m_mdav_3$mx)
  global_risk_pca_3[k-1] <- dRisk(obj=PID[,c("Age", "BloodPressure", "Insulin")], xm=m_pca_3$mx)
  utility_risk_onedims_3[k-1] <- dUtility(obj=PID[,c("Age", "BloodPressure", "Insulin")], xm=m_onedims_3$mx)
  utility_risk_mdav_3[k-1] <- dUtility(obj=PID[,c("Age", "BloodPressure", "Insulin")], xm=m_mdav_3$mx)
  utility_risk_pca_3[k-1] <- dUtility(obj=PID[,c("Age", "BloodPressure", "Insulin")], xm=m_pca_3$mx)
  
  m_onedims_2 = microaggregation(PID[,c("Age", "BloodPressure")], method="onedims", aggr=k)
  m_mdav_2 = microaggregation(PID[,c("Age", "BloodPressure")], method="mdav", aggr=k)
  m_pca_2 = microaggregation(PID[,c("Age", "BloodPressure")], method="pca", aggr=k)
  global_risk_onedims_2[k-1] <- dRisk(obj=PID[,c("Age", "BloodPressure")], xm=m_onedims_2$mx)
  global_risk_mdav_2[k-1] <- dRisk(obj=PID[,c("Age", "BloodPressure")], xm=m_mdav_2$mx)
  global_risk_pca_2[k-1] <- dRisk(obj=PID[,c("Age", "BloodPressure")], xm=m_pca_2$mx)
  utility_risk_onedims_2[k-1] <- dUtility(obj=PID[,c("Age", "BloodPressure")], xm=m_onedims_2$mx)
  utility_risk_mdav_2[k-1] <- dUtility(obj=PID[,c("Age", "BloodPressure")], xm=m_mdav_2$mx)
  utility_risk_pca_2[k-1] <- dUtility(obj=PID[,c("Age", "BloodPressure")], xm=m_pca_2$mx)
}
risk_pima <- data.frame(
  x = rep(min:max, 6),
  y = c(global_risk_onedims_3, global_risk_mdav_3, global_risk_pca_3, global_risk_onedims_2, global_risk_mdav_2, global_risk_pca_2),
  group = rep(c("global_risk_onedims_3", "global_risk_mdav_3", "global_risk_pca_3", "global_risk_onedims_2", "global_risk_mdav_2", "global_risk_pca_2"), each = (max - min + 1))
)
ggplot(risk_pima, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line() +
  scale_color_manual(values = c("global_risk_onedims_3" = "darkorchid", "global_risk_mdav_3" = "darkorchid", "global_risk_pca_3" = "darkorchid", "global_risk_onedims_2" = "darkorange1", "global_risk_mdav_2" = "darkorange1", "global_risk_pca_2" = "darkorange1")) +
  scale_linetype_manual(values = c("global_risk_onedims_3" = "solid", "global_risk_mdav_3" = "longdash", "global_risk_pca_3" = "dotted", "global_risk_onedims_2" = "solid", "global_risk_mdav_2" = "dashed", "global_risk_pca_2" = "dotted")) +
  theme_minimal() +
  labs(x = "k", y = "Global Risk") +
  theme(legend.position = "bottom")
utility_pima <- data.frame(
  x = rep(min:max, 6),
  y = c(utility_risk_onedims_3, utility_risk_mdav_3, utility_risk_pca_3, utility_risk_onedims_2, utility_risk_mdav_2, utility_risk_pca_2),
  group = rep(c("utility_risk_onedims_3", "utility_risk_mdav_3", "utility_risk_pca_3", "utility_risk_onedims_2", "utility_risk_mdav_2", "utility_risk_pca_2"), each = (max - min + 1))
)
ggplot(utility_pima, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line() +
  scale_color_manual(values = c("utility_risk_onedims_3" = "darkorchid", "utility_risk_mdav_3" = "darkorchid", "utility_risk_pca_3" = "darkorchid", "utility_risk_onedims_2" = "darkorange1", "utility_risk_mdav_2" = "darkorange1", "utility_risk_pca_2" = "darkorange1")) +
  scale_linetype_manual(values = c("utility_risk_onedims_3" = "solid", "utility_risk_mdav_3" = "longdash", "utility_risk_pca_3" = "dotted", "utility_risk_onedims_2" = "solid", "utility_risk_mdav_2" = "dashed", "utility_risk_pca_2" = "dotted")) +
  theme_minimal() +
  labs(x = "k", y = "Global Utility") +
  theme(legend.position = "bottom")

PID_copy <- PID
for(k in 2:20) {
  m_onedims_3 = microaggregation(PID[,c("Age", "BloodPressure", "Insulin")], method="onedims", aggr=k)
  PID_copy$Age <- m_onedims_3$mx$Age
  PID_copy$BloodPressure <- m_onedims_3$mx$BloodPressure
  PID_copy$Insulin <- m_onedims_3$mx$Insulin
  global_risk_onedims_3[k-1] <- dRisk(obj=PID, xm=PID_copy)
  utility_risk_onedims_3[k-1] <- dUtility(obj=PID, xm=PID_copy)
  
  m_mdav_3 = microaggregation(PID[,c("Age", "BloodPressure", "Insulin")], method="mdav", aggr=k)
  PID_copy$Age <- m_mdav_3$mx$Age
  PID_copy$BloodPressure <- m_mdav_3$mx$BloodPressure
  PID_copy$Insulin <- m_mdav_3$mx$Insulin
  global_risk_mdav_3[k-1] <- dRisk(obj=PID, xm=PID_copy)
  utility_risk_mdav_3[k-1] <- dUtility(obj=PID, xm=PID_copy)

  m_pca_3 = microaggregation(PID[,c("Age", "BloodPressure", "Insulin")], method="pca", aggr=k)
  PID_copy$Age <- m_pca_3$mx$Age
  PID_copy$BloodPressure <- m_pca_3$mx$BloodPressure
  PID_copy$Insulin <- m_pca_3$mx$Insulin
  global_risk_pca_3[k-1] <- dRisk(obj=PID, xm=PID_copy)
  utility_risk_pca_3[k-1] <- dUtility(obj=PID, xm=PID_copy)

  
  m_onedims_2 = microaggregation(PID[,c("Age", "BloodPressure", "Insulin")], method="onedims", aggr=k)
  PID_copy$Age <- m_onedims_2$mx$Age
  PID_copy$BloodPressure <- m_onedims_2$mx$BloodPressure
  global_risk_onedims_2[k-1] <- dRisk(obj=PID, xm=PID_copy)
  utility_risk_onedims_2[k-1] <- dUtility(obj=PID, xm=PID_copy)

  m_mdav_2 = microaggregation(PID[,c("Age", "BloodPressure")], method="mdav", aggr=k)
  PID_copy$Age <- m_mdav_2$mx$Age
  PID_copy$BloodPressure <- m_mdav_2$mx$BloodPressure
  global_risk_mdav_2[k-1] <- dRisk(obj=PID, xm=PID_copy)
  utility_risk_mdav_2[k-1] <- dUtility(obj=PID, xm=PID_copy)

  m_pca_2 = microaggregation(PID[,c("Age", "BloodPressure")], method="pca", aggr=k)
  PID_copy$Age <- m_pca_2$mx$Age
  PID_copy$BloodPressure <- m_pca_2$mx$BloodPressure
  global_risk_pca_2[k-1] <- dRisk(obj=PID, xm=PID_copy)
  utility_risk_pca_2[k-1] <- dUtility(obj=PID, xm=PID_copy)
}
risk_pima <- data.frame(
  x = rep(min:max, 6),
  y = c(global_risk_onedims_3, global_risk_mdav_3, global_risk_pca_3, global_risk_onedims_2, global_risk_mdav_2, global_risk_pca_2),
  group = rep(c("global_risk_onedims_3", "global_risk_mdav_3", "global_risk_pca_3", "global_risk_onedims_2", "global_risk_mdav_2", "global_risk_pca_2"), each = (max - min + 1))
)
ggplot(risk_pima, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line() +
  scale_color_manual(values = c("global_risk_onedims_3" = "darkorchid", "global_risk_mdav_3" = "darkorchid", "global_risk_pca_3" = "darkorchid", "global_risk_onedims_2" = "darkorange1", "global_risk_mdav_2" = "darkorange1", "global_risk_pca_2" = "darkorange1")) +
  scale_linetype_manual(values = c("global_risk_onedims_3" = "solid", "global_risk_mdav_3" = "longdash", "global_risk_pca_3" = "dotted", "global_risk_onedims_2" = "solid", "global_risk_mdav_2" = "dashed", "global_risk_pca_2" = "dotted")) +
  theme_minimal() +
  labs(x = "k", y = "Global Risk") +
  theme(legend.position = "bottom")
utility_pima <- data.frame(
  x = rep(min:max, 6),
  y = c(utility_risk_onedims_3, utility_risk_mdav_3, utility_risk_pca_3, utility_risk_onedims_2, utility_risk_mdav_2, utility_risk_pca_2),
  group = rep(c("utility_risk_onedims_3", "utility_risk_mdav_3", "utility_risk_pca_3", "utility_risk_onedims_2", "utility_risk_mdav_2", "utility_risk_pca_2"), each = (max - min + 1))
)
ggplot(utility_pima, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line() +
  scale_color_manual(values = c("utility_risk_onedims_3" = "darkorchid", "utility_risk_mdav_3" = "darkorchid", "utility_risk_pca_3" = "darkorchid", "utility_risk_onedims_2" = "darkorange1", "utility_risk_mdav_2" = "darkorange1", "utility_risk_pca_2" = "darkorange1")) +
  scale_linetype_manual(values = c("utility_risk_onedims_3" = "solid", "utility_risk_mdav_3" = "longdash", "utility_risk_pca_3" = "dotted", "utility_risk_onedims_2" = "solid", "utility_risk_mdav_2" = "dashed", "utility_risk_pca_2" = "dotted")) +
  theme_minimal() +
  labs(x = "k", y = "Global Risk") +
  theme(legend.position = "bottom")

##### We will start with 3 variables ######
PID_onedims <- PID
m_onedims = microaggregation(PID_onedims[,c("Age", "BloodPressure", "Insulin")], method="onedims", aggr=5, measure = "mean")
PID_onedims$Age <- m_onedims$mx$Age
PID_onedims$BloodPressure <- m_onedims$mx$BloodPressure
PID_onedims$Insulin <- m_onedims$mx$Insulin
write.csv(PID_onedims,"./modifiedDatasets/PID_onedims.csv", row.names = FALSE)
summary(PID[, c("Age", "BloodPressure", "Insulin")])
summary(PID_onedims[, c("Age", "BloodPressure", "Insulin")])

PID_mdav <- PID
m_mdav = microaggregation(PID[,c("Age", "BloodPressure", "Insulin")], method="mdav", aggr=5, measure = "mean")
PID_mdav$Age <- m_mdav$mx$Age
PID_mdav$BloodPressure <- m_mdav$mx$BloodPressure
PID_mdav$Insulin <- m_mdav$mx$Insulin
write.csv(PID_mdav,"./modifiedDatasets/PID_mdav.csv", row.names = FALSE)
summary(PID[, c("Age", "BloodPressure", "Insulin")])
summary(PID_mdav[, c("Age", "BloodPressure", "Insulin")])

PID_pca <- PID
m_pca = microaggregation(PID[,c("Age", "BloodPressure", "Insulin")], method="pca", aggr=5, measure = "mean")
PID_pca$Age <- m_pca$mx$Age
PID_pca$BloodPressure <- m_pca$mx$BloodPressure
PID_pca$Insulin <- m_pca$mx$Insulin
write.csv(PID_pca,"./modifiedDatasets/PID_pca.csv", row.names = FALSE)
summary(PID[, c("Age", "BloodPressure", "Insulin")])
summary(PID_pca[, c("Age", "BloodPressure", "Insulin")])


PID_onedims_test <- PID_test
m_onedims_test = microaggregation(PID_onedims_test[,c("Age", "BloodPressure", "Insulin")], method="onedims", aggr=5, measure = "mean")
PID_onedims_test$Age <- m_onedims_test$mx$Age
PID_onedims_test$BloodPressure <- m_onedims_test$mx$BloodPressure
PID_onedims_test$Insulin <- m_onedims_test$mx$Insulin
write.csv(PID_onedims_test,"./modifiedDatasets/PID_onedims_test.csv", row.names = FALSE)
summary(PID_test[, c("Age", "BloodPressure", "Insulin")])
summary(PID_onedims_test[, c("Age", "BloodPressure", "Insulin")])

PID_mdav_test <- PID_test
m_mdav_test = microaggregation(PID_test[,c("Age", "BloodPressure", "Insulin")], method="mdav", aggr=5, measure = "mean")
PID_mdav_test$Age <- m_mdav_test$mx$Age
PID_mdav_test$BloodPressure <- m_mdav_test$mx$BloodPressure
PID_mdav_test$Insulin <- m_mdav_test$mx$Insulin
write.csv(PID_mdav_test,"./modifiedDatasets/PID_mdav_test.csv", row.names = FALSE)
summary(PID_test[, c("Age", "BloodPressure", "Insulin")])
summary(PID_mdav_test[, c("Age", "BloodPressure", "Insulin")])

PID_pca_test <- PID_test
m_pca_test = microaggregation(PID_test[,c("Age", "BloodPressure", "Insulin")], method="pca", aggr=5, measure = "mean")
PID_pca_test$Age <- m_pca_test$mx$Age
PID_pca_test$BloodPressure <- m_pca_test$mx$BloodPressure
PID_pca_test$Insulin <- m_pca_test$mx$Insulin
write.csv(PID_pca_test,"./modifiedDatasets/PID_pca_test.csv", row.names = FALSE)
summary(PID_test[, c("Age", "BloodPressure", "Insulin")])
summary(PID_pca_test[, c("Age", "BloodPressure", "Insulin")])


############ Credit Card Default ##############
##### Overview #####
# We will use the variables LIMIT_BAL, BILL_AMT1, PAY_AMT1, EDUCATION
p1 <- ggplot(CCD, aes(LIMIT_BAL)) + geom_histogram()
p2 <- ggplot(CCD, aes(BILL_AMT1)) + geom_histogram()
p3 <- ggplot(CCD[-which(CCD$PAY_AMT1 == max(CCD$PAY_AMT1)), ], aes(PAY_AMT1)) + geom_histogram(binwidth = 1000)
p4 <- ggplot(CCD, aes(EDUCATION)) + geom_bar()
grid.arrange(p1, p2, p3, p4, ncol=2)

### We will try to use k=2 - k=20 ###
min = 2
max = 20

global_risk_onedims_4 <- rep(NA, length(min:max)); global_risk_onedims_5 <- rep(NA, length(min:max))
global_risk_mdav_4 <- rep(NA, length(min:max)); global_risk_mdav_5 <- rep(NA, length(min:max))
global_risk_pca_4 <- rep(NA, length(min:max)); global_risk_pca_5 <- rep(NA, length(min:max))
utility_risk_onedims_4 <- rep(NA, length(min:max)); utility_risk_onedims_5 <- rep(NA, length(min:max))
utility_risk_mdav_4 <- rep(NA, length(min:max)); utility_risk_mdav_5 <- rep(NA, length(min:max))
utility_risk_pca_4 <- rep(NA, length(min:max)); utility_risk_pca_5 <- rep(NA, length(min:max))

for(k in 2:20) {
  m_onedims_4 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="onedims", aggr=k)
  m_mdav_4 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="mdav", aggr=k)
  m_pca_4 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="pca", aggr=k)
  global_risk_onedims_4[k-1] <- dRisk(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], xm=m_onedims_4$mx)
  global_risk_mdav_4[k-1] <- dRisk(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], xm=m_mdav_4$mx)
  global_risk_pca_4[k-1] <- dRisk(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], xm=m_pca_4$mx)
  utility_risk_onedims_4[k-1] <- dUtility(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], xm=m_onedims_4$mx)
  utility_risk_mdav_4[k-1] <- dUtility(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], xm=m_mdav_4$mx)
  utility_risk_pca_4[k-1] <- dUtility(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], xm=m_pca_4$mx)
  
  m_onedims_5 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], method="onedims", aggr=k)
  m_mdav_5 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], method="mdav", aggr=k)
  m_pca_5 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], method="pca", aggr=k)
  global_risk_onedims_5[k-1] <- dRisk(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], xm=m_onedims_5$mx)
  global_risk_mdav_5[k-1] <- dRisk(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], xm=m_mdav_5$mx)
  global_risk_pca_5[k-1] <- dRisk(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], xm=m_pca_5$mx)
  utility_risk_onedims_5[k-1] <- dUtility(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], xm=m_onedims_5$mx)
  utility_risk_mdav_5[k-1] <- dUtility(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], xm=m_mdav_5$mx)
  utility_risk_pca_5[k-1] <- dUtility(obj=CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], xm=m_pca_5$mx)
}
risk_pima <- data.frame(
  x = rep(min:max, 6),
  y = c(global_risk_onedims_4, global_risk_mdav_4, global_risk_pca_4, global_risk_onedims_5, global_risk_mdav_5, global_risk_pca_5),
  group = rep(c("global_risk_onedims_4", "global_risk_mdav_4", "global_risk_pca_4", "global_risk_onedims_5", "global_risk_mdav_5", "global_risk_pca_5"), each = (max - min + 1))
)
ggplot(risk_pima, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line() +
  scale_color_manual(values = c("global_risk_onedims_4" = "darkorchid", "global_risk_mdav_4" = "darkorchid", "global_risk_pca_4" = "darkorchid", "global_risk_onedims_5" = "darkorange1", "global_risk_mdav_5" = "darkorange1", "global_risk_pca_5" = "darkorange1")) +
  scale_linetype_manual(values = c("global_risk_onedims_4" = "solid", "global_risk_mdav_4" = "longdash", "global_risk_pca_4" = "dotted", "global_risk_onedims_5" = "solid", "global_risk_mdav_5" = "dashed", "global_risk_pca_5" = "dotted")) +
  theme_minimal() +
  labs(x = "k", y = "Global Risk") +
  theme(legend.position = "bottom")
utility_pima <- data.frame(
  x = rep(min:max, 6),
  y = c(utility_risk_onedims_4, utility_risk_mdav_4, utility_risk_pca_4, utility_risk_onedims_5, utility_risk_mdav_5, utility_risk_pca_5),
  group = rep(c("utility_risk_onedims_4", "utility_risk_mdav_4", "utility_risk_pca_4", "utility_risk_onedims_5", "utility_risk_mdav_5", "utility_risk_pca_5"), each = (max - min + 1))
)
ggplot(utility_pima, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line() +
  scale_color_manual(values = c("utility_risk_onedims_4" = "darkorchid", "utility_risk_mdav_4" = "darkorchid", "utility_risk_pca_4" = "darkorchid", "utility_risk_onedims_5" = "darkorange1", "utility_risk_mdav_5" = "darkorange1", "utility_risk_pca_5" = "darkorange1")) +
  scale_linetype_manual(values = c("utility_risk_onedims_4" = "solid", "utility_risk_mdav_4" = "longdash", "utility_risk_pca_4" = "dotted", "utility_risk_onedims_5" = "solid", "utility_risk_mdav_5" = "dashed", "utility_risk_pca_5" = "dotted")) +
  theme_minimal() +
  labs(x = "k", y = "Global Utility") +
  theme(legend.position = "bottom")

CCD_copy <- CCD
for(k in 2:20) {
  m_onedims_4 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="onedims", aggr=k)
  CCD_copy$Age <- m_onedims_4$mx$Age
  CCD_copy$BloodPressure <- m_onedims_4$mx$BloodPressure
  CCD_copy$Insulin <- m_onedims_4$mx$Insulin
  global_risk_onedims_4[k-1] <- dRisk(obj=CCD, xm=CCD_copy)
  utility_risk_onedims_4[k-1] <- dUtility(obj=CCD, xm=CCD_copy)
  
  m_mdav_4 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="mdav", aggr=k)
  CCD_copy$Age <- m_mdav_4$mx$Age
  CCD_copy$BloodPressure <- m_mdav_4$mx$BloodPressure
  CCD_copy$Insulin <- m_mdav_4$mx$Insulin
  global_risk_mdav_4[k-1] <- dRisk(obj=CCD, xm=CCD_copy)
  utility_risk_mdav_4[k-1] <- dUtility(obj=CCD, xm=CCD_copy)
  
  m_pca_4 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="pca", aggr=k)
  CCD_copy$Age <- m_pca_4$mx$Age
  CCD_copy$BloodPressure <- m_pca_4$mx$BloodPressure
  CCD_copy$Insulin <- m_pca_4$mx$Insulin
  global_risk_pca_4[k-1] <- dRisk(obj=CCD, xm=CCD_copy)
  utility_risk_pca_4[k-1] <- dUtility(obj=CCD, xm=CCD_copy)
  
  
  m_onedims_5 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], method="onedims", aggr=k)
  CCD_copy$Age <- m_onedims_5$mx$Age
  CCD_copy$BloodPressure <- m_onedims_5$mx$BloodPressure
  global_risk_onedims_5[k-1] <- dRisk(obj=CCD, xm=CCD_copy)
  utility_risk_onedims_5[k-1] <- dUtility(obj=CCD, xm=CCD_copy)
  
  m_mdav_5 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], method="mdav", aggr=k)
  CCD_copy$Age <- m_mdav_5$mx$Age
  CCD_copy$BloodPressure <- m_mdav_5$mx$BloodPressure
  global_risk_mdav_5[k-1] <- dRisk(obj=CCD, xm=CCD_copy)
  utility_risk_mdav_5[k-1] <- dUtility(obj=CCD, xm=CCD_copy)
  
  m_pca_5 = microaggregation(CCD[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION", "MARRIAGE")], method="pca", aggr=k)
  CCD_copy$Age <- m_pca_5$mx$Age
  CCD_copy$BloodPressure <- m_pca_5$mx$BloodPressure
  global_risk_pca_5[k-1] <- dRisk(obj=CCD, xm=CCD_copy)
  utility_risk_pca_5[k-1] <- dUtility(obj=CCD, xm=CCD_copy)
}
risk_pima <- data.frame(
  x = rep(min:max, 6),
  y = c(global_risk_onedims_4, global_risk_mdav_4, global_risk_pca_4, global_risk_onedims_5, global_risk_mdav_5, global_risk_pca_5),
  group = rep(c("global_risk_onedims_4", "global_risk_mdav_4", "global_risk_pca_4", "global_risk_onedims_5", "global_risk_mdav_5", "global_risk_pca_5"), each = (max - min + 1))
)
ggplot(risk_pima, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line() +
  scale_color_manual(values = c("global_risk_onedims_4" = "darkorchid", "global_risk_mdav_4" = "darkorchid", "global_risk_pca_4" = "darkorchid", "global_risk_onedims_5" = "darkorange1", "global_risk_mdav_5" = "darkorange1", "global_risk_pca_5" = "darkorange1")) +
  scale_linetype_manual(values = c("global_risk_onedims_4" = "solid", "global_risk_mdav_4" = "longdash", "global_risk_pca_4" = "dotted", "global_risk_onedims_5" = "solid", "global_risk_mdav_5" = "dashed", "global_risk_pca_5" = "dotted")) +
  theme_minimal() +
  labs(x = "k", y = "Global Risk") +
  theme(legend.position = "bottom")
utility_pima <- data.frame(
  x = rep(min:max, 6),
  y = c(utility_risk_onedims_4, utility_risk_mdav_4, utility_risk_pca_4, utility_risk_onedims_5, utility_risk_mdav_5, utility_risk_pca_5),
  group = rep(c("utility_risk_onedims_4", "utility_risk_mdav_4", "utility_risk_pca_4", "utility_risk_onedims_5", "utility_risk_mdav_5", "utility_risk_pca_5"), each = (max - min + 1))
)
ggplot(utility_pima, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line() +
  scale_color_manual(values = c("utility_risk_onedims_4" = "darkorchid", "utility_risk_mdav_4" = "darkorchid", "utility_risk_pca_4" = "darkorchid", "utility_risk_onedims_5" = "darkorange1", "utility_risk_mdav_5" = "darkorange1", "utility_risk_pca_5" = "darkorange1")) +
  scale_linetype_manual(values = c("utility_risk_onedims_4" = "solid", "utility_risk_mdav_4" = "longdash", "utility_risk_pca_4" = "dotted", "utility_risk_onedims_5" = "solid", "utility_risk_mdav_5" = "dashed", "utility_risk_pca_5" = "dotted")) +
  theme_minimal() +
  labs(x = "k", y = "Global Risk") +
  theme(legend.position = "bottom")



##### Chosen variables #####
CCD_onedims <- CCD
m_onedims = microaggregation(CCD_onedims[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="onedims", aggr=15, measure = "median")
CCD_onedims$LIMIT_BAL <- m_onedims$mx$LIMIT_BAL
CCD_onedims$BILL_AMT1 <- m_onedims$mx$BILL_AMT1
CCD_onedims$PAY_AMT1 <- m_onedims$mx$PAY_AMT1
CCD_onedims$EDUCATION <- m_onedims$mx$EDUCATION
summary(CCD[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
summary(CCD_onedims[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
write.csv(CCD_onedims,"./modifiedDatasets/CCD_onedims.csv", row.names = FALSE)

CCD_mdav <- CCD
m_mdav = microaggregation(CCD_onedims[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="mdav", aggr=15, measure = "median")
CCD_mdav$LIMIT_BAL <- m_mdav$mx$LIMIT_BAL
CCD_mdav$BILL_AMT1 <- m_mdav$mx$BILL_AMT1
CCD_mdav$PAY_AMT1 <- m_mdav$mx$PAY_AMT1
CCD_mdav$EDUCATION <- m_onedims$mx$EDUCATION
summary(CCD[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
summary(CCD_mdav[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
write.csv(CCD_mdav,"./modifiedDatasets/CCD_mdav.csv", row.names = FALSE)

CCD_pca <- CCD
m_pca = microaggregation(CCD_onedims[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="pca", aggr=15, measure = "median")
CCD_pca$LIMIT_BAL <- m_pca$mx$LIMIT_BAL
CCD_pca$BILL_AMT1 <- m_pca$mx$BILL_AMT1
CCD_pca$PAY_AMT1 <- m_pca$mx$PAY_AMT1
CCD_pca$EDUCATION <- m_onedims$mx$EDUCATION
summary(CCD[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
summary(CCD_pca[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
write.csv(CCD_pca,"./modifiedDatasets/CCD_pca.csv", row.names = FALSE)


CCD_onedims_test <- CCD_test
m_onedims_test = microaggregation(CCD_onedims_test[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="onedims", aggr=20, measure = "median")
CCD_onedims_test$LIMIT_BAL <- m_onedims_test$mx$LIMIT_BAL
CCD_onedims_test$BILL_AMT1 <- m_onedims_test$mx$BILL_AMT1
CCD_onedims_test$PAY_AMT1 <- m_onedims_test$mx$PAY_AMT1
CCD_onedims_test$EDUCATION <- m_onedims_test$mx$EDUCATION
summary(CCD_test[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
summary(CCD_onedims_test[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
write.csv(CCD_onedims_test,"./modifiedDatasets/CCD_onedims_test.csv", row.names = FALSE)

CCD_mdav_test <- CCD_test
m_mdav_test = microaggregation(CCD_onedims_test[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="mdav", aggr=20, measure = "median")
CCD_mdav_test$LIMIT_BAL <- m_mdav_test$mx$LIMIT_BAL
CCD_mdav_test$BILL_AMT1 <- m_mdav_test$mx$BILL_AMT1
CCD_mdav_test$PAY_AMT1 <- m_mdav_test$mx$PAY_AMT1
CCD_mdav_test$EDUCATION <- m_onedims_test$mx$EDUCATION
summary(CCD_test[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
summary(CCD_mdav_test[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
write.csv(CCD_mdav_test,"./modifiedDatasets/CCD_mdav_test.csv", row.names = FALSE)

CCD_pca_test <- CCD_test
m_pca_test = microaggregation(CCD_onedims_test[,c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")], method="pca", aggr=20, measure = "median")
CCD_pca_test$LIMIT_BAL <- m_pca_test$mx$LIMIT_BAL
CCD_pca_test$BILL_AMT1 <- m_pca_test$mx$BILL_AMT1
CCD_pca_test$PAY_AMT1 <- m_pca_test$mx$PAY_AMT1
CCD_pca_test$EDUCATION <- m_onedims_test$mx$EDUCATION
summary(CCD_test[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
summary(CCD_pca_test[, c("LIMIT_BAL", "BILL_AMT1", "PAY_AMT1", "EDUCATION")])
write.csv(CCD_pca_test,"./modifiedDatasets/CCD_pca_test.csv", row.names = FALSE)



########## KDD ##########
##### Chosen variables #####

# We will use the variables LIMIT_BAL, BILL_AMT1, PAY_AMT1, EDUCATION
p1 <- ggplot(KDD, aes(age)) + geom_histogram(binwidth = 10)
p2 <- ggplot(KDD, aes(education)) + geom_bar()
p3 <- ggplot(KDD, aes(marital_stat)) + geom_bar()
p4 <- ggplot(KDD, aes(birth_country_mother)) + geom_bar()
grid.arrange(p1, p2, p3, p4, ncol=2)


#age, marital_stat, birth_country_mother, class_of_worker, education
KDD$education <- trimws(KDD$education)
KDD$education <- as.numeric(factor(ordered(KDD$education, 
                                           levels = c("Less than 1st grade", "1st 2nd 3rd or 4th grade", 
                                                      "5th or 6th grade", "7th and 8th grade", "9th grade", "10th grade", 
                                                      "11th grade", "12th grade no diploma", "High school graduate", 
                                                      "Some college but no degree", "Associates degree-academic program", 
                                                      "Associates degree-occup /vocational", "Bachelors degree", 
                                                     "Masters degree", "Prof school degree", "Doctorate degree"))))
KDD$marital_stat <- trimws(KDD$marital_stat)
KDD$marital_stat <- as.numeric(factor(ordered(KDD$marital_stat, 
                                           levels = c("Never married", "Widowed", "Divorced", "Separated", "Married-spouse absent",
                                                      "Married-A F spouse present", 
                                                      "Married-civilian spouse present"))))

sdc_KDD <- createSdcObj(
  dat = KDD,
  keyVars = c("birth_country_mother", "class_of_worker"),
  numVars = c("age", "marital_stat", "education", "occup_code"),
)
m_onedims <- microaggregation(
  obj = sdc_KDD,
  measure = "median",
  aggr = 2000,
  method = "onedims"
)
KDD_onedims <- KDD
KDD_onedims$age <- m_onedims@manipNumVars$age
KDD_onedims$marital_stat <- m_onedims@manipNumVars$marital_stat
KDD_onedims$education <- m_onedims@manipNumVars$education
summary(KDD[, c("age", "marital_stat", "education")])
summary(KDD_onedims[, c("age", "marital_stat", "education")])
write.csv(KDD_onedims,"./modifiedDatasets/KDD_onedims.csv", row.names = FALSE)

m_mdav <- microaggregation(
  obj = sdc_KDD,
  measure = "median",
  aggr = 2000,
  method = "mdav"
)
KDD_mdav <- KDD
KDD_mdav$age <- m_mdav@manipNumVars$age
KDD_mdav$marital_stat <- m_mdav@manipNumVars$marital_stat
KDD_mdav$education <- m_mdav@manipNumVars$education
summary(KDD[, c("age", "marital_stat", "education")])
summary(KDD_mdav[, c("age", "marital_stat", "education")])
write.csv(KDD_mdav,"./modifiedDatasets/KDD_mdav.csv", row.names = FALSE)

m_mdav <- microaggregation(
  obj = sdc_KDD,
  measure = "median",
  aggr = 2000,
  method = "mdav"
)
KDD_mdav <- KDD
KDD_mdav$age <- m_mdav@manipNumVars$age
KDD_mdav$marital_stat <- m_mdav@manipNumVars$marital_stat
KDD_mdav$education <- m_mdav@manipNumVars$education
summary(KDD[, c("age", "marital_stat", "education")])
summary(KDD_mdav[, c("age", "marital_stat", "education")])
write.csv(KDD_mdav,"./modifiedDatasets/KDD_pca.csv", row.names = FALSE)

KDD_test$education <- trimws(KDD_test$education)
KDD_test$education <- as.numeric(factor(ordered(KDD_test$education, 
                                           levels = c("Less than 1st grade", "1st 2nd 3rd or 4th grade", 
                                                      "5th or 6th grade", "7th and 8th grade", "9th grade", "10th grade", 
                                                      "11th grade", "12th grade no diploma", "High school graduate", 
                                                      "Some college but no degree", "Associates degree-academic program", 
                                                      "Associates degree-occup /vocational", "Bachelors degree", 
                                                      "Masters degree", "Prof school degree", "Doctorate degree"))))
KDD_test$marital_stat <- trimws(KDD_test$marital_stat)
KDD_test$marital_stat <- as.numeric(factor(ordered(KDD_test$marital_stat, 
                                              levels = c("Never married", "Widowed", "Divorced", "Separated", "Married-spouse absent",
                                                         "Married-A F spouse present", 
                                                         "Married-civilian spouse present"))))


sdc_KDD_test <- createSdcObj(
  dat = KDD_test,
  keyVars = c("birth_country_mother", "class_of_worker"),
  numVars = c("age", "marital_stat", "education", "occup_code"),
)
m_onedims_test <- microaggregation(
  obj = sdc_KDD_test,
  measure = "median",
  aggr = 2000,
  method = "onedims"
)
KDD_onedims_test <- KDD_test
KDD_onedims_test$age <- m_onedims_test@manipNumVars$age
KDD_onedims_test$marital_stat <- m_onedims_test@manipNumVars$marital_stat
KDD_onedims_test$education <- m_onedims_test@manipNumVars$education
summary(KDD_test[, c("age", "marital_stat", "education")])
summary(KDD_onedims_test[, c("age", "marital_stat", "education")])
write.csv(KDD_onedims_test,"./modifiedDatasets/KDD_onedims_test.csv", row.names = FALSE)

m_mdav <- microaggregation(
  obj = sdc_KDD,
  measure = "median",
  aggr = 2000,
  method = "mdav"
)
KDD_mdav_test <- KDD_test
KDD_mdav_test$age <- m_mdav_test@manipNumVars$age
KDD_mdav_test$marital_stat <- m_mdav_test@manipNumVars$marital_stat
KDD_mdav_test$education <- m_mdav_test@manipNumVars$education
summary(KDD_test[, c("age", "marital_stat", "education")])
summary(KDD_mdav_test[, c("age", "marital_stat", "education")])
write.csv(KDD_mdav_test,"./modifiedDatasets/KDD_mdav_test.csv", row.names = FALSE)

m_mdav_test <- microaggregation(
  obj = sdc_KDD,
  measure = "median",
  aggr = 2000,
  method = "mdav"
)
KDD_mdav_test <- KDD
KDD_mdav_test$age <- m_mdav@manipNumVars$age
KDD_mdav_test$marital_stat <- m_mdav@manipNumVars$marital_stat
KDD_mdav_test$education <- m_mdav@manipNumVars$education
summary(KDD_test[, c("age", "marital_stat", "education")])
summary(KDD_mdav_test[, c("age", "marital_stat", "education")])
write.csv(KDD_mdav_test,"./modifiedDatasets/KDD_mdav.csv", row.names = FALSE)

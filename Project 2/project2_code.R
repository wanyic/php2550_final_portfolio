library(tidyverse)
library(mice)
library(gtsummary)
library(dplyr)
library(glmnet)  
library(leaps)
library(tableone)
library(DescTools)
library(Metrics)
library(pROC)
library(GGally)
library(L0Learn) # best subset
library(caret)


data <- read.csv("project2.csv") %>%
  select(-mat_race)
head(data)
dim(data)

complete_perc <- nrow(data[complete.cases(data) == TRUE,])/nrow(data)
complete_perc
# complete cases are only about 15%

# row with all na data
na_ind <- apply(data, 1, function(x) all(is.na(x)))
length(na_ind[na_ind == TRUE]) # no row with all na data

unique(data$center)

# fill the missing center numbers
data$center[is.na(data$center)] <- 1
data[is.na(data$center),]

head(data)


# look for duplicated observations and remove
data <- data[!duplicated(data) == TRUE,]

# transform categorical variables to factors

data <- data %>%
  mutate(mat_chorio = case_when(mat_chorio == "No" ~ 2,
                                mat_chorio == "Yes" ~ 1,
                                mat_chorio == "Unknown" ~ 3,
                                mat_chorio == NA ~ NA)) %>%
  mutate(sga = case_when(sga == "Not SGA" ~ 0,
                         sga == "SGA" ~ 1,
                         sga == NA ~ NA)) %>%
  mutate(any_surf = case_when(any_surf == "No" ~ 2,
                              any_surf == "Yes" ~ 1,
                              any_surf == "Unknown" ~ 3,
                              any_surf == NA ~ NA)) %>%
  mutate(prenat_ster = case_when(prenat_ster == "No" ~ 2,
                                 prenat_ster == "Yes" ~ 1,
                                 prenat_ster == "Unknown" ~ 3,
                                 prenat_ster == NA ~ NA)) %>%
  mutate(com_prenat_ster = case_when(com_prenat_ster == "No" ~ 2,
                                     com_prenat_ster == "Yes" ~ 1,
                                     com_prenat_ster == "Unknown" ~ 3,
                                     com_prenat_ster == NA ~ NA)) %>%
  mutate(gender = case_when(gender == "Male" ~ 1,
                            gender == "Female" ~ 2,
                            gender == "Ambiguous" ~ 3,
                            gender == NA ~ NA))

data$mat_ethn <- as.factor(data$mat_ethn)
data$del_method <- as.factor(data$del_method)
data$prenat_ster <- as.factor(data$prenat_ster)
data$com_prenat_ster <- as.factor(data$com_prenat_ster)
data$mat_chorio <- as.factor(data$mat_chorio)
data$gender <- as.factor(data$gender)
data$sga <- as.factor(data$sga)
data$med_ph.36 <- as.factor(data$med_ph.36)
data$med_ph.44 <- as.factor(data$med_ph.44)
data$any_surf <- as.factor(data$any_surf)
data$ventilation_support_level.36 <- as.factor(data$ventilation_support_level.36)
data$ventilation_support_level_modified.44 <- as.factor(data$ventilation_support_level_modified.44)

# create outcome variable based on death and trach status
data <- data %>%
  mutate(Death = case_when(Death == "No" ~ 0,
                           Death == "Yes" ~ 1,
                           Death == NA ~ NA)) %>%
  mutate(outcome = case_when(Death == 1 & Trach == 1 ~ 1,
                             Death == 1 & Trach == 0 ~ 0,
                             Death == 0 & Trach == 1 ~ 0,
                             Death == 0 & Trach == 0 ~ 0,
                             Death == NA & Trach == NA ~ NA,
                             Death == 1 & Trach == NA ~ NA,
                             Death == NA & Trach == 1 ~ NA))
data$Trach <- as.factor(data$Trach)
data$Death <- as.factor(data$Death)
data$outcome <- as.factor(data$outcome)
data <- data %>% select(-mat_ethn, -hosp_dc_ga)
data$interaction1 <- as.numeric(data$ventilation_support_level.36) * 
  as.numeric(data$inspired_oxygen.36)
data$interaction2 <- as.numeric(data$ventilation_support_level.36) * 
  as.numeric(data$med_ph.36)
data$interaction3 <- as.numeric(data$ventilation_support_level_modified.44) * 
  as.numeric(data$inspired_oxygen.44)
data$interaction4 <- as.numeric(data$ventilation_support_level_modified.44) * 
  as.numeric(data$med_ph.44)
head(data, 10)


# calculate missing percentage for each variables
missing <- round(apply(data, 2, function(x) sum(is.na(x)))/nrow(data), 4)
missing1 <- round(apply(data %>% filter(center == 1), 2, function(x) sum(is.na(x)))/nrow(data), 4) * 100
missing2 <- round(apply(data %>% filter(center == 2), 2, function(x) sum(is.na(x)))/nrow(data), 4) * 100
missing3 <- round(apply(data %>% filter(center == 3), 2, function(x) sum(is.na(x)))/nrow(data), 4) * 100
missing4 <- round(apply(data %>% filter(center == 4), 2, function(x) sum(is.na(x)))/nrow(data), 4) * 100
missing5 <- round(apply(data %>% filter(center == 5), 2, function(x) sum(is.na(x)))/nrow(data), 4) * 100
missing7 <- round(apply(data %>% filter(center == 7), 2, function(x) sum(is.na(x)))/nrow(data), 4) * 100
missing12 <- round(apply(data %>% filter(center == 12), 2, function(x) sum(is.na(x)))/nrow(data), 4) * 100
missing16 <- round(apply(data %>% filter(center == 16), 2, function(x) sum(is.na(x)))/nrow(data), 4) * 100
missing20 <- round(apply(data %>% filter(center == 20), 2, function(x) sum(is.na(x)))/nrow(data), 4) * 100
missing21 <- round(apply(data %>% filter(center == 21), 2, function(x) sum(is.na(x)))/nrow(data), 4) * 100
missing <- missing * 100

missingness <- cbind(missing, missing1, missing2, missing3, missing4,
                     missing5, missing7, missing12, missing16, missing20,
                     missing21)


missing_table <- data.frame(missingness) 

missing_table$vars <- rownames(missingness)
missing_table <- missing_table %>%
  arrange(desc(missing)) %>%
  filter(missing > 10) %>%
  relocate(vars, .before = missing)

colnames(missing_table) <- c("Vars", "Overall Missingness", "1", "2", "3",
                             "4", "5", "7", "12", "16", "20", "21")

missing_table <- missing_table |>
  gt() |>
  tab_caption(caption = md("**Table 2.** Missing percentages by centers for the variables that have an overall missingness percentage greater than 10% are included in this table. See Table 5 in appendix for the full table."))

missing_table


des_summary <- data %>%
  group_by(center) %>%
  mutate(num_patients = n()) %>%
  mutate(trach_freq = mean(ifelse(Trach == 0, 0, 1), na.rm = T)) %>%
  mutate(death_freq = mean(ifelse(Death == 0, 0, 1), na.rm = T)) %>%
  mutate(outcome_freq = mean(ifelse(outcome == 0, 0, 1), na.rm = T)) %>%
  mutate(female_freq = mean(ifelse(data$gender == 1, 0, 1), na.rm=T)) %>%
  mutate(bw_mean = mean(bw)) %>%
  mutate(ga_mean = mean(ga)) %>%
  mutate(blength_mean = mean(blength, na.rm = T)) %>%
  mutate(birth_hc_mean = mean(birth_hc, na.rm = T)) %>%
  mutate(sga_freq = mean(ifelse(sga == 1, 1, 0), na.rm =T)) %>%
  select( center, num_patients, trach_freq, death_freq, outcome_freq, bw_mean, ga_mean, blength_mean,
          birth_hc_mean, sga_freq) %>%
  mutate(across(where(is.numeric), round, digits=4)) %>%
  arrange(desc(num_patients))


kableone(unique(des_summary))


# a function to customize fitted lines in scatter plots
lower_func <- function(data, mapping, method = "lm", ...) {
  #' @description customize fitted lines in ggally plots
  #' @param data input data for plotting
  #' @param mapping the mapping for variables in the input data
  #' @param method the method to be used for fitted lines
  #' @return a ggplot with fitted lines and the corresponding mapping 
  
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  
  return(p)
}

# plot correlations
ggpair_df1 <- data[, c("bw","ga",
                       "weight_today.36", "weight_today.44", 
                       "inspired_oxygen.36", "outcome")] 
ggpair_df1$outcome <- ifelse(ggpair_df1$outcome == 0, 0, 1)

ggpairs(ggpair_df1, columns = 1:ncol(ggpair_df1), 
        lower = list(continuous = wrap(lower_func, method = "lm")), 
        title = "",  
        axisLabels = "show", columnLabels = colnames(ggpair_df1))


# perform mice for missing data
# split data into test and train sets (25% and 75%)
set.seed(1)
test_indice <- sample(nrow(data), 249, replace = FALSE)
train_data <- data[-test_indice, ]
test_data <- data[test_indice, ]

# use mice to impute the missingness in cross validation sets
train_mice <- mice(train_data, m = 5, print = FALSE, seed = 1)
test_mice <- mice.mids(train_mice, newdata = test_data)

train_mice1 <- mice::complete(train_mice,1)[,c(2:25, 28)]
test_mice1 <- mice::complete(test_mice,1)[,c(2:25, 28)]
train_mice2 <- mice::complete(train_mice,2)[,c(2:25, 28)]
test_mice2 <- mice::complete(test_mice,2)[,c(2:25, 28)]
train_mice3 <- mice::complete(train_mice,3)[,c(2:25, 28)]
test_mice3 <- mice::complete(test_mice,3)[,c(2:25, 28)]
train_mice4 <- mice::complete(train_mice,4)[,c(2:25, 28)]
test_mice4 <- mice::complete(test_mice,4)[,c(2:25, 28)]
train_mice5 <- mice::complete(train_mice,5)[,c(2:25, 28)]
test_mice5 <- mice::complete(test_mice,5)[,c(2:25, 28)]


# obtain predictions
train_model_predict <- function(df, coefs){
  #' @description a prediction function for outputted model coefs
  #' @param df a dataframe of data used to derive model
  #' @param coefs a vector of coefficients
  
  # add up the values  
  outcome_preds <- rep(NA, nrow(df))
  for (i in 1:nrow(df)){
    preds <- coefs[1] + (df$center[i] * coefs[2]) + 
      (df$bw[i] * coefs[3]) + (df$ga[i] * coefs[4]) +
      (df$blength[i] * coefs[5]) + (df$birth_hc[i] * coefs[6]) +
      (as.numeric(df$del_method[i] == 2) * coefs[7]) +
      (as.numeric(df$prenat_ster[i] == 2) * coefs[8]) +
      (as.numeric(df$com_prenat_ster[i] == 2) * coefs[9]) +
      (as.numeric(df$mat_chorio[i] == 2) * coefs[10]) +
      (as.numeric(df$gender[i] == 2) * coefs[11]) +
      (as.numeric(df$sga[i] == 1) * coefs[12]) +
      (as.numeric(df$any_surf[i] == 2) * coefs[13]) +
      (df$weight_today.36[i] * coefs[14]) +
      (as.numeric(df$ventilation_support_level.36[i] == 1) * coefs[15]) + 
      (as.numeric(df$ventilation_support_level.36[i] == 2) * coefs[16]) + 
      (df$inspired_oxygen.36[i] * coefs[17]) + 
      (df$p_delta.36[i] * coefs[18]) + 
      (df$peep_cm_h2o_modified.36[i] * coefs[19]) +
      (as.numeric(df$med_ph.36[i] == 1) * coefs[20]) + 
      (df$weight_today.44[i] * coefs[21]) +
      (as.numeric(df$ventilation_support_level_modified.44[i] == 1) * coefs[22]) +
      (as.numeric(df$ventilation_support_level_modified.44[i] == 2) * coefs[23]) +
      (df$inspired_oxygen.44[i] * coefs[24]) + 
      (df$p_delta.44[i] * coefs[25]) +
      (df$peep_cm_h2o_modified.44[i] * coefs[26]) +
      (as.numeric(df$med_ph.44[i] == 1) * coefs[27])
    
    outcome_preds[i] <- preds
  }
  return(outcome_preds)
}

# use lasso to obtain coefficients for cross validation sets
train_mice_coef1 <- as.vector(lasso(train_mice1))
train_mice_coef2 <- as.vector(lasso(train_mice2))
train_mice_coef3 <- as.vector(lasso(train_mice3))
train_mice_coef4 <- as.vector(lasso(train_mice4))
train_mice_coef5 <- as.vector(lasso(train_mice5))

# use best subset to obtain coefficients 
x1 <- as.matrix(model.matrix(outcome~., data = train_mice1)[,-1])
x2 <- as.matrix(model.matrix(outcome~., data = train_mice2)[,-1])
x3 <- as.matrix(model.matrix(outcome~., data = train_mice3)[,-1])
x4 <- as.matrix(model.matrix(outcome~., data = train_mice4)[,-1])
x5 <- as.matrix(model.matrix(outcome~., data = train_mice5)[,-1])
y1 <- train_mice1$outcome

train_bset_coef1 <- L0Learn.cvfit(x1, y1, nFolds=10, seed=500, penalty="L0L2",
                                  maxSuppSize=20, nGamma=3, gammaMin=0.0001, gammaMax = 10)
train_bset_coef2 <- L0Learn.cvfit(x2, y1, nFolds=10, seed=500, penalty="L0L2", 
                                  maxSuppSize=20, nGamma=3, gammaMin=0.0001, gammaMax = 10)
train_bset_coef3 <- L0Learn.cvfit(x3, y1, nFolds=10, seed=500, penalty="L0L2", 
                                  maxSuppSize=20, nGamma=3, gammaMin=0.0001, gammaMax = 10)
train_bset_coef4 <- L0Learn.cvfit(x4, y1, nFolds=10, seed=500, penalty="L0L2", 
                                  maxSuppSize=20, nGamma=3, gammaMin=0.0001, gammaMax = 10)
train_bset_coef5 <- L0Learn.cvfit(x5, y1, nFolds=10, seed=500, penalty="L0L2", 
                                  maxSuppSize=20, nGamma=3, gammaMin=0.0001, gammaMax = 10)


bset_coef1 <- as.vector(coef(train_bset_coef1, lambda=4.68207e-06, gamma=0.0001))
bset_coef2 <- as.vector(coef(train_bset_coef2, lambda=4.68207e-06, gamma=0.0001))
bset_coef3 <- as.vector(coef(train_bset_coef3, lambda=4.68207e-06, gamma=0.0001))
bset_coef4 <- as.vector(coef(train_bset_coef4, lambda=4.68207e-06, gamma=0.0001))
bset_coef5 <- as.vector(coef(train_bset_coef5, lambda=4.68207e-06, gamma=0.0001))


# get coefficients by averaging across the 5 imputed data sets
train_mean_mice_mat <- cbind(train_mice_coef1, train_mice_coef2, train_mice_coef3, 
                             train_mice_coef4, train_mice_coef5)
bset_mean_mice_mat <- cbind(bset_coef1, bset_coef2, bset_coef3, bset_coef4,
                            bset_coef5)
train_mean_mice_coef <- rowMeans(train_mean_mice_mat)
bset_mean_mice_coef <- rowMeans(bset_mean_mice_mat)

coef_names <- rownames(as.matrix(coef1))
train_coef_freq <- rowSums(train_mean_mice_mat != 0)
bset_coef_freq <- rowSums(bset_mean_mice_mat != 0)

# create table for coefs
coef_table <- data.frame(c(coef_names, "interaction1", "interaction2",
                           "interaction3", "interaction4"),
                         #                         round(mean_mice_coef, 6), coef_freq,
                         t_coef, t_inter_coef,
                         b_coef, b_inter_coef)
coef_table <- coef_table %>%
  filter(abs(t_inter_coef) > 0.0001 | abs(b_inter_coef) > 0.0001) 

colnames(coef_table) <- c("Coefficient",  
                          "Lasso Coef Estimate", 
                          "Lasso Coef with Interactions",
                          "Best Subset Coef Estimate",
                          "Best Subset Coef with Interactions")
table3 <- coef_table |>
  gt() |>
  tab_caption(caption = md("**Table 3.** Coefficient estimates and coefficient frequency for LASSO and Best Subset models."))
table3

# gt::gtsave(table3, file = "project2_table3.png")

# obtain predictions and trying out different thresholds
test_data_mice_m5 <- rbind(test_mice1, test_mice2, test_mice3, test_mice4,
                           test_mice5)

test_imputed_preds <- train_model_predict(test_data_mice_m5, train_mean_mice_coef)
bset_imputed_preds <- train_model_predict(test_data_mice_m5, bset_mean_mice_coef)
test_imputed_preds1 <- ifelse(test_imputed_preds > 0.17, 1, 0)
bset_imputed_preds1 <- ifelse(bset_imputed_preds > 0.17, 1, 0)
test_imputed_preds2 <- ifelse(test_imputed_preds > 0.17, 1, 0)
bset_imputed_preds2 <- ifelse(bset_imputed_preds > 0.17, 1, 0)

# brier scores
test_bs <- BrierScore(train_model_predict(test_data_mice_m5, train_mean_mice_coef),
                      ifelse(test_data_mice_m5$outcome == 1, 1, 0))
bset_bs <- BrierScore(train_model_predict(test_data_mice_m5, bset_mean_mice_coef),
                      ifelse(test_data_mice_m5$outcome == 1, 1, 0))


# using confusionMatrix function to obtain other useful evaluation metrics
t_scores <- confusionMatrix(as.factor(test_imputed_preds1), 
                            as.factor(test_data_mice_m5$outcome), mode = "everything")
b_scores <- confusionMatrix(as.factor(bset_imputed_preds1), 
                            as.factor(test_data_mice_m5$outcome), mode = "everything")

t_scores2 <- confusionMatrix(as.factor(test_imputed_preds2), 
                             as.factor(test_data_mice_m5$outcome), mode = "everything")
b_scores2 <- confusionMatrix(as.factor(bset_imputed_preds2), 
                             as.factor(test_data_mice_m5$outcome), mode = "everything")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(gtsummary)
library(DescTools)
library(faux)    
library(fGarch)
library(tableone)
library(gt)


set.seed(2550) # for simulation and sampling

# read data
nhanes_df <- read.csv("df_2017.csv")[,-1]
fram_df <- read.csv("framingham_df.csv")[,-1]

# Get blood pressure based on whether or not on BPMEDS
nhanes_df$SYSBP_UT <- ifelse(nhanes_df$BPMEDS == 0, 
                             nhanes_df$SYSBP, 0)
nhanes_df$SYSBP_T <- ifelse(nhanes_df$BPMEDS == 1, 
                            nhanes_df$SYSBP, 0)

head(nhanes_df)
dim(nhanes_df)

head(fram_df)
dim(fram_df)

# check missingness and possible duplicates or errors in data
# nhanes data
complete_perc <- nrow(nhanes_df[complete.cases(nhanes_df) == TRUE,])/nrow(nhanes_df)
complete_perc
# complete cases are only about 16%

# row with all na nhanes_df
na_ind <- apply(nhanes_df, 1, function(x) all(is.na(x)))
length(na_ind[na_ind == TRUE]) # no row with all na nhanes_df

# look for duplicated observations and remove
nhanes_df <- nhanes_df[!duplicated(nhanes_df) == TRUE,]


dim(nhanes_df)
head(nhanes_df)

# framingham data
complete_perc <- nrow(fram_df[complete.cases(fram_df) == TRUE,])/nrow(fram_df)
complete_perc
# complete cases are only about 16%

# row with all na nhanes_df
na_ind <- apply(fram_df, 1, function(x) all(is.na(x)))
length(na_ind[na_ind == TRUE]) # no row with all na fram_df

# look for duplicated observations and remove
fram_df <- fram_df[!duplicated(fram_df) == TRUE,]


dim(fram_df)
head(fram_df)


# include only the complete cases and remove irrelevant variables in NHANES
nhanes_df <- nhanes_df %>%
  select(SEX, HDLC, TOTCHOL, AGE, SYSBP, SYSBP_UT, SYSBP_T, CURSMOKE, DIABETES) %>%
  filter(AGE >= 40)
nhanes_df <- nhanes_df[complete.cases(nhanes_df) == TRUE,]

# train and test split on framingham
fram_df$SPLIT <- sample(c(0, 1), nrow(fram_df), replace=TRUE, prob=c(0.75, 0.25))

fram_train_df <- fram_df[fram_df$SPLIT == 0,] %>%
  select(-SPLIT)
fram_test_df <- fram_df[fram_df$SPLIT == 1,] %>%
  select(-SPLIT)

# Fit models with log transforms for all continuous variables with Framingham
# train data only
mod_men <- glm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
                 log(SYSBP_T+1)+CURSMOKE+DIABETES,
               data= fram_train_df[fram_train_df$SEX == 1,], family= "binomial")


mod_women <- glm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
                   log(SYSBP_T+1)+CURSMOKE+DIABETES,
                 data= fram_train_df[fram_train_df$SEX == 2,], family= "binomial")


fram_test_df$SOURCE <- rep(1, 640)
fram_test_women <- fram_test_df %>% filter(SEX == 2)
fram_test_men <- fram_test_df %>% filter(SEX == 1)

fram_test_women$PRED <- predict(mod_women, fram_test_women, 
                                type = "response")
fram_test_men$PRED <- predict(mod_men, fram_test_men, 
                              type = "response")

nhanes_women <- nhanes_df %>%
  filter(SEX == 2)  # 2 is female
nhanes_women$SOURCE <- rep(0, 724)
nhanes_women$CVD <- rep(NA, 724)
nhanes_women$PRED <- predict(mod_women, nhanes_women, 
                             type = "response")

fram_test_women2 <- fram_test_women %>% select(SEX, HDLC, TOTCHOL, AGE, SYSBP, SYSBP_UT,
                                               SYSBP_T, CURSMOKE, DIABETES, SOURCE, CVD, PRED)

combined_women2 <- rbind(nhanes_women, fram_test_women2)

# obtain predictions for framingham test set
fram_test_df$SOURCE <- rep(1, 640)
fram_test_df$ID <- rep(1:640)
fram_test_women <- fram_test_df %>% filter(SEX == 2)
fram_test_men <- fram_test_df %>% filter(SEX == 1)

fram_test_women$PRED <- predict(mod_women, fram_test_women, 
                                type = "response")
fram_test_men$PRED <- predict(mod_men, fram_test_men, 
                              type = "response")
fram_preds <- rbind(fram_test_women, fram_test_men) %>%
  select(ID, PRED) %>%
  arrange(ID)
fram_test_df$PRED <- fram_preds$PRED

# obtain predictions for combined data set from models
combined_women$PRED <- predict(mod_men, combined_women, 
                               type = "response")

combined_men$PRED <- predict(mod_women, combined_men, 
                             type = "response")
preds <- rbind(combined_men, combined_women) %>% 
  select(PRED, ID) %>%
  arrange(ID)

combined_df$PRED <- preds$PRED

# nhanes women
nhanes_women <- nhanes_df %>%
  filter(SEX == 2)  # 2 is female
nhanes_women$CVD <- rep(NA, 724)

fram_test_women2 <- fram_test_women %>% select(SEX, HDLC, TOTCHOL, AGE, SYSBP, SYSBP_UT,
                                               SYSBP_T, CURSMOKE, DIABETES, SOURCE, CVD)
# combine fram and nhanes women
combined_women2 <- rbind(nhanes_women, fram_test_women2)
combined_women2$PRED <- predict(mod_women, combined_women2, 
                                type = "response")
combined_women2$SOURCE <- c(rep(0, 724), rep(1, 357))

# nhanes men
nhanes_men <- nhanes_df %>%
  filter(SEX == 1)  # 1 is male
nhanes_men$CVD <- rep(NA, 690)

fram_test_men2 <- fram_test_men %>% select(SEX, HDLC, TOTCHOL, AGE, SYSBP, SYSBP_UT,
                                           SYSBP_T, CURSMOKE, DIABETES, SOURCE, CVD)

# combine fram and nhanes men
combined_men2 <- rbind(nhanes_men, fram_test_men2)
combined_men2$PRED <- predict(mod_men, combined_men2, 
                              type = "response")
combined_men2$SOURCE <- c(rep(0, 690), rep(1, 283))

# create plots to get better understanding of the combined data set
p1 <- ggplot(fram_df, aes(fill = as.factor(SEX), x = as.factor(CVD))) +
  geom_bar(position = "dodge") +
  labs(title = "Framingham: CVD by Sex", x = "CVD Occurence", y = "Count",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Did not occur", "Did occur")) +
  theme(text = element_text(size=8))

# High Density Lipoprotein Cholesterol (mg/dL). Available for Period 3 only. 
# Values range from 10-189.
p2 <- ggplot(fram_df, aes(fill = as.factor(SEX), x = HDLC)) +
  geom_density(alpha = 0.6) +
  labs(title = "Framingham: HDLC by Sex", 
       x = "High Density Lipoprotein Cholesterol (mg/dL)", 
       y = "Density",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  theme(text = element_text(size=8))

# Serum Total Cholesterol (mg/dL). Values range from 107-696.
p3 <- ggplot(fram_df, aes(fill = as.factor(SEX), x = TOTCHOL)) +
  geom_density(alpha = 0.6) +
  labs(title = "Framingham: TOTCHOL by Sex", 
       x = "Serum Total Cholesterol (mg/dL)", 
       y = "Density",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  theme(text = element_text(size=8))

# Age at exam (years). Values range from 32-81.
p4 <- ggplot(fram_df, aes(fill = as.factor(SEX), x = AGE)) +
  geom_density(alpha = 0.6) +
  labs(title = "Framingham: Age by Sex", 
       x = "Age", 
       y = "Density",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  theme(text = element_text(size=8))

# Systolic Blood Pressure (mean of last two of three measurements) (mmHg). 
# Values range from 83.5-295.
p5 <- ggplot(fram_df, aes(fill = as.factor(SEX), x = SYSBP)) +
  geom_density(alpha = 0.6) +
  labs(title = "Framingham: SYSBP by Sex", 
       x = "Systolic Blood Pressure (mmHg)", 
       y = "Density",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  theme(text = element_text(size=8))

# Current cigarette smoking at exam. 0 = Not current smoker (n = 6598), 
# 1 = Current smoker (n = 5029).
p6 <- ggplot(fram_df, aes(fill = as.factor(SEX), x = as.factor(CURSMOKE))) +
  geom_bar(position = "dodge") +
  labs(title = "Framingham: CURSMOKE by Sex", x = "Current Smoking Status", 
       y = "Count",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Not current smoker", "Current smoker")) +
  theme(text = element_text(size=8))

# Diabetic according to criteria of first exam treated or first exam with 
# casual glucose of 200 mg/dL or more. 0 = Not a diabetic (n = 11097), 
# 1 = Diabetic (n = 530)
p7 <- ggplot(fram_df, aes(fill = as.factor(SEX), x = as.factor(DIABETES))) +
  geom_bar(position = "dodge") +
  labs(title = "Framingham: DIABETES by Sex", x = "Diabetic Condition", 
       y = "Count",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Not a diabetic", "Diabetic")) +
  theme(text = element_text(size=8))

p6 + p7 + p2 + p3 + p4 + p5 


# High Density Lipoprotein Cholesterol (mg/dL). Available for Period 3 only. 
# Values range from 10-189.
p8 <- ggplot(nhanes_df, aes(fill = as.factor(SEX), x = HDLC)) +
  geom_density(alpha = 0.6) +
  labs(title = "NHANES: HDLC by Sex", 
       x = "High Density Lipoprotein Cholesterol (mg/dL)", 
       y = "Density",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  theme(text = element_text(size=8))

# Serum Total Cholesterol (mg/dL). Values range from 107-696.
p9 <- ggplot(nhanes_df, aes(fill = as.factor(SEX), x = TOTCHOL)) +
  geom_density(alpha = 0.6) +
  labs(title = "NHANES: TOTCHOL by Sex", 
       x = "Serum Total Cholesterol (mg/dL)", 
       y = "Density",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  theme(text = element_text(size=8))

# Age at exam (years). Values range from 32-81.
p10 <- ggplot(nhanes_df, aes(fill = as.factor(SEX), x = AGE)) +
  geom_density(alpha = 0.6) +
  labs(title = "NHANES: Age by Sex", 
       x = "Age", 
       y = "Density",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  theme(text = element_text(size=8))

# Systolic Blood Pressure (mean of last two of three measurements) (mmHg). 
# Values range from 83.5-295.
p11 <- ggplot(nhanes_df, aes(fill = as.factor(SEX), x = SYSBP)) +
  geom_density(alpha = 0.6) +
  labs(title = "NHANES: SYSBP by Sex", 
       x = "Systolic Blood Pressure (mmHg)", 
       y = "Density",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  theme(text = element_text(size=8))

# Current cigarette smoking at exam. 0 = Not current smoker (n = 6598), 
# 1 = Current smoker (n = 5029).
p12 <- ggplot(nhanes_df, aes(fill = as.factor(SEX), x = as.factor(CURSMOKE))) +
  geom_bar(position = "dodge") +
  labs(title = "NHANES: CURSMOKE by Sex", x = "Current Smoking Status", 
       y = "Count",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Not current smoker", "Current smoker")) +
  theme(text = element_text(size=8))

# Diabetic according to criteria of first exam treated or first exam with 
# casual glucose of 200 mg/dL or more. 0 = Not a diabetic (n = 11097), 
# 1 = Diabetic (n = 530)
p13 <- ggplot(nhanes_df, aes(fill = as.factor(SEX), x = as.factor(DIABETES))) +
  geom_bar(position = "dodge") +
  labs(title = "NHANES: DIABETES by Sex", x = "Diabetic Condition", 
       y = "Count",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Not a diabetic", "Diabetic")) +
  theme(text = element_text(size=8))

p12 + p13 + p8 + p9 + p10 + p11

p14 <-  ggplot(fram_preds_df, aes(fill = as.factor(SEX), x = as.factor(CVD))) +
  geom_bar(position = "dodge") +
  labs(title = "Framingham: Predicted CVD by Sex", x = "CVD Occurence", y = "Count",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Did not occur", "Did occur")) +
  theme(text = element_text(size=8))

p15 <-  ggplot(nhanes_preds_df, aes(fill = as.factor(SEX), x = as.factor(CVD))) +
  geom_bar(position = "dodge") +
  labs(title = "NHANES: Predicted CVD by Sex", x = "CVD Occurence", y = "Count",
       fill = "Sex") +
  scale_fill_discrete(labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Did not occur", "Did occur")) +
  theme(text = element_text(size=8))

p1 + p14 + p15

# get summary statistics of comined data set using gtsummary
combine_all_df$SEX <- ifelse(combine_all_df$SEX == 1, "Male", "Female")
combine_all_df$CURSMOKE <- ifelse(combine_all_df$CURSMOKE == 1, "Current Smoker", 
                                  "Not Current Smoker")
combine_all_df$DIABETES <- ifelse(combine_all_df$DIABETES == 1, "Diabetic", 
                                  "Not Diabetic")
table1 <- combine_all_df %>%
  select(SOURCE, SEX, HDLC, TOTCHOL, AGE, SYSBP, CURSMOKE, DIABETES) %>%
  tbl_summary(by = SOURCE,
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              ),
              missing_text = "(Missing)") %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  modify_header(label ~ "**Variable**") %>%
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Data Source**") %>%
  modify_caption("**Table 1.** Summary statistics of Framingham and NHANES data sets.")

gt::gtsave(as_gt(table1), file = "project3_table1.png")

# afunction to calculate brier score in transportability analysis
brier_est <- function(df){
  #' @description brier score estimator for transportability analysis when target
  #' population does not include outcome of interest
  #' @param df a combined dataframe of target and source data
  #' @return brier score for the combined data
  
  # prediction model
  mod <- glm(SOURCE ~ log(abs(HDLC)) + log(abs(TOTCHOL)) + log(abs(AGE)) + 
               log(abs(SYSBP_UT+1)) + log(abs(SYSBP_T+1)) + CURSMOKE + DIABETES, 
             data= df, family= "binomial")
  
  prob <- predict(mod, type = "response")
  df$o_hat <- (1 - prob)/prob # inverse odds weight
  df_temp <- df[df$SOURCE == 1, ]
  # brier score estimate
  score <- sum(df_temp$o_hat*(df_temp$CVD - df_temp$PRED)^2) / 
    nrow(df[df$SOURCE == 0,])
  
  return(score)
  
}

BrierScore(fram_test_df$CVD[fram_test_df$SEX == 1],
           fram_test_df$PRED[fram_test_df$SEX == 1])
BrierScore(fram_test_df$CVD[fram_test_df$SEX == 2],
           fram_test_df$PRED[fram_test_df$SEX == 2])


# simulation 
n_target <- 1506
n_source <- 640
age_sd <- c(3, 8, 13, 18, 23)
corr <- c(0.01, 0.25, 0.5, 0.75, 0.99)

# create function for simulation
sim_fun <- function(){
  #' @description a simulation function that simulate a new combined data set
  #' with sample size of 1000 based on varying n_target and age_mean
  
  # create df for results
  res_df = data.frame(corr = numeric(0),
                      sd_age = numeric(0),
                      b_score = numeric(0),
                      b_women = numeric(0),
                      b_men = numeric(0))
  
  # loop through the varying factors and obtain brier for each df
  for (i in 1:length(corr)){
    for (j in 1:length(age_sd)){
      SEX <- sample(c(1, 2), n_target, replace=TRUE, prob=c(0.5,0.5)) # SEX
      HDLC <- rsnorm(n_target, 52, 16, xi = 3) # HDLC
      TOTCHOL <- rnorm(n_target, 186, 43) # TOTCHOL
      AGE <- abs(rnorm_pre(HDLC, mu = 62, sd = age_sd[j], r = corr[i])) # AGE
      BPMEDS <- sample(c(0, 1), n_target, replace=TRUE, prob=c(0.14, 0.86)) # BPMEDS
      SYSBP <- rnorm(n_target, 137, 20) # SYSBP
      CURSMOKE <- sample(c(0, 1), n_target, replace=TRUE, prob=c(0.84,0.16)) # CURSMOKE
      DIABETES <- sample(c(0, 1), n_target, replace=TRUE, prob=c(0.7,0.3)) # DIABETES
      CVD <- rep(NA, n_target)
      SOURCE <- rep(0, n_target)
      target_df <- data.frame(cbind(CVD, SEX, TOTCHOL, AGE, SYSBP, CURSMOKE, 
                                    DIABETES, BPMEDS, HDLC, SOURCE))
      target_df$SYSBP_UT <- ifelse(target_df$BPMEDS == 0, 
                                   target_df$SYSBP, 0)
      target_df$SYSBP_T <- ifelse(target_df$BPMEDS == 1, 
                                  target_df$SYSBP, 0)
      target_df <- target_df %>% select(-c(SYSBP, BPMEDS)) %>% 
        relocate(SYSBP_UT, .after = HDLC) %>%
        relocate(SYSBP_T, .after = SYSBP_UT)
      
      source_df <- fram_test_df %>%
        select(-ID, -PRED, -DIABP, -BMI, -SYSBP, -BPMEDS)
      source_df$SOURCE <- rep(1, n_source)
      
      sim_df <- rbind(target_df, source_df)
      sim_df$ID <- rep(1:2146)
      
      sim_women <- sim_df %>% filter(SEX == 2)
      sim_men <- sim_df %>% filter(SEX == 1)
      
      # obtain predictions from model
      sim_women$PRED <- predict(mod_women, sim_women, type = "response")
      sim_men$PRED <- predict(mod_men, sim_men, type = "response")
      sim_preds <- rbind(sim_men, sim_women) %>%
        select(ID, PRED) %>%
        arrange(ID)
      sim_df$PRED <- sim_preds$PRED
      
      # bind result
      res_df[dim(res_df)[1]+1,] <- c(corr[i], age_sd[j],
                                     brier_est(sim_df), brier_est(sim_women),
                                     brier_est(sim_men))
      
    }
  }
  return(res_df)
}

# repeat 1000 times 
n_sim <- 1000

sim_res <- replicate(n_sim, sim_fun())
sim_res_df <- data.frame(corr = numeric(0),
                         sd_age = numeric(0),
                         b_score = numeric(0),
                         women_score = numeric(0),
                         men_score = numeric(0))
# bind result into one dataframe
for (i in 1:n_sim){
  sim_res_df <- rbind(as.data.frame(sim_res[, i]), sim_res_df)
}

sim_res_df <- sim_res_df %>% 
  arrange(corr, sd_age)


# calculate performance of the brier score estimator
performance <- function(df){
  #' @param df a dataframe
  #' @return a list of calculated performance measures 
  
  # mean briers
  mean.est.brier <- mean(df$b_score)
  # brier from non-simulated data
  real_b <- 0.1777906
  corr <- df$corr[1]
  sd_age <- df$sd_age[1]
  
  # bias 
  bias <- sum(df$b_score -  real_b)/n_sim
  mc.bias.se <- sqrt(sum((df$b_score -  mean.est.brier)^2)/(n_sim*(n_sim-1)))
  
  # empse
  empse <- sqrt(sum((df$b_score -  mean.est.brier)^2)/(n_sim-1))
  mc.empse.se <- empse/sqrt(2*(n_sim-1))
  
  # MSE
  MSE <- sum((df$b_score - real_b)^2)/n_sim
  mc.MSE.se <- sqrt(sum((df$b_score - real_b)^2 - 
                          MSE)^2 / (n_sim*(n_sim-1)))
  
  
  
  res <- cbind(corr, sd_age, bias, mc.bias.se, empse, mc.empse.se, MSE, 
               mc.MSE.se)
  return(res)
}

# organize performance metrics
perf_res <- sim_res_df %>%
  group_split(grp = as.integer(gl(25, n_sim, n_sim*25)), .keep = FALSE)

# bind performance measures results
perf_df <- data.frame(bias = numeric(0),
                      bias.se = numeric(0),
                      empse = numeric(0),
                      empse.se = numeric(0),
                      mse = numeric(0),
                      mse.se = numeric(0))
for (i in 1:25){
  perf_df <- rbind(performance(perf_res[[i]]), perf_df)
}


# plot performance measures
p1 <- ggplot(perf_df) +
  geom_line(aes(x = corr, y = bias, 
                color = as.factor(sd_age))) +
  geom_point(aes(x = corr, y = bias, 
                 color = as.factor(sd_age))) +
  labs(x = "Correlation of AGE and SYSBP", y = "Bias", title = "Bias", color = "AGE SD")

p2 <- ggplot(perf_df) +
  geom_line(aes(x = corr, y = empse, 
                color = as.factor(sd_age))) +
  geom_point(aes(x = corr, y = empse, 
                 color = as.factor(sd_age))) +
  labs(x = "Sample Size of Target Data", y = "EmpSE", title = "EmpSE", color = "Age SD")

p3 <- ggplot(perf_df) +
  geom_line(aes(x = corr, y = MSE, 
                color = as.factor(sd_age))) +
  geom_point(aes(x = corr, y = MSE, 
                 color = as.factor(sd_age))) +
  labs(x = "Correlation of AGE and SYSBP", y = "MSE", title = "MSE", color = "AGE SD")

p1 + p3


fram_test_b <- c(BrierScore(fram_test_df$CVD[fram_test_df$SEX == 1],
                            fram_test_df$PRED[fram_test_df$SEX == 1]),
                 BrierScore(fram_test_df$CVD[fram_test_df$SEX == 2],
                            fram_test_df$PRED[fram_test_df$SEX == 2]))
combined_b <- c(brier_est(combined_men2),
                brier_est(combined_women2))
# sim_combined_b <- c(mean(perf_res[[8]]$b_men),
#                     mean(perf_res[[8]]$b_women))
sim_combined_b <- c(0.1054886, 0.0436755)
mods <- c("Men", "Women")
b_table <- data.frame(mods, fram_test_b, combined_b, sim_combined_b)
colnames(b_table) <- c("Model", "Framingham", "Framingham + NHANES", 
                       "Framingham + Simulated NHANES")
# kableone(b_table)
table3 <- b_table |> 
  gt() |>
  tab_caption(caption = md("**Table 3.** Brier Scores for non-simulated and simulated data sets."))
table3
gt::gtsave(table3, file = "project3_table3.png")
library(tidyverse)
library(data.table)
library(GGally)
library(ggplot2) # correlations
library(gtsummary)
library(tableone)
library(cowplot)
library(magick) 
library(patchwork) # arrange ggplots
library(gridExtra)
library(ggpubr)   # correlation plot


# read data
data <- read.csv("~/Downloads/project1.csv")

dim(data)
head(data)

# looking at mom_numcig
# in order to look at correlations, convert this variable to numeric

data$mom_numcig[data$mom_numcig %in% c("" , "None" , "44989" , 
                                       "2 black and miles a day")] <- NA
data$mom_numcig[data$mom_numcig == "20-25"] <- 20
data$mom_numcig <- as.numeric(data$mom_numcig)

# calculate missing percentage for each variables
missing <- round(apply(data, 2, function(x) sum(is.na(x)))/nrow(data), 4)
missing <- missing * 100

# remove row with all missing data
na_ind <- apply(data, 1, function(x) all(is.na(x)))
data <- data[!na_ind, ]

missing

# reorganizing the format of parent and child data
parent_data <- data[ ,1:51] %>%
  mutate(mom_smoke_16wk = case_when(mom_smoke_16wk == "2=No" ~ 0,
                                    mom_smoke_16wk == "1=Yes" ~ 1,
                                    mom_smoke_16wk == "" ~ NA)) %>%
  mutate(mom_smoke_22wk = case_when(mom_smoke_22wk == "2=No" ~ 0,
                                    mom_smoke_22wk == "1=Yes" ~ 1,
                                    mom_smoke_22wk == "" ~ NA)) %>%
  mutate(mom_smoke_32wk = case_when(mom_smoke_32wk == "2=No" ~ 0,
                                    mom_smoke_32wk == "1=Yes" ~ 1,
                                    mom_smoke_32wk == "" ~ NA)) %>%
  mutate(mom_smoke_pp1 = case_when(mom_smoke_pp1 == "2=No" ~ 0,
                                   mom_smoke_pp1 == "1=Yes" ~ 1,
                                   mom_smoke_pp1 == "" ~ NA)) %>%
  mutate(mom_smoke_pp2 = case_when(mom_smoke_pp2 == "2=No" ~ 0,
                                   mom_smoke_pp2 == "1=Yes" ~ 1,
                                   mom_smoke_pp2 == "" ~ NA)) %>%
  mutate(mom_smoke_pp12wk = case_when(mom_smoke_pp12wk == "2=No" ~ 0,
                                      mom_smoke_pp12wk == "1=Yes" ~ 1,
                                      mom_smoke_pp12wk == "" ~ NA)) %>%
  mutate(mom_smoke_pp6mo = case_when(mom_smoke_pp6mo == "2=No" ~ 0,
                                     mom_smoke_pp6mo == "1=Yes" ~ 1,
                                     mom_smoke_pp6mo == "" ~ NA))

# convert income from character type to numeric
parent_data$income <- as.numeric(parent_data$income) 
child_data <- data[ ,c(1, 52:78)]

dim(parent_data)
dim(child_data)

head(parent_data)

# long data for continuous variables
parent_long <- parent_data %>%
  select(parent_id, page, income, momcig, mom_numcig, cotimean_34wk,
         cotimean_pp6mo_baby, cotimean_pp6mo, swan_inattentive, 
         swan_hyperactive, bpm_att_p, bpm_ext_p, bpm_int_p,
         ppmq_parental_knowledge, ppmq_child_disclosure, ppmq_parental_solicitation,
         ppmq_parental_control, bpm_att_a, bpm_ext_a, bpm_int_a, erq_cog_a, 
         erq_exp_a)
parent_long <- pivot_longer(parent_long, cols = c(2:22), names_to = "variable",
                            values_to = "value")

child_long <- child_data %>%
  select(parent_id, bpm_att, bpm_ext, bpm_int, erq_cog, erq_exp, pmq_parental_knowledge,
         pmq_child_disclosure, pmq_parental_solicitation, pmq_parental_control)
child_long <- pivot_longer(child_long, cols = c(2:10), names_to = "variable",
                           values_to = "value")

# explore interrelatedness between SDP and ETS
sdp_data <- parent_data %>% filter(!is.na(mom_smoke_16wk)) %>%
  filter(!is.na(mom_smoke_22wk)) %>%
  filter(!is.na(mom_smoke_32wk)) %>%
  filter(!is.na(smoke_exposure_5yr)) %>%
  filter(!is.na(smoke_exposure_4yr)) %>%
  filter(!is.na(smoke_exposure_3yr)) %>%
  filter(!is.na(smoke_exposure_2yr)) %>%
  filter(!is.na(smoke_exposure_12mo)) %>%
  filter(!is.na(smoke_exposure_6mo)) 

# create intensities variables for sdp and ets
sdp_data$sdp <- ifelse(sdp_data$mom_smoke_32wk == 1, 3, 
                       ifelse(sdp_data$mom_smoke_22wk == 1, 2, 
                              ifelse(sdp_data$mom_smoke_16wk == 1, 1, 0)))

sdp_data$ets <-ifelse(sdp_data$smoke_exposure_5yr == 1, 6,
                      ifelse(sdp_data$smoke_exposure_4yr == 1, 5,
                             ifelse(sdp_data$smoke_exposure_3yr == 1, 4,
                                    ifelse(sdp_data$smoke_exposure_2yr == 1, 3,
                                           ifelse(sdp_data$smoke_exposure_12mo == 1, 2,
                                                  ifelse(sdp_data$smoke_exposure_6mo == 1, 1, 0))))))

# plot the intensity variables and see correlation strength
ggscatter(sdp_data, x = "sdp", y = "ets",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.method = "pearson",
          xlab = "Smoke During Pregnancy Intensity", 
          ylab = "Environmental Tabacco Smoke Intensity",
          title = "Correlation between SDP and ETS Intensities")

# calculate missing percentage for each variables
parent_missing <- round(apply(parent_data, 2, function(x) sum(is.na(x)))/nrow(parent_data), 4)
parent_missing <- parent_missing * 100

parent_missing_table <- as.data.frame(parent_missing)
parent_missing_table$variable <- colnames(parent_data)
parent_missing_table <- parent_missing_table[ c(15:31, 34:51),] %>%
  relocate(parent_missing, .after = variable) %>%
  select(parent_missing)

#kableone(parent_missing_table)

# calculate missing percentage for each variables
child_missing <- round(apply(child_data, 2, function(x) sum(is.na(x)))/nrow(child_data), 4)
child_missing <- child_missing * 100

child_missing_table <- as.data.frame(child_missing)
child_missing_table$variable <- colnames(child_data)
child_missing_table <- child_missing_table[ c(12:28),] %>%
  relocate(child_missing, .after = variable) 

# bind missing percentages to create table
child_na <- data.frame(variable = rep("", 18),
                       child_missing = rep("", 18))
child_missing_table <- rbind(child_missing_table, child_na)

parent_child_missing <- cbind(parent_missing_table, child_missing_table)
#kableone(child_missing_table)

# descriptive summary for selected variables of mothers
table1 <- parent_data %>%
  tbl_summary(include = c(page, paian, pasian, pnhpi,
                          pblack, pwhite, prace_other, mom_numcig,
                          mom_smoke_16wk, mom_smoke_22wk, mom_smoke_32wk,
                          mom_smoke_pp1, mom_smoke_pp2, mom_smoke_pp12wk,
                          mom_smoke_pp6mo, cotimean_34wk, cotimean_pp6mo,
                          cotimean_pp6mo_baby),
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n}  ({p}%)"
              ),
              missing_text = "NA")%>%
  modify_caption("**Table 1. Summary statistics for selected variables/information of mothers.**")
table1

# gt::gtsave(as_gt(table1), file ="project1_table1.png")



# create boxplots for continuous variables 
parent_long1 <- parent_long[parent_long$variable %in% c("bpm_att_p", 
                                                        "bpm_ext_p", "bpm_int_p",
                                                        "bpm_att_a", "bpm_ext_a",
                                                        "bpm_int_a", "erq_cog_a",
                                                        "erq_exp_a"),]
parent_long2 <- parent_long[parent_long$variable %in% c("ppmq_parental_knowledge",
                                                        "ppmq_child_disclosure",
                                                        "ppmq_parental_solicitation",
                                                        "ppmq_parental_control"),]
parent_long3 <- parent_long[parent_long$variable %in% c("swan_inattentive", 
                                                        "swan_hyperactive"),]
parent_long4 <- parent_long[parent_long$variable == "cotimean_34wk",]
parent_long5 <- parent_long[parent_long$variable == "cotimean_pp6mo",]
parent_long6 <- parent_long[parent_long$variable == "cotimean_pp6mo_baby",]
# a function to create boxplots
boxplot_func <- function(df) {
  #' @description creat multiple boxplots for the given data
  #' @param df a dataframe
  #' @return a set of boxplots for the given data
  
  ggplot(df, aes(variable, value, fill = variable)) + 
    geom_violin(alpha = 0.7) + # visualizes the shape of the distribution as well
    geom_boxplot(alpha = 0.7) +
    scale_x_discrete(name = "Variable") +
    scale_y_continuous(name = "Value") +
    ggtitle("Boxplots for Selected Variables") +
    theme(axis.text.x = element_blank())
}

parent_boxplot1 <- boxplot_func(parent_long1)
parent_boxplot2 <- boxplot_func(parent_long2)
parent_boxplot3 <- boxplot_func(parent_long3)
parent_boxplot4 <- boxplot_func(parent_long4)
parent_boxplot5 <- boxplot_func(parent_long5)
parent_boxplot6 <- boxplot_func(parent_long6)
# parent_boxplot1
# parent_boxplot2
# parent_boxplot3
# parent_boxplot4
# parent_boxplot5
# parent_boxplot6

project1_boxplots1 <- parent_boxplot1 / (parent_boxplot3 + parent_boxplot4 +
                                           parent_boxplot5 +parent_boxplot6) +
  plot_annotation( title = 'Figure 1: Multiple box plots for selected variables in parent data.',
                   theme = theme(plot.title = element_text(size = 13)))
project1_boxplots1
# ggsave("project1_boxplots1.jpg", project1_boxplots1)


# summary statistics for child data
table2 <- child_data %>%
  tbl_summary(include = c(taian, tasian, tnhpi,
                          tblack, twhite, trace_other, cig_ever, e_cig_ever,
                          mj_ever, alc_ever),
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n}  ({p}%)"
              ),
              missing_text = "NA")%>%
  modify_caption("**Table 2. Summary statistics for selected variables/information of adolescents.**")
table2

# gt::gtsave(as_gt(table2), file ="project1_table2.png")

project1_table1 <- ggdraw() + draw_image("project1_table1.png")
project1_table2 <- ggdraw() + draw_image("project1_table2.png", scale = 0.8)
project1_table12 <- plot_grid(project1_table1, project1_table2)
# ggsave("project1_table12.jpg", project1_table12)

# create boxplots for continuous variables 
child_long1 <- child_long[child_long$variable %in% c("bpm_att", 
                                                     "bpm_ext", "bpm_int"),]
child_long2 <- child_long[child_long$variable %in% c("erq_cog", "erq_exp"),]
child_long3 <- child_long[child_long$variable %in% c("pmq_parental_knowledge",
                                                     "pmq_child_disclosure",
                                                     "pmq_parental_solicitation",
                                                     "pmq_parental_control"),]

child_boxplot1 <- boxplot_func(child_long1)
child_boxplot2 <- boxplot_func(child_long2)
child_boxplot3 <- boxplot_func(child_long3)
child_boxplot1
child_boxplot2
child_boxplot3

project1_boxplots2 <- child_boxplot3 / (child_boxplot1 + child_boxplot2) +
  plot_annotation( title = 'Figure 2: Multiple box plots for selected variables in child data.', theme = theme(plot.title = element_text(size = 13)))
project1_boxplots2
# ggsave("project1_boxplots2.jpg", project1_boxplots2)

# check smoke exposure in child
smoke_exposure_df <- data[ , c("parent_id", "smoke_exposure_6mo", "smoke_exposure_12mo",
                               "smoke_exposure_2yr", "smoke_exposure_3yr",
                               "smoke_exposure_4yr", "smoke_exposure_5yr")] 

smoke_exposure_df <- melt(smoke_exposure_df, id.vars = "parent_id",
                          variable.name = "smoke_exposure_time")
smoke_exposure_df <- smoke_exposure_df %>% group_by(smoke_exposure_time) %>% 
  summarize(yes = length(value[value==1]), no = length(value[value ==0]))

smoke_exposure_df <- melt(smoke_exposure_df, id.vars = "smoke_exposure_time",
                          variable.name = "smoke_exposure_bin")

ggplot(smoke_exposure_df, aes(x = smoke_exposure_time, 
                              y = value, fill = smoke_exposure_bin)) +
  geom_bar(position="dodge", stat = "identity")


# calculate smoke exposure duration
data$smoke_exposure_duration <- (data$smoke_exposure_6mo - 0.5) * (data$smoke_exposure_6mo != 0) +
  (data$smoke_exposure_12mo - 0.5) * (data$smoke_exposure_12mo != 0) +
  data$smoke_exposure_2yr + data$smoke_exposure_3yr + data$smoke_exposure_4yr +
  data$smoke_exposure_5yr

# calculate mom smoke duration during pregnancy
data$mom_smoke_pregnant <- parent_data$mom_smoke_16wk + 
  parent_data$mom_smoke_22wk + parent_data$mom_smoke_32wk

# calculate mom smoke duration after pregnancy
data$mom_smoke_post <- parent_data$mom_smoke_pp12wk + parent_data$mom_smoke_pp6mo



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


# use ggally for correlations and scatterplot
ggpair_df3 <- data[, c("smoke_exposure_duration", "bpm_att", "bpm_ext",
                       "bpm_int", "erq_cog", "erq_exp")]

corplot1 <- ggpairs(ggpair_df3, columns = 1:ncol(ggpair_df3), 
                    lower = list(continuous = wrap(lower_func, method = "lm")), 
                    title = "Figure 3: Scatter plots and correlations for smoke exposures duration and self-regulation variables",  
                    axisLabels = "show", columnLabels = colnames(ggpair_df3))
corplot1 <- corplot1 + theme(plot.title = element_text(size = 10))
ggsave("corplot1.jpg", corplot1)

# use ggally for correlations and scatterplot
ggpair_df4 <- data[, c("mom_smoke_pregnant", "bpm_att", "bpm_ext",
                       "bpm_int", "erq_cog", "erq_exp")]

corplot2 <- ggpairs(ggpair_df4, columns = 1:ncol(ggpair_df4), 
                    lower = list(continuous = wrap(lower_func, method = "lm")), 
                    title = "Figure 4: Scatter plots and correlations for smoke during pregnancy and self-regulation variables",  
                    axisLabels = "show", columnLabels = colnames(ggpair_df4))
corplot2 <- corplot2 + theme(plot.title = element_text(size = 10))
ggsave("corplot2.jpg", corplot2)


# use ggally for correlations and scatterplot
ggpair_df1 <- data[, c("mom_smoke_pregnant", "cotimean_34wk", 
                       "cotimean_pp6mo",
                       "bpm_att", "bpm_ext", 
                       "bpm_int")]

ggpairs(ggpair_df1, columns = 1:ncol(ggpair_df1), 
        lower = list(continuous = wrap(lower_func, method = "lm")), 
        title = "",  
        axisLabels = "show", columnLabels = colnames(ggpair_df1))


# use ggally for correlations and scatterplot
ggpair_df2 <- data[, c("momcig", "bpm_att", "bpm_ext",
                       "bpm_int", "erq_cog", "erq_exp")]

ggpairs(ggpair_df2, columns = 1:ncol(ggpair_df2), 
        title = "",  
        lower = list(continuous = wrap(lower_func, method = "lm")), 
        axisLabels = "show", columnLabels = colnames(ggpair_df2))

# fit simple linear models to see main effects of smoke exposure on self-regulation
lm1 <- lm(bpm_att ~ smoke_exposure_duration + mom_smoke_pregnant + mom_smoke_post +
            mom_numcig + momcig, data = data)

summary(lm1)

lm2 <- lm(erq_exp ~ smoke_exposure_duration + mom_smoke_pregnant + mom_smoke_post +
            mom_numcig + momcig, data = data)

summary(lm2)
## ============================================================================== ##
# --- Take-home challenge: understanding MDD treatments in routine clinical care
# --- Date: 10/07/2024
# --- Author: L Hadaya
## ============================================================================== ##

### === Load libraries === ####
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(reshape2)

### === Load data === ####
clinical_data <- read_csv("clinical_data.csv")
bill_amount <- read_csv("bill_amount.csv")
bill_id <- read_csv("bill_id.csv")
demographics <- read_csv("demographics.csv")

### === Preliminary pre-processing/quality checks/understanding the data === ####
# understand data
summary(clinical_data)
summary(demographics)
summary(bill_amount)
summary(bill_id)

str(clinical_data)
str(demographics)
str(bill_amount)
str(bill_id)

View(clinical_data)
View(demographics)
View(bill_amount)
View(bill_id)

# checking for any duplicate IDs
duplicate_cases <- sum(duplicated(clinical_data$id)) 
duplicate_cases # there are 400 duplicate IDs - indicating 400 patients have been re-admitted (potential marker of relapse)

unique_cases <- length(unique(clinical_data$id)) 
unique_cases # n=3000 unique cases

row_duplicates <- clinical_data$id %in% clinical_data$id[which(duplicated(clinical_data$id))] # which rows contain duplicates
date_admission_duplicates <- clinical_data[row_duplicates, c("id", "date_of_admission", "date_of_discharge")]
View(date_admission_duplicates[order(date_admission_duplicates$id, decreasing = TRUE), ]) #eyeballing to check that they have different admission dates 

(duplicate_cases/unique_cases)*100 # 13.33% of patients re-admitted 

# check for any missing data in key variables of interest: 
sum(is.na(clinical_data[,c("trt_ssr", 
                           "trt_anx", 
                           "trt_con", 
                           "trt_adt", 
                           "trt_the", 
                           "trt_oth",
                           "medical_history_anx" ,  
                           "medical_history_mood")])) # no missing data 

### === Data analysis ====
### ===== a) Frequencies of patients taking each treatment option and total number of treatments === ####
###### -----  Calculate frequencies of each treatment
treatment_frequencies <- colSums(clinical_data[,c("trt_ssr", "trt_anx", "trt_con", "trt_adt", "trt_the", "trt_oth")])
names(treatment_frequencies)
treatment_frequencies_df <- data.frame(Treatment = names(treatment_frequencies), Freq = treatment_frequencies)

# Calculate %s 
treatment_frequencies_df$percent <- (treatment_frequencies_df$Freq/dim(clinical_data)[1])*100
treatment_frequencies_df

# bar plot - total number of patients taking each treatment option
treatment_frequencies_df %>% 
  ggplot(aes(x = Treatment, y=Freq, fill=Treatment)) +
  geom_bar(stat="identity") +
  theme_minimal()+
  labs(y = "Frequency", x = "Treatment")+
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")+
  geom_text(aes(label=Freq), position = position_dodge(0.90), size = 2.5, 
            vjust=-1.25, colour = "gray25")

###### ----- Calculate frequencies of number of treatments people are on  
clinical_data <- clinical_data %>%
  mutate(treatment_combo_total = rowSums(clinical_data[,c("trt_ssr", "trt_anx", "trt_con", "trt_adt", "trt_the", "trt_oth")]))
range(clinical_data$treatment_combo_total)
table(clinical_data$treatment_combo_total)

# percentages: 
(table(clinical_data$treatment_combo_total)/3400)*100

# bar plot - total number of people taking different number of treatments
clinical_data %>% 
  ggplot(aes(x = treatment_combo_total)) +
  geom_bar() +
  theme_minimal()+
  labs(y = "Frequency", x = "Number of treatments")+
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")+
  geom_text(stat = "count", aes(label=after_stat(count)), position = position_dodge(0.90), size = 2.5, 
            vjust=-1.25, colour = "black")


### ===== b) Frequencies of patients taking each treatment option and total number of treatments, stratified by psych co-morbidities === ####
###### -----  Calculate number of co-occurring psych conditions
clinical_data <- clinical_data %>%
  mutate(psych_comorbidities = as.factor(rowSums(clinical_data[,c("medical_history_anx" ,  "medical_history_mood")])))
str(clinical_data$psych_comorbidities)

###### -----  Calculate frequencies of each treatment, grouped by psych_comorbidities
treatment_frequencies_grouped <- aggregate(. ~ psych_comorbidities, 
                                            data = clinical_data[, c("psych_comorbidities", "trt_ssr", "trt_anx", "trt_con", "trt_adt", "trt_the", "trt_oth")], 
                                            FUN = sum)
# melt the data frame
treatment_frequencies_grouped_long_format <- melt(treatment_frequencies_grouped, 
                    id.vars = "psych_comorbidities", 
                    variable.name = "treatment", 
                    value.name = "frequency")
treatment_frequencies_grouped_long_format$treatment <- as.character(treatment_frequencies_grouped_long_format$treatment)

# plot bar plot: frequency of each treatment taken by each psych comorbidity group
treatment_frequencies_grouped_long_format %>% 
  ggplot(aes(x = treatment, y = frequency, fill=psych_comorbidities)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(y = "Frequency", x = "Treatments", fill="Psychiatric comorbidities")+
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2")+
  geom_text(aes(label=frequency), position = position_stack(0.90), size = 2.5, 
            vjust=0.5, colour = "gray25")

treatment_frequencies_grouped # get absolute numbers


# bar plot - co-morbidities x number of treatments
clinical_data %>% 
  ggplot(aes(x = treatment_combo_total, fill=psych_comorbidities)) +
  geom_bar() +
  theme_minimal()+
  labs(y = "Frequency", x = "Number of treatments", fill="Psychiatric comorbidities")+
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2")+
  geom_text(stat = "count", aes(label=after_stat(count)), position = position_stack(0.90), size = 2.5, 
            vjust=0.5, colour = "gray25")

table(clinical_data$psych_comorbidities, clinical_data$treatment_combo_total) # get absolute numbers

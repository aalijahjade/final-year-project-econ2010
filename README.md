# final-year-project-econ2010

```{r Libraries}
# Load libraries

  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tidyverse)
  library(rempsyc)
  library(tinytex)
  library(kableExtra)
  library(gtsummary)
  library(ggpubr)
  library(rstatix)
  library(ggstatsplot)
  library(PMCMRplus)
  library(nortest)
  library(Hmisc)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  library(texreg)
  library(psych)
  library(corrplot)
  library(see)
  library(performance)
  library(huxtable)
  library(jtools)
  library(cluster) # for cluster analysis 
  library(Rtsne)
  library(factoextra)
  library(dendextend)

```
```{r, Importation of the dataset}
# Import data set 

  firms <- read_sav("C:/Users/white/Downloads/IFPG_COVID19_Final_Data_2020.sav")
    View(firms) 
```

```{r Sampling}
# Set seed for reproducibility

  set.seed(763)

# Create a 90% sample of the dataset 

  sample_indices <- sample(nrow(firms), size = 0.9 * nrow(firms))
   firm_sample <- firms[sample_indices, ]

# Inspect sample
    firm_sample %>%
    head() %>%
    glimpse()
    
```

```{r Descriptive Statistics}
# Demographics 
# COUNRTY-------------------------------------------------------------------------------
# Re-code the COUNTRY column to show the country names instead their country codes
  
  sample <- firm_sample
    country <- c(`123` = "Jamaica", `134` = "Antigua and Barbuda", `137` = "Barbados", 
                      `138` = "Dominica", `139` = "Grenada", `140` = "Guyana", 
                      `141` = "St Kitts and Nevis", `142` = "St Lucia", `143` = "St Vincent",
                      `144` = "Suriname", `155` = "Belize", `334` = "The Bahamas", 
                      `780` = "Trinidad and Tobago")
    sample$COUNTRY <- as.character(sample$COUNTRY) 
    sample$COUNTRY <- country[sample$COUNTRY]   

      
# Firm type-----------------------------------------------------------------------------
# Re-code B1's column to show the names of the types of firm in the Caribbean
# The variable name was changed from B1 to firm_type to make the variable more recognizable for analysis
      
  sample$firm_type <- ifelse(sample$B1 %in% c(1, 2), "Shareholding firms",
                       ifelse(sample$B1 %in% c(4, 5), "Partnerships", "Sole proprietorship"))
    sample <- sample[!is.na(sample$firm_type), ]  #To remove NAs from the firm_type column
    
  
# Calculating percentages for each firm type
   
  dummy_sample1 <- sample %>%
    count(firm_type) %>%
      mutate(percentage = n / sum(n) * 100)
      
   
# Creating a bar chart with percentages
     
   ggplot(dummy_sample1, aes(x = reorder(firm_type, percentage), y = percentage, fill = firm_type, label = sprintf("%.1f%%", percentage))) +
    geom_bar(stat = "identity") +     # Use data values as visual ones
    geom_text(vjust = -0.5) +
    labs(title = "Type of Firms in the Region", 
             x = "Firm Type", 
             y = "Percentage") +
    scale_fill_brewer(palette = "Set3") + # Change the color of each bar to represent the data value
    theme_minimal() +
    ggeasy::easy_center_title()

      
# Main Market--------------------------------------------------------------------------------
# Re-code the D7A column to display the name of the market the firm produces for
  
  sample$D7A <- ifelse(sample$D7A == 1, "local",
                 ifelse(sample$D7A == 2, "national",
                        ifelse(sample$D7A == 3, "international", NA)))

# Calculating percentages for each market in 'D7A'

  dummy_sample2 <- sample %>%
    filter(!is.na(D7A)) %>%
      count(D7A) %>%
        mutate(percentage = n / sum(n) * 100)

  
# Creating a pie chart with percentages shown for each section

  ggplot(dummy_sample2, aes(x = "", y = percentage, fill = D7A, label = sprintf("%.1f%%", percentage))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(position = position_stack(vjust = 0.5)) +
    labs(title = "Main Markets", 
             x = NULL, 
             y = NULL, 
          fill = "Main Market") +
    theme_void() +
    scale_fill_brewer(palette = "Accent") +
    ggeasy::easy_center_title()
      

# Industry----------------------------------------------------------------------------------
# Assuming sampling_stratum is a factor with levels 1 and 2, re-code the column
# The variable name was changed from SAMPLING_STRATUM to industry to make the variable more recognizable
   
  sample$industry <- factor(sample$SAMPLING_STRATUM, levels = c(1, 2), labels = c("Manufacturing", "Services"))
  
  
# Calculating percentages within each country and sampling_stratum_group
  
  dummy_sample3 <- sample %>%
    count(COUNTRY, industry) %>%
      group_by(COUNTRY) %>%
        mutate(percentage = n / sum(n) * 100)

  
# Creating a stacked bar chart with percentages shown on the bars

  ggplot(dummy_sample3, aes(x = COUNTRY, y = percentage, fill = industry, label = sprintf("%.1f%%", percentage))) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(position = position_stack(vjust = 0.5), size = 3) +
    labs(title = "Distribution of Industries in the Region",
          x = "Country", 
          y = "Percentage (%)", 
          fill = "Industry") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
    scale_fill_brewer(palette = "burlywood") +
    ggeasy::easy_center_title()


   
# Firm size----------------------------------------------------------------------------------
# Re-code the H2A1A column of the data frame to represent firm size 
# The variable name was changed from H2A1A to firm_size to make the variable more recognizable
   
  sample$firm_size <- ifelse(sample$H2A1A <= 20, "small",
                   ifelse(sample$H2A1A >= 21 & sample$H2A1A <= 99, "medium",
                          ifelse(sample$H2A1A >= 100, "large", sample$H2A1A)))
  
  
# Calculating percentages for each category in 'size'
    
  dummy_sample4 <- sample %>%
    filter(!is.na(firm_size)) %>%
      count(firm_size) %>%
        mutate(percentage = n / sum(n) * 100)

  
# Creating a bar chart with percentages shown on the bars
  
  ggplot(dummy_sample4, aes(x = firm_size, y = percentage, fill = firm_size, label = sprintf("%.1f%%", percentage))) +
    geom_bar(stat = "identity") +
    geom_text(vjust = -0.5) +
    labs(title = "Distribution of Firm Sizes in the Caribbean", 
             x = "Firm Size", 
             y = "Percentage") +
    theme_minimal() + 
    scale_fill_brewer(palette = "Purples") +
    ggeasy::easy_center_title()

      
# Age of firm-----------------------------------------------------------------------------------------
# Add a column to the dataset called age_of_firm 
  sample <- sample %>%
    mutate(age_of_firm = 2020 - B4A)

      
# Create a graph to present the data
  
  ggplot(sample, aes(x = age_of_firm)) +
    geom_freqpoly(binwidth = 5) +  # used a bandwidth of 5 to get a smoother estimate
    labs(title = "Age Distribution of Establishments in the Caribbean",
             x = "Age of the establishment",
             y = "Frequency") +
    theme_minimal() +
    ggeasy::easy_center_title() 
      
      
# Gender-------------------------------------------------------------------------------------
# Re-coding B5A to display those observations that are male-owned, female-owned and both 

  sample$B5A <- ifelse(sample$B5A == 1 | sample$B5A == 2, "Male owned",
                      ifelse(sample$B5A == 4 | sample$B5A == 5, "Female owned", "Both"))
        sample <- sample[!is.na(sample$B5A), ] #to remove NAs from the B5A column
      
        
# Calculating percentages for each gender category

  dummy_sample5 <- sample %>%
    count(B5A) %>%
      mutate(percentage = n / sum(n) * 100)

  
# Creating a bar chart with percentages shown on the bars and each bar in a different color

  ggplot(dummy_sample5, aes(x = B5A, y = percentage, fill = B5A, label = sprintf("%.1f%%", percentage))) +
    geom_bar(stat = "identity") +
    geom_text(vjust = -0.5) +
    labs(title = "Distribution of Gender Ownership in the Caribbean", 
             x = "Gender", 
             y = "Percentage") +
    theme_minimal() +
    scale_fill_brewer(palette = "GnBu") +
    ggeasy::easy_center_title()
    
      
```


```{r GOAL 1}
# Descriptive Statistics--------------------------------------------------------------------
# Select all the columns that will be used for the analysis and filter out all special codes 
  
  sampled_data1 <- sample %>%
    select(I8A3, COV_B2A, COV_B2A_X, industry) %>% 
      filter(I8A3 > 0, COV_B2A_X > 0, COV_B2A > 0)


# Re-code the COV_B2A column to represent the percentage increase or decrease in total sales 

  sampled_data1$COV_B2A_recode <- ifelse(sampled_data1$COV_B2A_X == 1, sampled_data1$I8A3 * (sampled_data1$COV_B2A/100), 
                                ifelse(sampled_data1$COV_B2A_X == 2, abs(sampled_data1$I8A3 * (- sampled_data1$COV_B2A/100)), 
                                  sampled_data1$I8A3))


# Create a graph to show the relationship between I8A3 and COV_B2A

  ggplot(sampled_data1) +
    geom_qq(aes(sample = log(COV_B2A_recode), color = "Total Sales During The Pandemic")) +
    geom_qq(aes(sample = log(I8A3), color = "Total Sales Before The Pandemic")) +
    scale_color_manual(name = "Variable", values = c("Total Sales During The Pandemic" = "lightgreen", "Total Sales Before The Pandemic" = "mediumslateblue")) +
    labs(title = "Distribution of Total Sales Before and Before COVID-19",
             x = "Quantile",
             y = "Count") +
    facet_wrap(~industry)


# Inferential test
# Independent Sample T Test-----------------------------
  
  t.test.results <- nice_t_test(data = sampled_data1,
              response = "COV_B2A_recode",
               group = "industry",
              warning = FALSE, 
              var.equal = TRUE)
  
  t.test.table <- nice_table(t.test.results)
  t.test.table


# Box-plot to show the violation of the test  

  ggplot(sampled_data1, aes(industry, COV_B2A_recode, fill = industry)) +
    geom_boxplot() +
    labs(title = "Boxplot of Total Sales During the Pandemic", 
             x = "Industry", 
             y = "COV_B2A") +
    scale_y_log10() + 
    scale_fill_brewer(palette = "Accent")
  

# Conduct a Non-parametric test since the assumptions were violated 
# Mann Whitney Test
# -----------------------------------------------------------------------------
# Filter data for the two groups

  group1 <- sampled_data1 %>% 
    filter(industry == "Manufacturing") %>% 
      pull(COV_B2A_recode) 
  
  group2 <- sampled_data1 %>% 
    filter(industry == "Services") %>% 
      pull(COV_B2A_recode)


# Perform the Mann-Whitney U test 

  wilcox.test(group1, group2, alternative = "two.sided")


```


```{r GOAL 2}
# Descriptive Statistics---------------------------------------------------------------------------
# Select the columns that will be used 

  sampled_data2 <- sample %>%
    select(E1B1, COV_B1A_X, COV_B1A, firm_size) %>%
  
# Filter out all -99, -44 and all other special interviewer codes from the E1B1, and COV_B1A_X columns
      
    filter(COV_B1A_X >= 0) %>%

# Factor the COV_B1A_X column into 2 levels
      
    mutate(covid_impact = factor(COV_B1A_X, levels = 1:2, labels = c("Increase", "Decrease"))) %>%
      drop_na() #to remove all NA values from the data frame


# Calculating the data values for each size category
  
  dummy_sampled_data2 <- sampled_data2 %>%
    count(firm_size, covid_impact)
  
  
# Graph the variables 
  ggplot(dummy_sampled_data2, aes(x = firm_size, y = n, fill = covid_impact, label = n)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
    labs( title = "Distribution of Capacity Levels",
              x = "Firm Size", 
              y = "Frequency") +
    theme_minimal() +
    scale_fill_brewer(palette = "YlGnBu") +
    ggeasy::easy_center_title()


# Inferential Statistics----------------------------------------------------------------------
# Convert the COV_B2A_X column's values to an integer to make it easier to showcase the percentage increase/decrease in capacity utilization
  
  sampled_data2$COV_B1A_X <- as.numeric(sampled_data2$COV_B1A_X) 
    sampled_data2$COV_B1A_recode <- ifelse(sampled_data2$COV_B1A_X == 1, sampled_data2$COV_B1A, 
                                  ifelse(sampled_data2$COV_B1A_X == 2, - (sampled_data2$COV_B1A), 
                                    NA))

    
# Select and rename the columns to be used
      
  sampled_data2 <- sampled_data2 %>%
    select(E1B1, COV_B1A_recode) %>%
      rename(pre = E1B1, post = COV_B1A_recode)

  
# Creating an observation id

  sampled_data2$observation_id <- seq.int(nrow(sampled_data2))

  
# Sub-setting observation id and pre and calling it pre

  pre <- sampled_data2[, c("observation_id", "pre")]

  
# Changing the name of pre to percentage and adding a column called time

  colnames(pre)[2] <- "percent"
    pre$time <- "pre"

  
# Doing the same for post

  post <- sampled_data2[, c("observation_id", "post")]
    colnames(post)[2] <- "percent"
      post$time <- "post"

      
# Merge both data frames

  capacity <- rbind(pre, post)


# Conduct the Paired Sample T Test 

  capacity %>%
    filter(complete.cases(.)) %>%
    group_by(observation_id) %>%
    filter(n() == 2) %>%
    ungroup() %>%
    tbl_summary(by = time, include = -observation_id,
                statistic = list(all_continuous() ~ "{mean} ({sd})")
                ) %>%
    add_p(test = list(
      percent   ~ "paired.t.test"),
      group    = observation_id)


# Calculating the Cohen's d 

  cohens_d(capacity, percent ~ time, paired = TRUE)


# Checking for outliers in the data

  ggplot(capacity, aes(x = time, fill = time, y = percent)) + 
    geom_boxplot() +
    labs(title = "Box of Pre- and Post-pandemic Capcity Levels", 
             x = "Capacity Levels (time)", 
             y = "Percentage")


# Nonparametric test: Wilcoxon Signed Rank Test  
#------------------------------------------------------------------------------------------------
# Filter data for the two groups: before and after

  before <- capacity %>% 
    filter(time == "pre") %>% 
      pull(percent) 

  after <- capacity %>% 
    filter(time == "post") %>% 
      pull(percent)

  
# Perform the Wilcoxon Signed Rank Test

  wilcox.test(before, after, paired = TRUE)

```

```{r GOAL 3}
# Select all columns that will be used to conduct the test 
  
  sample_data3 <- sample %>%
    select(COV_A5, industry, firm_size, firm_type, age_of_firm, D13B4) %>%
    filter(D13B4 > -44)
  

 sample_data3$D13B4 <- ifelse(sample_data3$D13B4 %in% c(1, 2, 3), "yes", "no")


# Prepare the data by factoring each categorical variable
sample_data3 <- sample_data3 %>%
  mutate(
    resilience = factor(COV_A5, levels = c("1", "2")),
    size = factor(firm_size, levels = c("small", "medium", "large")),
    industry = factor(industry, levels = c("Manufacturing", "Services")),
    type = factor(firm_type, levels = c("Shareholding firms", "Partnerships", "Sole proprietorship")),
    age = age_of_firm, 
    innovation = factor(D13B4, levels = c("yes", "no"))
       )


# Run the Logistic regression 
  
  logitmodel <- glm(resilience ~ size + industry + type + age + innovation, data = sample_data3, family = "binomial")
 
   
# Model Output Version 1  
  tab_model(logitmodel)  
  
  
# Model Output Version 2   
  summ(logitmodel, exp = TRUE)  # produces a table that presents the test output nicely



```


```{r GOAL 4}
# Filter out all the special interviewer codes (i.e. -77. -88. etc) and select the columns to be worked with
  
  sampled_data4 <- sample %>%
    filter(C15A > 0, COV_A7A > 0) %>%
      select(industry, C15A, COV_A7A)


# Create graph to show the main challenges faced by firms before and during the pandemic

  ggplot(sampled_data4, aes(x = industry, y = factor(COV_A7A))) +
    geom_jitter(aes(color = "After COVID-19")) +
    geom_jitter(aes(y = C15A, color = "Before COVID-19")) +
    labs(title = "Main Challenges Faced By Firms Before and Since the Advent of COVID-19",
             y = "Main challenges Faced") +
    theme_minimal() +
    scale_color_manual(values = c("black", "lightblue"), 
                       labels = c("Before COVID-19", "After COVID-19")) +
    theme(legend.position = "right")

  
# Inferential Test---------------------------------------------------------------------------
# Rename columns C15A to pre and COV_A7A to post
  
  sampled_data4 <- sample %>%
    rename(pre = C15A, post = COV_A7A) %>%
      select(firm_type, pre, post) %>%
# Re-code the pre and post columns
    mutate(pre_post_recode = ifelse(pre == post, 1, 2))

  
# Create a bar graph to present the data

  plot_xtab (
    x = sampled_data4$firm_type ,
    grp = sampled_data4$pre_post_recode ,
    margin = "row",
    bar.pos = "stack",
    show.summary = TRUE , 
    coord.flip = TRUE )

# Analysis-squared analyis 

  tab_xtab(var.row = sampled_data4$firm_type ,
  var.col = sampled_data4$pre_post_recode ,
  title = "Chi Squared Analysis",
  var.labels = c("Firm Type" , "Similarities vs Differences"), #used to change variable names
  show.row.prc = TRUE) #shows Column Percentages


```


```{r GOAL 5, Index Creation}

# Define 'sample' data set first

sample1 <- sample %>%
  select(COUNTRY, SAMPLING_STRATUM)

# Select columns from 'sample' data set to create 'sample_data5'

sample_data5 <- sample %>%
  select(D7C, E1B1, COV_B1A, COV_D1A1A_X, COV_A7A, COV_B2A1)

# Merge the two data sets using cbind()

merged_data <- cbind(sample_data5, sample1)


# Remove all numbers less than 0 from e1b1 and cov_b1a columns

merged_data <- merged_data %>% filter(COV_B1A > -44)
merged_data <- merged_data %>% filter(COV_D1A1A_X > -44)
merged_data <- merged_data %>% filter(D7C > -77)
merged_data <- merged_data %>% filter(COV_B2A1 > -44)

# Recode COV_B2A1 column

merged_data$COV_B2A1 <- ifelse(merged_data$COV_B2A1 == 2 | merged_data$COV_B2A1 == 3, "moderate risk", 
                      ifelse(merged_data$COV_B2A1 == 4 | merged_data$COV_B2A1 == 5, "high risk", "no risk"))
# Convert the recoded variable back to a factor
    merged_data$COV_B2A1 <- as.factor(merged_data$COV_B2A1)

    
# Calculate the change in capacity utilization
# Subtract cov_b1a from e1b1

merged_data$capacity_difference <- merged_data$E1B1 - merged_data$COV_B1A


# Determine increase, decrease, or no change

merged_data$capacity_status <- ifelse(merged_data$capacity_difference > 0, "increase",
                    ifelse(merged_data$capacity_difference < 0, "decrease", "no change"))


# Recode D7C column

merged_data$D7C <- factor(merged_data$D7C, levels = c(1, 2, 3), labels = c("increased", "remained the same", "decreased"))


# Recode COV_D1A1_X colummn 

merged_data$COV_D1A1A_X <- factor(merged_data$COV_D1A1A_X, levels = c(1, 2, 3), labels = c("increase", "decrease", "no change"))

# Index Creation ------------------------------------------------------------
# Recode the variables 

index_data <- merged_data %>%
  mutate(
    D7C = case_when(
      D7C == "increased" ~ "1",
      D7C == "decreased" ~ "2",
      D7C == "remained the same" ~ "0",
      TRUE ~ NA_character_
    ),
    COV_D1A1A_X = case_when(
      COV_D1A1A_X == "increase" ~ "1",
      COV_D1A1A_X == "decrease" ~ "2",
      COV_D1A1A_X == "no change" ~ "0",
      TRUE ~ NA_character_
    ),
    capacity_status = case_when(
      capacity_status == "increase" ~ "1",
      capacity_status == "decrease" ~ "2",
      capacity_status == "No change" ~ "0",
      TRUE ~ NA_character_
    ), 
    COV_B2A1 <- case_when(
      COV_B2A1 == "no risk" ~ "0", 
      COV_B2A1 == "moderate risk" ~ "1",
      COV_B2A1 == "high risk" ~ "2",     #this was done so that higher values are linekd to a negative impact 
      TRUE ~ NA_character_
    )
  ) %>%
  select(COUNTRY, SAMPLING_STRATUM, COV_B2A1, capacity_status, COV_D1A1A_X, D7C) %>%
  na.omit()

subset_data <- index_data[, 3:6] %>%
  na.omit()

# Convert data to numeric to make it easier to create the index 

subset_data_numeric <- subset_data %>%
  mutate(across(where(is.factor), as.numeric),
         across(where(is.character), as.numeric))


# Calculate the Chronbach's Alpha 

alpha_coefficient <- psych::alpha(subset_data_numeric, check.keys = TRUE)
print(alpha_coefficient)


# Calculate the denominator 

max_capacity_status <- max(subset_data_numeric$capacity_status)
max_COV_D1A1A_X <- max(subset_data_numeric$COV_D1A1A_X)
max_D7C <- max(subset_data_numeric$D7C)
max_COV_B2A1 <- max(subset_data_numeric$COV_B2A1)

# Calculate the denominator

denominator <- max_capacity_status + max_COV_D1A1A_X + max_D7C + max_COV_B2A1
  print(denominator)

# Calculate the index 

subset_data_numeric$index <- (subset_data_numeric$capacity_status + subset_data_numeric$D7C + subset_data_numeric$D7C + subset_data_numeric$COV_B2A1 ) / denominator


# Summary statistics of Index

index_summary <- summary(subset_data_numeric$index)
summary_table <- data.frame(
  Measure = c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum"),
  Value = c(min(index_summary), quantile(index_summary, 0.25), median(index_summary),
            mean(index_summary), quantile(index_summary, 0.75), max(index_summary))
)
summary_table


# Inferential Statistics-------------------------------------
# Merge the two data sets using cbind()

merged_data2 <- cbind(subset_data_numeric, index_data)

#Prepare the data 
# Select the columns that will be used to conduct the t-test 

  sample_test1 <- merged_data2 %>%
      select(SAMPLING_STRATUM, index)


# Recode the SAMPLING STRATUM coulmn to show the names of the industries

    sample_test1$industry <- factor(sample_test1$SAMPLING_STRATUM, levels = c(1, 2), labels = c("Manufacturing", "Services"))

# Select the columns that will be used to conduct the t-test 

  sample_test1 <- sample_test1 %>%
      select(industry, index)

# Conduct the Independent Sample T Test and obtain the results 
    
 t.test.results <- nice_t_test(data = sample_test1,
              response = "index",
               group = "industry",
              warning = FALSE, 
              var.equal = TRUE)
  
  t.test.table <- nice_table(t.test.results)
  t.test.table

  
# Box-plot to show the violation of the test  

  ggplot(sample_test1, aes(industry, index, fill = industry)) +
    geom_boxplot() +
    labs(title = "Boxplot of Index Values Between the Different Industries", 
             x = "Industry", 
             y = "Index") +
    scale_y_log10() + 
    scale_fill_brewer(palette = "Accent")
  

# Conduct a Non-parametric test since the assumptions were violated 
# Mann Whitney Test---------------------------------------------------------
# Filter data for the two groups

  group1 <- sample_test1 %>% 
    filter(industry == "Manufacturing") %>% 
      pull(index) 
  
  group2 <- sample_test1 %>% 
    filter(industry == "Services") %>% 
      pull(index)


# Perform the Mann-Whitney U test 

  wilcox.test(group1, group2, alternative = "two.sided")

  

```



```{r GOAL 6, Cluster Analysis}
# Prepare the data 
# Select the variables of interest and filter out unwanted values 

cluster_data <- sample %>%
  select(COV_F2A, COV_F2B, COV_F2C, COV_A5A) %>%
    filter(COV_A5A >= 0) %>%
    mutate(  
      COV_F2A = as.factor(COV_F2A), 
      COV_F2B = as.factor(COV_F2B), 
      COV_F2C = as.factor(COV_F2C), 
      COV_A5A = as.factor(COV_A5A)
    )
  
    
# Take a subset of 200 variables (rows)

  cluster_data <- cluster_data[sample(nrow(cluster_data), 200), ]
  

# Compute proximity matrix
  
  DistanceMatrix <- daisy(cluster_data,   # the daisy function was used since both categorical and quantitative variables were used 
    metric = "gower",
    type = list(logratio = 3))

    
# Hierarchical clustering
  
  hc <- hclust(DistanceMatrix , method = "complete")


# Determining Optimal Number of Clusters 
# Elbow Plot
# ===============================

fviz_nbclust(cluster_data, FUN = hcut, method = "wss")
# Optimal Number = at the kink


# Silhouette Plot
# ===============================
fviz_nbclust(cluster_data, FUN = hcut,
             method = "silhouette")
# Optimal Number = at highest point


  
# Visualizing Clusters
# Plot Dendrogram

plot(hc, main = "Complete Linkages")


# Plot Dendrogram with Clusters Highlighted
rect.hclust(hc, k = 4, border = 10:10)
# change k based on original plot

# Colour the Clusters on the Dendrogram
# ========================================
dendgram <- as.dendrogram (hc)

ColourDendgram <- color_branches(dendgram, h = 4)

plot(ColourDendgram)


# choose k, number of clusters
# ====================================
clusterLabs <- cutree(hc, k = 4)

# add cluster to original data
# ====================================
cluster_data <- cbind(cluster_data , as.factor(clusterLabs))

# Visualize on 2-D plot
# ====================================
#Transform the data to numerc
cluster_data <- sample %>%
  select(COV_F2A, COV_F2B, COV_F2C, COV_A5A) %>%
  filter(COV_A5A >= 0) %>%
    mutate( 
      COV_F2A = as.numeric(COV_F2A), 
      COV_F2B = as.numeric(COV_F2B), 
      COV_F2C = as.numeric(COV_F2C), 
      COV_A5A = as.numeric(COV_A5A))

cluster_data <- cluster_data[sample(nrow(cluster_data), 200), ]

fviz_cluster(list(data = cluster_data,
                  cluster = clusterLabs))


# Summary of clusters
clusters <- cutree(hc, k = 4)  # Assuming we want 4 clusters

# Add cluster assignment to the data frame
cluster_data$cluster <- as.factor(clusters)
summary_table <- cluster_data %>%
  group_by(cluster) %>%
  summarise(
    Avg_COV = mean(COV_A5A),
    Count = n(),
    .groups = 'drop'
  )

# Print summary table
print(summary_table)

```

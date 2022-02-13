---
title: "SI 544 Final Project"
author: "Michelle Cheng (michengz@umich.edu)"
output: html_document
---

\

## **Introduction**
### Project Goals & Overview
For this project, I selected the Austin Animal Shelter dataset as my main data source and aimed to answer several key questions related to pet adoption, particularly dogs and cats. During the process, I explained the descriptive statistics, performed data wrangling, and delivered visualizations during the analysis phase. In terms of statistic methods, I conducted multiple linear regression to find the correlation between pet age/type and their days in shelter, as well as performed hypothesis testing to test whether the dogs have a higher likelihood of getting adopted than the cats.  
  
Questions to answer:  
**1. For adopted pets from the AAC, is there a correlation between pet age/type and days in shelter? (Multiple Regression)**  
**2. Are the dogs in the AAC Shelter more likely to be adopted than cats? (Hypothesis Testing)**    

### About the Dataset
The dataset I chose was a csv file downloaded from Kaggle: https://www.kaggle.com/aaronschlegel/austin-animal-center-shelter-intakes-and-outcomes?select=aac_intakes_outcomes.csv. This dataset is about the intakes and outcomes of the animals from Austin Animal Shelter(ACC), covering detailed information about each pet that was sent to the shelter such as breed, intake/outcome type, pet age, etc.

\

## **Preparing for Analysis**
### Loading Libraries
```{r}
library(tidyverse)
library(moderndive)
library(skimr)
library(knitr)
library(infer)
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

\

### Read Data
```{r}
raw_data <- read.csv("aac_intakes_outcomes.csv")
glimpse(raw_data)
```

\

### Data Wrangling
The original dataset holds several columns that are unrelavent to my key questions. Let's select only the columns that might be useful for our analysis:
```{r}
shelter <- raw_data %>% 
  select('animal_type', 'breed', 'color', 'sex_upon_outcome', 'intake_type', 'outcome_type', 'age_upon_intake_.days.', 'time_in_shelter_days')
glimpse(shelter)
```

\

Next, let's do some data cleaning and extract only rows that are related to adopted cats and dogs:
```{r}
adopt_dogcat <- shelter %>% 
  filter(outcome_type == 'Adoption',
         animal_type == 'Dog' | animal_type == 'Cat') %>%
  rename(age_upon_intake_days = age_upon_intake_.days.)
```

\

Now let's take a look at some **descriptive statistics**:
```{r}
skim(adopt_dogcat)
```

\

Great, no missing values! As shown from the summary statistics above, there are 1444 different breeds of pets and 4 different intake types within this dataset. As for numeric variables, the average age of pets upon intake is 594 days, while the average time in shelter is 29.5 days. Both of these variables has a relatively wide range compared to the mean. Moreover, from the minimized histogram, we could see that both distributions look significantly skewed.  
Let's take a closer look at what the two histograms look like:
```{r, out.width = '50%', fig.align = 'center'}
ggplot(adopt_dogcat, aes(x = age_upon_intake_days)) +
  geom_histogram()
ggplot(adopt_dogcat, aes(x = time_in_shelter_days)) +
  geom_histogram()
```

\

As shown from the histograms above, the two variables are skewed, with which both of them have several outliers on the right side. In order to make better analyses out of this data, we can mutate columns that transforms the two variables using log10 then plot the histograms once again:
```{r, out.width = '50%', fig.align = 'center'}
# Mutating age and days in shelter columns
adopt_dogcat <- adopt_dogcat %>%
  mutate(log10_age = log10(age_upon_intake_days),
         log10_time = log10(time_in_shelter_days))

# Replacing infinite values by NA
adopt_dogcat <- do.call(data.frame, 
                   lapply(adopt_dogcat,
                          function(x) replace(x, is.infinite(x), NA)))

# Plotting the histograms
ggplot(adopt_dogcat, aes(x = log10_age)) +
  geom_histogram()
ggplot(adopt_dogcat, aes(x = log10_time)) +
  geom_histogram()
```

\

## **Data Analysis**
### Analysis 1: Multiple Regression
Let's get started with our first analysis answering the question about whether there are any correlation between animal age/type and days in shelter. For this question, we have:  

  -  One outcome variable: days in shelter (log10_time)

  -  Two explanatory variables (one numeric and one categorical): pet age (log10_time) and pet type (animal_type)

We'll start by checking out the linear regression for the two numeric variables - pet age and days in shelter:
```{r, out.width = '50%', fig.align = 'center'}
ggplot(adopt_dogcat, 
       aes(x = log10_age, y = log10_time)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(y = "log10 days in shelter", 
       x = "log10 age", 
       title = "")
```
```{r}
adopt_dogcat %>%
  get_correlation(log10(time_in_shelter_days) ~ log10(age_upon_intake_days))
```
\

From the scatterplot and calculated correlation coefficient above, we could see that the two variables are weakly positively correlated. Now let's take animal type into consideration, which in this case, we're only comparing between two categories - cats and dogs.

```{r, out.width = '50%', fig.align = 'center'}
ggplot(adopt_dogcat, 
       aes(x = log10_age, y = log10_time, col = animal_type)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(y = "log10 days in shelter", 
       x = "log10 age", 
       title = "")
```
As shown in the resulting graph above, the regression line for cats is negatively sloped, while the line for dogs is positively sloped and slightly steeper. This means that, compared to the cats, the dogs are on average more likely to stay longer in shelter if they have a higher age. For the cats, on the other hand, there doesn't seem to have a strong correlation between their age and days in shelter.  
Now let's again fit the regression using the linear model and get the regression table:
```{r}
shelter_days_model <- lm(log10_time ~ log10_age * animal_type, data = adopt_dogcat)
get_regression_table(shelter_days_model)
```
From the regression table, we can get that:  

  - The intercept for the cats is 1.506 and the slope for age for the cats is -0.110    
  
  - The intercept for the dogs is 1.506 + (-0.848), and the slope for age for the dogs is (-0.110) + 0.242  
  
We can get the equations to get fitted values as well:  

  - Cats: shelter_days = **1.506 + (-0.110) * age**   
  
  - Dogs: shelter_days = 1.506 + (-0.848) + ((-0.110) + 0.242) * age = **0.658 + 0.132 * age**    

From the regression lines and equations, we can also see that, when looking at the pets with relatively younger ages, the cats tend to stay at the shelters longer than the dogs on average.

\

### Analysis 2: Hypothesis Testing
Now that we got some insight from the first analysis, let's shift our gears and conduct some hypothesis testing to see if dogs are more likely to get adopted than cats in shelters! To start off, we would need to define our null hypothesis and alternative hypothesis:  
**H0: a_d = a_c** (there is no big difference in adoption for cats and dogs)  
**HA: a_d > a_c** (dogs are more likely to get adopted than cats)  
As for significance level, we'll go with the most common used α = 0.05.

\

Before we begin our analysis, let's first do some data wrangling in order to get the right data. There will be two animal types - dogs and cats, as well as two outcome types - adoption and euthanasia.
```{r}
adoption <- shelter %>%
  filter(animal_type == 'Dog' | animal_type == 'Cat', 
         outcome_type == 'Adoption'| outcome_type == 'Euthanasia') 
```

\

After that, let's also check the difference in counts between cats and dogs:
```{r, out.width = '50%', fig.align = 'center'}
ggplot(adoption, aes(x = animal_type, fill = outcome_type)) +
  geom_bar() 
```
As shown in the bar chart above, it looks like much more dogs were adopted in comparison with cats. However, we're still not completely sure if this is evident enough to prove that dogs are more likely to get adopted than cats. Let's get started with hypothesis testing!

\

First, let's calculate the observed difference between the dogs and cats:
```{r}
obs_diff_prop <- adoption %>% 
  specify(outcome_type ~ animal_type, success = "Adoption") %>% 
  calculate(stat = "diff in props", order = c("Dog", "Cat"))
obs_diff_prop
```

\

Next, let's construct the null distribution:
```{r}
null_distribution <- adoption %>% 
  specify(outcome_type ~ animal_type, success = "Adoption") %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("Dog", "Cat"))
null_distribution
```

\

Now that we have our null distribution, we can visualize distribution as well as mark the shaded p-value:
```{r, out.width = '50%', fig.align = 'center'}
visualize(null_distribution, bins = 10) + 
  shade_p_value(obs_stat = obs_diff_prop, direction = "right")
```
As shown in the resulting chart above, the marked observed difference is extremely far away from the null distribution with no shaded regions formed. This means that the probability of obtaining a test statistic just as or more extreme than the observed test statistic is 0. 

\
  
Let's check to see if the p-value is really 0 as we expected from the chart above:
```{r}
null_distribution %>% 
  get_p_value(obs_stat = obs_diff_prop, direction = "right")
```
As a result, we should reject the null hypothesis, meaning that there is a significant difference between the likelihood of adoption for cats and dogs, and that the dogs are more likely to get adopted compared to the cats in the AAC Shelter.

\

## **Conclusion**
**1. For adopted pets from the AAC, is there a correlation between pet age/type and days in shelter? (Multiple Regression)**  

  -  Compared to the cats, the dogs are on average more likely to stay longer in shelter if they have a higher age. 
  
  -  For the cats, there doesn't seem to have a strong correlation between their age and days in shelter.  
  
  -  When looking at the pets with relatively younger ages, the cats tend to stay at the shelters longer than the dogs on average.

**2. Are the dogs in the AAC Shelter more likely to be adopted than cats? (Hypothesis Testing)**    
  
  -  The dogs in the shelter are more likely to get adopted in comparison with the cats.

\
\



  
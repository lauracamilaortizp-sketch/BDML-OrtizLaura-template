# Include here your code for your first chosen imputation method
## Install Packages

if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(rio,        # import/export data
       tidyverse,  # tidy-data
       skimr,      # summary data
       gridExtra,  # visualizing missing data
       corrplot,   # Correlation Plots 
       stargazer,  # tables/output to TEX. 
       MASS)       # various statistical functions


## load data
df <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")

df <- as_tibble(df) 

## select some variables

skim(df) %>% head()

# total income in millions
df <- df  %>%
  mutate(ingtot =ingtot/1000000 )


ggplot(df, aes(ingtot)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(df$ingtot, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(df$ingtot, na.rm = TRUE), linetype = "dashed", color = "blue") +  
  ggtitle(" Ingreso Total") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))


plot(df$totalHoursWorked, df$ingtot, col='blue', pch=19)

df %>%
  summarise(count = sum(is.na(totalHoursWorked) & is.na(ingtot)))
# 480
df %>%
  summarise(count = sum(is.na(totalHoursWorked)))
#  1652
df %>%
  summarise(count = sum(is.na(ingtot)))
# 480

library(fastDummies)
df <- df %>%
  dummy_cols(select_columns = c("maxEducLevel", "regSalud"),
             remove_first_dummy = FALSE,      # keep full set of dummies
             remove_selected_columns = TRUE)  # drop original categorical cols


# imputar con regresion lineal

linear_imput_model <- lm(formula = ingtot ~  sex + age + college +
                           maxEducLevel_3 + maxEducLevel_4 + maxEducLevel_2 +
                           maxEducLevel_5 + maxEducLevel_6 + maxEducLevel_7, data = df)

#

df$predicted_y <- predict(linear_imput_model, newdata = df)

df %>% dplyr::select(directorio, predicted_y, ingtot  ) %>% tail() 

df <-  df %>%  mutate(ingtot = ifelse(is.na(ingtot) == TRUE, predicted_y , ingtot))


# Distribution of total income
ggplot(df, aes(ingtot)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(df$ingtot, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(df$ingtot, na.rm = TRUE), linetype = "dashed", color = "blue") +  
  ggtitle(" Ingreso Total") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

sum(is.na(df$ingtot))
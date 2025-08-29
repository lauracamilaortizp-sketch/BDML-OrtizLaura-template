# Include your code here for you second imputation method
# install pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)

# require/install packages on this session

p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       visdat, # visualizing missing data
       corrplot, # Correlation Plots 
       stargazer,
       zoo) # tables/output to TEX. 


## load data
df <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")

df <- as_tibble(df) ## from dataframe to tibble

#linear interpolation

df$ingtot <- na.approx(df$ingtot)

# Distribution of total income
ggplot(df, aes(ingtot)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(df$ingtot, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(df$ingtot, na.rm = TRUE), linetype = "dashed", color = "blue") +  
  ggtitle(" Ingreso Total") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

sum(is.na(df$ingtot))

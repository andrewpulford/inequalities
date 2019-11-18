################################################################################
#####  Explanation charts for inequality gap and gradient
################################################################################

## load packages
library(tidyverse)

## create data for example
quintile <- (c(1:5)) # deprivation quintiles 
rate <- c(5.4, 4.7, 3.4, 2.8, 2.2) # made-up rates
df <- data.frame(quintile, rate) # combine into dataframe
df <- df %>% as_tibble()

## specify standard theme for all charts
std_theme <-   theme_minimal() +
  theme(plot.subtitle = element_blank(),
        legend.title =  element_blank(),
        legend.text = element_blank(),
        axis.title.y= element_blank(), 
        axis.title = element_blank(),
        axis.text =  element_text(size = 12),
        axis.text.x =  element_text(hjust = 0.5),
        axis.text.y =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

### create charts  ----------------------

## chart a - basic rank chart

chart_a <- df %>% ggplot(aes(x = quintile, y = rate)) +
  geom_col() +
  std_theme +
  scale_x_continuous(breaks = c(1:5),
                     labels = c("Most deprived", 
                                "", "", "",
                                "Least deprived")) +
  scale_y_continuous(limits = c(0, 1+max(rate)))
  
chart_a

## chart b - add hlines for Q1 and Q5, plus arrows to show gap

chart_b <- chart_a + 
  geom_hline(yintercept = max(df$rate), linetype="dashed", color = "red") +
  geom_hline(yintercept = min(df$rate), linetype="dashed", color = "red") +
  geom_segment(aes(x = 5, y = min(df$rate), # pointing up
                   xend = 5), yend = max(df$rate), 
               linetype = "dashed",  color = "red", 
               arrow = arrow(length = unit(0.06, "npc"))) +
  geom_segment(aes(x = 5, y = max(df$rate), # pointing down
                   xend = 5), yend = min(df$rate), 
               linetype = "dashed",  color = "red", 
               arrow = arrow(length = unit(0.06, "npc")))
 
chart_b

## chart c - remove quintiles 2-4 to show data/pop not considered by gap
chart_c <- df %>% mutate(rate =ifelse(quintile %in% c(1, 
                                         5), rate, NA)) %>% 
  ggplot(aes(x = quintile, y = rate)) +
  geom_col() +
  std_theme +
  scale_x_continuous(breaks = c(1:5),
                     labels = c("Most deprived", 
                                "", "", "",
                                "Least deprived")) +
  scale_y_continuous(limits = c(0, 1+max(rate))) +
  geom_hline(yintercept = max(df$rate), linetype="dashed", color = "red") +
  geom_hline(yintercept = min(df$rate), linetype="dashed", color = "red") +
  geom_segment(aes(x = 5, y = min(df$rate), # pointing up
                   xend = 5), yend = max(df$rate), 
               linetype = "dashed",  color = "red", 
               arrow = arrow(length = unit(0.06, "npc"))) +
  geom_segment(aes(x = 5, y = max(df$rate), # pointing down
                   xend = 5), yend = min(df$rate), 
               linetype = "dashed",  color = "red", 
               arrow = arrow(length = unit(0.06, "npc")))

chart_c

## chart d - quintiles with gradient line

# get intercept and coefficient from regression model
lin_mod <- lm(rate ~ quintile, data = df)
print(lin_mod)

# plot chart
chart_d <- chart_a +
  geom_abline(intercept = lin_mod$coef[1], 
              slope = lin_mod$coef[2],
              linetype = "dashed",  color = "red") 
chart_d





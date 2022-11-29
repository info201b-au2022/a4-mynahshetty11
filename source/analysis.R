library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

get_data <- function(num_records=-1) {
  fname <- "~/Documents/info201/data/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
  return(df)
}

library(dplyr)
library(stringr)
## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

state <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/year-end-prison-2021.csv')
View(state)
county <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')
View(county)

# What county has the current highest total black jail population in 2021?
#(Variable: 'current_max_black_county')
#Los Angelas County

current_county <- filter(county, year == max(year))
max_black_county <- max(current_county$black_jail_pop, na.rm = TRUE)
current_max_black_county <- filter(current_county, black_jail_pop == 5024) %>% 
  pull(county_name)

# Plot a dataframe of the changes to the black population (from the ages 15 to 64) over the last 20 years in the county found in the last prompt
# (Variable: 'plot_last_20')
los_angeles_county <- filter(county, county_name == "Los Angeles County", na.rm = TRUE)

last_20 <- los_angeles_county %>%
  select(year, black_jail_pop)
last_20 <- tail(last_20, n = 20)
plot_last_20 <- plot(last_20)

# Plot a dataframe of the changes to the white population (from ages 15 to 64) over the the last 20 years in the county found in the last prompt
# (Variable: 'plot_last_20_white')
last_20_white <- los_angeles_county %>%
  select(year, white_jail_pop)
last_20_white <- tail(last_20_white, n = 20)
plot_last_20_white <- plot(last_20_white)


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function returns a data frame that is suitable for visualization. This function takes no parameters. 
get_year_jail_pop <- function() {
  total_jail_population_df <- 
    select(county, year, total_jail_pop) %>%
    arrange(., year) %>%
    na.omit(total_jail_population_df) %>%
    group_by(year) %>%
    summarise(total_jail_pop = sum(total_jail_pop)) %>%
    ungroup()
return(total_jail_population_df)   
}
View(get_year_jail_pop())
 
library(ggplot2)
library(tidyverse)

# This function should return the chart. This function: (1) Takes no parameters; and (2) Should call the data wrangling function.
plot_jail_pop_for_us <- function()  {
  plot_us <- get_year_jail_pop() %>%
    ggplot(aes(x = year, y = total_jail_pop)) +
    geom_bar(stat = "identity", fill = "#97B3C6") +
    theme_bw() +
    xlab("Year") +
    ylab("Total Jail Population") +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)",
         subtitle = "This chart shows the increase in jail populations throught the U.S from the year 1970 to the year 2018)",
         caption = "This chart displays an upward trend of prison populations throughout the U.S from thr 1970's to late 2010s.
       However, we can also see how the chart is beginning to even out or even decrease starting at a peak at around 2010 after
       very visibly trending upwards for 40 years. This may idnicate that after 40 years of growth prison populations may have
       begin to either level out or decrease in the past 10 year")
  
  return(plot_us)   
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#This data wrangling function should return a data frame that is suitable for visualization. The parameter states should be a vector of states.
states <- county$state %>% 
  unique(state) %>% 

get_jail_pop_by_states <- function(states) {
  jail_pop_by_state_df <-
    county %>% 
    select(year, state, total_jail_pop) %>% 
    group_by(state) %>% 
    arrange(.,year) %>% 
    na.omit(jail_pop_by_state_df) %>% 
    group_by(year, state) %>% 
    summarise(total_jail_pop = sum(total_jail_pop)) %>% 
    arrange(., state) %>% 
    rename(states = state) %>% 
  return(jail_pop_by_state_df[jail_pop_by_state_df$states == states, ])
}

View(get_jail_pop_by_states(states))
View(get_jail_pop_by_states("WA"))


wa_and_ca <- return(jail_pop_by_state_df[jail_pop_by_state_df$states == "WA", "CA"])

states <- county$state %>% 
  unique(state) %>% 

#This plotting function should return the chart. The parameter states should be a vector of states. This function should call the data wrangling function.  
  
plot_jail_pop_by_states <- function(states) {
  line_chart <- get_jail_pop_by_states(states) %>% 
    filter(states == "CA" &
             states == "NY" &
             states == "TX") %>% 
    ggplot(aes(x = year, y = total_jail_pop, colour = states)) +
    geom_point(size = 5, alpha = 0.3) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "State Jail Population by Year",
         subtitle = "Chart compares the Jail Population from 1970-2018 by State",
         caption = "This chart enables users to compare the Jail Populations from the years 1970-2018 for any U.S state.
         This is exteremly useful for identifying which states may be overpoliced. Users are able to compare spikes and falls 
         in the data and identify trends by using states that are in the same regions or have similar populations.") %>% 
  return(line_chart)
}

# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

#Create a line chart of the changes to the aapi, black, latinx, native, and white jail population (ages 15 to 64)
#in the last 20 years from the county with the highest total jail population.
#(Variable name: 'section_5_plot')

highest_current_county <- max(current_county$total_jail_pop, na.rm = TRUE)
highest_current_county <- filter(current_county, total_jail_pop == 17208)
los_angeles_county <- filter(county, county_name == "Los Angeles County", na.rm = TRUE)
View(los_angeles_county)
last_20_2 <- los_angeles_county %>%
  select(year, white_jail_pop, native_jail_pop, latinx_jail_pop, black_jail_pop, aapi_jail_pop)
last_20_2 <- tail(last_20_2, n = 20)
View(last_20_2)

last_20_2 <- last_20_2 %>% 
  pivot_longer(-c('year'), names_to = "race", values_to = "jail_pop")
last_20_2 <- last_20_2 %>% 
  arrange(.,race)


last_20_2_plot <- last_20_2 %>% 
  ggplot(aes(year, jail_pop, colour = race ))+
  geom_point( size = 5, alpha = 0.3)+
  geom_line(size = 1)+
  theme_minimal()+
  labs(title = "Recent Changes to Jail rate by Race in LA County",
       subtitle = "Changes to the total jail populations of different races in the past 20 years within LA County",
       caption = "This chart answers the question of how jail rates by race have changed in recent years in the county with the highest jail rates
       We can see from the chart that Latinx, Black and White (in that order) have the highest rates out of the listed races.
       Native and AAPI races have the lowest rates. A spike is observed in the early 2000s but the rates seem to be evening out individually.")
  
View(last_20_2_plot)
section_5_plot <- last_20_2_plot


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


# Does the black jail population increase when moving closer to a big city (EX: California)?

## Load data frame ---- 

library(ggplot2)
install.packages('maps')
library(maps)
library(dplyr)
library(urbnmapr)

#Write a function that returns a chlorepleth map of jail rates by state.
#(Variable name: 'rate_plot')

# jail rate by state

View(current_county)
df_1 <- current_county %>% 
  select(state, total_pop_15to64, total_jail_pop)
View(df_1)
df_1$rate <- df_1$total_jail_pop / df_1$total_pop_15to64
df_1 <- na.omit(df_1)
df_2 <- df_1 %>% 
  select(state, rate)
View(df_2)
df_2 <- df_2 %>% 
  group_by(state) %>% 
  summarise(across(c(rate), mean))
View(state_shape)
library(stringr)
df_2 <- df_2 %>% 
  mutate(state = state.name[match(state, state.abb)]) 
state_shape <- state_shape  %>% 
  mutate(state = str_to_title(state))

state_shape <- state_shape %>% 
  left_join(df_2, by = "state")
state_shape <- state_shape %>% 
  rename(region = state)


rate_states <- state_shape %>% 
  left_join(df_2, by = c( "region" = "state")) 
rate_states <- rate_states %>% 
  rename(jail.rate = rate)
View(rate_states)

rate_plot <- rate_states %>% 
  left_join(df_2, by=c("region"="state")) %>%
  ggplot(aes(x=long,y=lat,group=group, fill = jail.rate)) +
  geom_polygon(color = "gray90", linewidth = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  #scale_fill_brewer("Oranges")+
  theme_minimal() +
  labs(title = "Rate of Jailing by State",
       subtitle = "Rate of total jail population/ total population aged 15-64 by state.",
       caption = "This chart illustrates the ration of jail population and total population that
       is old enough to be jailed. This can be exteremely useful for users as it takes in to account population.
       Users may will see this chart and be able to clearly answer the question of 
       'Which states may be overpoliced?'")



  

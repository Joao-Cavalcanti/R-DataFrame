library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)
library(viridis)
library(hrbrthemes)

master_df <- read.csv('faang_stocks_pandemic_data.csv')
stock_list <- c('Google', 'Netflix', 'Apple', 'Amazon', 'Facebook')

master_df$X <- NULL

master_df <- master_df %>% drop_na()
master_df$Date <- strptime(master_df$Date, format='%Y-%m-%d')
min_time <- min(master_df$Date)
max_time <- max(master_df$Date)
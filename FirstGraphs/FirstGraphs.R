library(tidyverse)
library(here)
library(lubridate)
library(cowplot)


russia_personnel <- read_csv(here('data', 'russia_losses_personnel.csv'))
russia_equipment <- read_csv(here('data', 'russia_losses_equipment.csv'))


data <- data.frame(russia_personnel$personnel)
casuality <- diff(data$russia_personnel.personnel)
max_jump <- max(casuality)
day_of_max <- russia_personnel$date[which.max(casuality)]





russia_personnel %>%
    ggplot() +
    geom_point(aes(x = day, y = personnel)) +
    labs(
        x = "Days Since Start of War",
        y = "Estimated Russian Personnel Lost\n Ukr Army Estimates in Hundreds of Thousands",
        title = "Ukrainian Estimate of Russian Casualities since the Start of the War"
    ) +
    theme_minimal_grid(font_size = 10) +
    theme(legend.position = "none") +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-3))


russia_equipment %>%
    ggplot() +
    geom_line(aes(x = day, y = tank, color = "tank")) +
    geom_line(aes(x = day, y = APC, color = "APC")) +
    theme_minimal_grid(font_size = 10) +
    labs(
        title= "Russian Armoured Vehicle Losses since the start of the Russo-Ukrain War",
        x = "Days since the beginning of the war",
        y = "Armoured Vehicle Losses",
        color = "Type of \nVehicle"
    )


russia_equipment$tank <- c(0, diff(russia_equipment$tank))
russia_equipment %>%
    ggplot() +
    geom_col(aes(x = day, y = tank)) +
    geom_smooth(aes(x = day, y = tank), method = "gam", color = "blue") +
    theme_minimal() +
    labs(
        title= "Russian Tank Losses since the start of the Russo-Ukrain War",
        x = "Days since the beginning of the war",
        y = "Tank Losses"
    )


russia_equipment$aircraft <- c(0, diff(russia_equipment$aircraft))
russia_equipment %>%
    ggplot() +
    geom_col(aes(x = day, y = aircraft)) +
    geom_smooth(aes(x = day, y = aircraft), method = "gam", color = "blue") +
    theme_minimal() +
    labs(
        title= "Russian Plane Losses since the start of the Russo-Ukrain War",
        x = "Days since the beginning of the war",
        y = "Plane Losses per Day"
    )









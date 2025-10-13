## Sea Ice Analysis

## load data from the web,
## see also http://jse.amstat.org/v21n1/witt.pdf
library(tidyverse)
seaIce <- read_tsv("http://jse.amstat.org/v21n1/witt/sea_ice_data.txt")
names(seaIce) <- c("Year", "SeaIceExtent") # rename columns for convenience
head(seaIce) # preview the data

## plot the data
ggplot(seaIce, aes(x = Year, y = SeaIceExtent)) +
    geom_point(size = 3) +
    scale_y_continuous(limits = c(0, 9)) +
    labs(y = "Arctic sea ice extent (1,000,000 sq km)") +
    theme_minimal(base_size = 18)

## fit a simple linear model, regress sea ice extent on year
seaIce1.lm <- lm(SeaIceExtent ~ Year, data = seaIce)

## make predictions that we can plot
seaIce <- seaIce |>
    mutate(Predicted = predict(seaIce1.lm, newdata = seaIce))

ggplot(seaIce, aes(x = Year, y = SeaIceExtent)) +
    geom_point(size = 3) +
    geom_line(aes(x=Year, y=Predicted), color="blue") +
    scale_y_continuous(limits = c(0, 9)) +
    labs(y = "Arctic sea ice extent (million sq km)") +
    theme_minimal(base_size = 18)

print(coef(seaIce1.lm))
## On average, the extent of sea ice is decreasing by ~90,000 sq km per year (about the size of Portugal)
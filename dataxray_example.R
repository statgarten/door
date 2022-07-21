
library(dataxray)
library(dplyr)
library(ggplot2)

diamonds <- diamonds %>%
  mutate(price = structure(price, label = 'price in US dollars'),
         carat = structure(carat, label = 'weight of the diamond'),
         cut = structure(cut, label = 'quality of the cut (Fair, Good, Very Good, Premium, Ideal)'),
         color = structure(color, label = 'diamond colour, from D (best) to J (worst)'),
         clarity = structure(clarity, label = 'a measurement of how clear the diamond is
                                               (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))'),
         x = structure(x, label = 'length in mm'),
         y = structure(y, label = 'width in mm'),
         z = structure(z, label = 'depth in mm'),
         depth = structure(depth, label = 'total depth percentage = z / mean(x, y) = 2 * z / (x + y)'),
         table = structure(table, label = 'width of top of diamond relative to widest point'))

diamonds %>%
  report_xray(data_name = 'Diamonds', study = 'ggplot2')


diamonds %>%
  make_xray() %>%
  view_xray()

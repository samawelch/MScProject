rm(list=ls())
require(dplyr)
require(tidyr)
require(ggplot2)

setwd("C:/Users/Sam Welch/Google Drive/ICL Ecological Applications/Project/Work/Scripts/Data/Bug_Rangefinding/Simple_Test")

ampicillin <- read.csv("Ampicillin_Test.csv")

ampicillin <- tbl_df(ampicillin)

ampicillin2 <- ampicillin %>%
  gather(time, OD, X00.00.06:X54.35.17)
ampicillin2 <- ampicillin2[3:5]

ampicillin3 <- ampicillin2 %>%
  mutate(
    time = (substring(time,2,3))
  )

ampicillin4 <- ampicillin3 %>%
  group_by(ID, time) %>%
  summarise(mean_od = mean(OD))

ggplot(ampicillin4[1:54,],
       aes(
         x = time,
         y = mean_od
       ))+
  geom_point(
    aes(
      x = time,
      y = mean_od,
      colour = "red"
    )
  )

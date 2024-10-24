rm(list = ls())
library(data.table)
library(dplyr)
library(dtplyr)
d <- fread('calendar.csv')
d

# Default settings
# Rule: 2 normal days consequtive or scatterd can be day-offs.

#
# Golden Week
#
d |> 
  filter('04-25' <= md & md <= '05-10' & gr == 1) |>
  select(date, dow, name, event, md, gr)

# [Group1]       2015, 2020: 04-29 -- 05-06
# [Group2]       2010, 2021: 04-29 -- 05-05 (assuming that 4/30 is a day-off)
# [Group3] 2011, 2016, 2022: 04-29 -- 05-05 (assuming that 5/2 is a day-off)
# [Group4]       2017, 2023: 04-29 -- 05-07 (assuming that 5/1 and 5/2 are day-offs)
# [Group5]       2012, 2018: 04-28 -- 05-06 (assuming that 5/1 and 5/2 are day-offs)
# [Group6] 2013, 2019, 2024: 05-03 -- 05-06 (2019 has two extra holidays due to inauguration of the emperor)
# [Group7]       2014, 2025: 05-03 -- 05-06

#
# Obon ( 8/13--8/16 )
# 山の日　2020年8月10日, 2021年8月8日に変更（8/11）
d |> 
  filter('08-08' <= md & md <= '08-22' & gr == 7) |> 
  select(date, dow, name, event, md, gr)

# [Group1]       2015, 2020: 08-13 -- 08-16
# [Group2]       2010, 2021: 08-13 -- 08-16
# [Group3] 2011, 2016, 2022: 08-13 -- 08-16 (assuming that 8/12 is a day-off)
# [Group4]       2017, 2023: 08-13 -- 08-16 (assuming that 8/14 is a day-off)
# [Group5]       2012, 2018: 08-13 -- 08-17 (assuming that 8/17Fri is a day-off)
# [Group6] 2013, 2019, 2024: 08-13 -- 08-16
# [Group7]       2014, 2025: 08-13 -- 08-15

#
# New Years Days ( 12/29--1/4 )
#
d |> 
  filter( gr == 7 & ('12-20' <= md & md <= '12-31' | '01-01' <= md & md <= '01-10')) |> 
  select(date, dow, name, event, md, gr)

# [Group1]       2015, 2020: 12-28 -- 01-03 (assuming that 12/28 is a day-off)
# [Group2]       2010, 2021: 12-29 -- 01-03
# [Group3] 2011, 2016, 2022: 12-29 -- 01-03
# [Group4]       2017, 2023: 12-29 -- 01-03
# [Group5]       2012, 2018: 12-29 -- 01-04 (assuming that 1/4 is a day-off)
# [Group6] 2013, 2019, 2024: 12-28 -- 01-05
# [Group7]       2014, 2025: 12-27 -- 01-04



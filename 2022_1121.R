library(tidyverse)
library(lubridate)
library(ggpubr)
library(scales)
library(ggplot2)

df = read_csv("takeda_data/oneday.csv")

df1 = 
  df |> mutate(date=date(dat)) |> 
  rename("depth"="depth_ave",
         "temp"="temp_ave") |> 
  mutate(year=year(date),
         month=month(date),
         day=day(date)) |> 
  select(-c(dat,date)) |>
  mutate(ym=paste(year,month)) |> 
  mutate(ym=ym(ym)) |> 
  select(c(no,depth, temp, year, month, ym)) 

dn = df1 |> 
  group_by(no) |> 
  summarise(n = n()) |> 
  filter(n >=365) |> 
  select(no)

#ひと月の水温、深さの範囲をプロット
dn |> left_join(df1,by = "no") |> 
  group_by(no,ym) |> 
  summarise(temp_max = max(temp),
            temp_min = min(temp),
            depth_max = max(depth),
            depth_min = min(depth)) |> 
  mutate(d_temp = temp_max - temp_min,
         d_depth = depth_max - depth_min) |> 
  mutate(m = month(ym)) |> 
  ggplot() +
  geom_point(aes(x = d_temp, y = d_depth, color = no),size = 3) +
  scale_y_continuous(trans = "reverse") +
  facet_wrap(vars(m))

#日較差をプロット
df |> mutate(date=date(dat)) |> 
  rename("depth"="depth_ave",
         "temp"="temp_ave") |> 
  select(c(no,depth, temp, date)) |> 
  group_by(no) |> 
  arrange(no,date) |> 
  mutate(d_depth = depth-lag(depth),
         d_temp = temp-lag(temp)) |> 
  mutate(m = month(date)) |> 
  summarise(m_depth = )
  ggplot() +
  geom_point(aes(x = abb(d_temp), y = abb(d_depth), color = no),size = 3) +
  scale_y_continuous(trans = "reverse") +
  facet_wrap(vars(m))



  
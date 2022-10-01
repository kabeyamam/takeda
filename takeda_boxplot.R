#takeda 5/31 zemi

library(tidyverse)
library(lubridate)
library(ggpubr)
library(scales)
library(ggplot2)

# font の設定.

#データの読み込み
df=read.csv("takeda_data/oneday.csv") |> 
  tibble() 

df = df |> mutate(date=date(dat)) |> 
  rename("depth"="depth_ave",
         "temp"="temp_ave") |> 
  mutate(year=year(date),
         month=month(date),
         day=day(date)) |> 
  select(-c(dat,date)) |>
  mutate(ym=paste(year,month)) |> 
  mutate(ym=ym(ym)) |> 
  select(c(depth, temp, year, month, ym)) 

#0~10には5,11~20には15みたいな感じで割り振りを行う
df = df |> 
  mutate(maxdepth = 10*(ceiling(depth/10)),
         mindepth = 10*(floor(depth/10)))

data1 = tibble(d = seq(5,295,by = 10)) |> 
  mutate(mindepth = d-5,
         maxdepth = d+5) 

df1 = df |> 
  full_join(data1) |>
  drop_na() |> 
  select(c("year","month","ym","d","temp"))

#percentを与える
month_totall_n = df |> full_join(data1) |>
  drop_na() |> 
  select(c("year","month","ym","d","temp")) |> 
  group_by(month) |> 
  summarise(m_total_n = n(),.groups = "drop")

per = df1 |> 
  group_by(month,d) |> 
  summarise(n = n(),.groups = "drop") |> 
  full_join(month_totall_n) |> 
  mutate(percent = 100*n/m_total_n) |> 
  select(c(month,d,percent)) |> 
  mutate(month = factor(month, 
                        levels = 1:12,
                        labels = month.abb)) 

ym_totall_n = df |> full_join(data1) |>
  drop_na() |> 
  select(c("year","month","ym","d","temp")) |> 
  group_by(ym) |> 
  summarise(ym_total_n = n(),.groups = "drop")

ym_per = df1 |> 
  group_by(ym,d) |> 
  summarise(n = n(),.groups = "drop") |> 
  full_join(ym_totall_n) |> 
  mutate(percent = 100*n/ym_total_n) |> 
  select(c(ym,d,percent))  
# mutate(month = factor(month, 
#                       levels = 1:12,
#                       labels = month.abb)) 


#作図
df1 |> 
  mutate(month = factor(month, 
                        levels = 1:12,
                        labels = month.abb)) |> 
  left_join(per, by = c("d", "month")) |> 
  group_by(month, d) |> 
  mutate(percent = ifelse(row_number() == 1, percent, NA)) |> 
  ungroup() |> 
  ggplot() +
  # geom_histogram(aes(y = d),
  #                binwidth = 10,
  #                breaks = seq(0,300,by = 10)) +
  geom_bar(aes(x = d,
               y = percent),
           fill = "white",
           color = "black",
           stat = "identity",
           position = position_dodge(width = 0))+
  geom_boxplot(aes(x = d, y = temp*5/3, group = d)) +
  coord_flip() +
  scale_x_continuous(trans = "reverse",
                     breaks = seq(0,300,by = 50),
                     name = "Depth (m)",
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0,50),
                     # breaks = c(0,60,120,180,240,300),
                     # labels = c(0,20,40,60,80,100),
                     name = "Percent (%)",
                     expand = c(0,0),
                     sec.axis = sec_axis(~.*3/5,
                                         name="Temperature ()")) +
  facet_wrap(vars(month),nrow = 2) +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank())

xlabel_top = "'Temperature'~(degree*'C')"

df1 |> 
  # mutate(month = factor(month, 
  #                       levels = 1:12,
  #                       labels = month.abb)) |> 
  left_join(ym_per, by = c("d", "ym")) |> 
  group_by(ym, d) |> 
  mutate(percent = ifelse(row_number() == 1, percent, NA)) |> 
  ungroup() |> 
  ggplot() +
  # geom_histogram(aes(y = d),
  #                binwidth = 10,
  #                breaks = seq(0,300,by = 10)) +
  geom_bar(aes(x = d,
               y = percent),
           fill = "white",
           color = "black",
           stat = "identity",
           position = position_dodge(width = 0))+
  geom_boxplot(aes(x = d, y = temp*5/3, group = d)) +
  coord_flip() +
  scale_x_continuous(trans = "reverse",
                     breaks = seq(0,300,by = 50),
                     name = "Depth (m)",
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0,50),
                     # breaks = c(0,60,120,180,240,300),
                     # labels = c(0,20,40,60,80,100),
                     name = "Percent (%)",
                     expand = c(0,0),
                     sec.axis = sec_axis(~.*3/5,
                                         name = parse(text = xlabel_top))) +
  facet_grid(year~month) +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank()) 



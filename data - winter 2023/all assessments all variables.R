### setup

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggsignif)

### data sets

HW_Identifying_Barriers <- read.csv("IDingBarriers.csv")
HW_Youth_and_Unions <- read.csv("youthUnions.csv")
HW_Womens_March <- read.csv("womensMarch.csv")
HW_Free_tuition_and_UBI <- read.csv("tuitionUBI.csv")

# bar plotting

HW_Identifying_Barriers <- HW_Identifying_Barriers %>% select(Analytic, Clout, Tone, Cognition, Social, Lifestyle, work, Perception, space) %>% mutate(Homework = "HW_Identifying_Barriers")
HW_Youth_and_Unions <- HW_Youth_and_Unions %>% select(Analytic, Clout, Tone, Cognition, Social, Lifestyle, work, Perception, space) %>% mutate(Homework = "HW_Youth_and_Unions")
HW_Womens_March <- HW_Womens_March %>% select(Analytic, Clout, Tone, Cognition, Social, Lifestyle, work, Perception, space) %>% mutate(Homework = "HW_Womens_March")
HW_Free_tuition_and_UBI <- HW_Free_tuition_and_UBI %>% select(Analytic, Clout, Tone, Cognition, Social, Lifestyle, work, Perception, space) %>% mutate(Homework = "HW_Free_tuition_and_UBI")

# combine to one data set
all_scores <- rbind(HW_Identifying_Barriers, HW_Youth_and_Unions, HW_Womens_March, HW_Free_tuition_and_UBI)
all_scores <- all_scores %>% 
  pivot_longer(-Homework, names_to = "Variable", values_to = "score")

# aggregate scores based on dimensions
library(stringr)
all_scores$Variable <- str_to_title(all_scores$Variable)
all_scores <- all_scores %>% mutate(var2 = case_when(Variable == "Analytic"~"Thinking",
                                                     Variable == "Cognition" ~"Thinking",
                                                     Variable == "Clout" ~ "Social Leadership",
                                                     Variable == "Social"  ~ "Social Leadership",
                                                     Variable == "Lifestyle" ~  "Work and Lifestyle",
                                                     Variable == "Work" ~  "Work and Lifestyle",
                                                     Variable == "Perception"~"Experiential",
                                                     Variable == "Space"~"Experiential",
                                                     Variable =="Tone"~"Experiential"
))

all_scores$Variable <- factor(all_scores$Variable, levels =c("Analytic","Cognition", "Clout", "Social", "Lifestyle", "Work", "Perception", "Space", "Tone"))



all_scores %>% 
  group_by(Variable, Homework) %>% 
  mutate(mean = mean(score,na.rm=T)) %>% 
  mutate(Homework = factor(Homework, levels = c("HW_Identifying_Barriers", "HW_Youth_and_Unions", "HW_Womens_March", "HW_Free_tuition_and_UBI", "FINAL_Youth_and_Unions", "FINAL_Free_tuition_and_UBI"))) %>% 
  ggplot(aes(x = Variable,  y = mean, fill = Homework)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "", caption = "Percentage of words in student responses that fall into select LIWC-22 categories.", x = "Linguistic dimension",  y = "Mean score") +
  theme(legend.position = "right") +
  ylim(0, 90) +
  geom_signif(y_position = c(90,80), xmin = c(0.5,2.5), 
              xmax = c(2.4,4.4), annotation = c("Thinking","Social Leadership"),
              tip_length = .05) +
  geom_signif(y_position = c(60,75), xmin = c(4.6,6.5), 
              xmax = c(6.4,9.4), annotation = c("Work & Lifestyle","Experiential"),
              tip_length = .05)

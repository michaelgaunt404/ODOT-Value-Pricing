#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script extracts all data contained in the Oswego PDFs.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script extracts only `24 hour classificaiton` counts
#-------- script needs to be in R folder 
#-------- comment out `path and data set-up` section if sourced from RMarkdown
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(data.table)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(janitor)


#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/")
rstudioapi::getSourceEditorContext()$path %>%
  as.character() %>%
  gsub("R.*","\\1", .) %>%
  path.expand() %>%
  setwd()

#data inport~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
umbrella = fread("./data/umbrella_schedule_2.csv") %>%  
  .[,`:=`(Section_Sequence = cumsum(Task_Order)), by = Task_Section] %>% 
  .[,`:=`(Start = parse_date_time(Start, "mdy") %>% 
            as.Date(),
          End = parse_date_time(End, "mdy") %>%  
            as.Date(),
          Task_Order = cumsum(Task_Order))] %>% 
  .[order(-Task_Order)] %>% 
  .[,`:=`(Task = Task %>% 
            as.factor() %>% 
            fct_inorder())] 

schedule = read_excel("./data/umbrella_schedule_2.xlsx", 
                      sheet = "schedule", col_names = TRUE) %>%  
  remove_empty("cols") %>% 
  data.table() %>% 
  .[,`:=`(Section_Sequence = cumsum(Task_Order)), by = Task_Section] %>%
  # .[,`:=`(Start = parse_date_time(Start, "mdy") %>% 
  #           as.Date(),
  #         End = parse_date_time(End, "mdy") %>%  
  #           as.Date(),
  #         Task_Order = cumsum(Task_Order))] %>% 
  .[,`:=`(Task_Order = cumsum(Task_Order))] %>% 
  .[order(-Task_Order)] %>% print()
  .[order(Task_Section, Task_Order)] %>% 
  .[,`:=`(Task = Task %>% 
            as.factor() %>% 
            fct_inorder())] 
  
  
  schedule = read_excel("./data/umbrella_schedule_2.xlsx", 
                        sheet = "schedule", col_names = TRUE) %>%  
    remove_empty("cols") %>% 
    modify_at(c("Start", "End", "Second_Date"),  ~{as.numeric(.x) %>% 
        excel_numeric_to_date() %>%  
         as_date()}) %>% 
    data.table() %>% 
    .[,`:=`(Section_Sequence = cumsum(Task_Order)), by = Task_Section] %>%
    .[,`:=`(Task_seq = cumsum(Task_Order))] %>%
    .[order(-Task_Order)] %>%
    .[!is.na(End)]

  
  events = read_excel("./data/umbrella_example_old.xlsx", 
                      sheet = "events", col_names = TRUE) %>%  
    remove_empty("cols") %>%  
    modify_at(c("Start", "End", "Second_Date"),  
              ~{as_date(.x)}) %>% 
    data.table()
  
  schedule_combined = merge.data.table(events, schedule[,.(Task, Task_seq)],
                     by = c("Task")) %>%  
      bind_rows(schedule, .) %>%  
      .[order(-Task_seq)] %>% 
      .[,`:=`(Task = Task %>% 
                as.factor() %>% 
                fct_inorder())] 


#munging~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~raw combined data saved for prosterity 

bold.cars = c(schedule_combined[Parent_Task == Task, Task] %>% as.character())

bold.labels <- ifelse(levels(schedule_combined$Task) %in% bold.cars, yes = 20, no = 10)
tmp = max(as.numeric(schedule_combined$Task))

SSS"black")) +
  labs(x = "", y = "") +
  coord_flip() 

p1 %>%  
  plotly::ggplotly(tooltip = c("text1", "text"))


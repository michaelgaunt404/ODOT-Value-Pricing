#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Utility function for mapping.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script pulls from multiple data streams
#-------- script performs applies filtering to keep or drop files
#-------- script performs all mapping operations
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(magrittr)
# if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
# setwd("~/")
# rstudioapi::getSourceEditorContext()$path %>%
#   as.character() %>%
#   gsub("/R.*","\\1", .) %>%
#   path.expand() %>%
#   setwd()
# }
# getwd()
#sourcing utility script~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# source("global.R")

#data import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[, col])

#tabular sources================================================================
#===============================================================================
path = "./data/MasterDeliverablesList.xlsx"
del = lapply(excel_sheets(path), read_excel, path = path)[1:8]  %>%  
  rbindlist %>%  
  data.table() %>%  
  .[,`:=`(class = str_remove_all(Code, "[:punct:]") %>%  
            str_remove_all("[:digit:]") %>%  
            as.factor())]

# del = del[,-c("Description")]

del_main = del[level == 1]
del_not_main = del[level != 1]
del_group = del[level == 2]
del_deliverable = del[level == 3 & 
                        `level description` != "Milestone",]
del_milestone = del[level == 3 & 
                        `level description` == "Milestone"]

hey = del_main[, .(Title, class)] %>%  
  merge.data.table(., del_group, by = "class") %>% 
  .[, .(Title.x, Title.y)]

listter = split(hey$Title.y, hey$Title.x)

phase = read_xlsx("./data/DA_PDA_DDP_Checklist.xlsx", sheet = "phases") %>%
  remove_empty("rows") %>%  
  mutate(input_index = str_glue("{Phase_name} ({Phase_percent})"), 
         `Notes (Phase)` = str_glue('<a href="#" onclick="alert(\`Notes (Phase)`}\');">Click for Description</a>'),
         `Notes (Milestone)` = str_glue('<a href="#" onclick="alert(\`Notes (Milestone)`}\');">Click for Description</a>')) %>%  
  data.table()

deliverables = read_xlsx("./data/DA_PDA_DDP_Checklist.xlsx", sheet = "deliverables", skip = 1) %>%
  remove_empty("rows") %>%  
  select(-starts_with("DM")) %>% 
  mutate(Note = str_glue('<a href="#" onclick="alert(\`{Notes}\');">Click for Description</a>')) %>% 
  data.table()








































# # del_not_main %>%  
# #   group_by(class) %>%  
# #   nest() %>%  
# #   mutate(list = map(data, function(x) x[, "Title"] %>%  
# #                       list())) %>%  
# #   unnest(cols = list)
# phase = read_xlsx("./data/DA_PDA_DDP_Checklist.xlsx", sheet = "phases") %>%
#   remove_empty("rows")
# 
# 
# 
# date_input_dable = data.table(Phase = c("Today", phase$Phase_name %>%  
#                                           unique()), 
#                               `Expected Compeletion` = Sys.Date() %>% 
#                                 as.Date() %>% 
#                                 c(., c("2020-11-23", "2021-07-20", "2022-10-20", "2024-07-20") %>%  
#                                     as.Date())) %>%  
#   .[,`:=`(`Expected Start` = lag(`Expected Compeletion`,1))] %>%  
#   mutate(text = str_glue("Phase: {Phase} \nDuration: {`Expected Start`} to {`Expected Compeletion`}")) %>%  
#   data.table()
#   # .[,`:=`(Duration = paste0(`Expected Start`, " to ", `Expected Compeletion`))]
# 
# 
# gg = date_input_dable %>%  
#   .[Phase != "Today"] %>% 
#   ggplot() + 
#   geom_segment(aes(x = `Expected Start`, xend = `Expected Compeletion`, 
#                    y = "x", yend = "x", color = as.factor(Phase), text = text)) +
#   
# 
# plotly::ggplotly(gg, tooltip  = "text")
# 
# 
# library(gapminder)
# p <- gapminder %>%  
#   mutate(text =  paste("Year:", year, "\nCountry:", country)) %>% 
#   # mutate(text =  paste("Country:", '<a href = "https://www.wikipedia.org/"> ', country,' </a>')) %>% 
#   
#   ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, text = text)) +
#   geom_point(alpha = (1/3)) + scale_x_log10()  
# 
# ggplotly(p, tooltip = "text")
# 
# 
# paste0('<a href = "https://www.wikipedia.org/"> ', Link tocountry,' </a>')
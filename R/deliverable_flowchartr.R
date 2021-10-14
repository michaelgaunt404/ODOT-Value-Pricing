#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Maps processes as described in ST deliverables excel chart.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: N/A
#-------- N/A
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package library~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(readxl)
library(data.table)
library(magrittr)
library(stringr)
library(tidyverse)

#data import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
del_DF = read_excel("C:/Users/USMG687637/WSP O365/ST WSDOT Umbrella Agreement TO 33.00 - General/DRAFT_ST-WSDOT Key Deliverables 191119 - for ST (002).xlsx", 
                    sheet = "nu_del", 
                    na = "") %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("rows") %>% 
  map_df(str_remove_all, "\r\n") %>%
  # map_df(str_replace_all, ",", ", \r\n") %>%
  # map_df(str_replace_all, " ", "_") %>%
  # map_df(str_replace_all, ",", ", ") %>%
  map_df(str_replace_all, "Engineering", "Eng") %>%
  map_df(str_replace_all, "Attorney General", "AG") %>%
  map_df(str_replace_all, "Deputy Executive Director", "DED") %>%
  map_df(str_replace_all, "Assistant", "Asst") %>%
  map_df(str_replace_all, "Manager", "Mgr") %>%
  map_df(str_replace_all, "Deputy", "Dep") %>%
  map_df(str_replace_all, "Adminstrator", "Admin") %>%
  map_df(str_replace_all, "Director", "Dir") %>%
  map_df(str_replace_na) %>%  
  data.table()

#data clean~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
del_DF$NA_Count = del_DF %>% 
  modify(str_count, "NA") %>%  
  rowSums()

del_DF$TBD_Count = del_DF %>% 
  modify(str_count, "TBD") %>%  
  rowSums()

del_DF$Full_Process = del_DF$NA_Count + del_DF$TBD_Count

#might not need this in the long run 
# person_lookup = del_DF %>% 
#   .[,c("wsdot_lead", "wsdot_sign", "wsdot_decide", "st_recommend", 
#        "st_agree", "st_perform", "st_input", "st_decide")] %>% 
#   .[,`:=`(Dummy = "dummy")] %>% 
#   melt.data.table(id.vars = "Dummy", value.name = "Person") %>%  
#   .[, Person] %>%
#   str_trim() %>% 
#   unique() %>%
#   sort() %>% 
#   data.table(Person = .,
#              Seq = seq(0, to = length(.)-1, by =1))

#utility functions~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
count_gettr = function(df, col1, col2, level1, level2){
  df[,.(Process_Count = .N), by = c('deliverable', col1, col2)] %>% 
    setnames(c("deliverable", "From", "To", "Process_Count")) %>%  
    .[,`:=`(From = paste0(From,"_",level1),
            To = paste0(To,"_",level2))] 
  
}

sankey_plotr = function(DF, color){
  tmp = bind_rows(count_gettr(DF, "st_input", "st_perform", "input", "per"),
                  count_gettr(DF, "st_perform", "st_recommend", "per", "rec"),
                  count_gettr(DF, "st_recommend", "st_decide", "rec", "dec"),
                  count_gettr(DF, "st_decide", "st_agree", "dec", "agr"),
                  count_gettr(DF, "st_agree", "wsdot_decide", "agr", "wsdot_dec"), 
                  count_gettr(DF, "wsdot_decide", "wsdot_sign", "wsdot_dec", "wsdot_sign"))
  
  person_lookup = data.table(Person = c(tmp$From, tmp$To)) %>%  
    .[order(Person)] %>% 
    unique() %>% 
    .[,`:=`(Seq = seq(0, to = nrow(.)-1, by =1),
            Agency = ifelse(str_detect(Person, "wsdot"), "WSDOT", "Sount Transit") %>%  
              as.factor())] 
  
  tmp2 = tmp %>% 
    merge.data.table(., person_lookup, 
                     by.x = "To", by.y = "Person") %>% 
    merge.data.table(., person_lookup, 
                     by.x = "From", by.y = "Person", suffixes = c("_to", "_from")) %>% 
    .[order(-Process_Count)] %>% 
    .[,`:=`(group = ifelse(deliverable == "Design Approval (at 30%)", "DA", "Other") %>%  
              as.factor())] %>%  
    .[,-c("Agency_to", 'Agency_from')]
  
  # my_color <- 'd3.scaleOrdinal() .domain(["Sount Transit", "WSDOT", "Real Property Interests", "Project Approvals", "Agreements", "Other"]) .range(["#8B2E8B", "#55FFFF", "#D98594", "#228BDC", "#D96237", "#00B19D"])'
  
  if (color=="Y") {
    plot = networkD3:::sankeyNetwork(Links = tmp2, Nodes = person_lookup, 
                                     Source = "Seq_from", Target = "Seq_to", 
                                     Value = "Process_Count", NodeID = "Person", 
                                     fontSize = 10, nodeWidth = 15,
                                     LinkGroup = "group", NodeGroup = "Agency", nodePadding = 40)
  } else {
    plot = networkD3:::sankeyNetwork(Links = tmp2, Nodes = person_lookup, 
                                     Source = "Seq_from", Target = "Seq_to", 
                                     Value = "Process_Count", NodeID = "Person", 
                                     fontSize = 10, nodeWidth = 15, 
                                     NodeGroup = "Agency", nodePadding = 40)
  }
  return(plot)
}

DF = del_DF_tmp
person_lookup %>%  str()
del_DF_tmp = del_DF
del_DF_tmp = del_DF %>% .[1:20,]
del_DF_tmp = del_DF %>% .[type == "Real Property Interests",]
del_DF_tmp = del_DF %>% .[type == "Project Approvals",]
del_DF_tmp = del_DF %>% .[type == "Agreements",]
del_DF_tmp = del_DF %>% .[NA_Count == 0]

del_DF_tmp = del_DF %>% .[deliverable == "Design Approval (at 30%)",]
  
sankey_plotr(del_DF_tmp, "n")
sankey_plotr(del_DF_tmp, "Y")




















set.seed(1999)
links <- data.table(
  src = rep(0:4, times=c(1,1,2,3,5)),
  target = sample(1:11, 12, TRUE),
  value = sample(100, 12)
)[src < target, ]  # no loops
nodes <- data.table(name=LETTERS[1:12]) %>% 
  data.table()

## Add text to label
txt <- links[, .(total = sum(value)), by=c('target')] %>%  
  data.table()
nodes[txt$target+1L, name := paste0(name, '<br>(', txt$total, ')')]












del_DF %>%  
  .[,.(.N), by = .(type, NA_Count)]




















summary_to_long <- function(summary_table,prefix = "_dm$",id_cols = character()){
  print(prefix)
  
  prefix_set = paste0("(",prefix,"|",
                      str_replace(prefix,"\\$","_low$"),"|",
                      str_replace(prefix,"\\$","_upp$"),")")
  
  long_table <- summary_table %>% 
    dplyr::select(one_of(id_cols),
                  matches(prefix_set)) %>% 
    pivot_longer(cols=matches(prefix_set),
                 names_to = "variable",
                 values_to = "prevalence") %>% 
    mutate(sex = case_when(str_detect(variable,"^f_") ~ "female",
                           str_detect(variable,"^m_") ~ "male",
                           str_detect(variable,"^c_") ~ "child",
                           TRUE ~ NA_character_),
           estimate = case_when(str_detect(variable,"low$") ~ "lci",
                                str_detect(variable,"upp$") ~ "uci",
                                TRUE ~ "est"),
           
           variable = str_replace(prefix,"_","") %>% str_replace(.,"\\$","")
    ) %>% 
    pivot_wider(names_from=estimate,values_from=prevalence)
  
  return(long_table)
  
  
}

# Fonction tableau simple
tableau <- function(data, filtre_com, var_quali, pond=IPONDL, nom_var_quali){
  
  tab <- data %>% 
    filter(COMMUNE == filtre_com) %>% 
    count({{ var_quali }}, wt={{ pond }}) %>% 
    mutate(Pourcentage=prop.table(n)*100, Pourcentage=round(Pourcentage, 1)) %>% 
    adorn_totals("row") %>% 
    rename(Effectif=n, {{nom_var_quali}}:={{ var_quali }}) 
  
  return(tab)
  
}



# Fonction somme
somme <- function(data, ..., var_gpe, nom_var, var1){
  
  som_1 <- data %>% 
    filter(...) %>% 
    group_by({{var_gpe}}) %>% 
    summarise({{nom_var}}:=sum({{var1}}, na.rm=T)) 
  
  return(som_1)
  
}


# Fonction tableau de contingence
tab_cont_n_iris <- function(data, arrdt_filtre, ..., nom_var, var, prefix_var)
{
  tab_n <- data %>% 
            filter(ARM %in% arrdt_filtre) %>%
            group_by(...) %>%
            summarise({{ nom_var }} := round(sum(IPONDL))) %>% 
            pivot_wider(names_from = {{ var }}, values_from = {{ nom_var }},
                        values_fill = 0, names_prefix = prefix_var)
  
  return(tab_n)
}




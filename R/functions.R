
# Graphical functions -----------------------------------------------------

# Crée un histogram représentant un tri à plat de la variable demandée en argument
# en fonction du genre
# Changement de couleurs du fill, du nom des axes x,y.
# Le nom de l'axe x et du titre varie selon la variable demandeé
graphiques_factor <- function(var){
  ggplot(df) +
    geom_bar() +
    aes(x = {{var}}, fill = Gender) +
    theme_classic() +
    scale_fill_brewer(palette = 'Set1') +
    labs(y = 'Count',
         x = as_string(ensym(var)) %>%
           str_replace_all(.,'_',' ') %>%
           str_to_title(., locale = 'en'),
         title = paste(as_string(ensym(var)) %>%
                         str_replace_all(.,'_',' ') %>%
                         str_to_title(., locale = 'en'),"variable's repartition"),
         subtitle = 'According to gender')
  
}



# Table function ----------------------------------------------------------

# Fonction qui prend une liste de variable comme argument et qui ressort
# un tri à plat des variables en fonction du genre

table_gender <- function(group_var){
  # Créer un tableau où chaque ligne est un duo de table entre la variable demandée et le genre
  list_of_table <- tibble(Variables = group_var, Gender = rep('Gender', length(group_var)))
  
  # Map en parallèle les deux colonnes et pour chaque ligne, ressort une table
  # On ressort la table en fréquence (et non en effectif) que l'on arrondit et on ajoute les sommes
  # Change le nom des index de la fonction map pour les faire correspondre aux variables d'intérêts
  table <- map2(list_of_table$Variables,list_of_table$Gender, ~ table(df[[.x]],df[[.y]])) %>%
    map(~ round(prop.table(.), digits = 2)) %>%
    map(~ addmargins(.)) %>%
    setNames(group_var)
  
  return(table)
}


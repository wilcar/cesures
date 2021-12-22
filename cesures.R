##############################################################################
# Suppression des césures en fin des lignes et reconstitution des mots brisés
# Wilfrid Cariou 2021
###############################################################################

text <- readLines("AF4eC.txt", encoding = "UTF-8")
                
# https://stackoverflow.com/questions/69497026/ocr-unbreak-the-words-cut-at-the-end-of-lines-and-keep-the-paragraphs/69497218#69497218


# convertion en df 

textdf <- as.data.frame((text))


library(dplyr)
textdf  <- textdf %>%
  rename( text = '(text)') 


textdf <-  textdf %>%
  # mots se terminant par  "-"
  mutate(cut_word = grepl('-$', text), 
         # supprimer le dernier "-"
         text = sub('¬$', '', text), 
         #Si cut_word récupérer le 1er mot de la valeur suivante et le coller dans la valeur actuelle.
         text = ifelse(cut_word, paste0(text, stringr::word(lead(text), 1)), text), 
         #supprimer premier mot si le mot précédédent est coupé.
         text = ifelse(lag(cut_word, default = FALSE), sub('.*?\\s', '', text), text))

# Conversion en vecteur

text <-  pull(textdf, text)

# L'argument useBytes permet de gérer le s long au niveau de l'encodage
writeLines(text, "annee_foise4eCampSansCesures.txt" ,  useBytes = TRUE)



  
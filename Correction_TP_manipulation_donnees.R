
## ----librairie_nettoyage, message = FALSE, warning = FALSE, include = TRUE----
rm(list = ls()) # Nettoyage de l'environnement de travail
library(tidyverse) # Chargement de la librairie tidyverse


## ----sol_initial, include = TRUE---------------------------
sol_initial <- read.table("donnees_sols_chimies.csv",
                          sep = ",", # Séparateur de champs
                          header = TRUE) %>% # La 1ere ligne donne le nom des colonnes
  as_tibble() # Facilite l'affichage ensuite
sol_initial


## ----abondance_initial, include = TRUE---------------------
abondance_initial <- read.table("donnees_abondance.csv",
                                sep = ",", # Separateur de champ
                                header = TRUE, # 1ere ligne donne le nom des colonnes
                                row.names = 1 # La 1ere colonne est le numero de ligne
                                ) %>%
  as_tibble() # Facilite l'affichage ensuite
abondance_initial


## ----q_1a--------------------------------------------------
sol_intermediaire_1 <- select(sol_initial, -Plot, 
                              -Subplot, -Block, 
                              -Name1, -Names2)


## ----Ex1_q_1b----------------------------------------------
sol_intermediaire_2 <- rename(sol_intermediaire_1,
                              Eau = MC, 
                              Exc.Cations = ECEC,
                              Sol = Soil, Site = Name, 
                              Profondeur = Depth, 
                              SatBase = BS,
                              Argile = Clay, 
                              Limon = Silt,
                              Sable = Sand)


## ----Ex1_q_1c----------------------------------------------
sol_intermediaire_3 <- mutate(sol_intermediaire_2, # Tableau dans lequel
                              # on modifie les colonnes
                              Sol = factor(Sol, 
                                           levels = c("Alluvial", "Sandstone", "Heath"),
                                           labels = c("Alluvial", "Grès", "Dunaire")),
                              Profondeur = factor(Profondeur,
                                                  levels = c("0-5 cm", "5-20 cm", "20-35 cm")))
head(sol_intermediaire_3)
table(sol_intermediaire_2$Profondeur)

ggplot(sol_intermediaire_3) +
  aes(x = Profondeur, y = pH) +
  geom_boxplot()
## ----Ex1_q_1d----------------------------------------------
sol_intermediaire_4 <- sol_intermediaire_3%>%mutate(Argile=as.double(Argile))%>%group_by(Sol) %>% 
  mutate_if(is.numeric, # On ne change que les colonnes numériques
            # On remplace les na par la moyenne par colonne
            function(colonne) replace_na(colonne, floor(mean(colonne, na.rm = TRUE)))) %>% 
  ungroup() # On dégroupe pour la suite


## ----suppresion_sols_intermediaire, include = TRUE---------
# On supprime les tableaux intermediaires inutiles
rm(sol_intermediaire1, sol_intermediaire2, 
   sol_intermediaire3)


## ----Ex1_q_2-----------------------------------------------
sol_propre <- sol_initial %>% 
  select(-Plot, -Subplot, -Block, -Name1, -Names2) %>% 
  rename(Eau = MC, Exc.Cations = ECEC,
         Sol = Soil, Site = Name, 
         Profondeur = Depth, SatBase = BS,
         Argile = Clay, Limon = Silt,
         Sable = Sand) %>% 
  mutate(Sol = factor(Sol, 
                      levels = c("Alluvial", "Sandstone", "Heath"),
                      labels = c("Alluvial", "Grès", "Dunaire")),
         Profondeur = factor(Profondeur,
                             levels = c("0-5 cm", "5-20 cm", "20-35 cm"))) %>% 
  group_by(Sol) %>% 
  mutate_if(is.numeric, # On ne change que les colonnes numériques
            # On remplace les na par la moyenne par colonne
            function(colonne) replace_na(colonne, mean(colonne, na.rm = TRUE))) %>% 
  ungroup() # On dégroupe pour la suite


## ----Ex1_q_3-----------------------------------------------
# Sol superficiel
sol_superficiel <- sol_propre %>% # Dans sol propre
  # On filtre les lignes
  filter(Profondeur == "0-5 cm") %>% # On ne sélectionne que les lignes
  # de la couche 0-5cm
  dplyr::select(-Profondeur) # On vire la profondeur

# Sol moyen
sol_moyen <- sol_propre %>% # Dans sol propre
  group_by(Sol, Site) %>% # On groupe par site (pour chaque site, j'ai 3 lignes)
  # On groupe aussi par sol pour conserver l'information sol
  # Chaque traitement futur sera fait par site (sur plein de tableaux à 3 lignes)
  summarise_if(is.numeric, # Pour chaque groupe, on a trois lignes, on résume
               mean) # En prenant la moyenne


## ----Ex1_q4------------------------------------------------
abondance_propre <- abondance_initial %>% 
  # On renomme la colonne Name en Site
  rename(Site = Name) %>% 
  # On modifie cette colonne
  mutate(Site = str_replace(Site, " ", "_"))


## ----abondance_long----------------------------------------
abondance_long <- pivot_longer(abondance_propre, # Tableau à transformer
                               # Colonnes à regrouper
                               cols = -c("Site"), # On regroupe tout le monde sauf la
                               # colonne Site (d'où le "-" devant le nom de colonne)
                               names_to = "Espece", # Les noms des colonnes initiales
                               # seront regroupés dans une unique colonne "Espece"
                               values_to = "NbIndividus") # Les valeurs associées
# seront retournées dans une unique colonne NbIndividus


## ----Ex1_q_6-----------------------------------------------
abondance_genre_metasite <- abondance_long %>% 
  # On sépare la colonne Site en deux colonnes
  separate(Site, into = c("MetaSite", "Bloc"), sep = "_") %>%
  # On sépare la colonne espece en deux colonnnes sur la base du "."
  # REMARQUE: Ici, 3 especes ont deux "." dans leur nom, d'où les warning.
  separate(Espece, into = c("Genre", "Espece"), sep = "[.]") %>% 
  # On groupe par MetaSite et Genre
  group_by(MetaSite, Genre) %>% 
  # On résume dans la colonne NbArbres, on compte le nombre en sommant le nombre d'individus
  summarise(NbArbres = sum(NbIndividus))


## ----Ex1_q_7-----------------------------------------------
richesse_par_site <- abondance_long %>% 
  filter(NbIndividus > 0) %>% # On filtre sur la condition nombre d'individus plus grand que 0
  group_by(Site) %>% # On groupe par site
  # Et on donne deux résumés
  summarise(densite_arbre = sum(NbIndividus) / 400, # La somme du nombre d'individus par Site
            richesse_specifique = n()) # Le nombre d'espèces vu (car on a enlevé les 0)
# La fonction n() compte le nombre de lignes du tableau dans lequel on est
# Donc ici, par site


## ----Ex1_q_8-----------------------------------------------
# left join
left_join(richesse_par_site, sol_superficiel)

# inner join
# Jointure interne (intersection)
# Seuls les 179 sites présents dans les deux tables sont conservés.
# Dimension 179 x 22
inner_join(richesse_par_site, sol_superficiel)

# donnees_richesse
donnees_richesses <- inner_join(richesse_par_site, sol_superficiel)


## ----Ex2_q_1-----------------------------------------------
ggplot(donnees_richesses) + # Données à représenter
  # Quelles colonnes sont représentées comment?
  aes(x = richesse_specifique, # En abscisse
      y = densite_arbre, # En ordonnée
      color = Sol) + # En couleur
  geom_point()


## ----Ex2_q_2-----------------------------------------------
ggplot(donnees_richesses) + # Données à représenter
  # Quelles colonnes sont représentées comment?
  aes(x = richesse_specifique, # En abscisse
      y = densite_arbre, # En ordonnée
      color = Sol) + # En couleur
  geom_point() +
  labs(x = "Richesse spécifique", y = "Densité d'arbres", 
       title = "Densité d'arbres en fonction de la richesse spécifique à Bornéo")


## ----Ex2_q_3-----------------------------------------------
ggplot(sol_propre) + # Données à représenter
  # Quelles colonnes sont représentées comment?
  aes(x = Sol, # En abscisse
      y = Exc.Cations) + # En ordonnée
  geom_boxplot() +
  labs(x = "Type de Sol", y = "Quantité de cations échangeables")


## ----Ex2_q_4-----------------------------------------------
ggplot(sol_propre) + # Données à représenter
  # Quelles colonnes sont représentées comment?
  aes(x = Sol, # En abscisse
      y = Exc.Cations,  # En ordonnée
      fill = Profondeur) + # Selon quelle colonne on remplit
  geom_boxplot() +
  labs(x = "Type de Sol", y = "Quantité de cations échangeables")


## ----Ex2_q_5-----------------------------------------------
ggplot(sol_propre) + # Données à représenter
  # Quelles colonnes sont représentées comment?
  aes(x = NO3) + # En abscisse
  geom_histogram(breaks = seq(0, 25, by = 1),
                 fill = "lightblue", color = "black") +
  facet_wrap(~Sol) # Un graphe par type de Sol


## ----Ex2_q_6-----------------------------------------------
ggplot(sol_propre) + # Données à représenter
  # Quelles colonnes sont représentées comment?
  aes(x = NO3,  # En abscisse
      fill = Profondeur) + # Remplissage selon la colonne Profondeur
  geom_histogram(breaks = seq(0, 25, by = 1),
                 position = "identity", alpha = 0.5) +
  labs(x = "Quantité de nitrates",
       y = "Nombre d'occurences")


## ----Ex2_q_7-----------------------------------------------
ggplot(sol_propre) + # Données à représenter
  # Quelles colonnes sont représentées comment?
  aes(x = NO3,  # En abscisse
      fill = Profondeur) + # Remplissage selon la colonne Profondeur
  geom_histogram(breaks = seq(0, 25, by = 1),
                 position = "identity", alpha = 0.5) +
  labs(x = "Quantité de nitrates",
       y = "Nombre d'occurences") +
  facet_wrap(~Sol) + # Un graphe par type de Sol
  theme_bw()


## ----Ex2_q_8-----------------------------------------------
ggplot(abondance_genre_metasite) + # Données à représenter
  # Quelles colonnes sont représentées comment?
  aes(x = Genre,  # En abscisse
      y = MetaSite, # En ordonnée
      fill = NbArbres) + # Remplissage selon la colonne Profondeur
  geom_raster()


## ----theme, eval = FALSE, include = TRUE-------------------
## theme(legend.position = "none", # pas de légende
##       axis.text.x = element_text(angle = 90, size = 6), # Le texte en absisses est est vertical et petit
##       axis.text.y = element_text(size = 7)) # On rapetisse la taille du texte en y


## ----Ex2_q_9-----------------------------------------------
ggplot(abondance_genre_metasite) + # Données à représenter
  # Quelles colonnes sont représentées comment?
  aes(x = Genre,  # En abscisse
      y = MetaSite, # En ordonnée
      fill = NbArbres) + # Remplissage selon la colonne Profondeur
  geom_raster() +
  theme(legend.position = "none", # pas de légende
      axis.text.x = element_text(angle = 90, size = 6), # Le texte en absisses est est vertical et petit
      axis.text.y = element_text(size = 7)) # On rapetisse la taille du texte en y


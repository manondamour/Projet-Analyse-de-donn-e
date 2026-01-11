# Installation des packages (si nécessaire)
install.packages("tidyverse")
install.packages("igraph")
install.packages("rvest")
install.packages("lubridate")

#chargement des packages 
library(tidyverse)
library(igraph)
library(rvest)
library(lubridate)

# Création d'une traduction des mois pour lubridate
mois_fr_an <- c("jan"="Jan", "fév"="Feb", "mar"="Mar", "avr"="Apr", "mai"="May", "juin"="Jun", 
                "juil"="Jul", "août"="Aug", "sep"="Sep", "oct"="Oct", "nov"="Nov", "déc"="Dec")

# Importation des données
#  Importation des abonnements 
following <- read_html("Analyse de donnée/connections/followers_and_following/following.html") %>% {
  page <- . 

# Création d'un tableau propre 
tibble(
  followed = page %>% html_elements("h2") %>% html_text(), 
  date_raw = page %>% html_elements("._a6-p div > div:last-child") %>% html_text(),
  follower = "me"
)
} %>%
  mutate(timestamp = str_replace_all(date_raw, mois_fr_en) %>% mdy_hm())

# Importation des abonnés
followers <- read_html("Analyse de donnée/connections/followers_and_following/followers_1.html") %>% {
  page <- .
  
  #création d'un tableau propre
  tibble(
    follower = page %>% html_elements(".pam a") %>% html_text(),
    date_raw = page %>% html_elements("._a6-p div > div:last-child") %>% html_text(),
    followed = "me"
  )
} %>%
  mutate(timestamp = str_replace_all(date_raw, mois_fr_en) %>% mdy_hm())

# Affichage
head(following)
head(followers)

# 2. Différenciation des profils (célébrités ou personnel)

marques_stars <- c("adidas", "netflix", "fcbarcelona", "psg", "pullandbear", "badgalriri", "arianagrande")

following <- following %>%
  mutate(type = if_else(followed %in% marques_stars | !str_detect(followed, "[._]"), 
                        "Marque/Célébrité", "Personnel"))
head(following)

# 3. analyse de la réciprocité et de l'ancienneté dans le réseau 

# Relations réciproques
reciprocal <- following %>%
  inner_join(followers, by = c("followed" = "follower"))
head(reciprocal)

# Relations non réciproques (Detection des désabonnements ou non-retours)
non_reciprocal <- following %>%
  anti_join(followers, by = c("followed" = "follower")) %>%
  filter(type == "Personnel") %>% # Focus sur les vrais désabonnements personnels
  # Analyse de l'ancienneté : calcul du nombre de jours depuis l'abonnement
  mutate(jours_anciennete = as.numeric(difftime(now(), timestamp, units = "days")))
head(non_reciprocal)

# 4. Les désabonnements

# Import du fichier des désabonnements récents
unfollowed_by_me <- read_html("Analyse de donnée/connections/followers_and_following/recently_unfollowed_profiles.html") %>% {
  page <- . 
 
  tibble(
    username = page %>% html_elements(".pam a") %>% html_text(),
    date_raw = page %>% html_elements("._a6-p div > div:last-child") %>% html_text()
  )
} %>%

  mutate(date_unfollow = str_replace_all(date_raw, mois_fr_en) %>% mdy_hm())

head(unfollowed_by_me)

unfollowed_followers <- unfollowed_by_me %>%
  inner_join(followers, by = c("username" = "follower"))
head(unfollowed_followers)

# 5. Visualisation du réseau par des graphiques

# 1-Courbe de densité de l'ancienneté
ggplot(non_reciprocal, aes(x = timestamp)) +
  geom_area(stat = "bin", bins = 25, fill = "pink", alpha = 0.3) +
  geom_line(stat = "bin", bins = 25, color = "pink", size = 0.8) +
  theme_minimal() +
  labs(title = "Chronologie des relations non réciproques",
       subtitle = "Analyse des flux d'abonnements asymétriques",
       x = "Date d'abonnement", 
       y = "Densité de comptes")

# 2- Donut sur la réciporcité
# Préparation des données du Donut
stats_donut <- tibble(
  Categorie = c("Réciproques", "Non-Réciproques", "Marques/Stars"),
  Valeur = c(
    nrow(reciprocal), 
    nrow(non_reciprocal), 
    nrow(following %>% filter(type == "Marque/Célébrité"))
  )
)

# Visualisation Donut
ggplot(stats_donut, aes(x = 2, y = Valeur, fill = Categorie)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) + 
  theme_void() +
  scale_fill_manual(values = c("Marques/Stars" = "blue", 
                               "Non-Réciproques" = "pink", 
                               "Réciproques" = "green")) +
  labs(title = "Audit de Réciprocité",
       subtitle = "Analyse structurelle des abonnements")

# 3- Graphe du réseau
# Construction des liens
lien_social <- bind_rows(
  following %>% select(from = follower, to = followed),
  followers %>% select(from = follower, to = followed)
)

# Création de l'objet igraph
graph <- graph_from_data_frame(lien_social, directed = TRUE)

# Amélioration de la lisiblité
lien_cible <- ends(graph, E(graph))[,2]
E(graph)$color <- if_else(lien_cible %in% non_reciprocal$followed, "red", "green")

# 4. Personnalisation 
V(graph)$color <- if_else(V(graph)$name == "me", "gold", "lightgray")
V(graph)$frame.color <- NA

# 5. Affichage
plot(graph, 
     vertex.size = 4, 
     vertex.label = NA, 
     edge.arrow.size = 0.2,
     edge.curved = 0.1,
     main = "Topologie du Réseau Instagram")

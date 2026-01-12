---
title: "Analyse des relations Instagram"
author: "DAMOUR Manon / MAUDOUX Malorie"
date: "2026-01-12"
output:
  ioslides_presentation:
    widescreen: true
    smaller: true
    highlight: tango
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)# Manipulation de données
library(igraph) # Analyse et visualisation de graphes
library(rvest) # Extraction de données depuis des fichiers HTML
library(lubridate)

# bloc de préparation
# Ce bloc doit contenir tout le code nécessaire pour charger les fichiers

#dictionnaire des mois traduis pour lubridate
mois_fr_an <- c("jan"="Jan", "fév"="Feb", "mar"="Mar", "avr"="Apr", "mai"="May", "juin"="Jun", 
                "juil"="Jul", "août"="Aug", "sep"="Sep", "oct"="Oct", "nov"="Nov", "déc"="Dec")

# Import des abonnements 
following <- read_html("Analyse de donnée/connections/followers_and_following/following.html") %>% {
  page <- .
  
  # On crée le tibble avec de vrais vecteurs
  tibble(
    followed = page %>% html_elements("h2") %>% html_text(), 
    date_raw = page %>% html_elements("._a6-p div > div:last-child") %>% html_text(),
    follower = "me"
  )
}

#  Transformation de la colonne 'following'
following <- following %>%
  mutate(
    # On remplace les mois français par anglais, puis on convertit en POSIXct
    timestamp = str_replace_all(date_raw, mois_fr_an) %>% mdy_hm()
  )

# Import des abonnés 
followers <- read_html("Analyse de donnée/connections/followers_and_following/followers_1.html") %>% {
  page <- .
  
  tibble(
    follower = page %>% html_elements(".pam a") %>% html_text(),
    date_raw = page %>% html_elements("._a6-p div > div:last-child") %>% html_text(),
    followed = "me"
  )
} 

#  Création des variables nécessaires pour les slides suivantes
marques_stars <- c("adidas", "netflix", "fcbarcelona", "psg", "pullandbear", "badgalriri", "arianagrande")

following <- following %>%
  mutate(type = if_else(followed %in% marques_stars | !str_detect(followed, "[._]"), 
                        "Marque/Célébrité", "Personnel"))

#  Les relations réciproques (Pour le Donut)
reciprocal <- following %>% 
  inner_join(followers, by = c("followed" = "follower"))

# Les relations non récirpoques
non_reciprocal <- following %>%
  anti_join(followers, by = c("followed" = "follower")) %>%
  filter(type == "Personnel")

#  Le nombre de marques (Pour le Donut)
nb_marques <- nrow(following %>% filter(type == "Marque/Célébrité"))
```

## Contexte et objectif 

Instagram est un réseau social basé sur des relations dirigées.
Contrairement à l’amitié sur d'autres réseaux, une relation peut être non réciproque.

**Objectif du projet** :

- Analyser la structure des abonnements Instagram
- Identifier les relations réciproques et non réciproques
- Étudier leur évolution dans le temps
- Visualiser le réseau social personnel

## Packages utilisés 


- **tidyverse** : pour charger dplyr (pour la manipulation), ggplot2 (visuels) et stringr

- **rvest** : pour l'extraction de données depuis des fichiers HTML

- **lubridate** : pour la gestion simplifiée des formats de dates et d'heures
    
- **igraph** : pour l'analyse et la visualisation de graphes

```{r, eval=FALSE, echo=TRUE}
library(tidyverse)# Manipulation de données
library(igraph) # Analyse et visualisation de graphes
library(rvest) # Extraction de données depuis des fichiers HTML
library(lubridate) # Gestion simplifiée des formats de dates et d'heures
```

## Données utilisées 

Source des données :

- Export officiel Instagram
- Données personnelles uniquement

Fichiers utilisés :

- following.html : comptes que je suis
- followers_1.html : comptes qui me suivent
- recently_unfollowed_profiles.html : désabonnements récents

Informations extraites :

- Nom du compte
- Date de début de la relation
- Sens de l’abonnement

## Importation des abonnements et abonnés 


- Extraction des données HTML, nettoyage des données et création d'un tableau propre

- Problème technique apparu : pour le dossier **followers_1.html**, les abonnés sont donnés sous forme de lien, il faut donc demander à R de récupérer uniquement les noms des utilisateurs.

```{r, eval=FALSE, echo=TRUE}
# Import des abonnés 
followers <- read_html("Analyse de donnée/connections/followers_and_following/followers_1.html") %>% {
  page <- .
  
  tibble(
    follower = page %>% html_elements(".pam a") %>% html_text(),
    date_raw = page %>% html_elements("._a6-p div > div:last-child") %>% html_text(),
    followed = "me"
  )
} %>%
  mutate(timestamp = str_replace_all(date_raw, mois_fr_an) %>% mdy_hm())
```



##  Différenciation des profils



Nous ne nous attendons pas à ce que des personnes connus nous suivent en retour, il faut donc différencier les comptes certifiés des comptes personnels pour ne pas fausser les relations asymétriques.

- Isolemement des comptes connus (sert de base de référence)
- Recherche de caractères spéciaux utilisés pour les comptes personnels

```{r, eval=FALSE, echo=TRUE} 
marques_stars <- c("adidas", "netflix", "fcbarcelona", "psg", "pullandbear", "badgalriri", "arianagrande")

following <- following %>%
  mutate(type = if_else(followed %in% marques_stars | !str_detect(followed, "[._]"), 
                        "Marque/Célébrité", "Personnel"))
head(following)
```

## Identification des relations réciproques et non réciproques

 
 -  Pour les relations réciproques : Utilisation de la commande **inner_join** pour effectuer une jointure enre le tableau following et le tableau followers, et ainsi conserver uniquement les lignes présentent dans les deux tableaux
 
```{r, eval=FALSE, echo=TRUE}
reciprocal <- following %>%
  inner_join(followers, by = c("followed" = "follower"))
head(reciprocal)
```
 
  - Pour les relations asymétriques : Utilisation de la commande **anti_join** pour supprimer tous ceux qui se trouvent dans les deux tableaux

```{r, eval=FALSE, echo=TRUE}
non_reciprocal <- following %>%
  anti_join(followers, by = c("followed" = "follower")) %>%
  filter(type == "Personnel") %>% # Focus sur les vrais désabonnements personnels
```

- Analyse de l'ancienneté : la fonction **difftime(now(), timestamp, units = "days")** permet de calculer l'écart de temps entre "Maintenant" et le moment où on a suivi la personne
```{r, eval=FALSE, echo=TRUE}
mutate(jours_anciennete = as.numeric(difftime(now(), timestamp, units = "days")))
```

## Désabonnements

- Importation des désabonnments récemment (ceux que nous avons supprimés récemment) : Extraction des données HTML, nettoyage des données et création d'un tableau propre

```{r, eval=FALSE, echo=TRUE}
# Import du fichier des désabonnements récents
unfollowed_by_me <- read_html("Analyse de donnée/connections/followers_and_following/recently_unfollowed_profiles.html") %>% {
  page <- . 
  tibble(
    username = page %>% html_elements(".pam a") %>% html_text(),
    date_raw = page %>% html_elements("._a6-p div > div:last-child") %>% html_text()
  )
}
```


- Surveillance (ceux qui nous suivent encore alors qu'on s'est désabonné) : Utilisation de la commande **inner_join** de la même manière que pour les relations réciproques

```{r, eval=FALSE, echo=TRUE}
unfollowed_followers <- unfollowed_by_me %>%
  inner_join(followers, by = c("username" = "follower"))
head(unfollowed_followers)
```


```{r, include=FALSE}
# Ce bloc est invisible mais indispensable pour créer les objets
# 1. On identifie les non-réciproques
non_reciprocal <- following %>%
  anti_join(followers, by = c("followed" = "follower")) %>%
  filter(type == "Personnel") %>%
  mutate(jours_anciennete = as.numeric(difftime(now(), timestamp, units = "days")))

lien_social <- bind_rows(
  following %>% select(from = follower, to = followed),
  followers %>% select(from = follower, to = followed)
)

# Création de l'objet igraph
graph <- graph_from_data_frame(lien_social, directed = TRUE)
```


## Limites à identifier 

**Dans la segmentation des profils** : 

- Faux positifs : Un ami avec un pseudo simple (sans cractère spécial, juste son prénom) sera classé en "Marque/Célébrité"

- Faux négatifs : Une marque avec un point comme pull.and.bear (si elle n'est pas dans ta liste) sera classée en "Personnel"

**Autres limites** : 

- La confidentialité : on ne peut pas avoir accès aux comptes des autres (nombres d'abonnés...), juste avoir le nom d'utilisateur 

- La base de données exportée ne permet pas de voir qui a supprimé qui 

- On ne peut pas savoir depuis combien de temps une relation est asymétrique

- Les données exportées sont des données à un instant pour voir si des changements sont survenus plus récemment il faut ré-exporter des données

## Quelques visualisations 


```{r, echo=FALSE, message=FALSE, warning=FALSE, fig.height=5, fig.width=8}
ggplot(non_reciprocal, aes(x = timestamp)) +
  geom_area(stat = "bin", bins = 25, fill = "pink", alpha = 0.3) +
  geom_line(stat = "bin", bins = 25, color = "pink", size = 0.8) +
  theme_minimal() +
  labs(
    title = "Chronologie des relations non réciproques",
    x = "Date d'abonnement",
    y = "Densité"
  )
```

## 
```{r, echo=FALSE, message=FALSE, warning=FALSE, fig.height=5, fig.width=8}
# Calcul des stats pour le Donut
stats_donut <- tibble(
  Categorie = c("Réciproques", "Non-Réciproques", "Marques/Stars"),
  Valeur = c(nrow(reciprocal), 
             nrow(non_reciprocal), 
             nrow(following %>% filter(type == "Marque/Célébrité")))
)

# Affichage du Donut
ggplot(stats_donut, aes(x = 2, y = Valeur, fill = Categorie)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) + 
  theme_void() +
  scale_fill_manual(values = c("Marques/Stars" = "blue", 
                               "Non-Réciproques" = "pink", 
                               "Réciproques" = "green")) +
  labs(title = "Audit global du capital social")
```


##

```{r, echo=FALSE, message=FALSE, warning=FALSE, fig.height=5, fig.width=8}
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
```


## Conclusion {.smaller}

**Ce que le code permet** : 

- Afficher les personnes qui nous suivons sans qu'elles ne nous suivent en retour

- Afficher les personnes que nous avons unfollow et qui nous suivent encore

**Ce que l'on gagne** : 

- Suppression plus facile des profils qui ne nous suivent pas 

- Gain de temps dans le tri du réseau

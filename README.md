# Analyse de réseau Instagram et réciprocité 

- Auteurs : Damour Manon et Maudoux Malorie 
- Language : R 

## Introduction

Les réseaux sociaux occupent une place importante dans les sociétés et notamment dans la vie des jeunes. 
Ces applications comme Instagram permettent de construire un réseau virtuel d'une personne qui s'abonne soit à ses amis soit à des personnalités publiques ou des marques. 
Cependant les relations évoluent au fil du temps dans la réalité tout comme sur les réseaux. Une personne peut supprimer des abonnés de son réseau tout comme les autres peuvent la supprimer ce qui provoque une relation asymétrique (non réciproque) si la supression n'est pas faite des deux cotés. 
Notre projet a pour but de présenter une analyse d'un réseau Instagram et des relations asymétriques (désabonnements ou abonnements sans retour) basé sur les données personnelles exportées et téléchargées directement à partir d'Instagram.
L'objectif ici était d'automatiser un code afin de détecter des relations non réciproques (en différenciant les comptes des célébrités/marques, des personnels), pour éviter de continuer à suivre des personnes qui nous ont potentiellement supprimés ou ne nous ont jamais suivis en retour. 

Etapes : 
- Installation et chargement des packages
- Importation des données
- Séparation des profils
- Analyse de la réciprocité et de l'ancienneté
- Surveillance des désabonnements
- Visualisation avec des graphes 

## I. Installation et chargement des packages

Tout d'abord, nous installons (*install.packages("")*) et chargeons (*library()*) les packages nécessaires pour la suite du code.
Packages : 
- **tidyverse** : pour charger dplyr (pour la manipulation), ggplot2 (visuels) et stringr
- **rvest** : pour l'extraction de données depuis des fichiers HTML
- **lubridate** : pour la gestion simplifiée des formats de dates et d'heures
- **igraph** : pour l'analyse et la visualisation de graphes

## II. Importation des données

Juste avant la ligne de code de l'importation des followers et following, nous ajoutons une ligne pour créer un dictionnaire de traduction des données temporelles. 
Nous faisons cela car Instagram exporte les dates en français cependant les fonctions de R (lubridate) attendent souvent de l'anglais. On crée ce "dictionnaire" pour traduire les mois automatiquement.
Voici la ligne de code : 
```{r}
#dictionnaire des mois traduis pour lubridate
mois_fr_an <- c("jan"="Jan", "fév"="Feb", "mar"="Mar", "avr"="Apr", "mai"="May", "juin"="Jun", 
                "juil"="Jul", "août"="Aug", "sep"="Sep", "oct"="Oct", "nov"="Nov", "déc"="Dec")
```
Après cela nous commençons par importer les abonnements (following) et y apporter des modifications : 
```{r}
following <- read_html("Analyse de donnée/connections/followers_and_following/following.html") %>% {
  page <- .
  tibble(
    followed = page %>% html_elements("h2") %>% html_text(), 
    date_raw = page %>% html_elements("._a6-p div > div:last-child") %>% html_text(),
    follower = "me"
  )
} %>%
  mutate(timestamp = str_replace_all(date_raw, mois_fr_an) %>% mdy_hm()) 
```
- **read_html** : permet de charger le fichier
- **page <- .** : stocke le contenu dans une variable temporaire pour extraire plusieurs éléments à la fois
- **html_elements("h2")** : cible les balises HTML qui contiennent les noms d'utilisateurs. 'h2' signifie Heading 2, en HTML, dans le fichier following.html, Instagram a choisi de structurer la page en mettant chaque nom d'utilisateur dans une balise de titre. Utiliser 'h2' est le moyen le plus direct d'isoler le nom du compte sans récupérer les autres textes de la page.
- **html_text()** : nettoie les balises pour ne garder que le texte brut
- **tibble(...)** : structure les données dans un tableau propre (avec des lignes et des colonnes)

Le bloc **mutate**, permet de remplacer les mois en français par l'anglais grâce à la traduction placée plus haut et **mdy_hm()** permet de transformer le texte en un objet temporel (date et heure)

On importe ensuite les abonnés de la même manière : 
```{r}
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
La différence ici c'est que dans le fichier followers_1.html, les abonnés sont donnés sous forme de lien, en demandant *.pam a*, on dit à R de récupérer uniquement les noms des utilisateurs qui sont dans la liste centrale et d'ignorer le reste, on obtient ainsi les identifants des abonnés.
Le reste du code est le même que pour les abonnements

Après les importations nous regardons un extrait (les 6 premières lignes), pour voir ce qui a été importé et modifié : 
```{r}
# Affichage
head(following)
head(followers)
```
## III. Différenciation des profils 

Nous suivons tous sur les réseaux des personnes célèbres comme des chanteurs, des influenceurs... ou des marques comme nike, adidas...
Il est logique que ces personnes ne nous suivrons pas en retour, ils sont donc considérer comme des relations non réciproques logiques. Pour éviter de fausser la liste des relations asymétriques avec ces personnes, il faut différencier les comptes certifiés des comptes personnels. Pour cela nous utilisons la ligne de code : 
```{r}
marques_stars <- c("adidas", "netflix", "fcbarcelona", "psg", "pullandbear", "badgalriri", "arianagrande")

following <- following %>%
  mutate(type = if_else(followed %in% marques_stars | !str_detect(followed, "[._]"), 
                        "Marque/Célébrité", "Personnel"))
head(following)
```
On a d'abord crée une liste nommée **marques_stars** pour isoler les comptes connus qui ne s'abonneront pas en retour. Cette liste donne des comptes à R qu'il fait isoler mais lui donne aussi un exemple puisqu'on ne peut pas lister tous les comptes. On en a donc lister quelques uns et le reste du code tente de deviner les autres en se servant du vecteur. 

La fonction **mutate()** sert à ajouter une nouvelle colonne (nommée type) au tableau déjà existant. Pour décider ce qu'on écrit dans cette colonne, on utilise une condition **if_else()** : 
- **followed %in% marques_stars** : on vérifie si le nom d'utilisateur figure dans le vecteur *marques_stars*
- **!** : signifie "non"
- **str_detect** : renvoie "Vrai" si le symbole est trouvé
- **[._]**: cherche la présence d'un point ou d'un tiret du bas

La logique ici est la suivante : en principe les comptes personnels utilisent très souvent des points ou des tirets. À l'inverse, les comptes de marques ou de stars cherchent souvent la simplicité. Si un nom n'a ni point ni tiret, le code le classe par défaut en "Marque/Célébrité". Evidemment cela peut créer des faux positifs ou des faux négatifs. 

## IV. Analyse de la réciprocité et de l'ancienneté

C'est grâce à cette partie que l'on peut identifier qui nous suit en retour et avec qui nous avons un lien asymétrique. 
Nous avons séparer cette partie en 2 sous-parties : 
- les relations récirpoques :
```{r}
reciprocal <- following %>%
  inner_join(followers, by = c("followed" = "follower"))
head(reciprocal)
```
Nous avons utiliser la commande **inner_join** pour effectuer une jointure enre le tableau following et le tableau followers, et ainsi conserver uniquement les lignes présentent dans les deux tableaux. 
La commande **by = c("followed" = "follower")** permet de dire à R de vérifier si le nom d'utilisateur dans la colonne *followed* est le même que dans la colonne *follower*

- les relations non réciproques :
```{r}
# Relations non réciproques (Detection des désabonnements ou non-retours)
non_reciprocal <- following %>%
  anti_join(followers, by = c("followed" = "follower")) %>%
  filter(type == "Personnel") %>% # Focus sur les vrais désabonnements personnels
  # Analyse de l'ancienneté : calcul du nombre de jours depuis l'abonnement
  mutate(jours_anciennete = as.numeric(difftime(now(), timestamp, units = "days")))
head(non_reciprocal)
```
On procède presque de la même façon mais ici on utilise **anti_join**, donc on demande à R de regarder la liste following et de supprimer tous ceux qui se trouvent aussi dans la liste followers. On obtient uniquement les personnes qui ne nous suivent pas.
Dans le bloc **mutate** nous nous servons de **difftime(now(), timestamp, units = "days")** : cette fonction calcule l'écart de temps entre "Maintenant" et le moment où on a suivi la personne et **as.numeric(...)** qui transforme le résultat en un nombre.

## V. Les désabonnements 

Dans cette section, nous avons importé les données sur les personnes que nous avons nous même unfollow récemment pour ensuite voir si ces personnes nous suivent toujours. 
Pour l'importation nous procédons de la même manière que pour l'importation des abonnés et abonnements. 
```{r}
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
```
Nous regardons ensuite qui nous suit encore alors que nous les avons supprimés : 
```{r}
unfollowed_followers <- unfollowed_by_me %>%
  inner_join(followers, by = c("username" = "follower"))
head(unfollowed_followers)
```
Nous utilisons la même méthode que pour les relations réciproques : avec le **inner_join**

## VI. Visualisation 

Dans une dernière section nous avons construit des graphes afin d'avoir un support visuel de l'analyse du réseau qui permet d'avoir une visualisation de la constitution de notre réseau. 
Nous avons construit : 
- Une courbe de densité de l'ancienneté : Contrairement à l'histogramme qui segmente le temps de façon rigide, la densité offre une vision fluide du flux d'abonnements. Elle permet de localiser instantanément les périodes de forte volatilité du réseau social.
- Un donut sur les types d'abonnement : Il permet de visualiser la constitution de notre réseau en 3 abonnements différents (les réciproques, les non réciproques et les marques/célébrités)
- Une typologie du réseau : Qui montre d'une façon différente comment est constituer le réseau au travers de liens. En vert nous retrouvons les liens réciproques, ce sont donc les amis qui nous suivent et que l'on suit, et en rouge les liens asymétriques qui comprend les utilisateurs quie ne nous suivent plus et les célébrités et marques. Le point doré au centre représente notre compte.

## Conclusion 

Grâce à ce code, nous pouvons désormais prendre connaissance des personnes qui soit ne nous suivent plus et que nous suivons encore soit des personnes qui ne nous ont jamais suivis en retour afin de les supprimer également de notre réseau. Nous pouvons également voir si les abonnés que nous avons unfollow récemment nous suivent toujours afin de les supprimer de notre réseau. Ce projet facilite le tri des abonnés et abonnements et fait gagner du temps dans ce tri. 

Installation et Mise à jour :

Pour faire tourner l'application en local, il faut installer les packages R suivants : shiny, tidyverse, ggiraph, sf, DT, bslib, bsicons et haven.

Important : On rencontré des problèmes d'affichage avec des versions anciennes de certaines librairies. Si l'interface ou les graphiques paraissent bizarres, pensez à bien mettre à jour tous vos packages vers leur dernière version.

Fichiers requis :

Pour que l'application fonctionne, il est impératif que les fichiers suivants soient présents à la racine du dossier, à côté des scripts de l'application (global.R, ui.R et server.R) :

Achats_csv.csv : Données des transactions
Correspondances_sites.csv : Référentiel des sites web
clients.sas7bdat : Base de données clients (nécessite le package haven)
departements.geojson :Bbon affichage de la carte

Une fois ces fichiers réunis au même endroit, lancez simplement l'application via RStudio.
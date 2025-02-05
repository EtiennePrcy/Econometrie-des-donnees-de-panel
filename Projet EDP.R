library(readxl)
library(dplyr)
library(sandwich)
library(lmtest)
library(whitestrap)
library (plm)
library(corrplot)
library(stargazer)
library(car)
library(nlme)

base <- read_excel("C:/Users/etien/Documents/COURS FAC/M1/Econométrie/Econométrie des données de panel/Projet/BASE FUSIONNEE.xlsx")
base$PIB <- base$PIB/1000000
pbase <- pdata.frame(base, index = c("Pays", "Année"))
pbase <- pbase[ , -c(1:2,7)]


#statistiques descriptives


summary(pbase)

stargazer(pbase, type = "text", summary = TRUE)
stargazer(pbase, type = "latex", summary = TRUE)




OLS <- lm(PIB ~ CC + CP + ER + GN, 
          data = base)
stargazer(OLS, type = "text", title = "Moindres carrés ordinaires")



mod_EFWI <- plm(PIB ~ CC + CP + ER + GN, 
              data = base, 
              index = c("Pays", "Année"),  
              model = "within", 
              effect = "individual")

stargazer(
  mod_EFWI, 
  type = "text", 
  title = "Régression within individuel", 
  model.names = FALSE 
)

mod_EFWT <- plm(PIB ~ CC + CP + ER + GN, 
               data = base, 
               index = c("Pays", "Année"),  
               model = "within", 
               effect = "time")

stargazer(
  mod_EFWT, 
  type = "text", 
  title = "Régression within temporelle", 
  model.names = FALSE 
)

mod_EFWB <- plm(PIB ~ CC + CP + ER + GN, 
               data = base, 
               index = c("Pays", "Année"),  
               model = "within", 
               effect = "twoways")
stargazer(
  mod_EFWB, 
  type = "latex", 
  title = "Régression within totale", 
  model.names = FALSE 
)



















mod_EFBI <- plm(PIB ~ CC + CP + ER + GN, 
               data = base, 
               index = c("Pays", "Année"),  
               model = "between", 
               effect = "individual")
stargazer(
  mod_EFBI, 
  type = "text", 
  title = "Régression between individuelle", 
  model.names = FALSE 
)

mod_EFBT <- plm(PIB ~ CC + CP + ER + GN, 
                data = base, 
                index = c("Pays", "Année"),  
                model = "between", 
                effect = "time")
stargazer(
  mod_EFBT, 
  type = "text", 
  title = "Régression between temporelle", 
  model.names = FALSE 
)








# specification à un seul effet, effet individual
mod_Fgls_EAI <- plm(PIB ~ CC + CP + ER + GN,
                data = base,
                model="random", effect="indiv"
)
stargazer(
  mod_Fgls_EAI, 
  type = "text", 
  title = "Régression FGLS individuelle", 
  model.names = FALSE 
)


mod_Fgls_EAT <- plm(PIB ~ CC + CP + ER + GN,
                    data = base,
                    model="random", effect="time"
)
stargazer(
  mod_Fgls_EAT, 
  type = "text", 
  title = "Régression FGLS temporelle", 
  model.names = FALSE 
)


mod_Fgls_EAB <- plm(PIB ~ CC + CP + ER + GN,
                    data = base,
                    model="random", effect="twoways"
)
stargazer(
  mod_Fgls_EAB, 
  type = "text", 
  title = "Régression FGLS totale", 
  model.names = FALSE 
)






#### TABLEAU R2CAPITULATIF DES REGRESSIONS




# Estimateurs pt1

stargazer(
  OLS, mod_EFWI, mod_EFWT, mod_EFWB, mod_EFBI, mod_EFBT, 
  type = "latex", 
  title = "Résumé des régressions, MCO et effets fixes", 
  column.labels = c("MCO", "within indiv", "within temp", "within total", "between indiv", "between temp"),
  model.names = FALSE 
)


#estimateurs pt2

stargazer(
 mod_Fgls_EAB, mod_Fgls_EAI, mod_Fgls_EAT, 
  type = "latex", 
  title = "Résumé des régressions, effets aléatoires", 
  column.labels = c("Fgls total", "Fgls indiv", "Fgls temp"),
  model.names = FALSE 
)







############# tests


## F-tests

pFtest(PIB ~ CC + CP + ER + GN, 
       data = pbase, effect="indiv") 
pFtest(PIB ~ CC + CP + ER + GN, 
       data = pbase, effect="time")
pFtest(PIB ~ CC + CP + ER + GN, 
       data = pbase, effect="twoways")


## LM-tests

plmtest(PIB ~ CC + CP + ER + GN, 
        data = pbase, effect="time", type="bp") 
plmtest(PIB ~ CC + CP + ER + GN, 
        data = pbase, effect="individual", type="bp") 
plmtest(PIB ~ CC + CP + ER + GN, 
        data = pbase, effect="twoways", type="bp") 



## Test d'Hausman


Wi <- mod_EFWI <- plm(PIB ~ CC + CP + ER + GN, 
                      data = base, 
                      index = c("Pays", "Année"),  
                      model = "within", 
                      effect = "individual")

FGLSi <- mod_Fgls_EAI <- plm(PIB ~ CC + CP + ER + GN,
                    data = base,
                    model="random", effect="indiv")

phtest(Wi,FGLSi) 



WB <- mod_EFWB <- plm(PIB ~ CC + CP + ER + GN, 
                      data = base, 
                      index = c("Pays", "Année"),  
                      model = "within", 
                      effect = "twoways")
FGLSB <- mod_Fgls_EAB <- plm(PIB ~ CC + CP + ER + GN,
                             data = base,
                             model="random", effect="twoways")
phtest(WB, FGLSB)



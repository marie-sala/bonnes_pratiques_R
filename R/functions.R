# DÉFINITION DE FONCTIONS ----

# fonction de stat agregee
calcul_stat_desc <- function(data, stat = "moyenne", ...) {
  if (stat == "moyenne") {
    x <- mean(data, na.rm = TRUE, ...)
  } else if (stat == "ecart-type" || stat == "sd") {
    x <- sd(data, na.rm = TRUE, ...)
  } else if (stat == "variance") {
    x <- var(data, na.rm = TRUE, ...)
  }
  return(x)
}

decennie_a_partir_annee <- function(annee) {
  return(annee - annee %%
           10)
}
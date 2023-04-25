# DÉFINITION DE FONCTIONS ----


#' Calcule des statistiqus descriptives à partir d'un vecteur
#'
#' @param data un vecteur de nombres
#' @param stat une chaîne de caractères : "moyenne", "ecart-type" ou "variance"
#'
#' @return un nombre
#' @export
#'
#' @examples
#' calcul_stat_desc(rnorm(10))
#' calcul_stat_desc(rnorm(10), "ecart-type)
#' calcul_stat_desc(rnorm(10), "variance)

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
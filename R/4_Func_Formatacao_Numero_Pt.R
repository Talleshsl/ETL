#__________________________________________________________________________#
#___________            Formatação para Reais numero            ___________#
#__________________________________________________________________________#

#' @title         Formatação para Reais numero
#'
#' @description   Formata valor numerico para contabil, adicionando . nas casas de mil
#' @param values  Vetor com os valores numericos
#' @param nsmall  Numero de casas decimais
#'
#' @return         Retorna um tibble tipificado
#' @export
#'
editPT_numeric <- function(values, nsmall = 0) {
  values %>%
    as.numeric() %>% round(digits = nsmall) %>%
    format(nsmall = nsmall, decimal.mark = ",", big.mark = ".")
}

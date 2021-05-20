#__________________________________________________________________________#
#_____________________________       Moda       ___________________________#
#__________________________________________________________________________#

#' @title          Moda
#'
#' @description    Realiza o calculo de moda de um vetor
#' @param v        Vetor para calculo
#' @param na.rm    Booleno, tendo: TRUE -> remove valores nulo;
#'                                 FALSE -> mantem valores nulos
#' @param zero.rm  Booleno, tendo: TRUE -> remove valores zeros;
#'                                 FALSE -> mantem valores zeros
#'
#' @return         Retorna a moda
#' @export
#'
getmode <- function(v, na.rm=TRUE, zero.rm=TRUE) {
  if(na.rm==TRUE){ v <- v[!is.na(v)&v>0]}
  if(zero.rm==TRUE){ v <- v[v!=0]}
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
    a

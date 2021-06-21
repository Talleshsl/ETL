#__________________________________________________________________________#
#_____________________       Mapeamento de Campos       ___________________#
#__________________________________________________________________________#

#'@title           Mapeamento de variaveis
#'
#' @description    Realiza o mapeamento e tipificacao de campos utilizados
#'                 na composicao do relatorio
#' @param data     Base de dados como dataframe
#' @param mapping  Lista os campos utilizados no mapeamento, com suas
#'                 respectivas tipificacoes.
#'                 Sendo: Field: Campos do tibble a ser retornado;
#'                        Type:  Tipo de campo ('Texto','Data",'Inteiro','Duplo);
#'                        Variable: Nome do cabeçalho do dataframe;
#'                        Decimal: Numero de casas decimais.
#'                 Ex: tribble(~Campo,       ~Tipo,      ~Variavel,      ~Decimal,
#'                             "Campo1",     "Texto",    "CAMPO1_TBL",   NA,
#'                             "Campo2",     "Data",     "CAMPO2_TBL",   NA,
#'                             "Campo3",     "Inteiro",  "CAMPO3_TBL",   NA,
#'                             "Campo4",     "Duplo",    "CAMPO4_TBL",    2)
#' @param typeDate Indica o formato que o campo com tificicacao DATA esta sendo
#'                 importada.
#'                 Sendo: 'numerical : numero;
#'                        'dmy'      : dia-mes-ano;
#'                        'dym'      : dia-ano-mes;
#'                        'mdy'      : mes-dia-ano;
#'                        'myd'      : mes-ano-dia;
#'                        'ydm'      : ano-dia-mes;
#'                        'ymd'      : ano-mes-dia;
#'
#' @return         Retorna um tibble tipificado
#' @export
#'
map_vars <- function(data,mapping=list(Field=NULL,Type=NULL,Variable=NULL,Decimal=NULL),typeDate='numeric'){
  # Seleciona as variaveis
  data <- data[,colnames(data)%in%mapping$Variable]

  # Tipificacao das variaveis
  if(is.null(colnames(data))){
    data <- switch (mapping$Type[1],
                    "Texto"   = as.character(data),
                    "Duplo"   = round(as.numeric(data),as.numeric(mapping$Decimal[1])),
                    "Inteiro" = as.integer(data),
                    "Data"    = as.Date(data,origin ="1899-12-30")
    )
    # Inverte para tibble
    data2 <-tibble::as_tibble(data)
    names(data2) <- mapping$Field

  }else{
    for(x in 1:ncol(data)){
      id<-which(colnames(data)==mapping$Variable[x])
      data[,id] <- switch (mapping$Type[x],
                           "Texto"   = as.character(data[,id]),
                           "Duplo"   = round(
                             as.numeric(
                               stringr::str_replace_all(
                                 stringr::str_remove_all(data[,id],"[:alpha:]"),
                                 ",",".")),
                             as.numeric(mapping$Decimal[x])),
                           "Inteiro" = as.integer(
                             stringr::str_remove_all(data[,id],
                                                     "[:alpha:]")),
                           "Data"    = if(typeDate!='numeric'){
                             switch (typeDate,
                                     'dmy' = lubridate::dmy(stringr::str_sub(data[,id],end=10)),
                                     'dym' = lubridate::dym(stringr::str_sub(data[,id],end=10)),
                                     'mdy' = lubridate::mdy(stringr::str_sub(data[,id],end=10)),
                                     'myd' = lubridate::myd(stringr::str_sub(data[,id],end=10)),
                                     'ydm' = lubridate::ydm(stringr::str_sub(data[,id],end=10)),
                                     'ymd' = lubridate::ymd(stringr::str_sub(data[,id],end=10))
                             )} else{
                               as.Date(as.numeric(data[,id]),origin ="1899-12-30")
                             }
      )
    }
    # Ordena Colunas
    data2 <- dplyr::select(data,mapping$Variable)
    # Nomeia as variaveis
    colnames(data2) <- mapping$Field
    # Inverte para tibble
    data2 <-tibble::as_tibble(data2)
  }

  return(data2)
}

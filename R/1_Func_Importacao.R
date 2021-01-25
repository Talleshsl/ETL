#__________________________________________________________________________#
#____________________________     Importacao     __________________________#
#__________________________________________________________________________#

#'@title          Importacao de tabelas
#'
#' @description   Realiza importacao de arquivo ou leitura de banco
#' @param file    Lista com parametros para inportacao dos arquivos.
#'                Sendo: Name:   Nome do Arquivo;
#'                       Dir:    Diretorio do Arquivo;
#'                       Sheet:  Nome da Planilha caso seja xlsx
#'                       Title:  Nome atribuido ao objeto importado
#'                       Seq:    Sequencia de importacao, caso haja
#'                               conjunto de arquivos a serem importados.
#'                               Obs. O separador de sequencias é ';".
#'                       Id_Seq: Id atribuido ao conjuto de dados das sequencias
#'                Ex:tribble(~Name,      ~Dir,    ~Extension,   ~Sheet,   ~Title,   ~Seq,             ~Id_Seq
#'                           Arquivo1,   Dir1,    xlsx,         Plan1,    Titulo1,  NA,               NA,
#'                           Arquivo1,   Dir2,    csv,          NA,       Titulo2,  NA,               NA,
#'                           ? - comum,  Dir3,    xlsx,         Plan1,    Titulo3,  2020.05;2020.06,  5;6)
#'
#' @param BD     Lista com os parametros para realizacao de consulta em
#'               banco.
#'               Sendo: Conection: Objeto com a conexao com o  banco;
#'                      Query:     Consulta;
#'                      Title:     Nome atribuido ao objeto.
#'
#' @return           Retorna um dataframe
#' @export
#'
read <- function(file=list(Name=NULL,Dir=NULL,Extension=NULL,Sheet=NULL,
                           Title=NULL, Seq=NULL,Id_Seq=NULL),
                 BD=list(Conection=NULL,Query=NULL, Title=NULL)){
  saida <- Files <- Querys <- NULL
  # Arquivo
  if(!is.null(file$Name)){
    Files <- lapply(1:length(file$Title), function(i){
      if(stringr::str_detect(file$Name[i],'\\?')){
        tmp <- tryCatch(expr={
          ls <- unlist(stringr::str_split(file$Seq[i],';'))
          ids<- unlist(stringr::str_split(file$Id_Seq[i],';'))
          tmp <- lapply(ls, function(j){
            tmp <- tryCatch(expr ={
              name <- stringr::str_replace_all(file$Name[i],'\\?',j)
              if(file$Extension[i]=='xlsx'){
                tmp <- openxlsx::read.xlsx(paste0(file$Dir[i],'\\',name,'.',file$Extension[i],sep=''),
                                 sheet = file$Sheet[i], colNames = T,detectDates = F)
              }
              if(file$Extension[i]=='csv'){
                tmp <- utils::read.csv2(paste0(file$Dir[i],'\\',name,'.',file$Extension[i],sep=''),
                                 dec=',', h= T)
              }
              if(file$Extension[i]=='txt'){
                tmp <- utils::read.table(paste0(file$Dir[i],'\\',name,'.',file$Extension[i],sep=''),
                                  dec=',', h= T)
              }
              tmp2 <- cbind(rep(ids[ls==j],nrow(tmp)),tmp)
              colnames(tmp2) <- c('Seq',colnames(tmp))
              return(tmp2)
            },error=function(e){return()})
          })
          if(length(tmp)==1){
            tmp <- purrr::reduce(tmp,rbind)
          }else{
            tmp <- purrr::reduce(lapply(tmp, "[",
                                        c(1:min(unlist(purrr::map(tmp,ncol))))),
                                 rbind)
          }
          tmp
        },error=function(e){return()})
      }else{
        tmp <- tryCatch(expr = {
          if(file$Extension[i]=='xlsx'){
            tmp <- openxlsx::read.xlsx(paste0(file$Dir[i],'\\',file$Name[i],'.',file$Extension[i],sep=''),
                             sheet = file$Sheet[i], colNames = T,detectDates = F)
          }
          if(file$Extension[i]=='csv'){
            tmp <- utils::read.csv2(paste0(file$Dir[i],'\\',file$Name[i],'.',file$Extension[i],sep=''),
                             dec=',', h= T)
          }
          if(file$Extension[i]=='txt'){
            tmp <- utils::read.table(paste0(file$Dir[i],'\\',file$Name[i],'.',file$Extension[i],sep=''),
                              dec=',', h= T)
          }
          tmp
        },error=function(e){return()})
      }
      tmp
    })
    names(Files) <- c(file$Title)
  }
  # Banco de Dados
  if(!is.null(BD$Query)){
    Querys <- lapply(1:length(BD$Query), function(i){
      tmp <- tryCatch(expr = RODBC::sqlQuery(BD$Conection,BD$Query[i]),
                      error=function(e){return()})
      return(tmp)
    })
    names(Querys) <- c(BD$Title)
  }
  saida <- c(Files,Querys)
  return(saida)
}


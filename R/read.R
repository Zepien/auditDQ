
#' @export
read_data <- function(query, var_num = "", var_cat = "", var_dat = "", var_other =""){

  options(dec=",",Encoding="UTF-8")

  if(startsWith(gsub(" ", "", tolower(query)),'select')){
    if (!require('RODBC')) install.packages('RODBC',repos = "http://cran.us.r-project.org"); library('RODBC')
    conn <- odbcConnect("teradata")
    df_query <- sqlQuery(conn, query)
  }else if(endsWith(gsub(" ", "", tolower(query)),'.csv')){
    df_query <- read.csv(query,sep=",",header = TRUE)
  }else if(endsWith(gsub(" ", "", tolower(query)),'.sas7bdat')){
    if (!require('haven')) install.packages('haven',repos = "http://cran.us.r-project.org"); library('haven')
    df_query = as.data.frame(read_sas(query))
  }else{
    print("NO SE CONOCE EXTENSIÓN")
  }

    numericas = strsplit(gsub(" ", "", var_num),",")
    categoricas = strsplit(gsub(" ", "", var_cat),",")
    fechas = strsplit(gsub(" ", "", var_dat),",")
    otras = strsplit(gsub(" ", "", var_other),",")

    # si non pasamos nada e que on hai ese tipo de variables
    # lista coma separada de variabels de cada tipo

    for(i in 1:length(df_query)){

      if(names(df_query)[i] %in% categoricas[[1]]){
        df_query[,i] <- iconv(df_query[,i], from = "cp1251", to = "UTF-8")
        if(is.factor(df_query[,i][[1]][1])==FALSE){
          df_query[,i] <- as.factor(df_query[,i])
        }
      }else if(names(df_query)[i] %in% numericas[[1]]){
        if(is.numeric(df_query[,i][[1]][1])==FALSE){
          df_query[,i] <- as.double( df_query[,i])
        }
      }else if(names(df_query)[i] %in% fechas[[1]]){
        if(is.Date(df_query[,i][[1]][1])==FALSE){

          df_query[,i] <- as.Date(df_query[,i])
        }
      }else if(names(df_query)[i] %in% otras[[1]]){

        if(is.character(df_query[,i][[1]][1])==FALSE){
          df_query[,i] <- as.character(df_query[,i])
        }
      }

    }



  return(df_query)

}





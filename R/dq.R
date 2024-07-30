#' @export
genera_dq <- function(data_frame,disaggregate_var=""){

  n_col <- 4
  result_dq <- data.frame(matrix(ncol = n_col, nrow=0))
  names(result_dq) <- c('Control','Atributo','Descripcion','General')
  result_dq[1,] = c('a','a','a',1)
  result_dq = result_dq[-1,]

  if(nchar(disaggregate_var)>0){
    error_NA(data_frame,disaggregate_var)
    error_longitud(data_frame,disaggregate_var)
    n_col <- n_col+ length(data_frame[, c(disaggregate_var)] %>% unique())
    var_names <- gsub(" ","",data_frame[, c(disaggregate_var)] %>% unique() %>%
      sort(na.last = FALSE) %>%
      as.character()
      )
    result_dq <- data.frame(matrix(ncol = n_col, nrow=0))
    names(result_dq) <- c(c('Control','Atributo','Descripcion','General'),var_names)
    vector_suport <- c(1:n_col)
    colnames(result_dq)[which(is.na(colnames(result_dq)))] <- "NA"
    result_dq[1,] = c(c('a','a','a',1),c(1:n_col))
    result_dq = result_dq[-1,]
  }

  for(i in 1:ncol(data_frame)){

    analisis_missings <- conteo_missings(data_frame,disaggregate_var,i)
    result_dq <- bind_rows(result_dq, analisis_missings)
    analisis_ceros <- conteo_ceros(data_frame,disaggregate_var,i)
    result_dq <- bind_rows(result_dq, analisis_ceros)
    analisis_negativos <- conteo_negativos(data_frame,disaggregate_var,i)
    result_dq <- bind_rows(result_dq, analisis_negativos)

  }

  return(result_dq)

}


conteo_missings <- function(data_frame,disaggregate_var,i){

  analised_columns <- as.data.frame(data_frame[, c(i)])
  column_name <- colnames(data_frame[i])
  names(analised_columns) <-'var'


  missings_count <- analised_columns %>%
    mutate(total = n()) %>%
    mutate(is_missings = is.na(var)) %>%
    group_by(is_missings,total) %>%
    count() %>%
    rename(n_missings = n)

  missings_analysis_aggregate <- missings_count %>%
  filter(is_missings == TRUE) %>%
  full_join(missings_count%>%
              filter(is_missings == FALSE) %>%
              rename(n_no_missings = n_missings), by = c("total")) %>%
    mutate(n_missings = ifelse(is.na(is_missings.x),0,n_missings),
           n_no_missings = ifelse(is.na(is_missings.y),0,n_no_missings)) %>%
    mutate(missings = as.character(round(n_missings/total*100,2)),
           no_missings = as.character(round(n_no_missings/total*100,2))) %>%
    select(total,missings,no_missings)

  missings_analysis_result <- missings_analysis_aggregate %>%
    ungroup() %>%
    select(General = missings)

  if(nchar(disaggregate_var)!=0){

  n_disaggregate_col <- which(colnames(data_frame)==disaggregate_var)
  analised_columns <- data_frame[, c(i,n_disaggregate_col)]
  colnames(analised_columns) <- c("var", "dissagregate_var")

  missings_count_disagre <- analised_columns %>%
    group_by(dissagregate_var) %>%
    mutate(is_missings = is.na(var)) %>%
    group_by(dissagregate_var,is_missings) %>%
    count() %>%
    rename(n_missings = n) %>%
    group_by(dissagregate_var) %>%
    mutate(total = sum(n_missings))

  missings_analysis_disaggregate <- missings_count_disagre %>%
    filter(is_missings == TRUE) %>%
    full_join( missings_count_disagre %>%
                 filter(is_missings == FALSE) %>%
                 rename(n_no_missings = n_missings), by = c("dissagregate_var","total")) %>%
    mutate(n_missings = ifelse(is.na(is_missings.x),0,n_missings),
           n_no_missings = ifelse(is.na(is_missings.y),0,n_no_missings)) %>%
    mutate(missings = as.character(round(n_missings/total*100,2)),
           no_missings = as.character(round(n_no_missings/total*100,2))) %>%
    select(dissagregate_var,missings)

  missings_analysis_result <- missings_analysis_result %>%
    cbind(trapose_table(missings_analysis_disaggregate))
  }

  Control <- paste(i,'.1','-',column_name,sep="")
  Atributo <- column_name
  Descripcion <- 'Missing'

  missings_analysis_result <- cbind(Descripcion, missings_analysis_result)
  missings_analysis_result <- cbind(Atributo, missings_analysis_result)
  missings_analysis_result <- cbind(Control, missings_analysis_result)
  colnames(missings_analysis_result) = gsub(" ","",colnames(missings_analysis_result))
  colnames(missings_analysis_result)[which(is.na(colnames(missings_analysis_result)))] <- "NA"


  return(missings_analysis_result)
}
conteo_ceros <- function(data_frame,disaggregate_var,i){

  analised_columns <- as.data.frame(data_frame[, c(i)])
  column_name <- colnames(data_frame[i])
  names(analised_columns) <-'var'

  ceros_count <- analised_columns %>%
    mutate(total = n()) %>%
    mutate(is_cero = ifelse(var==0,T,F)) %>%
    group_by(is_cero,total) %>%
    count() %>%
    rename(n_ceros = n)

  ceros_analysis_aggregate <- ceros_count %>%
    filter(is_cero == TRUE) %>%
    full_join(ceros_count%>%
                mutate(is_cero = ifelse(is.na(is_cero),FALSE,is_cero)) %>%
                filter(is_cero == FALSE) %>%
                group_by(is_cero,total) %>%
                summarise(n_no_ceros = sum(n_ceros)), by = c("total")) %>%
    mutate(n_ceros = ifelse(is.na(is_cero.x),0,n_ceros),
           n_no_ceros = ifelse(is.na(is_cero.y),0,n_no_ceros)) %>%
    mutate(ceros = as.character(round(n_ceros/total*100,2)),
           no_ceros = as.character(round(n_no_ceros/total*100,2))) %>%
    select(total,ceros,no_ceros)

  ceros_analysis_result <- ceros_analysis_aggregate %>%
    ungroup() %>%
    select(General = ceros)

  if(nchar(disaggregate_var)!=0){

    n_disaggregate_col <- which(colnames(data_frame)==disaggregate_var)
    analised_columns <- data_frame[, c(i,n_disaggregate_col)]
    colnames(analised_columns) <- c("var", "dissagregate_var")

    ceros_count_disagre <- analised_columns %>%
      group_by(dissagregate_var) %>%
      mutate(is_cero = ifelse(var==0,T,F)) %>%
      group_by(dissagregate_var,is_cero) %>%
      count() %>%
      rename(n_ceros = n) %>%
      group_by(dissagregate_var) %>%
      mutate(total = sum(n_ceros))

    ceros_analysis_disaggregate <- ceros_count_disagre %>%
      filter(is_cero == TRUE) %>%
      full_join( ceros_count_disagre %>%
                   mutate(is_cero = ifelse(is.na(is_cero),FALSE,is_cero)) %>%
                   filter(is_cero == 'FALSE') %>%
                   group_by(is_cero,total,dissagregate_var) %>%
                   summarise( n_no_ceros = sum(n_ceros)), by = c("dissagregate_var","total")) %>%
      mutate(n_ceros = ifelse(is.na(is_cero.x),0,n_ceros),
             n_no_ceros = ifelse(is.na(is_cero.y),0,n_no_ceros)) %>%
      mutate(ceros = as.character(round(n_ceros/total*100,2)),
             no_ceros = as.character(round(n_no_ceros/total*100,2))) %>%
      select(dissagregate_var,ceros)

    ceros_analysis_result <- ceros_analysis_result %>%
      cbind(trapose_table(ceros_analysis_disaggregate))
  }

  Control <- paste(i,'.2','-',column_name,sep="")
  Atributo <- column_name
  Descripcion <- 'Ceros'

  ceros_analysis_result <- cbind(Descripcion, ceros_analysis_result)
  ceros_analysis_result <- cbind(Atributo, ceros_analysis_result)
  ceros_analysis_result <- cbind(Control, ceros_analysis_result)
  colnames(ceros_analysis_result) = gsub(" ","",colnames(ceros_analysis_result))
  colnames(ceros_analysis_result)[which(is.na(colnames(ceros_analysis_result)))] <- "NA"

  return(ceros_analysis_result)
}
conteo_negativos <- function(data_frame,disaggregate_var,i){

  analised_columns <- as.data.frame(data_frame[, c(i)])
  column_name <- colnames(data_frame[i])
  names(analised_columns) <-'var'
  tipo_variable <- analised_columns[1]

  negativos_count <- analised_columns %>%
    mutate(total = n()) %>%
    mutate(is_negativo = ifelse(var<0,T,F)) %>%
    group_by(is_negativo,total) %>%
    count() %>%
    rename(n_negativos = n)

  negativos_analysis_aggregate <- negativos_count %>%
    filter(is_negativo == TRUE) %>%
    full_join(negativos_count%>%
                mutate(is_negativo = ifelse(is.na(is_negativo),FALSE,is_negativo)) %>%
                filter(is_negativo == FALSE) %>%
                group_by(is_negativo,total) %>%
                summarise( n_no_negativos = sum(n_negativos)), by = c("total")) %>%
    mutate(n_negativos = ifelse(is.na(is_negativo.x),0,n_negativos),
           n_no_negativos = ifelse(is.na(is_negativo.y),0,n_no_negativos)) %>%
    mutate(negativos = as.character(round(n_negativos/total*100,2)),
           no_negativos = as.character(round(n_no_negativos/total*100,2))) %>%
    select(total,negativos,no_negativos)

  negativos_analysis_result <- negativos_analysis_aggregate %>%
    ungroup() %>%
    select(General = negativos)

  Descripcion <- 'Negativos'
  if(is.character(tipo_variable[[1]][1])){
    Descripcion <- 'Vacios'
  }

  if(nchar(disaggregate_var)!=0){

    n_disaggregate_col <- which(colnames(data_frame)==disaggregate_var)
    analised_columns <- data_frame[, c(i,n_disaggregate_col)]
    colnames(analised_columns) <- c("var", "dissagregate_var")

    negativos_count_disagre <- analised_columns %>%
      group_by(dissagregate_var) %>%
      mutate(is_negativo = ifelse(var<0,T,F)) %>%
      group_by(dissagregate_var,is_negativo) %>%
      count() %>%
      rename(n_negativos = n) %>%
      group_by(dissagregate_var) %>%
      mutate(total = sum(n_negativos))

    negativoss_analysis_disaggregate <- negativos_count_disagre %>%
      filter(is_negativo == TRUE) %>%
      full_join( negativos_count_disagre %>%
                   mutate(is_negativo = ifelse(is.na(is_negativo),FALSE,is_negativo)) %>%
                   filter(is_negativo == 'FALSE') %>%
                   group_by(is_negativo,total,dissagregate_var) %>%
                   summarise( n_no_negativos = sum(n_negativos)), by = c("dissagregate_var","total")) %>%
      mutate(n_negativos = ifelse(is.na(is_negativo.x),0,n_negativos),
             n_no_negativos = ifelse(is.na(is_negativo.y),0,n_no_negativos)) %>%
      mutate(negativos = as.character(round(n_negativos/total*100,2)),
             no_negativos = as.character(round(n_no_negativos/total*100,2))) %>%
      select(dissagregate_var,negativos)

    negativos_analysis_result <- negativos_analysis_result %>%
      cbind(trapose_table(negativoss_analysis_disaggregate))
  }

  Control <- paste(i,'.3','-',column_name,sep="")
  Atributo <- column_name
  # almacenar tipo de variable, si es un charracter esta prueba es para vacíos no negativos

  negativos_analysis_result <- cbind(Descripcion, negativos_analysis_result)
  negativos_analysis_result <- cbind(Atributo, negativos_analysis_result)
  negativos_analysis_result <- cbind(Control, negativos_analysis_result)
  colnames(negativos_analysis_result) = gsub(" ","",colnames(negativos_analysis_result))
  colnames(negativos_analysis_result)[which(is.na(colnames(negativos_analysis_result)))] <- "NA"


  return(negativos_analysis_result)
}
trapose_table <- function(df){

  df <-df[order(df$dissagregate_var,na.last = FALSE), ]
  df_t <- data.frame(t(df))
  colnames(df_t) <- df_t[1, ]
  df_t <- df_t[-1, ]
  rownames(df_t) <- NULL

  return(df_t)

}
error_NA <- function(df,var){
  if(any(is.na(df[,var])==FALSE)==FALSE){

    stop(paste("La variable",var,"es siempre NA y no se puede usar para desagregar el análisis. Selecciona otra variable con algún valor distinto a NA"))
  }

}
error_longitud <- function(df,var){

  if( length(df[,var] %>% unique())>100){

    stop(paste("La variable",var," presenta más de 100 valores diferentes. Utilizar una variable que tenga menos valores diferentes para realizar el análisis desagregado"))
  }

}


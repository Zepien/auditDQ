\name{read_data}
\alias{read_data}
\title{read data}
\usage{
read_data(query,var_num,var_cat,var_dat,var_other)
}
\arguments{
\item{query}{Argumento de la función con la query de sql en formato string, ruta y nombre del .csv o ruta y nombre de .sas7bdat para leer (Obligatorio)}
\item{var_num}{Argumento de la función donde se indica un string separado con comas con los campos de la tabla que se queieren tratar como numéricos (No Obligatorio)}
\item{var_cat}{Argumento de la función donde se indica un string separado con comas con los campos de la tabla que se queieren tratar como categóricos. (No Obligatorio)}
\item{var_dat}{Argumento de la función donde se indica un string separado con comas con los campos de la tabla que se queieren tratar como fechas. (No Obligatorio)}
\item{var_other}{Argumento de la función donde se indica un string separado con comas con los campos de la tabla que se queieren tratar otros. Ejemplo: NUC, SUBNUC y otros campos que son identificadores de operaciones. (No Obligatorio)}
}
\description{
Esta función sirve para leer tablas de teradata, .csvs o .sas7bdat. Tambien permite transformar los campos de la tabla especificando un string con los nombres de los campos que se queiren transformar en cada argumento de la función.
}

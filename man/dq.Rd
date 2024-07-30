\name{genera_dq}
\alias{genera_dq}
\title{DQ AI}
\usage{
genera_dq(data_frame, 'disaggregate_var')
}
\arguments{
\item{data_frame}{Dataframe de entrada para generar la tabla con los controles de DQ. Los controles serán aplicados a todos los campos del dataframe}

\item{disaggregate_var}{Nombre de la variable en formato string del data frame de entrada por el cual se quiere desagregar el análisis. Este variable no puede ser NA para todos los registros y no debe de tener >100 valores únicos}
}
\description{
Esta función crea una tabla con controles de missings, negativos y ceros de todos los campos del data frame que se le pase como input. También permite desagregar el análisis por un campo del data frame indicando el nombre de la variable en formato string.

La salida será un dataframe con las columnas Control - Atributo - Descripcion - General - Var1 - Var2 - ... - VarN:

Control: ID de cada prueba.

Atributo: Nombre de la variable de cada prueba.

Descripcion: Tipo de análisis: missgins, negativos o ceros.

General: Valor en porcentage con respecto al total de la cantidad de casos positivos en cada prueba.

Var1: Valor en porcentage con respecto al la variable con valor Var1 de la cantidad de casos positivos en cada prueba.

Var2: Valor en porcentage con respecto al la variable con valor Var2 de la cantidad de casos positivos en cada prueba.

VarN:
}
\examples{

}

#########################################
# Descarga y descompresión de los datos #
#########################################
fileURL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
download.file(fileURL, destfile="./DataSets/original/datos.zip", method="curl")
unzip("./DataSets/original/datos.zip", exdir="./DataSets/original")
remove(fileURL)

#####################
# Limpiar los datos #
#####################
# Cargamos los datos del dataset separados por ";" con la primera línea como cabecera
studentMat <- read.table("./DataSets/original/student-mat.csv", row.names = NULL, sep = ";", header=TRUE) 
studentPor <- read.table("./DataSets/original/student-mat.csv", row.names = NULL, sep = ";", header=TRUE) 
length(studentPor)

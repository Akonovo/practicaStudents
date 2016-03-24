students_raw <- list()
students_raw[["mat"]] <- read.table("./DataSets/original/student-mat.csv", row.names = NULL, sep = ";", header=TRUE)
students_raw[["por"]] <- read.table("./DataSets/original/student-por.csv", row.names = NULL, sep = ";", header=TRUE)
sapply(students_raw, dim)
# mat por
# [1,] 395 649
# [2,]  33  33

> View(studentMat)
> View(studentMat)
> View(studentPor)
> studentPor <- read.table("./DataSets/original/student-por.csv", row.names = NULL, sep = ";", header=TRUE)
> dim(studentMat)
[1] 395  33
> dim(studentPor)
[1] 649  33
> colnames(studentPor) == colnames(studentMat)
[1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
> all)colnames(studentPor) == colnames(studentMat))
Error: unexpected ')' in "all)"
> all(colnames(studentPor) == colnames(studentMat))
[1] TRUE


commonColumns <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
                   # guardian, traveltime) # Added
# duplicados <- merge(studentMat, studentPor, by=commonColumns)
# dim(duplicados)


sapply(students_raw, function(x) 
  {
    c(nrow(x), nrow(unique(x[,commonColumns])))
})
# mat por
# [1,] 395 649
# [2,] 391 637


allStudents <- rbind(students_raw$mat[,commonColumns], students_raw$por[, commonColumns])
dim(allStudents)
allStudents <- unique(allStudents)
dim(allStudents)
# 662
allStudents <- cbind(id=1001:(1000+nrow(allStudents)), allStudents)
head(allStudents)

# # Explorando...
# studentPor <- merge(allStudents, students_raw$por, by=commonColumns)
# studentPor <- studentPor[,which(!colnames(studentPor) %in% commonColumns)]
# duplicated <- table(studentPor$id)
# duplicated <- names(duplicated)[which(duplicated>1)]
# studentPor[studentPor$id %in% duplicated,"failures"]
# studentPor[studentPor$id %in% duplicated,]


# Añadir ID a las dos tablas
studentScores <- lapply(students_raw, function(x) 
  {
    tmp <- merge(allStudents, x, by=commonColumns)
    tmp[,which(!colnames(tmp) %in% commonColumns)]
  }) 
sapply(studentScores, dim)
# mat por
# [1,] 395 649
# [2,]  21  21

head(allStudents)
head(studentScores$mat)
# studentScores_merged <-  merge(studentScores$mat, studentScores$por, by="id")

# Principal compònent analisis
# tmp <- studentScores$mat[,2:18]
# class(tmp)
# tmp <- sapply(tmp, as.numeric)
# pca <- prcomp(tmp)
# head(pca$rotation)
# dim(pca$x)
# sort(pca$rotation[,"PC1"])
# # absences          Walc          Dalc      guardian      romantic         goout      failures        famsup     schoolsup          paid    activities    traveltime        higher        famrel        health
# # -0.9994380301 -0.0227032046 -0.0128837521 -0.0110147884 -0.0090742858 -0.0065273260 -0.0060578794 -0.0014553783 -0.0009273629 -0.0004822937  0.0008800147  0.0010586349  0.0015587558  0.0051091066  0.0052871372
# # studytime      freetime
# # 0.0067572626  0.0072091862
# summary(tmp[,"absences"])
# summary(tmp[,"freetime"])


library(randomForest)
incCols <- unique(c(colnames(allStudents), colnames(head(studentScores$mat)[,2:18])))[-1]
thisMat <- merge(allStudents, studentScores$mat, by="id")
dim(thisMat)
# Quitar los repetidos... 
thisMat <- lapply(split(thisMat, thisMat$id), function(x)
{
  x[which.max(x$G3),]
})
thisMat <- do.call(rbind, thisMat)
dim(thisMat)
rownames(thisMat) <- thisMat$id
head(thisMat)

rf1 <- randomForest(x=thisMat[,incCols], y=thisMat$G3, importance=TRUE, ntree=1000, proximity=TRUE)
rf1
names(rf1)
imp <- importance(rf1)
imp <- imp[order(imp[,1], decreasing=T),]
imp


## Do MDS on 1 - proximity:
rf.mds <- cmdscale(1 - rf1$proximity, eig=TRUE)
pairs(cbind(NotaFinal=thisMat$G3, rf.mds$points), cex=0.6, gap=0,
      # col=c("red", "green", "blue")[as.numeric(iris$Species)], <- aaplicar color en funcion de nota
      main="Predictors and MDS of Proximity Based on RandomForest")
print(rf.mds$GOF)

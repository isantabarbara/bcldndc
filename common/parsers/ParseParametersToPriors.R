
file <- "c:/ldndc/common/input/siteparameters/siteparameters.txt"
lresources <- "c:/ldndc/build/Lresources"
prior.file <- "c:/projects/calibrations/priorALL.txt"



params <- read.table(file=file, comment.char='#', fill=TRUE, header=F, stringsAsFactors=FALSE)[,c(-2,-5)]

params[,1] <- paste0("site.parameter.",params[,1],".value")
colnames(params) <- c("name","min","max")
params$init <- as.numeric(params$min)
params$min <- as.numeric(params$min)
params$max <- as.numeric(params$max)

lresources.lines <- read.table(file=lresources, comment.char='#', fill=TRUE, header=F, stringsAsFactors=FALSE)

for (name in params$name) {
  print(name)
  value <- as.numeric(lresources.lines[lresources.lines$V1 == name,]$V3)
  print(class(params[params$name == name,]$init))
  print(value)
  if (length(value) != 0) {
    params[params$name == name,]$init <- value
  }
}

params <- params[!is.na(params$min),]

check.init <- function(name,min,max,init) {
 
  if (as.numeric(init) > as.numeric(max)) {
    
    print(paste("Error max",name, min, max, init))
    params[params$name == name,]$init <<- as.numeric(max)
  } else if (as.numeric(init) < as.numeric(min)) {
    
    print(paste("Error",name, min, max, init))
    params[params$name == name,]$init <<- as.numeric(min)
  }
}

apply(params[,c(1,2,3,4)],1,function(x) check.init(x[1],x[2],x[3],x[4]))

write.table(params, file=prior.file, quote = F, sep="\t",  row.names = F)
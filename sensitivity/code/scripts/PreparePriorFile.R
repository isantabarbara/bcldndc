##
## Convert ldndc parameter file en my priors file

## /ldndc/common/input/siteparameters/siteparameters.txt
library(stringr)

file <- "~/ldndc/common/input/siteparameters/siteparameters.txt"
out.file <- "~/ldndc/common/input/siteparameters/priors.txt"

lines <- readLines(file)
newLines <- c("name\tmin\tinit\tmax")

#TODO: add the priors

for (line in lines) {
  line <- str_trim(line)
  
  if (length(grep("#",line)) == 0) {
    splits <- strsplit(line," ")
    if (!is.na(splits[[1]][1])) {
      newLines <- c(newLines, paste0("site.parameter.",splits[[1]][1],".value\t",splits[[1]][3],"\t",splits[[1]][3],"\t",splits[[1]][4]))
    }
  }
}

write(newLines, out.file)

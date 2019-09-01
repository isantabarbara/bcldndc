# 
# 
# path <- pathProjectFile
# projName <- nombre fichero del xml
# year <- obtener year del schedule
# Crear carpeta ~/projects/calibrations/superSen/arable/projName
# Copiar en projName la carpeta ldndc en fixed/arable
# Modificar en projName/ldndc/config.yml
# 
# Cambios en config.yml
# path$common: ~/projects/calibrations/superSen/arable/projName/ldndc
# sitesDefault$confInputPath: ~/projects/calibrations/pruebas/projName
# sites$site$projectFile: path #arable/DE_gebesee/DE_gebesee_metrx/DE_gebesee_metrx.xml
# sites$site$removeDays: [[year,1,365],[1993,1,2]]
library(stringr)
library(yaml)

folder <- "~/projects/calibrations/superSen/"
fixed.folder <- "~/projects/calibrations/superSen/fixed/ldndc"
model_inputs <- "~/projects/model-inputs/"
file <- "~/projects//calibrations/superSen/projectFiles.txt"
lines <- readLines(file)

cmd.lines <- c()


for (line in lines) {
  print(line)
  spls <- strsplit(line, "/")[[1]]
  file_path <- file.path(model_inputs,line)
  schedule <- str_trim(grep("*<schedule", readLines(file_path), value=T))
  year <- str_split(str_split(schedule, "=\"")[[1]][2],"-")[[1]][1]

  projName <- strsplit(spls[length(spls)],"\\.")[[1]][1]
  
  projDir <- paste0(folder,projName)
  dir.create(projDir)
  file.copy(from=fixed.folder, to=projDir, recursive=T, copy.mode=T)
  
  config.file <- file.path(projDir, "ldndc/config.yml")
  config.lines <- readLines(config.file)
  config.lines[2] <- paste0("  common: &folder ~/projects/calibrations/superSen/",projName,"/ldndc")
 # config.lines[2] <- paste0("  scheduleArg: &schedule --schedule=\"2006-01-01/1 -> +10-0-0\"")
  config.lines[44] <- paste0("  confInputPath: ~/projects/calibrations/pruebas/",projName)
  config.lines[49] <- paste0("    projectFile: ", line)
  config.lines[57] <- paste0("    removeDays: [[",year,",1,365],[1993,1,2]]")
  write.table(config.lines,config.file, quote=F, row.names=F, col.names=F)
  
  exec.cmd <- paste0("R --slave CMD BATCH '--args ",config.file,"' ~/projects/sensitivity/code/MainMorris.R")
  cmd.lines <- c(cmd.lines, exec.cmd)
}

setwd("~/projects/sensitivity/code")

for (cmd in cmd.lines) {
  tryCatch({
    print(cmd)
    system(cmd, wait=T)
  },
  error = function(err) {
    print(err)
    print(paste("Error executing",cmd))
  }) 
}


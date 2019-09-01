#
# Creates config object from yaml file provided
#

args=(commandArgs(T))

config.file <- "config.yml"

if (length(args) == 0) {
  print("No arguments supplied. We use config.yml as default config file")
} else {
  config.file <- args[[1]]
  print(paste("Config file:",config.file))
}

config <- yaml.load_file(config.file)

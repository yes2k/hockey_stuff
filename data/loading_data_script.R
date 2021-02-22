# This script is used to download hockey_data from google drive
# Also included a function to delete the locally downloaded data
library(tidyverse)
library(googledrive)
library(here)

download_hockey_data <- function(){
  # Getting all the hockey data csv id's and names
  hockey_data <- drive_ls(as_id("1JBYLksz9jeS6_ZN1wZRVf1mjbZWiZwX0"))
  
  # Downloading data
  apply(hockey_data, 1, function(x){
    file_path <- paste0(here(), "/data/", x$name)
    if(!file.exists(file_path)){
      drive_download(file=as_id(x$id), 
                     path=file_path)
    }
  })
}

deleting_hockey_data <- function(x){
  # Getting all the hockey data csv id's and names
  hockey_data <- drive_ls(as_id("1JBYLksz9jeS6_ZN1wZRVf1mjbZWiZwX0"))
  paste0(here(), "/data/", hockey_data$name) %>%
    file.remove(.)
}
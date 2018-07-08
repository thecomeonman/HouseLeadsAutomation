rm(list = ls())

cRootDirectory = '/home/ask/Desktop/HouseLeadsAutomation'

setwd(cRootDirectory)

setwd('ScrapingScripts')

sapply(
   list.files(),
   function(x) {

      print(x)
      source(x)
      
      print('Finished. 10 seconds wait because reasons.')
      # needed because either the googleSheets package or the authorisation can't process requests very quilckly
      Sys.sleep(10)

   }

)
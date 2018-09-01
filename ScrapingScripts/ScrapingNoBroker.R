# rm(list = ls())

library(googlesheets)
library(data.table)
library(dplyr)
library(rjson)

cFileName = 'Automated House Leads 2018'
cResultsSheetName = 'NoBroker'
cSearchURLPattern = 'nobroker'

# Getting details from the Google sheet
# Code is same across websites
# =============================================================================

# Reading the entire sheet
AHH <- gs_title(cFileName)

# Reading the list of search URLs from this website
gsWebpageSearch <- AHH %>% gs_read(ws = "QueryURLs")
vcWebpageSearch = gsWebpageSearch$URL
vcWebpageSearch = vcWebpageSearch[
   grepl(
      tolower(vcWebpageSearch), 
      pattern = cSearchURLPattern
   )
]

# Reading the listings already scraped from this website
gsListings <- AHH %>% gs_read(ws = cResultsSheetName)
vcAlreadySeenListings = gsListings$ZZURL

# Getting the filters to be applied to the new readings which get scraped
gsFilters <- AHH %>% gs_read(ws = paste0(cResultsSheetName, "Filters"))



# Getting the list of properties to scrape by scraping each search URL
# =============================================================================

# We'll populate this vector with the final URLs
vcURLsToScrape = c()

# counter to loop through all the search URLs
i = 1


repeat {
   
   if ( i > length(vcWebpageSearch) ) {
      break
   }

   print(paste('Searching',vcWebpageSearch[i]))
   
   iPageNo = 1
   vcURLsToScrapeFromThisPage = c()
   
   repeat {

      print(paste('Page number',iPageNo))
   
      # if the search page returns multiple pages of results then
      # need to go through each of them
      vcWebpage = readLines(
         paste0(
            vcWebpageSearch[i],
            '&pageNo=',
            iPageNo
         )
      )

      # Removing all the unnecessary data from this webpage
      # and get the section which has the search results
      vcWebpageListings = vcWebpage[
         grepl(
            x = vcWebpage, 
            pattern = 'overview open_detail_page"'
         )
      ]

      # Getting the listings
      vcWebpageListings = gsub(
         x = vcWebpageListings,
         pattern = '.*data-url="',
         replacement = ''
      )

      vcWebpageListings = gsub(
         x = vcWebpageListings,
         pattern = '".*',
         replacement = ''
      )
      
      # If there were no search results to be added from this page
      # then get out of the loop
      if ( all(vcWebpageListings %in% vcURLsToScrapeFromThisPage))  {
         
         break 
         
      }

      print(paste('Adding',vcWebpageListings))
      
      vcURLsToScrapeFromThisPage = c(
         vcURLsToScrapeFromThisPage, 
         vcWebpageListings
      )

      vcURLsToScrapeFromThisPage = unique(vcURLsToScrapeFromThisPage)
      
      # Incrementing page number
      iPageNo = iPageNo + 1
      
   }


   # Appending the results from this search to all the results from
   # previous searches
   vcURLsToScrape = c(
      vcURLsToScrape,
      vcURLsToScrapeFromThisPage
   )
   
   # Next search URL
   i = i + 1
   
   
}

rm(vcWebpage)



# Scraping search reults of apartment complexes
# =============================================================================

# Creating the URL from the ID of the property
vcURLsToScrape = unique(vcURLsToScrape)

vcURLsToScrape = gsub(
   vcURLsToScrape, 
   pattern = '.*/(.*?)/detail', 
   replacement = '\\1'
)

vcURLsToScrape = paste0(
   'https://nobroker.in/api/v1/property/', 
   vcURLsToScrape
)

# Removing the ones which have been queried already
vcFinalListingURLToKeeps = setdiff(
   vcURLsToScrape,
   vcAlreadySeenListings
)

vcURLsToScrape = vcFinalListingURLToKeeps

vcURLsToScrape = unique(vcURLsToScrape)


# If there are any new URLs to scrape left then scrape
if ( length (vcURLsToScrape) > 0 ) {

   # Looping through each URL  
   dtListings = rbindlist(
      lapply(
         vcURLsToScrape,
         function( cListing ) {

            print(paste('Scraping',cListing))

            # The data is there in a json-seque format. Getting it out
            vcWebpage = readLines(cListing)
            vcWebpage = fromJSON(vcWebpage)

            dtTemp = data.table(
               t(
                  vcWebpage$data[
                     c(
                        'buildingName',
                        'description',
                        'leaseType',
                        'propertySize',
                        'type',
                        'locality',
                        'street',
                        'society',
                        'bathroom',
                        'swimmingPool',
                        'gym',
                        'furnishing',
                        'amenities',
                        'location',
                        'powerBackup',
                        'waterSupply',
                        'rent',
                        'deposit',
                        'balconies',
                        'detailUrl'
                     )
                  ]
               )
            )[, 
               lastUpdateDate := strftime(
                  as.POSIXct(
                     vcWebpage$data$lastUpdateDate / 1000, 
                     origin = '1970-01-01'
                  ), 
                  "%Y%m%d"
               )
            ][, 
               availableFrom := strftime(
                  as.POSIXct(
                     vcWebpage$data$availableFrom / 1000, 
                     origin = '1970-01-01'
                  ), 
                  "%Y%m%d"
               )
            ][, 
               ZZURL := cListing
            ]

            dtTemp
            
         }
      ),
      fill = T
   )

}


# Cleaning the data
# =============================================================================

if ( exists('dtListings') ) {

   # Changing column names from their website name to something
   # that R can accept
   setnames(
      dtListings,
      make.names(colnames(dtListings))
   )

   setnames(
      dtListings,
      'detailUrl',
      'ZZHumanURL'
   )

   dtListings[, ZZHumanURL := paste0('https://nobroker.in', ZZHumanURL)]

}


# Automatic processing of the data
# Code is same across websites
# =============================================================================

if ( exists('dtListings') ) {

   #  Running the automated filter
   dtListings[, ZZStatus := '']
   dtListings[, ZZComments := '']

   dtFilters = data.table(gsFilters)

   if ( nrow(dtFilters) > 0 ) {

      for ( i in seq(nrow(dtFilters))) {

         if ( dtFilters[i, Column] %in% colnames(dtListings) ) {

            dtListings[
               grep(
                  x = get(dtFilters[i, Column]), 
                  pattern = dtFilters[i, Value], 
                  invert = dtFilters[i, Invert]
               ),
               c(
                  'ZZCalledBy',
                  'ZZStatus',
                  'ZZComments'
               ) := list(
                  'Automated', 
                  paste0(ZZStatus, dtFilters[i, Status], '; '),
                  paste0(ZZComments, dtFilters[i, Comment], '; ')
               )
            ]

         }

      }

   }

   rm(dtFilters)


}


# Uploading details of the properties back to the Google sheet
# Code is same across websites
# =============================================================================
if ( exists('dtListings') ) {

   # Putting old and new entries together
   dtListings = rbind(
      data.frame(gsListings), 
      dtListings, 
      fill = T
   )

   setDT(dtListings)

   # Changing order of columns such that user entered columns come last
   setcolorder(
      dtListings,
      c(
         grep(
            colnames(dtListings), 
            pattern = '^ZZ', 
            value = T, 
            invert = T
         ), 
         grep(
            colnames(dtListings), 
            pattern = '^ZZ', 
            value = T
         )
      )
   )

   # Error values are easier on the eye this way
   dtListings[dtListings == 'NULL'] = ''
   dtListings[dtListings == 'NA'] = ''
   dtListings[is.na(dtListings)] = ''

   # Deleting previous sheet and adding data as a new sheet
   # This is needed in case there are any new 
   # columns that got added in this iteration
   # AHH %>%
      # gs_ws_delete(ws = cResultsSheetName)
   AHH = gs_ws_rename(AHH, from = cResultsSheetName, to = 'temp')

   AHH <- gs_title(cFileName)

   AHH %>%
      gs_ws_new(
         ws_title = cResultsSheetName, 
         input = dtListings,
         trim = TRUE, 
         verbose = FALSE
      )

   AHH %>%
      gs_ws_delete(ws = 'temp')

}
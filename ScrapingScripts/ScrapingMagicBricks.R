# rm(list = ls())

library(googlesheets)
library(data.table)
library(dplyr)
library(rjson)

cFileName = 'Automated House Leads 2018'
cResultsSheetName = 'MagicBricks'
cSearchURLPattern = 'magicbricks'

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
   
   # if the search page returns multiple pages of results then
   # need to go through each of them
   iPageNo = 1
   vcURLsToScrapeFromThisPage = c()
   
   repeat {

      print(paste('Page number',iPageNo))

      cPageBeingRead = paste0(
         gsub(
            x = vcWebpageSearch[i], 
            pattern = 'page=.*?&', 
            replacement = paste0('page=',iPageNo, '&')
         )
      )

      print(cPageBeingRead)

      # Substituting the page number in the search page URL
      vcWebpage = readLines(
         cPageBeingRead
      )

      # Removing all the unnecessary data from this webpage
      # and get the section which has the search results
      
      iLastResultLine = grep(
         x = vcWebpage,
         pattern = 'No More Results'
      )

      if ( length(iLastResultLine) == 0 ) {

         iLastResultLine = length(vcWebpage)

      }

      vcListingURLs = vcWebpage[
         grep(
            x = vcWebpage[
               1:iLastResultLine         
            ], 
            pattern = 'resultBlockWrapper'
         )
      ]

      if ( length(vcListingURLs) == 0 ) {

         break

      }

      # Getting the listings
      vcListingURLs = gsub(
         x = vcListingURLs,
         pattern = '.*/propertyDetails',
         replacement = '/propertyDetails'
      )

      vcListingURLs = gsub(
         x = vcListingURLs,
         pattern = "',.*",
         replacement = ''
      )

      # Gets used in determining whether there are any new 
      # search results to be had from this page number
      iPreviousLength = length(vcURLsToScrapeFromThisPage)

      print(paste('Adding',vcListingURLs))

      vcURLsToScrapeFromThisPage = c(
         vcURLsToScrapeFromThisPage, 
         vcListingURLs
      )
      

      vcURLsToScrapeFromThisPage = unique(vcURLsToScrapeFromThisPage)

      # If there were no search results to be added from this page
      # then get out of the loop
      if ( 
         length(vcURLsToScrapeFromThisPage) == iPreviousLength |
         iLastResultLine < length(vcWebpage)
      ) {

         break

      }

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

# the URL by default doesn't have the full path
vcURLsToScrape = paste0('http://MagicBricks.com/', unique(vcURLsToScrape))

# Removing the ones which haave been queried already
vcURLsToScrape = setdiff(
   vcURLsToScrape,
   vcAlreadySeenListings
)


# If there are any new URLs to scrape left then scrape
if ( length (vcURLsToScrape) > 0 ) {

   # Looping through each URL  
   dtListings = rbindlist(
      lapply(
         vcURLsToScrape,
         function( cListing ) {
            
            print(paste('Scraping',cListing))

            vcWebpage = readLines(cListing)

            # Removing all the useless content which aren't details
            # of the property being looked at
      

            vcDetails =  vcWebpage[
               which(grepl(x = vcWebpage, pattern = 'propInfoBlockInn')):
               which(grepl(x = vcWebpage, pattern = 'belowSec'))
            ]

            # Getting all the details
            # vcDetails = paste(vcDetails, collapse = ''

            dtTemp = data.table(
               Category = paste(vcDetails[grep(vcDetails, pattern  = 'p_title')], vcDetails[grep(vcDetails, pattern  = 'p_title') + 1]),
               Value = paste(vcDetails[grep(vcDetails, pattern  = 'p_value')], vcDetails[grep(vcDetails, pattern  = 'p_value') + 1])
            )

            dtTemp[, Category := gsub(x = Category, pattern = '.*p_title\">', replacement = '')]
            dtTemp[, Category := gsub(x = Category, pattern = '<.*', replacement = '')]

            dtTemp[, Value := gsub(x = Value, pattern = '</div.*', replacement = '')]
            dtTemp[, Value := gsub(x = Value, pattern = '.*>', replacement = '')]

            dtTemp = rbind(
               dtTemp,
               data.table(
                  Category = 'Rent',
                  Value = gsub(
                     x = gsub(
                        x = vcWebpage[grep(x = vcWebpage, pattern = 'itemprop=\"price\"')],
                        pattern = '.*content=\"',
                        replacement = ''
                     ),
                     pattern = '\".*',
                     replacement = ''
                  )
               )
            )

            dtTemp = rbind(
               dtTemp,
               data.table(
                  Category = 'Bedrooms',
                  Value = vcDetails[grep(vcDetails, pattern  = 'seeBedRoomDimen') + 1][1]
               )
            )

            iLocatlityStartFlag = which(grepl(x = vcWebpage, pattern = 'p_address'))
            iLocatlityEndFlag = which(grepl(x = vcWebpage[iLocatlityStartFlag:length(vcWebpage)], pattern = '</span>'))[1]
            cLocality = paste(vcWebpage[iLocatlityStartFlag:(iLocatlityStartFlag+iLocatlityEndFlag-1)], collapse = '')

            cLocality = gsub(cLocality, pattern = "for rent in", replacement = '')
            cLocality = gsub(cLocality, pattern = "<.*?>", replacement = '')
            cLocality = gsub(cLocality, pattern = "^.*?>|<.*?$", replacement = '')

            dtTemp = rbind(
               dtTemp,
               data.table(
                  Category = 'Locality',
                  Value = cLocality
               )
            )

            dtTemp = rbind(
               dtTemp,
               data.table(
                  Category = 'Posted By',
                  Value = gsub(
                      x = grep(x = vcWebpage, pattern = 'nameTitle', value = T)[1], 
                      pattern = '<.*?>', 
                      replacement = ''
                  )
               )
            )


            iRowWithDescription = grep(x = vcWebpage, pattern = 'descriptionCont')
            if ( grepl(x = vcWebpage[iRowWithDescription + 2], 'table') ) {

               cDesc = gsub(
                  x = vcWebpage[ iRowWithDescription + 5],
                  pattern = '<.*?>', 
                  replacement = ''
               )

            } else {

               cDesc = vcWebpage[ iRowWithDescription + 2]

            }

            dtTemp = rbind(
               dtTemp,
               data.table(
                  Category = 'Desc',
                  Value = cDesc
               )
            )

            setDT(dtTemp)

            dtTemp[, ZZURL := cListing]
            
         }
      ),
      fill = T
   )

}

# dtListings2 = copy(dtListings)

# Cleaning the data
# =============================================================================

if ( exists('dtListings') ) {

   dtListings[, Value := gsub(x = Value, pattern = '&nbsp;', replacement = '')]
   dtListings[, Value := gsub(x = Value, pattern = '^ *| *$', replacement = '')]
   dtListings[, Value := gsub(x = Value, pattern = ' +', replacement = ' ')]
   dtListings[, Value := gsub(x = Value, pattern = "What's Nearby", replacement = '')]
   dtListings[, Category := gsub(x = Category, pattern = ' |[[:punct:]]', replacement = '')]

   # MB is grammar enthu and the label can be singular or plural
   dtListings[
      Category == 'Lift',
      Category := 'Lifts'
   ]
   dtListings[
      Category == 'Bathroom',
      Category := 'Bathrooms'
   ]
   dtListings[
      Category == 'Balcony',
      Category := 'Balconies'
   ]

   # Long format to wide format
   dtListings = dcast(
      dtListings, 
      ZZURL ~ Category, 
      value.var = 'Value',
      fun.aggregate = function(x) paste(x, collapse = ',')
   )
   
   # Changing column names from their website name to something
   # that R can accept
   setnames(
      dtListings,
      make.names(colnames(dtListings))
   )

   # Some useless columns that we probably don't care about
   dtListings = dtListings[, c('BrokerageResponse','Facing','PujaRoom','UnitsonFloor','Desc') := NULL]

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
   # columns that go added in this iteration
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
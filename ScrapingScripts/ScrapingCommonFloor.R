# rm(list = ls())

library(googlesheets)
library(data.table)
library(dplyr)
library(rjson)

cFileName = 'Automated House Leads 2018'
cResultsSheetName = 'CommonFloor'
cSearchURLPattern = 'commonfloor'

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

# This site lists a society as one entry which the user can
# click on to see the actual property entries. We'll need 
# to scrape these society pages separately after we're done
# with the search URLs. This vector will store them.
vcWebpagesAddToSearch = c()

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
      
      # Substituting the page number in the search page URL
      vcWebpage = readLines(
         paste0(
            gsub(
               x = vcWebpageSearch[i], 
               pattern = 'page=.*?&', 
               replacement = paste0('page=',iPageNo, '&')
            )
         )
      )

      vcURLsToScrapeFromThisPage = grep(
         x = vcWebpage,
         pattern = 'inforow clearfix',
         value = T
      )

         
      if ( length(vcURLsToScrapeFromThisPage) == 0 )  {

         # If there were no search results to be added from this page
         # then get out of the loop
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




# Scraping details of the properties
# =============================================================================

if ( length (vcURLsToScrape) > 0 ) {

   # the URL by default doesn't have the full path
   vcURLsToScrape = paste0('https://www.commonfloor.com', unique(vcURLsToScrape))

   # Removing the ones which haave been queried already
   vcURLsToScrape = setdiff(
      vcURLsToScrape,
      vcAlreadySeenListings
   )

}

# If there are any new URLs to scrape left then scrape
if ( length (vcURLsToScrape) > 0 ) {

   # Looping through each URL
   dtListings = rbindlist(
      lapply(
         vcURLsToScrape,
         function( cListing ) {
            

            print(paste('Scraping',cListing))

            vcWebpage = readLines(cListing)

            # The data is there in a json-seque format. Getting it out         
            cStringToChopOffAfterSuggestion = 'project_masthead_image'
            vcWebpage = vcWebpage[
               grepl(
                  x = vcWebpage, 
                  pattern = cStringToChopOffAfterSuggestion
               )
            ]

            vcWebpage = gsub(
               x = vcWebpage, 
               pattern = paste0(cStringToChopOffAfterSuggestion,'.*'), 
               replacement = ''
            )
            
            vcWebpage = substr(
               vcWebpage, 
               start = 1, 
               nchar(vcWebpage)-2
            )

            vcWebpage = gsub(
               vcWebpage, 
               pattern = '<script>var stateFromServer = \\{"flpBody":', 
               replacement = ''
            )

            vcWebpage = paste0(vcWebpage, '}')
            lWebPage = fromJSON(vcWebpage)
            
            # Extracting the fields of data from the json
            lWebPage = lWebPage[!sapply(lWebPage, is.null)]
            
            # Combining the amenities into one field
            dtTemp = data.table(t(lWebPage[setdiff(names(lWebPage), 'amenities')]))
            
            if ( length(lWebPage$amenities) > 0 ) {
               
               dtTemp = cbind(
                  dtTemp,
                  data.table(t(sapply(lWebPage$amenities, function(x) {paste(x, collapse = ',')})))
               )
               
               setDT(dtTemp)
            }

            dtTemp[, ZZURL := cListing]
            
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

   # Some useless columns that we probably don't care about
   dtListings[, 
      c('id','intent','is_negotiable','area_id','city','property_id','added_by','pause_mask','is_physically_verified','is_shortlisted','url','locality_url','facing','flooring_type','servant_accommodation','pet_allowed','ownership','is_deposit_negotiable','is_dining_space_available','brokerage_terms') := NULL
   ]

   # Some cleaning up of the data
   dtListings[, about_project := gsub(x = about_project, pattern = '%20', replacement = ' ')]
   dtListings[, location_url := paste0('https://www.commonfloor.com', location_url)]

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

         dtListings[
            grep(
               x = get(dtFilters[i, Column]), 
               pattern = dtFilters[i, Value], 
               invert = dtFilters[i, Filter]
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
   AHH %>%
      gs_ws_delete(ws = cResultsSheetName)

   AHH <- gs_title(cFileName)

   AHH %>%
      gs_ws_new(
         ws_title = cResultsSheetName, 
         input = dtListings,
         trim = TRUE, 
         verbose = FALSE
      )

}
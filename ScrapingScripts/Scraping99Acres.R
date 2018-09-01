# rm(list = ls())

library(googlesheets)
library(data.table)
library(dplyr)
library(rjson)

cFileName = 'Automated House Leads 2018'
cResultsSheetName = '99Acres'
cSearchURLPattern = '99acres'


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
gsFilters <- AHH %>% gs_read(ws = "99AcresFilters")




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

      # Substituting the page number in the search page URL
      vcWebpage = readLines(
         paste0(
            gsub(
               x = vcWebpageSearch[i], 
               pattern = 'ffid-.*?\\?', 
               replacement = paste0('ffid-page-',iPageNo, '?')
            )
         )
      )

      # Removing all the unnecessary data from this webpage
      # and get the section which has the search results
      vcWebpageListings = vcWebpage[
         grepl(
            x = vcWebpage, 
            pattern = 'data-propPos'
         )
      ]


      # The search results, for some reason that I didn't try
      # and understand, seem to be in one of four formats. I'm
      # splitting the contents of the webpage against all four 
      # formats to get the entries out

      # 1
      vcWebpageListingsHREF = vcWebpageListings[
         grepl(
            x = vcWebpageListings, 
            pattern = 'href="'
         )
      ]

      if ( length(vcWebpageListings) == 0 ) {

         break

      }

      vcWebpageListingsHREF = gsub(
         x = vcWebpageListingsHREF,
         pattern = '.*href="',
         replacement = ''
      )

      vcWebpageListingsHREF = gsub(
         x = vcWebpageListingsHREF,
         pattern = '".*',
         replacement = ''
      )


      # 2
      vcWebpageListingsHREF2 = vcWebpageListings[
         grepl(
            x = vcWebpageListings, 
            pattern = 'href=/'
         )
      ]

      vcWebpageListingsHREF2 = gsub(
         x = vcWebpageListingsHREF2,
         pattern = '.*href=',
         replacement = ''
      )

      vcWebpageListingsHREF2 = gsub(
         x = vcWebpageListingsHREF2,
         pattern = ' itemprop.*',
         replacement = ''
      )

      vcWebpageListingsHREF2 = gsub(
         x = vcWebpageListingsHREF2,
         pattern = ' data-fsl.*',
         replacement = ''
      )

      # 3
      vcWebpageListingsBlank = vcWebpageListings[
         grepl(
            x = vcWebpageListings, 
            pattern = 'target=_blank '
         )
      ]

      vcWebpageListingsBlank = vcWebpageListings[
         !grepl(
            x = vcWebpageListings, 
            pattern = 'href'
         )
      ]

      vcWebpageListingsBlank = gsub(
         x = vcWebpageListingsBlank,
         pattern = 'href=',
         replacement = ''
      )

      vcWebpageListingsBlank = gsub(
         x = vcWebpageListingsBlank,
         pattern = ' itemprop.*',
         replacement = ''
      )

      # 4
      vcWebpageListingsNoHREF = vcWebpageListings[
         !grepl(
            x = vcWebpageListings, 
            pattern = 'href'
         )
      ]

      vcWebpageListingsNoHREF = gsub(
         x = vcWebpageListingsNoHREF,
         pattern = '*.a data-propPos=',
         replacement = ''
      )

      vcWebpageListingsNoHREF = gsub(
         x = vcWebpageListingsNoHREF,
         pattern = '" data-fsl.*',
         replacement = ''
      )

      # Gets used in determining whether there are any new 
      # search results to be had from this page number
      iPreviousLength = length(vcURLsToScrapeFromThisPage)

      vcURLsToScrapeFromThisPage = c(
         vcURLsToScrapeFromThisPage, 
         vcWebpageListingsHREF,
         vcWebpageListingsHREF2,
         vcWebpageListingsNoHREF
      )

      vcURLsToScrapeFromThisPage = unique(vcURLsToScrapeFromThisPage)

      # If the current search page added zero new entries to the 
      # URLs to search then tthere probably aren't any more search
      # results to query. We'll use this variable to check that 
      # condition later.
      if ( length(vcURLsToScrapeFromThisPage) == iPreviousLength ) {

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

# the URL by default doesn't have the full path
vcURLsToScrape = paste0('http://99acres.com/', unique(vcURLsToScrape))

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
            iStartingIndex = which(grepl(
               x = vcWebpage,
               pattern = 'pdMainFacts type2'
            ))

            if ( length(iStartingIndex) == 0 ) {
               return ( data.table() )
            }

            iEndingIndex = which(grepl(
               x = vcWebpage[iStartingIndex:length(vcWebpage)],
               pattern = '</table>'
            ))[1]

            iEndingIndex = iStartingIndex + iEndingIndex - 1
                
            vcTable = vcWebpage[iStartingIndex:iEndingIndex]

            # Getting all the details
            vcTable = unlist(strsplit(
               unlist(
                  strsplit(
                     vcTable, 
                     '<tr>'
                  )
               ),
               '<td>'
            ))

            vcTable[length(vcTable)] = gsub(
               x = vcTable[length(vcTable)],
               pattern = '</table>.*',
               replacement = ''
            )

            vcTable = grep(
               x = vcTable,
               pattern = 'span.*id',
               value = T
            )

            dtTemp = data.table(
               Category = gsub(
                  x = vcTable,
                  pattern = '.*span.*id=\"(.*?)\">.*',
                  replacement = '\\1'
               ),

               Value = gsub(
                  x = gsub(
                     x = vcTable,
                     pattern = '.*\">',
                     replacement = ''
                  ),
                  pattern = '<.*',
                  replacement = ''
               )
            )

            dtTemp = dtTemp[!grepl(x = Category, pattern = '>|<')]

            vcFacilities = vcWebpage[
               grep(
                  vcWebpage, 
                  pattern = 'amnIcons'
               ) + 1
            ]

            vcFacilities = gsub(
               x = vcFacilities,
               pattern = '</div>',
               replacement = ''
            )

            vcFacilities = gsub(
               x = vcFacilities,
               pattern = '<div.*>',
               replacement = ''
            )

            vcFacilities = gsub(
               x = vcFacilities,
               pattern = ' *$|^ *',
               replacement = ''
            )

            vcFacilities = paste(
               vcFacilities[vcFacilities != ''], 
               collapse = ', '
            )

            dtTemp = rbind(
               dtTemp, 
               data.table(
                  Category = 'Facilities',
                  Value = vcFacilities
               )
            )

            setDT(dtTemp)

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

   # Some cleaning up of the data
   dtListings[, Value := gsub(x = Value, pattern = '^ *| *$', replacement = '')]
   dtListings[, Value := gsub(x = Value, pattern = ' +', replacement = ' ')]
   dtListings[, Category := gsub(x = Category, pattern = ' |[[:punct:]]', replacement = '')]

   # Going from long format to wide format
   dtListings = dcast(dtListings, ZZURL ~ Category, value.var = 'Value')

   setDT(dtListings)
   setnames(
      dtListings,
      make.names(colnames(dtListings))
   )

   # Some extra junk which doesn't get cleaned
   dtListings[, Rent := pdPrice2]
   dtListings[, pdPrice2 := NULL]
   dtListings[, Rent := gsub(x = Rent, pattern = '.*;', replacement = '')]
   dtListings[, Rent := gsub(x = Rent, pattern = ' |[[:punct:]]|[[:alpha:]]', replacement = '')]

 
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
         grep(colnames(dtListings), pattern = '^ZZ', value = T, invert = T), 
         grep(colnames(dtListings), pattern = '^ZZ', value = T)
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
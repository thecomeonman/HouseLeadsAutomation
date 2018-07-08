# House Leads Automation

When 2 or more flatmates are looking for a place to stay, over multiple weekends, in multiple areas, without a broker, it becomes a little tricky to collaborate. The UI for most of these real estate portals is very poorly designed and slows you down further.

These scripts scrape relevant postings from 4 popular websites and keep it in a simple, easy to compare format on Google Sheets.

![Sample screenshot](/Screenshots/SheetExample.png?raw=true)

## Installation
The scripts are written in the open source language, R. You need to:
1. Install R from https://cran.r-project.org/
2. Install RStudio from https://www.rstudio.com/
3. I've built this using libraries that others have contributed to R and you need to install those libraries too. The first time ever that you open RStudio, you need to have access to the internet and run this command - `install.packages(c('googlesheets','data.table','dplyr','rjson','httpuv'))`. 

## Running
1. You need to download this repository to your computer. You can download a zip file and expand it on your hard drive.
2. Upload Automated House Leads.xlsx to your Google Drive and share it with your flatmates. Your local copy of the file is irrelevant now. You can delete it if you want even. Everything is going to happen on Google sheets.
    - You have to open this file in Google Drive, where it will ask you if you want to open it with Google Sheets instead. Open it with Google Sheets.        After this, you should be able to edit cells, etc. and not just see a snapshot of the file.
    - Instructions to add the areas you're interested in, the budgets, etc. are in the Instructions sheet of this file.
    - If, for some reason, you want to change the name of the file then you need to change the value of the `cFileName` variable in each of the files in the ScrapingScripts folder.
3. Open Wrapper.R ( in RStudio preferably. Notepad, etc. should also be fine. NOT in MS Word, ) and change value of the variable `cRootDirectory` to point to the directory in which the Wrapper.R file is present. Remember to use `/` and not `\` in the path.
4. There are multiple ways to now run this. Two of the ways are:
    - Pasting all the lines in the RStudio console and then pressing enter.
    - Selecting all the lines in the RStudio editor and pressing ctrl + enter.
5. On running wrapper.R, you will need to permit the code to read adn write to your Google sheet. The code will automatically open the link in your default browser for you to give these permissions. I / some other evil entity doesn't get access to your account, don't worry.
5. Except for what has been mentioned above, avoid adding new files or modifying anything inside this folder.
6. You can rerun Wrapper.R any number of times you wish to get the latest results.

## Scope for Enhancements
- Adding other sites. I didn't find a lot of useful entries on other sites for my search which is why I implemented only these 4. CommonFloor also turned out to be a waste of time.
- Pushing the results back to the Google sheet is very slow. This is because the whole sheet for that website is being sent and not just the new entries found this time. That's because if new columns get added from the second run onwards, then they cause issues.
- Comparing across website. I was too lazy to standardise column names across websites, etc. because this was enough for me.
- Visualising the data. I thought of implementing a parallel coordinates based chart to read all this output and show typical combinations of houses, the cheaper deals, etc. but just the listings were enough for me so I didn't try.

## Bug reports
File an issue here or post a comment on the blog post - http://thecomeonman.blogspot.com/2017/05/house-leads-automation.html

## FAQ

The user still needs to visit the respective website to get the user contact details.

I'm not making any money from this so I hope this isn't illegal in any way. No, this is not something worth starting a company over.

I'm don't plan to actively work on this so it's unlikely that new features are going to get added. I might resolve bugs if I have time for it though.

Entries for the same property on different websites aren't being detected.

I have to figure out which license works best for me. If there is a license that says that except for using this for your own personal needs, you can't use this code in any other manner without my permission then that's the license this is going to be under.

I'm not really an expert on this so if there is a simpler way to do this then I'd love to know.

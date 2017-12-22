---
layout: post
title: How do election observers write about elections? A tidytext analysis
gh-repo: daattali/beautiful-jekyll
gh-badge:
  - star
  - fork
  - follow
published: true
date: '2017-12-22'
---
![EU Election Observers in Tunisia]({{site.baseurl}}/img/observers.jpg)

In an ironic twist, in August, election observers fell under scrutiny when Kenya's Supreme Court declared the presidential vote invalid - after most monitoring groups said the polls were genuine. This isn't the first time election observers have come under criticism. Scholars like [Thomas Carothers](http://carnegieendowment.org/files/Carothers_-_The_Observers_Observed.pdf) were criticizing their "amateurish work" as far back as the late 1990s.  Others, like [Judith Kelley](https://sites.duke.edu/kelley/files/2012/03/JOD.pdf), have looked at how different monitoring organizations have observed the same elections, but reached quite different conclusions. Her research suggests that  observers are more likely to endorse elections where the country is a foreign-aid recipient or where the election observers are part of missions from inter-governmental organisations, like the European Union (EU) or the Organization for Security and Cooperation in Europe (OSCE).  

I've always been interested in this question. In theory, election observers should adhere to the _[Declaration of Principles for International Election Observation](http://www.osce.org/odihr/16935?download=true)_, a document that lays out specific guidelines for their behaviour. Many of the most reputable organizations follow these guidelines closely and their reports are usually highly diplomatic texts, for good reason. Research has shown that the mere presence of observers can deter or displace fraud and reduce the likelihood of election-related protests.

Can we use text mining to tell us more about how election observers write about elections? What words are used the most? What is the average sentiment of reports? Is this different for the tpe of election (parliamentary or presidential)? Doe sentiment analysis work - that is, does it reflect whether or not the observers endorsed the election? 



#**Getting the data**

To start, I went to the OSCE website and used their internal search engine to search for the reports I needed. The first page of search results gave me only 50 reports and I scraped all of those links (and the following two pages by repeating the code - not very efficient, I know). 

~~~
library(rvest)
library(stringr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(robotstxt)
library(pdftools)

osce_reports <- read_html("http://www.osce.org/resources/documents/%22Final%20Report%22?page=1&filters=%20im_taxonomy_vid_3%3A%28120%29%20im_taxonomy_vid_1%3A%2824%29%20im_taxonomy_vid_22%3A%28472%29&solrsort=score%20desc&rows=50&category=Official%20Documents")

osce <- read_html("http://www.osce.org")

#get links to the English language PDFs
links <- osce_reports %>%
  html_nodes(".active") %>%
  html_attr("href")

#cast list as dataframe - I am pretty sure I could do this in a better way
links_df <- do.call(rbind, lapply(links, data.frame, stringsAsFactors=FALSE))

#rename the first column
names(links_df) <- "url"
~~~

At this point, I noticed that in getting the links to the PDF reports, I also grabbed a lot of extra links that I didn't need. In the next step, I had to filter out all of that junk.

~~~
#create a new column that has a logical value depending on whether or not the URL is to a downloadable report
links_df$report <- grepl('download=true', links_df$url, ignore.case=TRUE)

#keep only rows with TRUE value
links_df_subset <- links_df[links_df$report==TRUE, ]

#create full links
links_full <- paste("http://www.osce.org", links_df_subset$url, sep = "")

~~~

I was nearly ready to begin scraping the PDFs, but I checked first what the OSCE site says about scraping by using 

~~~
robotstxt::get_robotstxt("http://www.eods.eu")
~~~

It showed a crawl delay of 10 seconds, so I set my system to sleep for 10 seconds between downloading each PDF.

~~~
#loop to download reports
for (url in links_full) {
  download.file(url, destfile = basename(url),
                Sys.sleep(10))
}
~~~
I did this a few more times and ended up with 141 election reports after deleting any duplicates. (Note: From going through Stack Overflow questions, I'm pretty sure I could have set this up to scrape through the following pages of search results on the OSCE site, but I wasn't quite ready to start trying to figure that out. . .). 

There was one rather big problem with my approach though. I would need to read the PDF files into R in such a way that they could be associated with my variables of interest (country, type of election, and year). Since I had downloaded the files using the basename of their URLs on the OSCE site, this was kind of a mess. In the end, I settled on a filenaming system that looked something like this: "myanmar_parl2015.pdf". This way I could read the files into R using the filename as a variable, and create additional variables based on the filename - an approach I figured out using [this excellent answer](https://stackoverflow.com/questions/44254493/tidytext-read-files-from-folder) on Stack Overflow, by the creator of tidytext herself.

~~~
#list all files ending with extension .pdf
files <- list.files(pattern = "pdf$")

#read all PDFs and unnest them using tidytext, adding a filename variable 
osce_reports <- map_df(files, ~ data_frame(txt = pdf_text(.x)) %>%
         mutate(filename = .x) %>%
         unnest_tokens(word, txt))
         
#add 'type' column based on the text in the filename column
osce_reports$type <- ifelse(grepl("pres", osce_reports$filename, ignore.case = T), "President", 
                  ifelse(grepl("parl", osce_reports$filename, ignore.case = T), "Parliament",
                  ifelse(grepl("local", osce_reports$filename, ignore.case = T), "Local",
                  ifelse(grepl("ref", osce_reports$filename, ignore.case = T), "Referendum",
                  ifelse(grepl("general", osce_reports$filename, ignore.case = T), "General", "Other")))))

#extract years and create year column
#pattern to match numbers at the end of filenames
pattern_year <- "(\\d)+"
#pattern to match country names before the underscore
pattern_country <- "[^_]+"

#create year and country colummns
osce_reports$year <- str_extract(osce_reports$filename, pattern_year)

osce_reports$country <- str_to_title(str_extract(osce_reports$filename, pattern_country))

#add an 'organization' column specifying the organization 
osce_report$organization <- "OSCE"

#finally, convert the 'year' column to POSIxct
osce_reports$year <- as.Date(paste(osce_reports$year, 1, 1, sep = "-"))

~~~
I finally had a dataframe as a "one-row-per-term document" for tidy analysis. 

~~~
# A tibble: 1,171,527 x 6
                  filename      type       year    country           word organization
                     <chr>     <chr>     <date>     <fctr>          <chr>        <chr>
 1 uzbekistan_pres2007.pdf President 2007-01-01 Uzbekistan           2.85         OSCE
 2 uzbekistan_pres2007.pdf President 2007-01-01 Uzbekistan        420,815         OSCE
 3 uzbekistan_pres2007.pdf President 2007-01-01 Uzbekistan        434,111         OSCE
 4 uzbekistan_pres2007.pdf President 2007-01-01 Uzbekistan          88.10         OSCE
 5 uzbekistan_pres2007.pdf President 2007-01-01 Uzbekistan liberalisation         OSCE
 6 uzbekistan_pres2007.pdf President 2007-01-01 Uzbekistan     approprate         OSCE
 7 uzbekistan_pres2007.pdf President 2007-01-01 Uzbekistan            2,5         OSCE
 8 uzbekistan_pres2007.pdf President 2007-01-01 Uzbekistan         akhtam         OSCE
 9 uzbekistan_pres2007.pdf President 2007-01-01 Uzbekistan   commission63         OSCE
10 uzbekistan_pres2007.pdf President 2007-01-01 Uzbekistan        diloram         OSCE
# ... with 1,171,517 more rows
~~~
In the interest of time - and to get to the analysis - I'm going to skip the steps  that I used to get the data for EU Election Observation Missions. I scraped the data from the Election Observation and Democracy Support (EODS) [website](http://www.eods.eu/eom-reports/), which is the most up-to-date, and ended up with 76 reports after excluding non-English language texts. I did the same steps as for the OSCE data, then combined the dataframes. This left me with a rather large dataframe with 2,240,218 observations. 

Let's see which words are used the most often in election observation reports. After removing stop words and others which are less interesting (like "eu", "osce", "odihr, "eom", "elections," etc.), we end up with the following chart.  

![eu_osce_common_words.jpeg]({{site.baseurl}}/img/eu_osce_common_words.jpeg)

This is interesting because some of the most common words seem to allude to specific issues related to elections, like poltiical parties, candidates and campaigning; polling and the release of results; and voting and election day. 

We can also facet by the organization type and see which words were the most common for each.

![osce_eu_facet_words.jpeg]({{site.baseurl}}/img/osce_eu_facet_words.jpeg)

We can see slight differences in the most common words, but not much. Words like "parliamentary," "complaints," and "legal" are among the most common for the OSCE reports, whereas the EU reports contain "presidential," "stations," and (voter) "registration." 

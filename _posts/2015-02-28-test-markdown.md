---
layout: post
title: How do election observers write about elections? A tidytext analysis
subtitle: Each post also has a subtitle
gh-repo: daattali/beautiful-jekyll
gh-badge:
  - star
  - fork
  - follow
published: true
---
![]({{site.baseurl}}/img/observers.jpg)

In an ironic twist, in August, election observers fell under scrutiny when Kenya's Supreme Court declared the presidential vote invalid - after most observation groups deemed the polls genuine. This isn't the first time election observers have come under criticism. Scholars like T[homas Carothers](http://carnegieendowment.org/files/Carothers_-_The_Observers_Observed.pdf) were criticizing their "amateurish work" as far back as the late 1990s.  Others, like [Judith Kelley](https://sites.duke.edu/kelley/files/2012/03/JOD.pdf), have used statistical analysist to look at how different monitoring organizations have observed the same elections, but reached quite different conclusions. Her research suggests that election observers are more likely to endorse elections where the country is a foreign-aid recipient or where the election observers are part of missions from inter-governmental organisations, like the European Union (EU) or the Organization for Security and Cooperation in Europe (OSCE).  

I've always been interested in this question. In theory, election observers should adhere to the _Declaration of Principles for International Election Observation_ - a document endorsed by some 40 international and non-governmental election monitoring organizations - which lays out specific guidelines for their behaviour. Many of the most reputable organizations follow these guidelines closely and the reports of election observers are usually highly diplomatic texts and for good reason. Research has shown that the mere presence of observers can deter or displace fraud, cause would-be riggers to tinker with laws well before election day, and reduce the chances of election-related violence.

Thinking about election reports, I wondered if we could apply tidytext analysis to tease out the nuances. Can we use tidytext to tell us more about how election observers write about elections? 

To start, I went to the OSCE website and used their internal search engine to retrieve "Report" type documents containing "Final Report" in the title. The first page of search results gave me only 50 reports and I scraped all of those links (and the following two pages by repeating the code - not very efficient, I know). 

~~~
library(rvest)
library(stringr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(robotstxt)
library(pdftools)

#read in the link to the first 50 results after searching and filtering on the OSCE documents page for content type "reports" and "final report" 
osce_reports <- read_html("http://www.osce.org/resources/documents/%22Final%20Report%22?page=1&filters=%20im_taxonomy_vid_3%3A%28120%29%20im_taxonomy_vid_1%3A%2824%29%20im_taxonomy_vid_22%3A%28472%29&solrsort=score%20desc&rows=50&category=Official%20Documents")

osce <- read_html("http://www.osce.org")

#get links 
links <- osce_reports %>%
  html_nodes(".active") %>%
  html_attr("href")

#cast list as dataframe - I am pretty sure I could do this in a better way
links_df <- do.call(rbind, lapply(links, data.frame, stringsAsFactors=FALSE))

#rename column
names(links_df_3) <- "url"

#create a new column that has a logical value depending on whether or not the URL contains a downloadable report
links_df$report <- grepl('download=true', links_df$url, ignore.case=TRUE)

#keep only rows with TRUE
links_df_subset <- links_df[links_df$report==TRUE, ]

#create full links
links_full <- paste("http://www.osce.org", links_df_subset$url, sep = "")

~~~

I'm nearly ready to begin scraping the PDFs, but I check first what the OSCE site says about scraping by using 

~~~
robotstxt::get_robotstxt("http://www.eods.eu")
~~~

It shows a crawl delay of 10 seconds, and the OSCE's Terms of Service state that we can use a crawler so long as we do not access "the Website in a manner that sends more request messages to the OSCE servers in a given period of time than a human can reasonably produce in the same period by using a conventional online web browser." If I abide by the crawl delay, I think I'm fine. Next, I scrape the PDFs.

~~~
#loop to download reports
for (url in links_full) {
  download.file(url, destfile = basename(url),
                Sys.sleep(10))
}
~~~
![osce_eu_top_words.jpeg]({{site.baseurl}}/img/osce_eu_top_words.jpeg)


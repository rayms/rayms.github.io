---
layout: post
title: How do election observers write about elections? A tidytext analysis
published: true
date: '2017-12-22'
---
![EU Election Observers in Tunisia]({{site.baseurl}}/img/observers.jpg)

In an ironic twist, in August, election observers fell under scrutiny when Kenya's Supreme Court declared the presidential vote invalid - after most monitoring groups said the polls were genuine. This isn't the first time election observers have come under criticism. Scholars like [Thomas Carothers](http://carnegieendowment.org/files/Carothers_-_The_Observers_Observed.pdf) were criticizing their "amateurish work" as far back as the late 1990s.  Others, like [Judith Kelley](https://sites.duke.edu/kelley/files/2012/03/JOD.pdf), have looked at how different monitoring organizations have observed the same elections, but reached quite different conclusions. Her research suggests that  observers are more likely to endorse elections where the country is a foreign-aid recipient or where the election observers are part of missions from inter-governmental organisations, like the European Union (EU) or the Organization for Security and Cooperation in Europe (OSCE).  

I've always been interested in this question. In theory, election observers should adhere to the _[Declaration of Principles for International Election Observation](http://www.osce.org/odihr/16935?download=true)_, a document that lays out specific guidelines for their behaviour. Many of the most reputable organizations follow these guidelines closely and their reports are usually highly diplomatic texts, for good reason. Research has shown that the mere presence of observers can deter or displace fraud and reduce the likelihood of election-related protests.

Can we use text mining to tell us more about how election observers write about elections? What words are used the most? What is the average sentiment of reports? Is this different for the tpe of election (parliamentary or presidential)? Doe sentiment analysis work - that is, does it reflect whether or not the observers endorsed the election? 

(Warning that there's a ton of R code incoming, so if you're more interested in the analysis, scroll down below!).



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

I was nearly ready to begin scraping the PDFs, but I checked first what the OSCE site says about scraping by using: 

~~~
robotstxt::get_robotstxt("http://www.eods.eu")
~~~

Which showed a crawl delay of 10 seconds, so I set 10 seconds between downloading each PDF.

~~~
#loop to download reports
for (url in links_full) {
  download.file(url, destfile = basename(url),
                Sys.sleep(10))
}
~~~
I did this a few more times and ended up with 141 election reports after deleting any duplicates. (Note: From going through Stack Overflow questions, I'm pretty sure I could have set this up to scrape through the other pages of search results on the OSCE site, but I wasn't quite ready to start trying to figure that out. . .). 

There was one rather big problem with my approach though. I would need to read the PDF files into R in such a way that they could be associated with my variables of interest (country, type of election, and year). Since I had downloaded the files using the basename of their URLs on the OSCE site - and the filenaming conventions for these PDFs were all over the place - this was kind of a mess. In the end, I settled on a filenaming system that looked something like this: "myanmar_parl2015.pdf". This way I could read the files into R using the filename as a variable, and create additional variables based on the filename - an approach I figured out using [this excellent answer](https://stackoverflow.com/questions/44254493/tidytext-read-files-from-folder) on Stack Overflow, by one of the developers of [tidytext](https://cran.r-project.org/web/packages/tidytext/index.html), [Julia Silge](https://juliasilge.com). 

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
In the interest of time - and to get to the analysis - I'm going to skip the code that I used to get the data for EU Election Observation Missions. Basically, I scraped the data from the [Election Observation and Democracy Support (EODS) website](http://www.eods.eu/eom-reports/), and ended up with 76 reports after excluding non-English language reports. In total, I now had **217 eelction observation reports** ranging from 1997-2017 (but really, it was just one report in 1997, and most reports began from 2002) that covered **89 different countries**. Great! When I combined the OSCE and EU dataframes, I had a pretty large dataframe with 2,240,218 observations.

Let's see which words are used the most often in election observation reports. After removing stop words and other words like "eu", "osce", "odihr, "eom", "elections," etc., we have the following bar chart.  

![eu_osce_common_words.jpeg]({{site.baseurl}}/img/eu_osce_common_words.jpeg)

This is interesting because some of the most common words allude to specific issues related to elections, like political parties, candidates and campaigning; polling and the release of results; and voting and election day. 

We can also facet by the organization type and see which words were the most common for each organization.

![osce_eu_facet_words.jpeg]({{site.baseurl}}/img/osce_eu_facet_words.jpeg)

We can see slight differences in the most common words, but not much. Words like "parliamentary," "complaints," and "legal" are among the most common for the OSCE reports, whereas the EU reports contain "presidential," "stations," and (voter) "registration." 


## **Which human rights treaties do election observers reference?** 
Election observers  use international law as a standard to measure the quality of elections. This means assessing different elements of an election against the international treaties and obligations that a country has ratified. The most relevant international treaty for elections is called the International Covenant on Civil and Political Rights (ICCPR). Can we look at the election observer reports to see which treaties they reference the most? 

![eu_osce_ref_conventions.jpeg]({{site.baseurl}}/img/eu_osce_ref_conventions.jpeg)

Unsurprisingly, the ICCPR is the most referenced treaty. Next is the Convention on the Elimination of All Forms of Discrimination Against Women (CEDAW), the Universal Declaration of Human Rights (UDHR), and the Convention on the Rights of the Child (CRC). I have to admit that the last one surprised me.  

## **How often do election observers use the word "fraud"?**
Election observers are generally cautious about leveling allegations of electoral fraud, so how often do they use the term in their reports? 

![fraud_mentions.jpeg]({{site.baseurl}}/img/fraud_mentions.jpeg)

There are relatively few references to fraud in almost two decades worth of reports. But there were two years where mentions of fraud were quite high, in 2009 and 2014. What elections might have contributed to this?

~~~
# A tibble: 117 x 3
# Groups:   year [15]
         year       country  word
       <date>         <chr> <int>
 1 2009-01-01   Afghanistan   116
 2 2014-01-01   Afghanistan   104
 3 2007-01-01       Nigeria    46
 4 2005-01-01   Afghanistan    30
 5 2003-01-01       Nigeria    18
 6 2008-01-01 United States    13
 7 2009-01-01    Mozambique    12
 8 2010-01-01       Britain    11
 9 2010-01-01     Nicaragua    11
10 2006-01-01     Nicaragua    10
# ... with 107 more rows
~~~

Both the 2009 and 2014 presidential elections in Afghanistan were marred by ballot stuffing and other forms of fraud (for great analysis of detecting voter fraud with data, check out Development Seed's [take on the 2014 Afghanistan elections](https://developmentseed.org/blog/2014/07/28/afghanistan-runoff-site/)).

## **Which words are used more frequently by which election observers?**
Adapting code from Julia Silge and David Robinson's excellent book, [_Text Mining with R_](https://www.tidytextmining.com/twitter.html#word-frequencies-1), we can look at word frequencies in the final reports of EU and OSCE election observers. 

![eu_osce_word_freq.jpeg]({{site.baseurl}}/img/eu_osce_word_freq.jpeg)

There are a few things which stand out for both organizations, like how often the terms they use refer to countries where only they observed elections. For example, since the OSCE's Office for Democratic Institutions and Human Rights (ODIHR) only observes elections in OSCE participating states, you see a lot more European countries (France, Germany, Sweden, etc.) used by the OSCE; for the EU observation missions, we see more non-European countries (Pakistan, Mozambique, Jordan, Venezuela, etc.). We see that the EU references the Universal Declaration of Human Rights (UDHR) more often, whereas the OSCE references "Copenhagen," as in the [OSCE Copehagen Document](http://www.osce.org/odihr/elections/14304?download=true), an agreement by OSCE states on the "rulebook" for democratic elections and human rights. 

## **Tidy sentiment analysis**
We can again use code from _Text Mining with R_ to help us do sentiment analysis. What is the overall sentiment of EU/OSCE election observation reports? What if we facet by the organization?

![osce_eu_sentiments.jpeg]({{site.baseurl}}/img/osce_eu_sentiments.jpeg)

![osce_eu_sentiment_facet.jpeg]({{site.baseurl}}/img/osce_eu_sentiment_facet.jpeg)

The overall sentiment of election observation reports is on the positive side. When we facet by the organization, we see that OSCE reports are usually more positive than EU reports. Finally, we can also facet by the type of election. (I should note here that I had some difficulties with classification; a good number of election reports covered multiple elections like paralimentary and presidential, which I grouped into 'Other'). 

![eu_osce_sentiment_facet_type.jpeg]({{site.baseurl}}/img/eu_osce_sentiment_facet_type.jpeg)


Faceting by all types of elections is a little messy, so let's filter and look at the sentiment fo reports for only general, parliamentary, and presidental elections.

![eu_osce_type_facets.jpeg]({{site.baseurl}}/img/eu_osce_type_facets.jpeg)

### **What were the election reports with the most negative sentiment?**
We can also look at the elections where election reports were the most negative and the type of election.

![most_negative_elections.jpeg]({{site.baseurl}}/img/most_negative_elections.jpeg)

We see that parliamentary elections make up the bulk of reports that are the most negative. If we take a closer look at the elections in question, we also see that they were usually marked by military or ethnic violence, voting irregularities, ballot stuffing, bias by state media, and more. 

#**Conclusion**
In a future post, I'd like to take a look at how the reports of non-governemntal organizations compare to those by the EU and OSCE/ODIHR. Are non-governmental organizations' reports likely to be more negative in sentiment, and would the words they use reflect this? How  



_Photo: "EU Election Observation Mission in Tunisia," Ezequiel Scagnetti © European Union, (CC BY-NC-ND 2.0)._



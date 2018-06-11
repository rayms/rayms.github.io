---
layout: post
published: false
title: Analyzing Facebook ads in Ireland’s abortion referendum
---
![1stImageYES.png]({{site.baseurl}}/img/1stImageYES.png)

In May, Ireland held a referendum to repeal or retain the 8th amendment of the constitution, a provision that restricted access to abortion only when the life of the mother was at risk. By now, we know that Irish voters turned out in large numbers to repeal the 8th amendment by a [landslide](https://www.theguardian.com/world/2018/may/26/ireland-votes-by-landslide-to-legalise-abortion). But the "yes" vote came amid another Facebook [controversy](http://www.dw.com/en/facebook-blocking-foreign-ads-targeting-irish-abortion-referendum/a-43709270) in which foreign groups allegedly used Facebook ads to target Irish voters. 

Throughout the referendum, a group of volunteers setup the [Transparent Referendum Initiative](http://tref.ie), which urged Irish voters to install the [WhoTargetsMe](https://whotargets.me/en/) plugin and share screenshots of ads they'd received on Facebook. The group hosted an amazing, publicly accessible [database](http://tref.ie/database/) with different variables for the Facebook ads, and I wanted to see if some of the interactions data could be updated, since it appeared that these statistics were based on the screenshots people initally shared. I also wanted to retrieve additional posts that these Faebook advertisers had shared, as well as the comments on these posts. 


To achieve this, I needed to clean and pre-process TRE's data, which was in a Google Sheet. While trying to retrieve updated data using Facebook's API, I also discovered that one of the variables, `Post ID`, contained the wrong ids and needed to be turned into the format to retrieve data via Facebook's API. I have included all of this code and analysis in an RMarkdown document here and will just show the results of my analysis below. 

# Analysis
```{r}
## Setup and load packages 
library(readxl)
library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(stringr)
library(tidytext)
library(Rfacebook)

## Load and process data
targeted_ads <- read_excel("data/ref.xlsx", sheet = "Facebook Ads")

#add post_id column and add query_id variable that contains advertiser_id and post_id to query Facebook's API

targeted_ads_cleaned <- targeted_ads %>%
  mutate(post_id = str_replace(targeted_ads$Link, ".*/", ""),
         query_id = paste(`Advertiser ID`, post_id, sep = "_")) %>%
  select(advertisers = Advertiser, ad_text = `Ad Text`, first_seen = `First Seen`, like, comments, shares, interests, id = ID, post_id, query_id, advertiser_id = `Advertiser ID`)
````

## What was the frequency of Facebook ads over time? 


![irish_ads_timeseries.png]({{site.baseurl}}/img/irish_ads_timeseries.png)







## What was the emotional content of the Facebook ads targeting Irish voters? 

We can use sentiment lexicons to determine the emotional content of text by looking at individual words. The NRC Word-Emotion Lexicon by Saif Mohammad and Peter Turney is a list of 14,182 unigrams (words) associated with eight basic emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two binaries (positive and negative). The tidytext package, by Julia Silge and David Robinson, makes it easy to tokenize words and join them to the respective emotion categories and sentiments. 


![facebook_ads_sentiments.png]({{site.baseurl}}/img/facebook_ads_sentiments.png)




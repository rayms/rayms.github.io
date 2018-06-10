---
layout: post
published: false
title: Analyzing Facebook ads in Ireland’s abortion referendum
---
## A New Post

In May, Ireland held a referendum to repeal or retain the 8th amendment of the constitution, a provision that restricted access to abortion only when the life of the mother was at risk. By now, we know that Irish voters turned out in large numbers to repeal the 8th amendment by a [landslide](https://www.theguardian.com/world/2018/may/26/ireland-votes-by-landslide-to-legalise-abortion). But the "yes" vote came amid another Facebook [controversy](http://www.dw.com/en/facebook-blocking-foreign-ads-targeting-irish-abortion-referendum/a-43709270) in which foreign groups allegedly setup pages to target Irish voters. 

Throughout the referendum, a group of volunteers setup  the [Transparent Referendum Initiative](http://tref.ie), which urged Irish voters to install the [WhoTargetsMe](https://whotargets.me/en/) plugin and share screenshots of ads they'd received on Facebook. The group hosted an amazing, publicly accessible [database](http://tref.ie/database/) with different variables for the Facebook ads, and I wanted to see if some of the interactions data could be updated, since it appeared that these statistics were based on the screenshots people initally shared. I also wanted to retrieve additional posts that these Faebook advertisers had shared, as well as the comments on these posts. 


To start, I needed to clean and pre-process TRE's database, which was in a Google Sheet. (While trying to retrieve updated data using Facebook's API, I discovered that one of the variables, `Post ID`, contained the wrong data and needed to be turned into the format required to query the API).


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
With that out of the way, I could then use ```Rfacebook``` to get updated data for all of the posts. 

```{r}
#retrieve  posts and add possibly() to ignore warnings from the API when posts are inaccessible - for example, some of the targeted ads were events

pages_targeted_ads <- map(targeted_ads_cleaned$query_id, 
                        possibly(getPost, otherwise = data.frame()),
                        token = fb_oauth, reactions = TRUE)


pages_targeted_ads <- do.call(rbind_list, pages_targeted_ads)
```





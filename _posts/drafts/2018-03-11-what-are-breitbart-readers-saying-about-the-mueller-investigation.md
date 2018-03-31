---
layout: post
published: true
title: 'What are Breitbart readers saying about the Mueller investigation? '
date: '2018-03-31'
---
![breitbart.jpeg]({{site.baseurl}}/img/breitbart.jpeg)

The polarized response to the Nunes memo, Russia indictments, and other Mueller bombshells are a good illustration of the fragmented media landscape in the United States. More and more, we see that right-wing media outlets are retreating wilfully into their own bubbles: from the Russia scandal to Roy Moore’s sexual assaults, these organizations have sought to dismiss and distort. 

What do readers of these media think though? To get a better idea about what this ecosystem looks like up close, I gathered the 106 most recent Breitbart News articles related to the Mueller investigation and scraped over 300,000 comments. Which articles elicited the most engagement, and what are people saying about them? How long, on average, did it take people to comment on an article about Mueller? And could the comments section, like social media, be full of trolls and so-called “cyber troops” trying to shape the narrative in Trump’s favor?  
  

(_**Techincal note:** I used R for all of my analysis, including the scraping of links for articles, and then used the Digital Methods Initiative's [Discus Comment Scraper](https://wiki.digitalmethods.net/Dmi/ToolDatabase) to scrape the comments since doing so with RSelenium was a bit time-consuming. I've limited the R code in this blog post, but I plan to add an updated R Markdown document later._) 



# **Bad news for Trump and his family riles up Breitbart's readers**

![top_breitbart_articles.png]({{site.baseurl}}/img/top_breitbart_articles.png)


The Breitbart articles with the most comments are those about Mueller's investigation honing in on Trump Jr. and the Trump family, as well as the recent indictments of Russians for interference in the 2016 elections, and the potential for impeachment. Laura Ingraham manages to make it into two of the top articles, with one article attempting to cast aspersions on the Mueller investigation and the other attempting to divert attention to Hillary Clinton, John Kerry, and former President Obama. 

Clinton and Obama are the object of many articles on Breitbart News, so perhaps it's unsurprising that they are also a fixation for many readers as well.    

   
   
# **Breitbart readers can't stop talking about Hillary, Obama, and Comey**
Using [Julia Silge](https://twitter.com/juliasilge) and [David Robinson](https://twitter.com/drob)'s brilliant R package, [tidytext](https://github.com/juliasilge/tidytext), we can break readers' comments up into individual words, remove common words (or so-called "stop words," such as "the," "and," etc.) and count the frequency of their occurence. This gives us an idea about the main themes found in readers' comments. 

![tidy_comments.png]({{site.baseurl}}/img/tidy_comments.png)


Hillary, Obama, and Comey are some of the most frequently occuring words, but there are other interesting words here as well: "fire" (as in "fire Mueller"), "sessions" (Jeff), "collusion," and "uranium." How often are readers talking about Hillary, Obama, and Comey? 

![clinton_comey_obama.png]({{site.baseurl}}/img/clinton_comey_obama.png)

We see that the number of comments mentioning all three of them has stayed pretty consistent, and peaked in December 2017. What about "uranium" - how often are readers mentioning this word and why? 

![uranium_comments.png]({{site.baseurl}}/img/uranium_comments.png)


We can see that the number of comments mentioning 'uranium' peaked in October 2017. But what are these comments about? We can take a look with a sample.

````r
comments %>%
  filter(str_detect(post_raw_message, "uranium")) %>%
  select(post_raw_message)
  
  
  post_raw_message
<chr>
1 "Show me the man, and I'll show you the crime" is how they do it in RUSSIA, comrade. Ask Stalin. It is the very definition of a Witch Hunt!The only crimes have been perpetrated by Mueller and his Coup Company, such as his hand-delivering American uranium to RUSSIA for Hillary!
2 Hillary sold nothing.  American Uranium never leaves America. Russia doesn\xe4\xf3\xbbt need uranium from America, they and we both get it from Eastern European countries where 90% of it is mined.Opposition reasearch is done by all campaigns and is hardly illegal.Now who\xe4\xf3\xbbs the buffoon, buffoon?     
3 Nothing will happen to Trump....no collusion there.... Mueller is knee deep in uranium one and corrupt as hell
4 The con man is Mueller, he has exceeded his bounds, and wrongfully entrapped Flynn. He also blocked the Russian bribery investigation and covered up the uranium sale. Smashing into Manafort's home in the middle of the night was classy. That was for things that happened when Manafort was tied to the Podestas
5 The link was included previously, but you can Google it for yourself.  As for AG Sessions, I see in this morning's news that the DO is opening investigations into FISA abuse and into Hillary Clinton's use of a personal server for government business, with the focus being on whether she intended to circumvent the law by having this undocumented server.  As for di Genova, the evidence he discusses is in the public domain.  The FISA document is readily available, as is the statements from Admiral Rogers describing the FISA audit, the statement from Director Comey that the dossier is salacious and uncorroborated, the fact that Hillary Clinton green-lighted the sale of 20% of US uranium to the Russians (through a company where her brother Tony was a board member, BTW), the subsequent $500,000 speaking fee to husband Bill and the $145 M donation to the Clinton Foundation (see "Clinton Cash" by Peter Schweitzer, which is heavily annotated and documented)...the list goes on.   di Genova cites facts in evidence and makes conclusions based on them.  If you dispute his conclusions, that is your right, but dismissing the facts because they are repeated by a "partisan" -- (and what do you base that conclusion on?) is disengenuous.

````

Comments seem to be largely about allegations of a deal between Clinton and Russia in which Clinton traded US stakes in uranium for donations. This story has been pretty well covered and debunked by credible media (and, FWIW, [Snopes](https://www.snopes.com/fact-check/hillary-clinton-uranium-russia-deal/). We can also use the ````gtrendsR```` package to grab data from the last 12 months of Google Trends to see when people were searching for the term 'uranium.' 


![uranium_trends.png]({{site.baseurl}}/img/uranium_trends.png)

We see a similar peak in October. Although we don't know the intent behind Google searches, we know that this is the month when _the Hill_ released a new report about the Clinton-uranium allegations, so the uptick in searches (and comments on Breitbart) are likely a result of media reporting. 

# **How long did it take Breitbart readers to comment on Mueller-related articles?**

Each comment is associated with a ````post_created_at```` variable, which looks like ````"2018-03-02 19:54:36 PST"```` and by looking at the difference between this time and when the article was posted (```thread_created_at```), we can figure out how long it took a reader to comment on an article after it was posted. 

I was inititally interested in this question because I wondered if trolls would quickly comment on articles about the Mueller investigation, in some effort to shape the narrative in Trump's favor. In retrospect, this probably wasn't the best question to ask since trolls commenting on Breitbart articles would be preaching to the converted anyway (so why would they waste the time and effort).

![comment_hist.png]({{site.baseurl}}/img/comment_hist.png)

Since I was analyzing the ```post_created_at``` variable, I also looked at the number of comments for each hour and found that most Breitbart readers comment between 2000-0300 (PST). 

![commen_times.png]({{site.baseurl}}/img/commen_times.png)



# **What are the most frequent word pairings in Breitbart reader comments?**
Using the tidytext package again, we can also break comments up into the most common word pairs, or bigrams, and calculate the frequency of their occurence. There are some familiar bigrams here, such as "fake news" and "witch hunt," but also the more concerning "fire mueller." 

![breitbart_bigrams.png]({{site.baseurl}}/img/breitbart_bigrams.png)



# **What links do Breitbart readers share in their comments?**
We can also mine reader comments to find which other media they are referencing. Most links shared in reader comments are to YouTube videos - by a long stretch. I didn't have the time to go through every single video, but many of them are to far-right "media" and other channels dabbling in "Deep State" conspiracy theories, such as the [The Next News Network](https://www.youtube.com/watch?v=IolHk-Inf5k) and the [American Intelligence Media]( https://m.youtube.com/watch?v=469eKvVUovQ ). Next on the list is the far-right fringe website, The Gateway Pundit. 

![breitbart_domains.png]({{site.baseurl}}/img/breitbart_domains.png)

  
# **Conclusion**
I wish I could say I was surprised by some of these findings. But given that Breitbart News peddles in conspiracy theories, intentional lies, and paranoia, I suppose all this is pretty predictable. 


(_**Methodology**: 
I retrieved Breitbart News articles related to Robert Mueller's investigation by searching the site for "Mueller," which yielded 80 articles. I also scraped the website for articles that had the "Robert Mueller" tag. In total, I retrieved 106 articles related to the Mueller investigation._)

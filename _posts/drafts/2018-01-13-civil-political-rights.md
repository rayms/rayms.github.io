---
layout: post
published: false
title: >-
  Which civil and political rights do countries violate? Analyzing 40 years of
  treaty complaints
date: '2018-01-13'
---

![27698596140_58e569da23_z.jpg]({{site.baseurl}}/img/27698596140_58e569da23_z.jpg)

In my last blog post, I took a look at how election observers write about elections when reporting their findings. I also mentioned how election observers usually reference international treaties in their reports and assess how  countries fulfill their treaty obligations, for example, by ensuring that all citizens have the right to vote and stand in elections; freedom of association and expression during the campaign; the right to a fair trial and equality before the law; and more. The [International Covenant for Civil and Political Rights](http://www.ohchr.org/en/professionalinterest/pages/ccpr.aspx) (ICCPR) governs all of these rights, and 169 countries have ratified the treaty. 

Interestingly, the ICCPR also has what is called an "Optional Protcol" (you can read more about what an Optional Protocol is [here](http://www.un.org/womenwatch/daw/cedaw/protocol/whatis.htm). The ICCPR's [Optional Protocol](http://www.ohchr.org/EN/ProfessionalInterest/Pages/OPCCPR1.aspx) is itself a treaty, and countries can opt to ratify it, thereby allowing individuals to complain to the treaty's expert body - [the Human Rights Committee](http://www.ohchr.org/EN/HRBodies/CCPR/Pages/CCPRIndex.aspx) (HRC) - if they believe their rights have been violated by the state in question. Eight other treaties have a similar ["complaint mechanism"](http://www.ohchr.org/EN/HRBodies/TBPetitions/Pages/HRTBPetitions.aspx), which is usually monitored by the respective treaty body.  

But how often are complaints made to these expert treaty bodies? What kind of rights are being violated, and which countries are the main offenders? And how do these bodies rule on the cases brought before them? To explore these questions, I started with the ICCPR, since it's the treaty I am most familiar with in my field of work (elections), and I was interested to see if the rights that election observers often tout are also reflected in cases brought to the HRC.  


## Getting the data
Luckily, the [Centre for Civil and Political Rights](http://ccprcentre.org), an NGO based in Geneva, has an excellent [database](http://ccprcentre.org/database-decisions/) of case-law and briefs for complaints against countries party to the ICCPR's Optional Protcol. This database contains has table that has the title of the case (e.g., "Christopher Alger v. Australia 
CCPR/C/120/D/2237/2013"), relevant articles of the treaty (eg., "Articles 17, 18"), keywords associated with the case (e.g. "Fair trial," "Privacy," etc.), the date, and the treaty body's ruling, or outcome of the case ("Merits - violation," "Merits - no violation", etc). 

On the database page, I identified the table with all the information I needed and used ````rvest```` to get scraping. (I adapted this code from maellsalom's post about scraping the Guardian, which is also worth a read!). 

````r
library(rvest)
library(stringr)
library(tidyr)
library(robotstxt)
library(tidyverse)
library(purrr)

#function to get tables
get_table <- function(node) {
    html_nodes(node, '#decisions_table') %>%
    html_table(fill = TRUE)
}

#function to get links from each page of the database and get table
get_tables_from_page <- function(page_number) {
  Sys.sleep(10)
  link <- paste0("http://ccprcentre.org/database-decisions?page_num=", page_number)
  url <- read_html(link)
  get_table(url)
}

#create a list of 12 tables 
ccpr_decision_tables <- purrr::map(1:12, get_tables_from_page)

#use map_df to create a dataframe of all tables
decisions <- map_df(ccpr_decision_tables, ~as.data.frame(.x))
````

I now had a dataframe with 1762 observations and 6 variables. Next, I renamed the columns and cleaned up some of the dataframe.

````r
#add new column names
decisions <- decisions %>% 
  select(Title = Title.1, Articles = Relevant.Articles, Keywords, Outcome, Date)
  
#delete rows where the table head was duplicated
decisions <- decisions[!(decisions$Title == "Title"), ]

````

I knew that I wanted to add a Country column, and to do so I would need to extract the country names from the Title column. I noticed that each Title had a specific format, like so: 

````r
[1] "Zinaida Shumilina et al. v. Belarus   \r\n                    CCPR/C/120/D/2142/2012"
[2] "K.E.R. v. Canada                      \r\n                    CCPR/C/120/D/2196/2012"          [3] "Lounis Khelifati v. Algeria           \r\n                    CCPR/C/120/D/2267/2013"        
[4] "Hibaq Said Hash v. Denmark            \r\n                    CCPR/C/120/D/2470/2014"         
[5] "Anton Batanov v. Russian Federation   \r\n                    CCPR/C/120/D/2532/2015"
[6] "S. Z. v. Denmark                      \r\n                    CCPR/C/120/D/2625/2015"    
````
Countries were always found after the "v." and before the carriage return (\r) and new line (\n). I had planned to use ````stringr```` to extract all of the country names based on a pattern, but unfortunately, my knowledge of regex wasn't quite up to stuff. After several hours of trying and nearly succeeding and then poring through Stack Overflow, I finally submitted a question, and someone helped me out. I love Stack Overflow. 

````
#create Country column by using regex on Title column, extracting country names
decisions$Country <- trimws(str_match(decisions$Title, "\\bv(?:s?\\.|:)?\\s*(.*)")[,2])
````
Unfortunately, when I used this regex, it still didn't catch all of the countries. First, there were a number of countries that also had "(xxth session)" in the title. I had to identify and replace all of these. In other cases, I managed to extract the country name, as well as other text ("van der Hoot v. the Netherlands"), so I had to identify these rows and manually change them. There were also other quirks that weren't caught by the regular expresssion. All of it was a long, iterative process, and in the end, I still had to identify all of the unique problems and simply recode the values. This is how it went: 

````r
#fix all countries where the (xxth session) bit was also added
#Denmark
decisions$Country <- str_replace_all(decisions$Country, "Denmark\\s*\\([:alnum:]+\\ssession\\)", "Denmark")

#Jamaica
decisions$Country <- str_replace_all(decisions$Country, "Jamaica\\s*\\([:alnum:]+\\ssession\\)", "Jamaica")

#France
decisions$Country <- str_replace_all(decisions$Country, "France\\s*\\([:alnum:]+\\ssession\\)", "France")

#Spain
decisions$Country <- str_replace_all(decisions$Country, "Spain\\s*\\([:alnum:]+\\ssession\\)", "Spain")

#Uruguay
decisions$Country <- str_replace_all(decisions$Country, "Uruguay\\s*\\([:alnum:]+\\ssession\\)", "Uruguay")

#Canada
decisions$Country <- str_replace_all(decisions$Country, "Canada\\s*\\([:alnum:]+\\ssession\\)", "Canada")

#Belarus
decisions$Country <- str_replace_all(decisions$Country, "Belarus\\s*\\([:alnum:]+\\ssession\\)", "Belarus")

#Trinidad and Tobago
decisions$Country <- str_replace_all(decisions$Country, "Trinidad and Tobago\\s*\\([:alnum:]+\\ssession\\)", "Trinidad and Tobago")

#identify countries that had NAs for some reason or another
which(is.na(decisions$Country))

#fix them all
decisions$Country[1227] <- "Netherlands"
decisions$Country[1294] <- "Netherlands"
decisions$Country[1352] <- "Netherlands"
decisions$Country[1459] <- "Netherlands"
decisions$Country[1501] <- "Netherlands"
decisions$Country[1373] <- "Netherlands"
decisions$Country[1574] <- "Netherlands"
decisions$Country[1644] <- "Netherlands"
decisions$Country[542] <- "Czech Republic"
decisions$Country[1761] <- "Uruguay"
decisions$Country[1742] <- "Uruguay"
decisions$Country[1275] <- "Australia"
decisions$Country[1668] <- "Sweden"
decisions$Country[1521] <- "Zaire"

#replace 'the' with no text, in instances where the countries were written as, e.g., "the Netherlands," or "the Russian Federation," etc. 
decisions$Country <- str_replace_all(decisions$Country, "^[tT]he", "")


#nearly pull my hair out of my head and give up, then recode all of these different instances where I don't think any combination of a regex would've helepd catch them all
decisions$Country <- decisions$Country %>%
  recode("Libyan Arab Jamahiriya" = "Libya",
         "s Uruguay" = "Uruguay",
         ". Norway" = "Norway",
         ". Canada" = "Canada",
         "Sri lanka" = "Sri Lanka",
         "Jamaica (304/1988)" = "Jamaica",
         "R?publique d?mocratique du Congo" = "Democratic Republic of the Congo",
         "Democratic Republic of Congo" = "Democratic Republic of the Congo",
         "Bosnia Herzegovina" = "Bosnia and Herzegovina",
         "Bolivarian Republic of Venezuela" = "Bolivia",
         "Belgique" = "Belgium",
         "Republic of Guyana" = "Guyana",
         "Rep. of Korea" = "Republic of Korea",
         "Russian Federation (2016)" = "Russian Federation",
         "Canada (2)" = "Canada",
         "s Poland" = "Poland",
         ".Greece" = "Greece",
         "Guinea Ecuatorial" = "Equatorial Guinea")
         

#convert Date column to POSIXct
decisions$Date <- as.POSIXct(decisions$Date, format = "%Y-%m-%d")

````
I finally had a dataframe I could work with: 

````r
Observations: 1,762
Variables: 6
$ Title    <chr> "Zinaida Shumilina et al. v. Belarus                    \r\n                  ...
$ Articles <chr> "Article 19,Article 21", "Article 14,Article 17,Article 18,Article 19,Article ...
$ Keywords <chr> "Freedom of assembly,Freedom of expression", "Arbitrary arrest,Fair trial,Free...
$ Outcome  <chr> "Merits - violation", "Inadmissible", "Merits - violation", "Merits - violatio...
$ Date     <dttm> 2017-07-28, 2017-07-28, 2017-07-28, 2017-07-28, 2017-07-28, 2017-07-28, 2017-...
$ Country  <chr> "Belarus", "Canada", "Algeria", "Denmark", "Russian Federation", "Denmark", "K...
````


<br/>


## What are the most frequent rulings by the Human Rights Committee?
I started my analysis by looking at how the Human Rights Committee has ruled on complaints. 


````r
decisions %>%
  count(Outcome, sort = TRUE)

# A tibble: 13 x 2
                                                           Outcome     n
                                                             <chr> <int>
 1                                              Merits - violation   766
 2                                                    Inadmissible   656
 3                                           Merits - No Violation   145
 4                                                                    95
 5                                                  Discontinuance    31
 6                                                      Violations    31
 7                        Merits - violation, Partially Admissible    23
 8                                            Partially Admissible     6
 9                     Merits - No Violation, Partially Admissible     3
10 Merits - violation, Merits - No Violation, Partially Admissible     3
11                             Inadmissible, Merits - No Violation     1
12                                  Merits - violation, Violations     1
13                                Partially Admissible, Violations     1
````
This was interesting! There are almost as many _Inadmissible_ cases as there are cases with violations. But there are also a lot of categories that make this a little confusing. To clear things up, I e-mailed the CCPR Centre to ask about their coding of the cases. I won't go into too much detail, but "merits" just means that "all procedural conditions are fulfilled," violations or violations is clear, and "partially admissible" is where "one part of the case is declared inadmissible, which means the merits of that part of the case will not be examined. Other parts of the case, however, are admissible and the Committee will adopt a view for those parts. This is why the term 'partially admissible' is combined with 'violation' or 'no violation'."

For this analysis, I decided to simplify things and recoded the outcomes into the following groups, _Merits - violations, Merits - no violations, Inadmissible,_ and _Discontinued_.

````r
decisions$Outcome <- decisions$Outcome %>%
  recode("Merits - violation, Partially Admissible" = "Merits - violation",
         "Violations" = "Merits - violation",
         "Merits - No Violation, Partially Admissible" = "Merits - No Violation",
         "Merits - violation, Violations" = "Merits - violation",
         "Partially Admissible, Violations" = "Merits - violation",
         "Inadmissible, Merits - No Violation" = "Merits - No Violation")

#code blank observations as 'no decision'
decisions$Outcome <- sub("^$", "No decision", decisions$Outcome)

#convert observations to lower case
decisions$Outcome <- str_to_lower(decisions$Outcome)

#plot case outcomes by percentage
decisions %>%
  group_by(Outcome) %>%
  filter(!Outcome %in% c("partially admissible", "merits - violation, merits - no violation, partially admissible")) %>%
  summarize(case_outcomes = n(),
            pct_outcomes = case_outcomes / 1762) %>%
  ggplot(aes(x = reorder(Outcome, pct_outcomes), pct_outcomes, fill = Outcome)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_percent() +
  scale_fill_brewer(palette = "Paired") +
  theme_ipsum_rc(plot_title_size = 15, subtitle_size = 11) +
  labs(x = NULL, 
       y = "% of cases with outcome",
       title = "Inadmissible complaints before the Human Rights Committee\nmake up 37% of all cases",
       subtitle = "1,762 individual communications between 1976-2017",
       caption = "Source: Centre for Civil and Political Rights Database") +
  coord_flip()

````
![complaints_by_outcome.jpeg]({{site.baseurl}}/img/complaints_by_outcome.jpeg)

What exactly is happening with the complaints process that _inadmissible_ decisions are so high? We can't answer this question with the data we have here. But you can [read more](http://www.ohchr.org/EN/HRBodies/TBPetitions/Pages/IndividualCommunications.aspx#theadmissibility) about the admissibility of complaints and the many factors that committees must consider when making this decision. 


## Which countries are often accused of violating civil and political rights?
![countries_accused.jpeg]({{site.baseurl}}/img/countries_accused.jpeg)

What we're looking at here is the total number of times the country has been a defendant in a complaint. I find these results quite surprising. Some of the most advanced democracies in the world are in the top ten, alongside countries better known for their authoritarian tendencies. There could be many reasons for these findings though, such as strong advocates of rights who act as third parties and bring individual cases to the HRC; robust institutions like national human rights commissions, which also help to lodge such complaints; and, indeed, the simple fact that some countries here frequenty _do_ violate rights and citizens exhaust all other domestic remedies. 



## How have the number of complaints progressed over the years?
![complaints_by_year.jpeg]({{site.baseurl}}/img/complaints_by_year.jpeg)


_Photo: "Human Right Council - 32nd Session," UN Photo / Jean-Marc Ferré  (CC BY-NC-ND 2.0)._
---
layout: post
published: false
title: >-
  Which civil and political rights do countries violate? Analzying 40 years of
  treaty complaints
date: '2018-01-13'
---




In that post, I mentioned that election observers usually reference international treaties to assess the quality of elections, assessing how  countries fulfill their treaty obligations, for example, by ensuring that all citizens have the right to vote and stand in elections; freedom of association and expression; the right to a fair trial and equality before the law; and more. The International Covenant for Civil and Political Rights governs all of these rights.

And interestingly, the ICCPR also has what is called an "Optional Protcol" (you can read more about what an Optional Protocol is [here](http://www.un.org/womenwatch/daw/cedaw/protocol/whatis.htm). The ICCPR's [Optional Protocol](http://www.ohchr.org/EN/ProfessionalInterest/Pages/OPCCPR1.aspx) is itself a treaty, and countries can opt to ratify it, thereby allowing individuals to complain to the treaty's expert body - the Human Rights Committee - if they believe their rights have been violated by the state in question. Eight other treaties have a similar ["complaint mechanism"](http://www.ohchr.org/EN/HRBodies/TBPetitions/Pages/HRTBPetitions.aspx). 

So how often are complaints made to these expert treaty bodies? What kind of rights are being violated, and which countries are the main offenders? How do these bodies rule on the cases brought before them? To explore these questions, I started with the ICCPR, since it's the treaty I am most familiar with in my field of work (elections), and I was interested to see if the rights that election observers often tout are also reflected in cases  

# Getting the data
Luckily, the Centre for Civil and Political Rights, an NGO based in Brussels, has an excellent database of case-law and briefs for complaints against countries party the ICCPR's Optional Protcol. This database contains a large table that has the title of the case (e.g., ..), relevant articles of the treaty (eg., Articles 1, 6), keywords associated with the case (e.g. "Fair hearing," "Privacy," etc.), the date, and the treaty body's ruling, or outcome of the case ("Merits - violation," "Merits - no violation", etc). 

````
#functions to scrape pages 1-12 of the data, which is contained in a tidy table called 'decisions_table'
get_table <- function(node) {
  html_nodes(node, "#decisions_table") %>%
  html_table(fill = TRUE)
}

get_page <- function(page_number){
  Sys.sleep(10)
  link <- paste0("http://ccprcentre.org/database-decisions?page_num=", page_number)
  page <- read_html(link)
  get_table(page)
}

ccpr_decisions <- purrr::map(1:12, get_page) 

#use map_df to convert the results into a dataframe
decisions <- purrr::map_df(ccpr_decisions, ~ data.frame(.x))
````
This turned out to be my first major wrangling project. I needed to extract the country names from the 'Title' column and create a new 'Country' column. My regex wasn't quite up to the task, and after trying out various combinations for several hours and poring over Stack Overflow questions, I finally submitted my won question . The answer came within minutes, for which I am eternally grateful. 

Unfortunately, when I used this regex, it still didn't catch all of the countries. 

I had a lot of countries for which there were stray data

So let's start with a few basic things, such as what the count of rulings are for complaints, and which countries have the most complainants.

````
#case outcomes
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
This is interesting! There are almost as many 'Inadmissible' outcomes as there are cases with violations. But there are also a lot of categories that make this a little confusing. To clear things up, I decided to e-mail the CCPR Centre to ask about their coding of the cases. 

Explanation here.

For this analysis, I decided to simplify things and grouped all cases with violations into one group, 'Merits - violations', 'Merits - no violations', 'Inadmissible', and 'Discontinued', which is also what the CCPR told me would be their future classifying. 
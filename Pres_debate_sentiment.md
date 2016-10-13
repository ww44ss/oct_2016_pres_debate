# SENTIMENT OF OCT 2016 PRES DEBATE SPEECH
WW44SS  
Oct 10, 2016  


###SUMMARY
Can we learn anything about a debate and it's outcome from a "sentiment" analysis? I adapt methods recently hightlight by David Robinson and Julia Silge to create a kind of "movie" version of the sentiment analysis, with text of especially "strong" sentiment highlighted.

###DATA SOURCES AND METHODS
The text of the debate are downloaded from the [UCSB Presidency Project](http://www.presidency.ucsb.edu/debates.php). Transcripts were pasted into Apple Pages and stored as unformatted .txt files.  

The .txt files are made tidy by a separate `.R` program which removes punctuation and annotation and then categorized by speaker. The data are stored as a .csv file which is loaded here for analysis. 

The menthods are similar to a [previous post on the VP debates](http://rpubs.com/ww44ss/vp_debate), so I have suppressed most of the code from this printout.


```r
## this is an identity helper-function useful for simplifying debug of piped analysis steps
## it does "nothing", but does so as a function.
yo <-function(x){return(x)}
```


```r
    library(dplyr)
    library(animation)
    library(ggplot2)
    library(tidytext)
```





```r
## search for and read lightly cleaned debate text .csv
file_of_data <- list_of_files[grepl("_tidy", list_of_files)]
debate_text <- read.csv(paste0(directory, file_of_data), stringsAsFactors = FALSE) %>% 
    as_data_frame
```

We now begin processing by taking the text, unnesting the sentences, and removing stop words using the onix lexicon


```r
    ## compute stop_words_list
    list_of_stop_words <- stop_words %>%
        filter(lexicon == "snowball") %>% 
        select(word) %>% 
        yo

    ## create tidy df of debate words
    words_from_the_debate <- debate_text %>%
        unnest_tokens(word, text) %>%
        filter(!word %in% list_of_stop_words) %>% 
        yo
```

We create a "sentiment dictionary" from the information stored in the `tidytext` package and use a `left_join` to assicate words with the sentiment values.



To look at the trend of the sentiment, we can create an exponentially damped cummulative sum function to apply to the data. The idea is that words have immediate punch, but it wanes as time and words pass. So that's the idea...



We can now compute the sum of the sentiment and the cummulative sum.


```r
    ## compute sentiment of debate responses by regrouping and compute means and cumsums
    debate_sentiment <- debate_words_sentiments %>%
        group_by(X, name) %>%
        summarize(sentiment = sum(sentiment)) %>%
        group_by(name) %>%
        #mutate(cumm_sent = cumsum(sentiment)) %>%
        mutate(cumm_sent = decay_sum(sentiment, decay_rate = 0.02)) %>%
        yo
```

The final step is to pull it all together to create a plot data frame. 




```r
## suppress announcer and audience questioner text
plot_df <- plot_df %>% filter(name == "TRUMP" | name == "CLINTON")
```


From here use the `animation` package functions to create the gif.



I had to manually add annotation. Using just "high senitment" words, while interesting, doesn't really tell a narrative. Using the full text is just too long to read in a .GIF format. So in this case I have paraphrased the text. This has the problem that it doesn't currenlty scale (If I were to annotate each line it could, but that would take more time than I have). Here are the annotations. 


```r
annote = c("i have a very positive and optimistic view - the slogan of my campaign is stronger together", 
    "i want to do things - making our inner cities better for the african american citizens that are so great", 
    "yes i m very embarrassed by it i hate it but it's locker room talk", "so this is who donald trump is - this is not who we are", 
    "all you have to do is take a look at wikileaks", "after a year long investigation there is no evidence that anyone hacked the server", 
    "i want very much to save what works and is good about the affordable care act", 
    "the affordable care act was meant to try to fill the gap", "there are children suffering in this catastrophic war", 
    "i will tell you very strongly when bernie sanders said she had bad judgment", 
    "one thing i'd do is get rid of carried interest", "i understand the tax code better than anybody that's ever run for president", 
    "i don't like assad at all but assad is killing isis", "i have generals and admirals who endorsed me", 
    "the greatest disaster trade deal in the history of the world", "tweeting happens to be a modern day form of communication", 
    "justice scalia great judge died recently and we have a vacancy", "i respect his children his children are incredibly able and devoted", 
    "i consider her statement about my children to be a very nice compliment", 
    "she doesn't quit she doesn't give up i respect that")


ann_text <- data.frame(X = 220, sentiment = 20, lab = annote, name = plot_df$name[plot_df$X %in% 
    gif_steps])
```

I added some features to the plot, so here is the new code.


```r
    saveGIF({
    for (j in 1:length(gif_steps)) {
        i <- gif_steps[j]
        
        ## color code summary text
        if (plot_df$sentiment[plot_df$X == i] < 0) {
            title_color = "darkred"
        } else {
            title_color = "darkblue"
        }
        
        title_h = 0
        
        print( 
            ## the ggplot stack
            ggplot(plot_df, aes(x = X, y = sentiment, fill = name)) +
                geom_bar(stat = 'identity', alpha = 1., width = 2) +
                geom_line(data = plot_df%>% filter(X <= i), aes(x=X, y = 15*cumm_sent/max(abs(plot_df$cumm_sent))), size = 3, color = "grey20", alpha = 0.5) +
                xlim(range(plot_df$X)) +
                ylim(range(plot_df$sentiment)) +
                xlab("index") +
                ggtitle(paste0(plot_df$name[plot_df$X == i], ": ", ann_text$lab[j])) +
                facet_grid(name~.) +
                theme(plot.title = element_text(size = 14, hjust = title_h, color = title_color, face="bold"), legend.position = "bottom") +
                geom_vline(xintercept = i, color = "grey50")
        
            ) 
        }
        }, interval = 1.2, movie.name = paste0(directory,"sentiment_animation2.gif"), ani.width = 700, ani.height = 450)
```


Here is the gif produced. The title with the paraphrased text helps follow the debate. The grey bars advance with the title, while the mapped sentiment of the whole debate is displayed as a bar chart undrlying the trend lines.   
  
Some observations: 
* Hillary's cumulative sentiment line remains positive for the entire debate.  
* Donald's aggressive attacks are evident in the middle of the debate. These are expressed with mostly negative sentiment words.


![Gif](/Users/winstonsaunders/Documents/oct_2016_pres_debate/sentiment_animation2.gif) 

For what it's worth, here is the full text of the most polarized statements


```r
knitr::kable(plot_df %>% filter(X %in% gif_steps ) %>% select(X, name, sentiment, text), caption = "Most Positive and Negative Sentiment Phrases")
```



Table: Most Positive and Negative Sentiment Phrases

   X  name       sentiment  text                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
----  --------  ----------  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   6  CLINTON           13  i have a very positive and optimistic view about what we can do together that s why the slogan of my campaign is stronger together because i think if we work together if we overcome the divisiveness that sometimes sets americans against one another and instead we make some big goals and i ve set forth some big goals getting the economy to work for everyone not just those at the top making sure that we have the best education system from preschool through college and making it affordable and so much else                                
  14  TRUMP              9  but i want to do things that haven t been done including fixing and making our inner cities better for the african american citizens that are so great and for the latinos hispanics and i look forward to doing it it s called make america great again                                                                                                                                                                                                                                                                                                    
  19  TRUMP             -9  yes i m very embarrassed by it i hate it but it s locker room talk and it s one of those things i will knock the hell out of isis we re going to defeat isis isis happened a number of years ago in a vacuum that was left because of bad judgment and i will tell you i will take care of isis                                                                                                                                                                                                                                                             
  38  CLINTON           13  so this is who donald trump is and the question for us the question our country must answer is that this is not who we are that s why to go back to your question i want to send a message we all should to every boy and girl and indeed to the entire world that america already is great but we are great because we are good and we will respect one another and we will work with one another and we will celebrate our diversity                                                                                                                      
  65  TRUMP              9  so you talk about friend go back and take a look at those commercials a race where you lost fair and square unlike the bernie sanders race where you won but not fair and square in my opinion and all you have to do is take a look at wikileaks and just see what they say about bernie sanders and see what deborah wasserman schultz had in mind because bernie sanders between super delegates and deborah wasserman schultz he never had a chance and i was so surprised to see him sign on with the devil                                            
  83  CLINTON          -11  but i think it s also important to point out where there are some misleading accusations from critics and others after a year long investigation there is no evidence that anyone hacked the server i was using and there is no evidence that anyone can point to at all anyone who says otherwise has no basis that any classified material ended up in the wrong hands                                                                                                                                                                                    
 128  CLINTON            9  so i want very much to save what works and is good about the affordable care act but we ve got to get costs down we ve got to provide additional help to small businesses so that they can afford to provide health insurance but if we repeal it as donald has proposed and start over again all of those benefits i just mentioned are lost to everybody not just people who get their health insurance on the exchange and then we would have to start all over again                                                                                    
 143  CLINTON            9  and the affordable care act was meant to try to fill the gap between people who were too poor and couldn t put together any resources to afford health care namely people on medicaid obviously medicare which is a single payer system which takes care of our elderly and does a great job doing it by the way and then all of the people who were employed but people who were working but didn t have the money to afford insurance and didn t have anybody an employer or anybody else to help them                                                    
 191  CLINTON          -11  there are children suffering in this catastrophic war largely i believe because of russian aggression and we need to do our part we by no means are carrying anywhere near the load that europe and others are but we will have vetting that is as tough as it needs to be from our professionals our intelligence experts and others                                                                                                                                                                                                                       
 207  TRUMP             -9  and i will tell you very strongly when bernie sanders said she had bad judgment she has really bad judgment because we are letting people into this country that are going to cause problems and crime like you ve never seen we re also letting drugs pour through our southern border at a record clip at a record clip and it shouldn t be allowed to happen                                                                                                                                                                                             
 229  TRUMP             10  well one thing i d do is get rid of carried interest one of the greatest provisions for people like me to be honest with you i give up a lot when i run because i knock out the tax code and she could have done this years ago by the way she s a united states she was a united states senator                                                                                                                                                                                                                                                            
 249  TRUMP             12  see i understand the tax code better than anybody that s ever run for president hillary clinton and it s extremely complex hillary clinton has friends that want all of these provisions including they want the carried interest provision which is very important to wall street people but they really want the carried interest provision which i believe hillary s leaving very interesting why she s leaving carried interest                                                                                                                         
 298  TRUMP             -9  i don t like assad at all but assad is killing isis russia is killing isis and iran is killing isis and those three have now lined up because of our weak foreign policy                                                                                                                                                                                                                                                                                                                                                                                    
 312  TRUMP              9  and we have general flynn and we have look i have generals and admirals who endorsed me i have congressional medal of honor recipients who endorsed me we talk about it all the time they understand why can t they do something secretively where they go in and they knock out the leadership how why would these people stay there i ve been reading now                                                                                                                                                                                                 
 334  TRUMP            -18  because signed by her husband is perhaps the greatest disaster trade deal in the history of the world not in this country it stripped us of manufacturing jobs we lost our jobs we lost our money we lost our plants it is a disaster and now she wants to sign even though she says now she s for it she called it the gold standard and by the way at the last debate she lied because it turned out that she did say the gold standard and she said she didn t say it they actually said that she lied and she lied but she s lied about a lot of things 
 371  TRUMP             12  now tweeting happens to be a modern day form of communication i mean you can like it or not like it i have between facebook and twitter i have almost million people it s a very effective way of communication so you can put it down but it is a very effective form of communication i m not un proud of it to be honest with you                                                                                                                                                                                                                        
 400  TRUMP              9  justice scalia great judge died recently and we have a vacancy i am looking to appoint judges very much in the mold of justice scalia i m looking for judges and i ve actually picked of them so that people would see highly respected highly thought of and actually very beautifully reviewed by just about everybody                                                                                                                                                                                                                                    
 439  CLINTON           10  well i certainly will because i think that s a very fair and important question look i respect his children his children are incredibly able and devoted and i think that says a lot about donald i don t agree with nearly anything else he says or does but i do respect that and i think that is something that as a mother and a grandmother is very important to me                                                                                                                                                                                    
 443  TRUMP             20  well i consider her statement about my children to be a very nice compliment i don t know if it was meant to be a compliment but it is a great i m very proud of my children and they ve done a wonderful job and they ve been wonderful wonderful kids so i consider that a compliment                                                                                                                                                                                                                                                                     
 444  TRUMP              3  i will say this about hillary she doesn t quit she doesn t give up i respect that i tell it like it is she s a fighter i disagree with much of what she s fighting for i do disagree with her judgment in many cases but she does fight hard and she doesn t quit and she doesn t give up and i consider that to be a very good trait                                                                                                                                                                                                                       




## DEBATE DATA CLEANER. 

    ## Rev 0.1 ensured applause, interrupt etc are moved.
    ## add tidytext package

    ## A utility program to produce cleaned data from the raw data stored on this site. 
    ## Read a text file. Filters and conditions the text. Creates a data frame with the row number, candidate name, and text.
    ## INPUT:
    ##      the raw text files downloaded from the UCSB website.
    
    ## OUTPUT:
    ##      returns a tibble data frame with name and cleaned text, in order. 
    
    file_name <- "First_Candidates_Debate.txt"
    directory <- "/Users/winstonsaunders/Documents/oct_2016_pres_debate/"
    raw_data <- read.table(paste0(directory, file_name), header=FALSE, sep="\n", stringsAsFactors = FALSE, quote = "")
    
    library(tidytext)
    library(tidyr)
    library(dplyr)
    
    ## convert to tibble
    raw_data <- raw_data %>% as_data_frame
    
    ## get rid of all but the capitalized names and creates a new column $name
    
    raw_data <- raw_data %>% mutate(V1 = gsub("OK,","okay,", V1)) %>%
        mutate(V1 = gsub("ICE","Ice", V1)) %>%
        mutate(V1 = gsub("QUESTION","AUDIENCE", V1))

    cleaner_data <- raw_data %>% mutate(name = gsub(":.+|^[A-Z][^A-Z].+|^[a-z].+", "", V1 ))
    
    ## Fill in the blank rows of $name by looking to the row above if blank
    for (i in 2: nrow(cleaner_data)){
        if (cleaner_data$name[i] == "") cleaner_data$name[i] <- cleaner_data$name[i-1]
    }
    
    
    ## get rid of junk lines
    cleaner_data <- cleaner_data %>% filter(V1 != "[crosstalk]")
    cleaner_data <- cleaner_data %>% filter(name != "PARTICIPANTS:")
    cleaner_data <- cleaner_data %>% filter(name!= "MODERATORS:")
    cleaner_data <- cleaner_data %>% filter(name!= "MODERATOR:")
    
    ## fix a few parasitic cases
    
    cleaner_data <- cleaner_data %>% mutate(clean_text = gsub("\\.\\.\\. ", "", V1))
    
    cleaner_data <- cleaner_data %>% 
        mutate(clean_text = gsub("\\.\\.\\.", "", clean_text)) %>%
        mutate(clean_text = gsub("ISIS","Isis", clean_text)) %>%
        mutate(clean_text = gsub("ICE","Ice", clean_text)) %>%
        mutate(clean_text = gsub("\"","", clean_text))
    
   
    cleaner_data <- cleaner_data %>%
        mutate(clean_text = gsub ("interrupt", "", clean_text)) %>%
        mutate(clean_text = gsub ("\\[applause\\]", "",  clean_text))%>%
        mutate(clean_text = gsub ("\\[crosstalk\\]", "",  clean_text)) %>%
        mutate(clean_text = gsub ("\\[laughter\\]", "",  clean_text)) %>%
        mutate(clean_text = gsub ("[[:punct:]]", " ", clean_text)) %>%
        mutate(clean_text = gsub ("e mail", "email",  clean_text)) %>%
        mutate(clean_text = gsub ("[0-9]", "",  clean_text)) %>%
        mutate(clean_text = gsub ("[A-Z]{2,} ", "",  clean_text)) 
    
    cleaner_data <- cleaner_data %>%
        mutate(clean_text = tolower(clean_text))
    
    cleaner_data <- cleaner_data %>%
        mutate(clean_text = gsub (" {2,}", " ", clean_text))    
        
    clean_data <- cleaner_data %>% select(name, text = clean_text)
    
    write.csv(clean_data, paste0(directory, file_name, "_tidy.csv"), row.names = TRUE)



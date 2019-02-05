library(shiny)
library(stringr)
library(data.table)
library(digest)
library(pryr)
cacheEnv <- new.env()

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, clientData, session) {

    withProgress(message = 'Loading model data...', value = 0, min=1, max=10, {
        # load data
        assign("pred", readRDS('model_small_pred.RDS'), cacheEnv)
        assign("terms", readRDS('model_small_terms.RDS'), cacheEnv)
        assign("dups", readRDS('model_small_dups.RDS'), cacheEnv)
        assign("model_size", round(object_size(
            get("pred", cacheEnv), 
            get("terms", cacheEnv), 
            get("dups", cacheEnv))/1024^2), cacheEnv)
    })    

    observe ({
        val <- input$txtInput
        in_len <- nchar(val)
        if (in_len > 2) {
            if (substr(val, in_len -1, in_len) == '  ') {
                #val <- paste(substr(val, 1, in_len -1), predict(val), ' ', sep='')
                val <- paste( sub("\\s+$", "", val) , ' ', predict(val), ' ', sep='')
                updateTextInput( session, inputId="txtInput", value=val)
            }
        }
    })
    
    output$prediction <- renderText({
        predict(input$txtInput)
    })
    
    output$mem_used <- renderText({
        paste('Total memory used:', round(mem_used()/1024^2), 'MB')
    })
    output$mem_used_model <- renderText({
        paste('Model size:', get("model_size", cacheEnv), 'MB')
    })
})


# input string
clean_input <- function(input) {
    input <- tolower(input) 
    input <- gsub("\\S*\\d+\\S*", " 0 ", input)
    input <- gsub("[^[:alnum:][:space:]']", " ", input)
    # preserve 's , 're , 'm , 've, 'll, 'd, 't - replace them to #
    preserve <- c("s", "re", "m", "ve", "ll", "d", "t")
    for (pattern in preserve) {
        search <- gsub("pattern", pattern,  "(\\w)'pattern(\\W|$)")
        replace <- gsub("pattern", pattern,  "\\1#pattern\\2")
        input <- gsub(search, replace, input)
    }
    input <- gsub("'", " ", input)
    input <- gsub("#", "'", input)
    input <- gsub("0", " <0> ", input)
    input <- gsub("\\s+", " ", input)
    input <- str_trim(input)
}

getDigestRaw <- function (v) {
    as.numeric(paste('0x', digest( v, algo='xxhash32', serialize=F, raw=T, seed=0), sep=''))
}
getDigestRaw2 <- function (v) {
    -as.numeric(paste('0x', digest( v, algo='crc', serialize=F, raw=T, seed=0), sep=''))
}


get_digest <- function (phrase) {
    dups <- get("dups", cacheEnv)
    # check dups
    d1 <-getDigestRaw(phrase) 
    is_dup <- dups[J(d1), nomatch=0]$dups
    
    if (!length(is_dup)) {
        d1
    } else {
        getDigestRaw2(phrase)
    }
}


predict <- function (orig_input, show.match = F) {
    terms <- get("terms", cacheEnv)
    pred <- get("pred", cacheEnv)
    
    # clean
    input <- clean_input(orig_input)
    # tokenize
    input_t <- strsplit(input, ' ')[[1]]
    
    # filter unknown terms
    input_t[!(input_t %in% .subset2(terms[J(input_t), mult="first", nomatch=0], 'V1'))] <- '<n>'
    
    len <- length(input_t)
    m <- NULL
    rez <- NULL
    if (len > 5) {
        rez <- pred[J(get_digest(paste(input_t[(len-5):len], collapse=' '))), mult="first", nomatch=0]$pred
        m <- 7
    }
    if (len > 4) {
        rez <- pred[J(get_digest(paste(input_t[(len-4):len], collapse=' '))), mult="first", nomatch=0]$pred
        m <- 6
    }
    if (len > 3) {
        rez <- pred[J(get_digest(paste(input_t[(len-3):len], collapse=' '))), mult="first", nomatch=0]$pred
        m <- 5
    }
    if ((len > 2) & !length(rez)) {
        rez <- pred[J(get_digest(paste(input_t[(len-2):len], collapse=' '))), mult="first", nomatch=0]$pred
        m <- 4
    }
    if ((len > 1) & !length(rez)) {
        rez <- pred[J(get_digest(paste(input_t[(len-1):len], collapse=' '))), mult="first", nomatch=0]$pred
        m <- 3
    }
    if (!length(rez)) {
        rez <- pred[J(get_digest(input_t[len])), mult="first", nomatch=0]$pred
        m <- 2
    }
    if (show.match) {
        paste(str_trim(orig_input), terms[rez]$V1, m, sep=' ')
    } else {
        terms[rez]$V1
    }
}

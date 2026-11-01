word_cloud <- function(wt_sp500) {
    
    # ---- 1. Words to remove -----------------------------------------------------
    legal <- c(
        "INC", "CORP", "CORPORATION", "CO", "COMPANY", "LTD", "LIMITED",
        "LLC", "PLC", "LP", "NV", "SA", "AG", "SE", "SPA", "NA", "DE", "THE",
        "COM", "ORD", "ADR", "SHS", "REIT", "TRUST"
    )
    
    generic <- c(
        "HOLDINGS", "HOLDING", "GROUP", "TECHNOLOGIES", "TECHNOLOGY",
        "SYSTEMS", "INTERNATIONAL", "INTL", "ENTERPRISES", "INDUSTRIES",
        "FINANCIAL", "SOLUTIONS", "PLATFORMS", "SERVICES", "WORLDWIDE",
        "GLOBAL", "BANCORP", "RESOURCES", "PARTNERS", "STORES"
    )
    
    # Manual overrides for ambiguous cases (add your own)
    overrides <- c(
        "JOHNSON & JOHNSON"  = "J&J",
        "PROCTER & GAMBLE"   = "P&G",
        "BERKSHIRE HATHAWAY" = "Berkshire",
        "JPMORGAN CHASE"     = "JPMorgan",
        "BANK OF AMERICA"    = "BofA",
        "ALPHABET"           = "Alphabet"
    )
    
    
    # ---- 2. Name-cleaning function ---------------------------------------------
    clean_name <- function(x, symbol,
                           min_len = 3, max_len = 14, short_first = 6) {
        
        s <- x %>%
            str_to_upper() %>%
            str_replace_all("\\.COM", "") %>%                           # AMAZON.COM -> AMAZON
            str_replace_all("\\b(CLASS|CL|SERIES)\\s+[A-Z]\\b", "") %>% # CLASS A, CL B
            str_replace_all("[,\\.\\(\\)\\-/']", " ") %>%
            str_squish()
        
        # Apply manual overrides to the name after removing class suffixes
        for (k in names(overrides)) {
            if (str_detect(s, paste0("^", fixed_regex(k)))) {
                return(overrides[[k]])
            }
        }
        
        w <- str_split(s, "\\s+")[[1]]
        w <- w[!w %in% c(legal, "&", "AND")]
        
        # Remove generic words, but always keep at least one word
        keep <- !w %in% generic
        if (any(keep)) w <- w[keep]
        
        if (length(w) == 0) return(symbol)
        
        # Use the first word; if it is short, append the second one
        out <- w[1]
        
        if (nchar(out) < short_first && length(w) > 1) {
            out <- paste(w[1], w[2])
        }
        
        # Formatting: keep short abbreviations uppercase,
        # otherwise use Title Case
        out <- if (!str_detect(out, " ") && nchar(out) <= 4) {
            out
        } else {
            tools::toTitleCase(str_to_lower(out))
        }
        
        out <- str_trunc(out, max_len, ellipsis = "")
        
        # Fall back to the ticker symbol if the result is too short
        if (nchar(out) < min_len) symbol else out
    }
    
    
    # Helper function to safely use override keys as regex patterns
    fixed_regex <- function(x) {
        str_replace_all(
            x,
            "([\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}\\\\&])",
            "\\\\\\1"
        )
    }
    
    
    # ---- 3. Apply the cleaning function to the data frame ----------------------
    words <- wt_sp500 |>
        mutate(
            word = map2_chr(company, symbol, clean_name),
            freq = weight * 100
        ) |>
        select(word, freq) # Word cloud requires word and freq in %
    
    # Quick check of the resulting company names
    # wt_sp500 %>%
    #     mutate(word = map2_chr(company, symbol, clean_name)) %>%
    #     select(company, word) %>%
    #     print(n = 40)
    
    # Color palette
    pal <- c(
        "#00F5D4",
        "#00BBF9",
        "#9B5DE5",
        "#F15BB5",
        "#FEE440",
        "#FF9F1C"
    )
    
    return (
        wordcloud2(
            words,
            size = 0.6,
            color = rep(pal, length.out = nrow(words)),
            backgroundColor = "#0B1020",
            minRotation = 0,
            maxRotation = 0,
            gridSize = 12
        )
    )
}
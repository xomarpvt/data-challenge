api_key <- Sys.getenv("ETHERSCAN_API_KEY")
base_url <- "https://api.etherscan.io/v2/api"


get_block_number <- function(datetime, closest = c("before", "after")) {
    closest <- match.arg(closest)
    
    request(base_url) |>
        req_url_query(
            chainid   = 1,
            module    = "block",
            action    = "getblocknobytime",
            timestamp = as.numeric(datetime),
            closest   = closest,
            apikey    = api_key
        ) |>
        req_perform() |>
        resp_body_json() |>
        pluck("result") |>
        as.numeric()
}


fetch_page <- function(contract, start_block, end_block, page, offset, max_tries = 5) {
    
    for (attempt in seq_len(max_tries)) {
        
        response <- request(base_url) |>
            req_url_query(
                chainid         = 1,
                module          = "account",
                action          = "tokentx",
                contractaddress = contract,
                startblock      = start_block,
                endblock        = end_block,
                page            = page,
                offset          = offset,
                sort            = "asc",
                apikey          = api_key
            ) |>
            req_retry(max_tries = 3) |>
            req_perform() |>
            resp_body_json(simplifyVector = TRUE)
        
        # rate limit comes back as HTTP 200 with a string result
        if (is.character(response$result) &&
            str_detect(tolower(response$result), "rate limit")) {
            message("  rate limit hit, waiting...")
            Sys.sleep(1.5 * attempt)
            next
        }
        
        return(response)
    }
    
    stop("Too many failed attempts (rate limit)")
}


get_eth_transfers <- function(
        contract,
        start_block,
        end_block,
        offset = 1000,
        max_pages = 10,
        max_seconds = 1800
) {
    
    t0 <- Sys.time()
    chunks <- list()
    current_start <- start_block
    finished <- FALSE
    
    message("SMA: ", contract)
    
    while (!finished) {
        
        pages <- list()
        reached_end <- FALSE
        
        for (page in seq_len(max_pages)) {
            
            elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
            if (elapsed > max_seconds) {
                warning("Time limit reached: data are incomplete")
                reached_end <- TRUE
                break
            }
            
            message("  from block ", current_start, " | page ", page)
            
            response <- fetch_page(contract, current_start, end_block, page, offset)
            
            # no transactions (or error): result is a string
            if (is.character(response$result)) {
                if (!str_detect(tolower(response$result), "no transactions")) {
                    warning("Unexpected response: ", response$message, " - ", response$result)
                }
                reached_end <- TRUE
                break
            }
            
            pages[[page]] <- as_tibble(response$result)
            Sys.sleep(0.5)
            
            # fewer rows than offset: last page of the window
            if (nrow(pages[[page]]) < offset) {
                reached_end <- TRUE
                break
            }
        }
        
        chunk <- bind_rows(pages)
        
        if (reached_end) {
            chunks[[length(chunks) + 1]] <- chunk
            finished <- TRUE
        } else {
            # 10k cap reached: drop the last block (may be incomplete)
            # and restart from it
            last_block <- max(as.numeric(chunk$blockNumber))
            chunk <- chunk |> filter(as.numeric(blockNumber) < last_block)
            
            if (nrow(chunk) == 0) {
                stop("A single block holds >= 10,000 transfers: case not handled")
            }
            
            chunks[[length(chunks) + 1]] <- chunk
            current_start <- last_block
            message("  10k cap reached -> restarting from block ", last_block)
        }
    }
    
    bind_rows(chunks)
}
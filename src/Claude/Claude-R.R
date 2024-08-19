library(httr)
library(jsonlite)
library(pdftools)


claudeR <- function(prompt, 
                    model = "claude-3-5-sonnet-20240620", 
                    max_tokens = 100,
                    stop_sequences = NULL,
                    temperature = .7, top_k = -1, top_p = -1,
                    api_key = NULL, 
                    system_prompt = NULL) {
    
    # Load required libraries ----
    # library(httr)
    # library(jsonlite)
    
    if (grepl("claude-3", model) && !is.list(prompt)) {
        stop("Claude-3 requires the input in a list format, e.g., list(list(role = \"user\", content = \"What is the capital of France?\"))")
    }
    
    # Check if the API key is provided or available in the environment ----
    if (is.null(api_key)) {
        api_key <- Sys.getenv("ANTHROPIC_API_KEY")
        if (api_key == "") {
            stop("Please provide an API key or set it as the ANTHROPIC_API_KEY environment variable.")
        }
    }
    
    # Set up the API request ----
    url <- "https://api.anthropic.com/v1/messages"
    
    headers <- add_headers(
        "x-api-key" = api_key,
        "anthropic-version" = "2023-06-01",
        "Content-Type" = "application/json"
    )
    
    # Prepare the messages list ----
    message_list <- lapply(prompt, function(msg) {
        list(role = msg$role, content = msg$content)
    })
    
    # Prepare the request body as a list ----
    request_body_list <- list(
        model = model, 
        max_tokens = max_tokens, 
        temperature = temperature, 
        top_k = top_k, 
        top_p = top_p, 
        messages = message_list
    )
    
    # Include the system prompt if provided ----
    if (!is.null(system_prompt)) {
        request_body_list$system = system_prompt
    }
    
    # Convert the modified list to JSON ----
    body <- toJSON(request_body_list, auto_unbox = TRUE)
    
    
    # Send the API request ----
    # config: Additional configuration settings such as:
    # - http authentication (authenticate())
    # - additional headers (add_headers())
    # - cookies (set_cookies()), etc...
    response <- POST(url = url, config = headers, body = body, encode = "json")
    
    # Check if the request was successful ----
    if (http_status(response)$category == "Success") {
        ## Parse the JSON response ----
        result <- fromJSON(content(response, "text", encoding = "UTF-8"))
        return(result$content$text)
    } else {
        ## Print the error message ----
        warning(paste("API request failed with status", http_status(response)$message))
        cat("Error details:\n", content(response, "text", encoding = "UTF-8"), "\n")
        return(NULL)
    }
}

# An example

extract_text_from_pdf <- function(pdf_path) {
    text <- pdf_text(pdf_path)
    return(paste(text, collapse = " "))
}

pdf.text <- pdf_text(
    "references/Elek et al. - 2019 - The closer the better does better access to outpa.pdf"
)


prompt.message = "
以下の論文のテキストを要約してください．
"


response <- claudeR(
    prompt = list(list(role = "user", content = prompt.message)), 
    model = "claude-3-sonnet-20240229"
)

cat(response)

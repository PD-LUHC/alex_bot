## code to prepare `history_facts` dataset
#https://github.com/Michonvv/random-history-fact/blob/main/src/facts.php
# Convert PHP facts array -> data.frame(id, fact, source) -> sysdata.rda

# (A) Fetch the PHP file from GitHub (one-time during data prep)
php_url <- "https://raw.githubusercontent.com/Michonvv/random-history-fact/main/src/facts.php"
php_txt <- readLines(php_url, warn = FALSE, encoding = "UTF-8")

txt <- paste(php_txt, collapse = "\n")

# Remove newlines in array blocks to simplify regex matching
txt <- gsub("\\s+", " ", txt)

# ---- Extract all PHP double-quoted string literals in the array ----
# We match strings of the form " ... " allowing for escaped quotes and escapes inside.
# Regex: "(?:[^"\\]|\\.)*"
m <- gregexpr("\"(?:[^\"\\\\]|\\\\.)*\"", txt, perl = TRUE)
raw_matches <- regmatches(txt, m)[[1]]

# If nothing matched, bail out early with a helpful error
if (length(raw_matches) == 0L) {
    stop("No double-quoted strings found in the PHP array. Check input format.")
}

# Strip surrounding quotes and unescape common sequences (\n, \", \\)
facts <- sub('^"(.*)"$', "\\1", raw_matches)

# Drop any empty lines that might have slipped in
facts <- facts[nzchar(facts)]

history_facts <- as.data.frame(facts)

# (D) Save as internal data (R/sysdata.rda)
usethis::use_data(history_facts, internal = TRUE, overwrite = TRUE)
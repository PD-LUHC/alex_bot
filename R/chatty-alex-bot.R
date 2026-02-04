# ---- AlexBot enhanced console dialogue ----
.welcome_message <- function(area_type = "lad", chatty = FALSE) {
    if (!chatty) {
        return(invisible(NULL))
    }
    area_terms <- c(
        lad   = "Local Authority District",
        ctyua = "Counties and Unitary Authority",
        msoa  = "Middle Layer Super Output Area",
        pcon  = "Parliamentary Constituency"
    )

    bot_name <- "AlexBot"
    bot_version <- tryCatch(
        as.character(packageVersion("AlexBot")),
        error = function(e) "dev"
    )

    # Greeting with typing + a momentary blinking cursor
    typewrite(
        paste0("Hello, I'm ", bot_name, " v", bot_version)
    )
    Sys.sleep(0.2)

    typewrite(paste("I like maps"))
    typewrite(paste("Do you like maps?"))
    Sys.sleep(0.2)

    # Prompt with a typewriter effect
    prompt <- paste0("Let's play guess the ", unname(area_terms[area_type]))
    typewrite(prompt, delay = 0.02, cursor = TRUE, ellipsis = TRUE)

    # Decorative flashing cursor at the end (optional)
    flash_cursor(times = 4, on = 0.2, off = 0.2)
}

.cat_strike <- function() {
    paste0(sample(letters, sample(4:8, 1), replace = TRUE), collapse = "")
}

.oh_gosh_cat <- function(chatty = FALSE) {
    if (chatty == TRUE) {
        typewrite(paste(.cat_strike()))
        meow_cat()
        Sys.sleep(1)
        typewrite(paste("Oh Gosh!", .cat_strike()))
        typewrite(paste("my Cat has jumped onto my keyboard!"))
        typewrite(paste(.cat_strike()))
    }
}

.history_is_great <- function(chatty = FALSE) {
    if (chatty == TRUE) {
        typewrite("History is great")
        typewrite("I like History")
    }
}
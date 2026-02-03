# ASCII cat with a rounded speech bubble (Unicode by default)
meow_cat <- function(
  say = NULL,
  sand = 180,          # 256-color 'sandy' (nice: 179, 180, 187, 215, 222)
  bold_meow = TRUE,    # bold message text
  bubble_max = 24,     # wrap width inside the bubble
  pad = 1,             # horizontal padding inside the bubble
  ascii_only = FALSE,   # TRUE = ASCII-only rounded-ish bubble
  phrases = c(         # default phrase pool (feel free to edit)
    "Meow!",
    "i can haz cheezburger?",
    "i can haz map boundry?",
    "Take that keyboard!",
    "You have made the cat angry..."
  ),
  seed = NULL          # optional seed for reproducible phrase selection
) {
  # --- Random phrase selection if say is NULL ---
  if (is.null(say)) {
    if (!is.null(seed)) set.seed(seed)
    if (length(phrases) == 0) {
        say <- "Meeow!"
    }
    say <- sample(phrases, size = 1)
  }
  # --- ANSI helpers ---
  esc   <- function(x) sprintf("\033[%sm", x)
  reset <- esc(0)
  col   <- esc(paste0("38;5;", sand))
  bold  <- if (bold_meow) esc("1") else ""

  # --- Text wrap (space-based, no hyphenation) ---
  wrap_text <- function(txt, width) {
    if (nchar(txt) <= width) return(txt)
    words <- strsplit(txt, "\\s+")[[1]]
    out <- character()
    line <- ""
    for (w in words) {
      if (nchar(line) == 0) {
        line <- w
      } else if (nchar(line) + 1 + nchar(w) <= width) {
        line <- paste(line, w)
      } else {
        out <- c(out, line)
        line <- w
      }
    }
    c(out, line)
  }

  # --- Cat ASCII ---
  cat_lines <- c(
    " /\\_/\\ ",
    "( o.o )",
    " > ^ < "
  )

  # --- Bubble characters (rounded vs ASCII fallback) ---
  if (!ascii_only) {
    # Unicode rounded bubble
    ch_top_left  <- "\U256D"
    ch_top_right <- "\U256E"
    ch_bot_left  <- "\U2570"
    ch_bot_right <- "\U256F"
    ch_h         <- "—"
    ch_v         <- "|"
    tail_piece   <- "\U2570"
    tail_bar     <- "—"
    tail_point   <- "<"
  } else {
    # ASCII approximation of rounded
    ch_top_left  <- "."
    ch_top_right <- "."
    ch_bot_left  <- "'"
    ch_bot_right <- "'"
    ch_h         <- "-"
    ch_v         <- "|"
    tail_piece   <- "`"  # or "'"
    tail_bar     <- "-"
    tail_point   <- "<"
  }

  # --- Build bubble lines ---
  msg_lines <- wrap_text(say, bubble_max)
  inner_w   <- max(nchar(msg_lines))
  pad_str   <- if (pad > 0) paste(rep(" ", pad), collapse = "") else ""
  top       <- paste0(ch_top_left, paste(rep(ch_h, inner_w + 2 * pad), collapse = ""), ch_top_right)
  bottom    <- paste0(ch_bot_left, paste(rep(ch_h, inner_w + 2 * pad), collapse = ""), ch_bot_right)
  body      <- vapply(msg_lines, function(l) {
    spaces <- paste(rep(" ", inner_w - nchar(l)), collapse = "")
    paste0(ch_v, pad_str, l, spaces, pad_str, ch_v)
  }, character(1))

  # --- Layout decisions ---
  # Bubble goes to the right of the cat’s face; tail points from bubble toward the cat mouth line.
  face_width <- nchar(cat_lines[2])
  gap        <- 2
  indent     <- paste(rep(" ", face_width + gap), collapse = "")

  # Tail will appear at the start of the first bubble body line:
  #   cat mouth + space + "<" + (gap-2 spaces) + bubble body line
  tail_marker <- paste0(tail_piece)
  tail_line   <- function(body_line) body_line

  # --- Print ---
  # Line 1: cat head
  cat(paste0(col, cat_lines[1], reset, "\n"))

  # Line 2: cat face + top of bubble
  cat(paste0(col, cat_lines[2], reset, "    ", top, "\n"))

  # Line 3: cat mouth + tail + first body line
  # Put a small tail composed of the corner piece + bars + point "<"
  # Example (Unicode):  " ╰─<"
  tail_str <- paste0(" ", tail_marker, tail_bar, tail_point)
  # Remaining gap spaces between tail and bubble (if any)
  extra_gap <- max(gap - nchar(tail_str), 0)
  cat(paste0(
    col, cat_lines[3], reset,
    tail_str,
    paste(rep(" ", extra_gap), collapse = ""),
    esc(1) ,body[1], reset, "\n" # esc(1) and reset make the speech text bold
  ))

  # Additional body lines (if any)
  if (length(body) > 1) {
    for (i in 2:length(body)) {
      cat(paste0(indent, "  ", esc(1), body[i], reset, "\n"))
    }
  }

  # Bottom of bubble
  cat(paste0(indent, "  ", bottom, "\n"))
}

# Examples:
# meow_cat()                                           # default rounded bubble
# meow_cat("Mrrrow~ so cozy here under the sun!")      # longer message wraps
# meow_cat("Purr... snacks please?", sand = 222)       # warmer sandy tone
# meow_cat("ASCII-only bubble", ascii_only = TRUE)     # fallback if Unicode looks off
# meow_cat("Tiny pad", pad = 0)                        # no inner padding
# meow_cat("Narrow wrap width to demo wrapping.", bubble_max = 10)

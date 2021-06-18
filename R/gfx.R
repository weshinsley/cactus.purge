#' @importFrom magrittr %<>%

################################################################################
# GFX - support for "graphics", by which I mean cursor and colour control,     #
# and some exhilarating special text effects.                                  #
################################################################################

SCREEN_MARGIN_X <- 3
SCREEN_MARGIN_Y <- 1

FADE_IN <- 1
FADE_OUT <- 2
FADE_IN_OUT <- 3

LEFT <- 1
CENTRE <- 2
RIGHT <- 3

UNICORN <- c(16:21, 27, 33, 39, 45, 51, 87, 123, 159, 195)
GREY_SCALE <- 232:255

################################################################################
# Fade in/out/both of  1 or 3 bits of left/right/centre aligned text. If triple,
# then brightest in the middle, half brightness above and below.

fade_text <- function(G, x, y, text, cols, fade_mode,
                      delay = 0, triple = FALSE, align = CENTRE,
                      fade_speed = 20) {


  x <- max(0, min(G$tv_width, dplyr::case_when(
    align == CENTRE ~ x - round(nchar(text) / 2),
    align == RIGHT ~ x - nchar(text),
    TRUE ~ x)))

  col_span <- length(cols)
  if (fade_mode != FADE_OUT) {
    next_frame <- waitr::waitr_timestamp() + fade_speed
    for (i in seq_along(cols)) {
      G$cursor %<>% write_at(x, y, text, cols[i])
      if (triple) {
        G$cursor %<>% write_at(x, y - 1, text, cols[1 + floor(i / 2)])
        G$cursor %<>% write_at(x, y + 1, text, cols[1 + floor(i / 2)])
      }
      next_frame <- waitr::waitr_until(next_frame + fade_speed)
    }
  }

  if (fade_mode == FADE_IN) {
    return(G)
  }
  waitr::waitr_delay(delay)
  next_frame <- waitr::waitr_timestamp() + fade_speed
  for (i in rev(seq_along(cols))) {
    G$cursor %<>% write_at(x, y, text, cols[i])
    if (triple) {
      G$cursor %<>% write_at(x, y - 1, text, cols[1 + floor(i / 2)])
      G$cursor %<>% write_at(x, y + 1, text, cols[1 + floor(i / 2)])
    }
    next_frame <- waitr::waitr_until(next_frame + fade_speed)
  }

  # Erase afterwards...

  text <- paste(rep(" ", nchar(crayon::strip_style(text))), collapse = "")
  G$cursor %<>% write_at(x, y, text)
  if (triple) {
    G$cursor %<>% write_at(x, y - 1, text)
    G$cursor %<>% write_at(x, y + 1, text)
  }
  G
}

################################################################################
# Do a snazzy cascading unicorn fade in/out on 8 hi-scores...
# Assumes a black screen

snazzy_scores <- function(G, hs_file, mode) {
  hs <- load_hiscores(hs_file)
  longest_name <- floor(max(nchar(hs$name))/2)
  range <- 1:15
  if (mode != FADE_IN) range <- 15:1
  next_frame <- waitr::waitr_timestamp() + 100
  for (top_col in range) {
    for (col in top_col:max(1, top_col - 7)) {
      line <- (top_col + 1) - col
      sc <- stringr::str_pad(hs$score[line], 10)
      sc <- gsub("  ", ". ", sc)
      s <- paste0(stringr::str_pad(hs$name[line], 10), " . . . . . . . . ", sc)
      set_colour(UNICORN[col])
      G$cursor %<>% write_at(12 - longest_name, 1 + (2* line), s)
    }
    next_frame <- waitr::waitr_until(next_frame + 100)
  }
  G
}

################################################################################
# Show an ANSI picture with lines appearing in random order

show_pic <- function(G, file, pattern = "random") {
  suppressWarnings(txt <- readLines(file, encoding = "UTF-8"))
  if (pattern == "random") {
    row_order <- sample(seq_along(txt))
  } else if (pattern == "down") {
    row_order <- seq_along(txt)
  } else if (pattern == "up") {
    row_order <- rev(seq_along(txt))
  }
  next_frame <- waitr::waitr_timestamp() + 50
  for (line in row_order) {
    x <- txt[line]
    G$cursor %<>% pos_at(0, line - 1)
    G$cursor %<>% write(txt[line])
    next_frame <- waitr::waitr_until(next_frame + 50)
  }
  G
}

#' @importFrom magrittr %>%
NULL
clear_pic <- function(G, y) {
  next_frame <- waitr::waitr_timestamp() + 50
  for (line in sample(y)) {
    G$cursor %<>% pos_at(0, line - 1)
    G$cursor %<>% write(paste(rep(" ", G$tv_width), collapse=""))
    next_frame <- waitr::waitr_until(next_frame + 50)
  }
  G
}

################################################################################
# Get the ANSI colour string, and possibly set it as well.

get_colour <- function(fg, bg = 0) {
  sprintf("\033[38;5;%d;48;5;%dm", fg, bg)
}

set_colour <- function(fg, bg = 0) {
  cat(get_colour(fg, bg))
}

################################################################################
# Stuff about the cursor being visible or not.

cursor_off <- function() {
  cat("\x1b[?25l")
}

cursor_on <- function() {
  cat("\x1b[?25h")
}

################################################################################
# Position at - returning new co-ordinates (which should match where you told
# it to go). SCREEN_MARGIN_X and _Y are meant to be irrelevant to the user.

pos_at <- function(cursor, x, y) {

  cursor[1] <- cursor[1] + SCREEN_MARGIN_X
  cursor[2] <- cursor[2] + SCREEN_MARGIN_Y
  x <- x + SCREEN_MARGIN_X
  y <- y + SCREEN_MARGIN_Y

  if (y < cursor[2]) {
    cat(sprintf("\033[%dA", cursor[2] - y))
  } else if (y > cursor[2]) {
    cat(sprintf("\033[%dB", y - cursor[2]))
  }

  if (x < cursor[1]) {
    cat(sprintf("\033[%dD", cursor[1] - x))
  } else if (x > cursor[1]) {
    cat(sprintf("\033[%dC", x - cursor[1]))
  }
  x <- x - SCREEN_MARGIN_X
  y <- y - SCREEN_MARGIN_Y

  c(x, y)
}


################################################################################
# Write some text, updating cursor. Either at current position, or specified

write <- function(cursor, text, colour = NA) {
  if (!is.na(colour)) set_colour(colour)
  cat(text)
  cursor[1] <- cursor[1] + nchar(crayon::strip_style(text))
  cursor
}

write_at <- function(cursor, x, y, text, colour = NA, align = LEFT) {
  cursor %<>% pos_at(x, y)
  cursor %<>% write(text, colour)
  cursor
}


################################################################################
# Turn on the CRT.

draw_tv_screen <- function(wid = 60, hei = 20) {
  # TV screens are grey.
  set_colour(244)

  # Draw top, sides and bottom
  cat(paste(c("  ", rep("#", wid + 2)), collapse = ""), "\n")
  for (j in seq_len(hei)) {
    cat(paste(c("  #", rep(" ", wid), "#\n"), collapse=""))
  }
  cat(paste(c("  ", rep("#", wid + 2)), collapse = ""), "\n")

  # Set ANSI cursor on screen to match returned co-ordinate.

  cat(paste(rep(" ", SCREEN_MARGIN_X), collapse = ""))
  c(0, hei+1)
}

draw_divider <- function(G, y) {
  G$cursor %<>% pos_at(0, y)
  set_colour(248)
  G$cursor %<>% write(paste(rep("-", 60), collapse = ""))
  G
}

################################################################################
# Get a limited amount of keyboard input. (readLines won't work as it will mess
# up our co-ordinates... Colour 216 is apparently light salmon.

get_input <- function(G, x, y, max_len, col = 216, dotcol = 255) {
  cur_pos <- 1
  set_colour(col)
  G$cursor %<>% write_at(x, y, "?")
  res <- ""
  acceptable <- c(letters, toupper(letters), 0:9, " ", "!", "-", "_")
  while (TRUE) {
    kp <- keypress::keypress(block = TRUE)
    if ((kp %in% acceptable) && (cur_pos <= max_len)) {
      res <- paste0(res, kp)
      set_colour(col)
      G$cursor %<>% write_at(x + (cur_pos - 1), y, kp)
      G$cursor %<>% write("?")
      cur_pos <- cur_pos + 1

    } else if ((kp == "backspace") && (cur_pos > 1)) {
      res <- substring(res, 1, nchar(res) - 1)
      cur_pos <- cur_pos - 1
      set_colour(col)
      G$cursor %<>% write_at(x + (cur_pos - 1), y, "?")
      set_colour(dotcol)
      G$cursor %<>% write(".")


    } else if (kp == "enter") {
      G$text_input <- res
      if (nchar(res) < max_len) {
        set_colour(dotcol)
        G$cursor %<>% write_at(x + (cur_pos - 1), y, ".")
      } else {
        G$cursor %<>% write_at(x + (cur_pos - 1), y, " ")
      }
      break
    }
  }
  G
}

check_windows_ansi <- function() {
  do_cmd <- function(s) {
    suppressWarnings(res <- paste(
      system2("cmd", s, stdout = TRUE, stderr=TRUE),
      collapse = ""))
    res
  }

  if (.Platform$OS.type != "windows") {
    return(TRUE)
  }

  res <- do_cmd(
    "/c REG QUERY HKCU\\CONSOLE /v VirtualTerminalLevel")

  if (!grepl("REG_DWORD(\\s+)0x1", res)) {
    message("\nOn Windows, you need a registry key set to allow colours")
    message("to be used from RScript in a terminal window. The command needed:")
    message("REG ADD HKCU\\CONSOLE /f /v VirtualTerminalLevel /t REG_DWORD /d 1")
    message("\nPlease choose:-\n")
    message("1. Set this key now.")
    message("2. Don't set this key now.")
    x <- 0
    while (!x %in% c("1", "2")) {
      cat("\nYour choice: ")
      x <- readLines(file("stdin"), n = 1L)
    }
    if (x == "2") {
      message("\nNo changes made.")
      return(FALSE)
    }

    res <- do_cmd(
      "/c REG ADD HKCU\\CONSOLE /f /v VirtualTerminalLevel /t REG_DWORD /d 1")
    message("\nDone - you'll need to start a new Command Prompt window for")
    message("the changes to take effect.\n")
    return(FALSE)
  }
  return(TRUE)
}

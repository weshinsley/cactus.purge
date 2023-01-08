################################################################################

main_title <- function(cursor, TV_WIDTH = 60, TV_HEIGHT = 23) {
  config <- load_config()
  config <- check_sound_driver(config)

  cursor <- draw_divider(cursor, 19)
  cursor <- show_pic(cursor, pkg_file("gfx/title.txt"))

  name <- "CACTUS PURGE"
  audio_avail <- (nrow(audio::audio.drivers()) > 0)
  if (audio_avail) {
    audio_avail <- check_sound_card()
  }
  if (!audio_avail) {
    config$audio <- FALSE
  }

  instr1 <- "  : Sound             : Exit"

  game1 <- "SELECT CHAPTER...   [1] CACTZ   [2] CACTUSKI"

  cursor <- fade_text(cursor, 30, 21, name, GREY_SCALE, FADE_IN_OUT,
                      delay = 500, triple = TRUE)
  cursor <- fade_text(cursor, 30, 20, instr1, GREY_SCALE, FADE_IN)

  cursor <- write_at(cursor, 16, 20, "S", if (audio_avail) 46 else 244)
  cursor <- write_at(cursor, 34, 20, "ESC", 46)
  cursor <- write_at(cursor, 26, 20, if (config$audio) "ON " else "OFF", 195)

  cursor <- fade_text(cursor, 30, 22, game1, UNICORN, FADE_IN)

  cursor <- write_at(cursor, 28, 22, "[1]", 46)
  cursor <- write_at(cursor, 40, 22, "[2]", 46)

  while (TRUE) {
    kp <- keypress::keypress(block = TRUE)
    if (tolower(kp) %in% c("1", "2", "escape")) {
      break
    }
    if (tolower(kp) == "s") {
      if (audio_avail) {
        config$audio <- 1 - config$audio
        cursor <- write_at(cursor, 26, 20,
                           ifelse(config$audio, "ON ", "OFF"), 195)
      }
    }
  }

  cursor <- fade_text(cursor, 30, 20, instr1, GREY_SCALE, FADE_OUT)
  cursor <- fade_text(cursor, 30, 22, game1, UNICORN, FADE_OUT)
  set_colour(15)
  if (kp == "escape") {
    cursor <- clear_pic(cursor, TV_HEIGHT)
    cursor <- pos_at(cursor, 0, TV_HEIGHT + 2)
    return(list(cursor = cursor, config = config, res = "EXIT"))

  } else if (kp %in% c("1", "2")) {
    save_config(config)
    return(list(cursor = cursor, config = config,
                res = c("CACTZ", "CACTUSKI")[as.integer(kp)]))
  }

  invisible()
}

################################################################################

cactz_title <- function(cursor, config) {
  file.copy(pkg_file("other/cactz-hs.csv.xz"), user_file("cactz-hs.csv.xz"),
            overwrite = FALSE)
  cursor <- show_pic(cursor, pkg_file("gfx/cactz.txt"))
  cursor <- draw_divider(cursor, 19)
  instr1 <- paste0(get_colour(46), "P", get_colour(15), ": PLAY   ",
                   get_colour(159), "I", get_colour(15), ": INSTRUCTIONS   ",
                   get_colour(220), "H", get_colour(15), ": HI-SCORES   ",
                   get_colour(196), "Q", get_colour(15), ": QUIT")
  title1 <- "CACTZ!"
  cursor <- write_at(cursor, 3, 22, instr1)
  cursor <- fade_text(cursor, 30, 20, title1, UNICORN, FADE_IN)

  page <- "T"
  while (TRUE) {
    kp <- tolower(keypress::keypress(block = TRUE))
    if (kp == "i") {
      if (page == "I") {
        cursor <- show_pic(cursor, pkg_file("gfx/cactz.txt"))
        page <- "T"
      } else {
        cursor <- show_pic(cursor, pkg_file("gfx/cactz-inst.txt"))
        page <- "I"
      }

    } else if (kp %in% c('p', 'q')) {
      cursor <- fade_text(cursor, 30, 20, title1, UNICORN, FADE_OUT)
      cursor <- clear_pic(cursor, 23)
      if (kp == 'p') {
         cursor <- cactz(cursor, config)
         cursor <- show_pic(cursor, pkg_file("gfx/cactz.txt"))
         cursor <- draw_divider(cursor, 19)
         page <- "T"
         cursor <- write_at(cursor, 3, 22, instr1)
         cursor <- fade_text(cursor, 30, 20, title1, UNICORN, FADE_IN)

      } else {
        return(cursor)
      }
    } else if (kp == 'h') {
      if (page != "H") {
        cursor <- clear_pic(cursor, 19)
        cursor <- fade_text(cursor, 30, 1, "CACTZ HALL OF FAME", UNICORN, FADE_IN)
        cursor <- snazzy_scores(cursor, user_file("cactz-hs.csv.xz"), FADE_IN)
        page <- "H"
      } else {
        cursor <- snazzy_scores(cursor, user_file("cactz-hs.csv.xz"), FADE_OUT)
        cursor <- fade_text(cursor, 30, 1, "CACTZ HALL OF FAME", UNICORN, FADE_OUT)
        cursor <- show_pic(cursor, pkg_file("gfx/cactz.txt"))
        page <- "T"
      }
    }
  }
  cursor
}

################################################################################

cactuski_title <- function(cursor, config) {
  file.copy(pkg_file("other/cactuski-hs.csv.xz"), user_file("cactuski-hs.csv.xz"),
            overwrite = FALSE)
  cursor <- show_pic(cursor, pkg_file("gfx/cactuski.txt"))
  cursor <- draw_divider(cursor, 19)
  instr1 <- paste0(get_colour(46), "P", get_colour(15), ": PLAY   ",
                   get_colour(159), "I", get_colour(15), ": INSTRUCTIONS   ",
                   get_colour(220), "H", get_colour(15), ": HI-SCORES   ",
                   get_colour(196), "Q", get_colour(15), ": QUIT")
  title1 <- "CACTUSKI!"
  cursor <- write_at(cursor, 3, 22, instr1)
  cursor <- fade_text(cursor, 30, 20, title1, UNICORN, FADE_IN)

  page <- "T"
  while (TRUE) {
    kp <- tolower(keypress::keypress(block = TRUE))
    if (kp == "i") {
      if (page == "I") {
        cursor <- show_pic(cursor, pkg_file("gfx/cactuski.txt"))
        page <- "T"
      } else {
        cursor <- show_pic(cursor, pkg_file("gfx/cactuski-inst.txt"))
        page <- "I"
      }

    } else if (kp %in% c('p', 'q')) {
      cursor <- fade_text(cursor, 30, 20, title1, UNICORN, FADE_OUT)
      cursor <- clear_pic(cursor, 23)
      if (kp == 'p') {
        cursor <- cactuski(cursor, config)
        cursor <- show_pic(cursor, pkg_file("gfx/cactuski.txt"))
        cursor <-  draw_divider(cursor, 19)
        page <- "T"
        cursor <- write_at(cursor, 3, 22, instr1)
        cursor <- fade_text(cursor, 30, 20, title1, UNICORN, FADE_IN)

      } else {
        return(cursor)
      }
    } else if (kp == 'h') {
      if (page != "H") {
        cursor <- clear_pic(cursor, 19)
        cursor <- fade_text(cursor, 30, 1, "CACTUSKI HALL OF FAME", UNICORN, FADE_IN)
        cursor <- snazzy_scores(cursor, user_file("cactuski-hs.csv.xz"), FADE_IN)
        page <- "H"
      } else {
        cursor <- snazzy_scores(cursor, user_file("cactuski-hs.csv.xz"), FADE_OUT)
        cursor <- fade_text(cursor, 30, 1, "CACTUSKI HALL OF FAME", UNICORN, FADE_OUT)
        cursor <- show_pic(cursor, pkg_file("gfx/cactuski.txt"))
        page <- "T"
      }
    }
  }
  G
}

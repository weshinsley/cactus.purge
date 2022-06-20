################################################################################

main_title <- function(G) {
  G$config <- load_config()
  G$config %<>% check_sound_driver()
  G %<>% draw_divider(19)
  G %<>% show_pic(pkg_file("gfx/title.txt"))
  name <- "CACTUS PURGE"
  audio_avail <- (nrow(audio::audio.drivers()) > 0)
  if (audio_avail) {
    audio_avail <- check_sound_card()
  }
  if (!audio_avail) {
    G$config$audio <- FALSE
  }

  instr1 <- " : Sound                : Exit               : Play"

  game1 <- "CHAPTER 1... CACTZ!"

  G %<>% fade_text(30, 21, name, GREY_SCALE, FADE_IN_OUT, delay = 500, triple = TRUE)
  G %<>% fade_text(30, 20, instr1, GREY_SCALE, FADE_IN)

  G$cursor %<>% write_at(4, 20, "S", ifelse(audio_avail, 46, 244))
  G$cursor %<>% write_at(44, 20, "ENTER", 46)
  G$cursor %<>% write_at(25, 20, "ESC", 46)
  G$cursor %<>% write_at(13, 20, ifelse(G$config$audio, "ON ", "OFF"), 195)

  G %<>% fade_text(30, 22, game1, UNICORN, FADE_IN, fade_speed = 0.1)

  while (TRUE) {
    kp <- keypress::keypress(block = TRUE)
    if (tolower(kp) %in% c("enter", "escape")) {
      break
    }
    if (tolower(kp) == "s") {
      if (audio_avail) {
        G$config$audio <- 1 - G$config$audio
        G$cursor %<>% write_at(13, 20, ifelse(G$config$audio, "ON ", "OFF"), 195)
      }
    }
  }

  G %<>% fade_text(30, 20, instr1, GREY_SCALE, FADE_OUT)
  G %<>% fade_text(30, 22, game1, UNICORN, FADE_OUT)
  set_colour(15)
  if (kp == "escape") {
    G %<>% clear_pic(G$tv_height)
    G$cursor %<>% pos_at(0, G$tv_height + 2)
    G$main_menu_result <- "EXIT"

  } else if (kp == "enter") {
    G$main_menu_result <- "CACTZ"
    save_config(G$config)
  }

  G
}

################################################################################

cactz_title <- function(G) {
  file.copy(pkg_file("other/cactz-hs.csv.xz"), user_file("cactz-hs.csv.xz"),
            overwrite = FALSE)
  G %<>% show_pic(pkg_file("gfx/cactz.txt"))
  G %<>% draw_divider(19)
  instr1 <- paste0(get_colour(46), "P", get_colour(15), ": PLAY   ",
                   get_colour(159), "I", get_colour(15), ": INSTRUCTIONS   ",
                   get_colour(220), "H", get_colour(15), ": HI-SCORES   ",
                   get_colour(196), "Q", get_colour(15), ": QUIT")
  title1 <- "CACTZ!"
  G$cursor %<>% write_at(3, 22, instr1)
  G %<>% fade_text(30, 20, title1, UNICORN, FADE_IN)

  page <- "T"
  while (TRUE) {
    kp <- tolower(keypress::keypress(block = TRUE))
    if (kp == "i") {
      if (page == "I") {
        G %<>% show_pic(pkg_file("gfx/cactz.txt"))
        page <- "T"
      } else {
        G %<>% show_pic(pkg_file("gfx/cactz-inst.txt"))
        page <- "I"
      }

    } else if (kp %in% c('p', 'q')) {
      G %<>% fade_text(30, 20, title1, UNICORN, FADE_OUT)
      G %<>% clear_pic(23)
      if (kp == 'p') {
         G %<>% cactz()
         G %<>% show_pic(pkg_file("gfx/cactz.txt"))
         G %<>% draw_divider(19)
         page <- "T"
         G$cursor %<>% write_at(3, 22, instr1)
         G %<>% fade_text(30, 20, title1, UNICORN, FADE_IN)

      } else {
        return(G)
      }
    } else if (kp == 'h') {
      if (page != "H") {
        G %<>% clear_pic(19)
        G %<>% fade_text(30, 1, "CACTZ HALL OF FAME", UNICORN, FADE_IN)
        G %<>% snazzy_scores(user_file("cactz-hs.csv.xz"), FADE_IN)
        page <- "H"
      } else {
        G %<>% snazzy_scores(user_file("cactz-hs.csv.xz"), FADE_OUT)
        G %<>% fade_text(30, 1, "CACTZ HALL OF FAME", UNICORN, FADE_OUT)
        G %<>% show_pic(pkg_file("gfx/cactz.txt"))
        page <- "T"
      }
    }
  }
  G
}

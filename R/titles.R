library(crayon)

################################################################################

main_title <- function(G) {
  G %<>% draw_divider(19)
  G %<>% show_pic("data/title.txt")
  name <- "CACTUS PURGE"
  instr1 <- paste0("Only one chapter so far. ENTER to play, or ESCAPE to exit")
  game1 <- "CHAPTER 1... CACTZ!"


  G %<>% fade_text(30, 21, name, GREY_SCALE, FADE_IN_OUT, 1, triple = TRUE)
  G %<>% fade_text(30, 20, instr1, GREY_SCALE, FADE_IN)
  G %<>% fade_text(30, 22, game1, UNICORN, FADE_IN, fade_speed = 0.1)

  while (TRUE) {
    kp <- keypress::keypress(block = TRUE)
    if (kp %in% c("enter", "escape")) break
  }

  G %<>% fade_text(30, 20, instr1, GREY_SCALE, FADE_OUT)
  G %<>% fade_text(30, 22, game1, UNICORN, FADE_OUT)

  if (kp == "escape") {
    G %<>% clear_pic(G$tv_height)
    G$cursor %<>% pos_at(0, G$tv_height + 2)
    cursor_on()
    set_colour(15)
    stop_quietly()
  }

  G
}

################################################################################

cactz_title <- function(G) {
  G %<>% show_pic("data/cactz.txt")
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
        G %<>% show_pic("data/cactz.txt")
        page <- "T"
      } else {
        G %<>% show_pic("data/cactz-inst.txt")
        page <- "I"
      }

    } else if (kp %in% c('p', 'q')) {
      G %<>% fade_text(30, 20, title1, UNICORN, FADE_OUT)
      G %<>% clear_pic(23)
      if (kp == 'p') {
         G %<>% cactz()
         G %<>% show_pic("data/cactz.txt")
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
        G %<>% snazzy_scores("data/cactz-hs.csv.xz", FADE_IN)
        page <- "H"
      } else {
        G %<>% snazzy_scores("data/cactz-hs.csv.xz", FADE_OUT)
        G %<>% fade_text(30, 1, "CACTZ HALL OF FAME", UNICORN, FADE_OUT)
        G %<>% show_pic("data/cactz.txt")
        page <- "T"
      }
    }
  }
  G
}

#' @importFrom magrittr %<>%

################################################################################
# Set up initial track

cactuski_setup <- function(curs, wid = 28) {
  cols <- get_colour(c(202,208,214,220,226,227,228,229,230,231,222))
  space <- 37 - wid

  str_start <- paste0(paste(paste0(cols, ">"), collapse=""),
                      strrep(">", space %/% 2),"|", strrep(" ",wid), "|",
                      strrep("<", space %/% 2),
                      paste(paste0(rev(cols), "<"), collapse=""))

  for (y in 0:19) {
    curs %<>% write_at(0, y, str_start)
  }

  set_colour(244)
  curs %<>% write_at(0, 20, paste(rep("#", 60), collapse=""))
  curs %<>% write_at(42,21, "SCORE: ", 33)
  curs %<>% write_at(42,22, "LIVES: ", 37)
  curs %<>% write_at(49,22, "0000003.00", 49)
  curs %<>% write_at(40,21, "#", 244)
  curs %<>% write_at(40,22, "#", 244)
  curs
}

cactuski_init_track <- function(T) {
  T$mid <- rep(30L, 20)
  if (is.null(T$wid_zero)) T$wid_zero <- 32L
  T$wid <- rep(T$wid_zero, 20)

  T$left <- T$mid - ((T$wid %/% 2L) + 1L)
  T$right <- T$mid + (T$wid %/% 2L)
  T$left_zero <- T$left[1]
  T$right_zero <- T$right[1]

  T$next_wiggle_in <- 0
  T$origin_x <- 30L
  T$amplitude <- 0L
  T$direction <- 0L
  T$target_123 <- 3L
  T$period <- 0L
  T$top <- 1L
  T
}


################################################################################
# Update which baddies are coming next...

cactuski_incoming_thing <- function(G, T, kp, px = NULL) {
  G$factive <- c(G$factive, 1)
  G$fleftlimit <- c(G$fleftlimit, 0)
  G$frightlimit <- c(G$frightlimit, 0)
  G$fdx <- c(G$fdx, 0)
  yindex <- ifelse(T$top == 1, 19, T$top - 1)
  G$fy <- c(G$fy, 20)
  G$fspaces <- c(G$fspaces, "   ")

  if (kp == "h") {
    G$ftype <- c(G$ftype, 2)
    G$fstring <- c(G$fstring, paste0(get_colour(145), "\\_/"))
    if (is.null(px)) {
      px <- sample((T$left[yindex] + 3):(T$right[yindex] - 3), 1)
    }
    G$fx <- c(G$fx, px)

  } else if (kp == "f") {
    G$ftype <- c(G$ftype, 5)
    G$fstring <- c(G$fstring, paste0(get_colour(82), "<%>"))
    if (is.null(px)) {
      px <- sample((T$left[yindex] + 3):(T$right[yindex] - 3), 1)
    }
    G$fx <- c(G$fx, px)

  } else if (kp == "s") {
    if (is.null(px)) {
      mid <- ((T$left[yindex] + 1) + (T$right[yindex] - 3)) %/% 2
      px <- sample((mid - 3):(mid + 3), 1)
    }
    G$fx <- c(G$fx, px)
    G$flipper <- 1 - G$flipper
    newtype <- ifelse(G$flipper == 1, 3, 4)
    G$ftype <- c(G$ftype, newtype)
    G$fstring <- c(G$fstring,
                   paste0(get_colour(117), ifelse(newtype == 3, "<--", "-->")))

  } else if (kp == "t") {
    G$ftype <- c(G$ftype, 6L)
    G$fstring <- c(G$fstring, paste0(get_colour(64), "o",
                   get_colour(11), "O", get_colour(64), "o"))
    if (is.null(px)) {
      px <- sample((T$left[yindex] + 3):(T$right[yindex] - 3), 1)
    }
    G$fx <- c(G$fx, px)
  }
  G
}

cactuski_incoming_baddy <- function(G, T, px = NULL) {
  G$fy <- c(G$fy, 20)
  G$ftype <- c(G$ftype, 1)
  G$fstring <- c(G$fstring, "")
  G$fspaces <- c(G$fspaces, "  ")
  G$factive <- c(G$factive, 1)
  yindex <- ifelse(T$top == 1, 19, T$top - 1)
  G$fleftlimit <- c(G$fleftlimit, T$left[yindex])
  G$frightlimit <- c(G$frightlimit, T$right[yindex])
  G$fdx <- c(G$fdx, sample(c(2,-2), 1))
  if (is.null(px)) {
    px <- sample((T$left[yindex] + 4):(T$right[yindex] - 4), 1)
  }
  G$fx <- c(G$fx, px)
  if (G$config$audio) G$bgo %<>% play_sound()
  G
}

cactuski_incoming_wall <- function(G, T, wtype) {
  if (G$wy == -1) {
    G$wy <- 20
    yindex <- ifelse(T$top == 1, 19, T$top - 1)
    G$wx <- T$left[yindex] + 1
    G$wwid <- (T$right[yindex] - G$wx)
    if (wtype == 5) wtype <- sample(1:4, 1)
    G$wtype <- wtype
    G$gapwid <- max(6, as.integer(G$wwid / 2.5))
    G$gapx <- sample((G$wx + 2):((G$wx+G$wwid)-(G$gapwid + 2)))[1]
    G$gapdx <- ifelse(G$wtype %in% c(1,3), 0, sample(c(-1, 1))[1])
  }
  G
}

cactuski_incoming_boss <- function(G, T, info) {
  info <- as.integer(strsplit(info, ";")[[1]])
  if (G$bossy == -1) {
    G$bossy <- 18
    yindex <- ifelse(T$top == 1, 19, T$top - 1)
    G$bossx <- sample((T$left[yindex] + 4):(T$right[yindex] - 4), 1)
    G$bosst <- info[1]
    G$bosshfx <- 0
    G$bossh <- info[2]
    G$bossfreq <- info[3]
    G$bossgap <- info[3]
  }
  G
}

cactuski_incoming_finch <- function(G, T, px = NULL) {
  if (G$cfy == -1) {
    G$cfy <- 19L
    G$cfr <- 1L
    yindex <- ifelse(T$top == 1L, 19L, T$top - 1L)
    G$cff <- sample(c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), 1L)
    if (is.null(px)) {
      px <- as.integer(T$left[yindex] + (G$cff * (T$right[yindex] - T$left[yindex])))
    }
    G$cfx <- px
    if (G$config$audio) G$finch %<>% play_sound()
  }
  G
}

cactuski_update_future <- function(G, T, MSGS) {

  extras <- function(G) {
    if (length(G$mline) < 2) return(G)
    prob <- runif(6)
    if (prob[1] > G$mline$p_hole) prob[1] <- 0
    if (prob[2] > G$mline$p_food) prob[2] <- 0
    if (prob[3] > G$mline$p_baddy) prob[3] <- 0
    if (prob[4] > G$mline$p_slalom) prob[4] <- 0
    if (prob[5] > G$mline$p_finch) prob[5] <- 0
    if (prob[6] > G$mline$p_camel) prob[6] <- 0
    if (any(prob > 0)) {
      i <- which(prob == max(prob))[1]
      if (i == 1) G %<>% cactuski_incoming_thing(T, "h")
      else if (i == 2) G %<>% cactuski_incoming_thing(T, "f")
      else if (i == 3) G %<>% cactuski_incoming_baddy(T)
      else if (i == 4) G %<>% cactuski_incoming_thing(T, "s")
      else if (i == 5) G %<>% cactuski_incoming_finch(T)
      else if (i == 6) G %<>% cactuski_incoming_thing(T, "t")
    }
    G
  }

  # Negative phase (with catch for first time)
  # - ready for next bit of future...

  if (is.na(G$mp)) return(G)

  # Pause counting while there's a boss
  # (So msgs must be scripted prior to the boss script line)

  if (G$bossy != -1) return(G)

  if (G$mp < 0) {
    G$mn <- G$mn + 1
    G$mline <- G$mscript[G$mn, ]
    # If it's a goto...

    # else

    G$mp <- G$mline$n_phases
    G$mc <- G$mline$clust_size
    G$mcgap <- G$mline$clust_gap
    G$mpgap <- G$mline$phase_gap

    msgs <- G$mline$msgs
    if (!msgs %in% c(NA, "0")) {
      if (grepl("\\|", msgs)) {
        msgs <- sample(strsplit(msgs, "\\|")[[1]], size = 1)
      }
      msgs <- as.integer(strsplit(msgs, ";")[[1]])
      G$msgs <- c(G$msgs, MSGS$text[msgs])
    }

    return(G)
  }

  # Otherwise... reduce phase-gap...

  if (G$mpgap > 0) {
    G$mpgap <- G$mpgap - 1
    return(extras(G))
  }

  # now handle cluster gap

  if (G$mcgap > 0) {
    G$mcgap <- G$mcgap - 1
    return(extras(G))
  }

  # So phase-gap = 0, and cluster-gap = 0.
  # Lay down one of a cluster.

  if (G$mc > 0) {
    G$mc <- G$mc - 1
    G$mcgap <- G$mline$clust_gap
    if (G$mline$type %in% c("MSG", "PAU")) return(G)
    else if (G$mline$type == "HOL") G %<>% cactuski_incoming_thing(T, "h")
    else if (G$mline$type == "FOO") G %<>% cactuski_incoming_thing(T, "f")
    else if (G$mline$type == "BAD") G %<>% cactuski_incoming_baddy(T)
    else if (G$mline$type == "SLA") G %<>% cactuski_incoming_thing(T, "s")
    else if (G$mline$type == "CAM") G %<>% cactuski_incoming_thing(T, "t")
    else if (G$mline$type == "WAL") G %<>% cactuski_incoming_wall(T, G$mline$extra)
    else if (G$mline$type == "BOS") G %<>% cactuski_incoming_boss(T, G$mline$extra)
    else if (G$mline$type == "FIN") G %<>% cactuski_incoming_finch(T)
    else if (G$mline$type == "THI") G$thin <- 2
    else if (G$mline$type == "FAT") G$thin <- (-2)
    else if (G$mline$type == "END") G %<>% cactuski_end()
    return(G)
  }

  # End of cluster

  G$mp <- G$mp - 1
  if (G$mp == 0) G$mp <- (-1)
  if (length(G$mline) < 2) return(G)

  G$mpgap <- G$mline$phase_gap
  G$mc <- G$mline$clust_size
  G$mcgap <- G$mline$clust_gap

  G
}

###############################################################################
# Handle bosses

cactuski_do_bosses <- function(G, T) {
  if (G$bossy == -1) return(G)

  if ((G$bosshfx == 0) && (G$bossh == 0)) {
    G$cursor %<>% write_at(G$bossx, 18, "   ")
    G$cursor %<>% write_at(G$bossx, 19, "   ")
    G$bossy <- -1
    return(G)
  }

  bosses <- list(
    c(paste0(get_colour(246), "O", get_colour(83), "#", get_colour(246), "O"),
      paste0(get_colour(246), "o", get_colour(83), "#", get_colour(246), "o"),
      paste0(get_colour(246), "O", get_colour(40), "U", get_colour(246), "O"),
      paste0(get_colour(246), "o", get_colour(40), "U", get_colour(246), "o")),

    c(paste0(get_colour(214), "/", get_colour(123), "@", get_colour(214), "\\"),
      paste0(get_colour(178), "-", get_colour(87), "@", get_colour(178), "-"),
      paste0(get_colour(216), "/", get_colour(147), "V", get_colour(216), "\\"),
      paste0(get_colour(180), "-", get_colour(183), "v", get_colour(180), "-")),

    c(paste0(get_colour(255), "|", get_colour(196), "^", get_colour(255), "|"),
      paste0(get_colour(148), "¦", get_colour(226), "^", get_colour(248), "¦"),
      paste0(get_colour(216), "/", get_colour(208), "@", get_colour(216), "\\"),
      paste0(get_colour(180), "/", get_colour(196), "O", get_colour(180), "\\")),

    c(paste0(get_colour(195), "x", get_colour(226), "?", get_colour(159), "+"),
      paste0(get_colour(87), "+", get_colour(220), "!", get_colour(117), "x"),
      paste0(get_colour(117), "+", get_colour(214), "!", get_colour(87), "x"),
      paste0(get_colour(159), "x", get_colour(178), "?", get_colour(195), "+")))

  if (G$bosshfx == 0) {
    toprow <- bosses[[G$bosst]][(T$top %% 2) + 1]
    bottomrow <- bosses[[G$bosst]][(T$top %% 2) + 3]
  } else {
    excol <- sample(c(get_colour(196), get_colour(202), get_colour(208),
                      get_colour(214), get_colour(220), get_colour(226)))

    char1 <- sample(c("#", "@"))[1]
    char2 <- sample(c("#", "@"))[1]

    toprow <- paste0(excol[1], "/", excol[2], char1, excol[3], "\\")
    bottomrow <- paste0(excol[4], "\\", excol[5], char2, excol[6], "/")
    G$bosshfx <- G$bosshfx - 1
  }



  # Move x/y

  yindex1 <- T$top - 2
  yindex2 <- T$top - 1
  if (yindex1 < 1) yindex1 <- yindex1 + 19
  if (yindex2 < 1) yindex2 <- yindex2 + 19

  if ((G$bossx < T$left[yindex1] + 1) ||
     (G$bossx < T$left[yindex2] + 1)) {
    G$cursor %<>% write_at(G$bossx, 18, "   ")
    G$cursor %<>% write_at(G$bossx, 19, "   ")
    G$bossx <- max(T$left[yindex1] + 1, T$left[yindex2] + 1)
    G$cursor %<>% write_at(G$bossx, 18, toprow)
    G$cursor %<>% write_at(G$bossx, 19, bottomrow)

  } else if ((G$bossx > T$right[yindex1] - 3)  ||
             (G$bossx > T$right[yindex2] - 3)) {
    G$cursor %<>% write_at(G$bossx, 18, "   ")
    G$cursor %<>% write_at(G$bossx, 19, "   ")
    G$bossx <- min(T$right[yindex1] - 3, T$right[yindex2]  - 3)
    G$cursor %<>% write_at(G$bossx, 18, toprow)
    G$cursor %<>% write_at(G$bossx, 19, bottomrow)

  } else {

    dx <- runif(1)
    if ((dx <= 0.33) && (G$bossx > T$left[yindex1] + 2) &&
                        (G$bossx > T$left[yindex2] + 2))
      dx <- (-1L)

    else if ((dx >= 0.66) && (G$bossx < T$right[yindex1] - 4) &&
                             (G$bossx < T$right[yindex2] - 4))
      dx <- (1L)

    else dx <- 0L

    pre <- if (dx == 1L) " " else ""
    post <- if (dx == -1L) " " else ""

    if (dx == -1L) G$bossx <- G$bossx + dx

    G$cursor %<>% write_at(G$bossx, 18,
      paste0(pre, toprow, post))

    G$cursor %<>% write_at(G$bossx, 19,
     paste0(pre, bottomrow, post))

    if (dx == 1L) G$bossx <- G$bossx + dx
  }

  G$bossgap <- G$bossgap - 1
  if (G$bossgap == 0) {
    G$bossgap <- G$bossfreq
    if (G$bosst == 1) G %<>% cactuski_incoming_thing(T, "h", G$bossx)
    else if (G$bosst == 2) G %<>% cactuski_incoming_finch(T, G$bossx)
    else if (G$bosst == 3) G %<>% cactuski_incoming_baddy(T, G$bossx)
    else {
      gs <- sample(1:6, 1)
      if (gs == 1) G %<>% cactuski_incoming_thing(T, "h", G$bossx)
      else if (gs == 2) G %<>% cactuski_incoming_finch(T, G$bossx)
      else if (gs == 3) G %<>% cactuski_incoming_baddy(T, G$bossx)
      else if (gs == 4) G %<>% cactuski_incoming_thing(T, "s", G$bossx)
      else if (gs == 5) G %<>% cactuski_incoming_thing(T, "f", G$bossx)
      else if (gs == 6) G %<>% cactuski_incoming_thing(T, "t", G$bossx)
    }
  }
  return(G)
}


###############################################################################
# Handle teleprinter

cactuski_do_messages <- function(G) {

  replace_cols <- function(s) {
    cpos <- gregexpr("\\$", s)[[1]][1]
    while (cpos != -1) {
      col <- as.integer(substr(s, cpos + 1, cpos + 3))
      s <- paste0(substr(s, 1, cpos - 1),
                    sprintf("\033[38;5;%d;48;5;0m", col),
                    substring(s, cpos + 4))
      cpos <- gregexpr("\\$", s)[[1]][1]
    }
    s
  }

  if (G$msg_pointer > nchar(G$msgs[1])) {
    if (length(G$msgs) == 1) {
      return(G)
    }
    msg <- replace_cols(gsub("#", "", G$msgs[1]))

    G$cursor %<>% write_at(1, 21, msg)
    G$cursor %<>% write_at(1, 22, "                                      ")
    G$msgs <- G$msgs[-1]
    G$msg_pointer <- 1
    G$msg_x <- 1
  }

  char <- substr(G$msgs[1], G$msg_pointer, G$msg_pointer)

  if (char == '$') {
    col <- as.integer(substr(G$msgs[1], G$msg_pointer + 1, G$msg_pointer + 3))
    G$msg_col <- get_colour(col)
    G$msg_pointer <- G$msg_pointer + 4
    char <- substr(G$msgs[1], G$msg_pointer, G$msg_pointer)
  }

  if (char == '#') {
    char <- replace_cols(substring(G$msgs[1], G$msg_pointer + 1))
    G$msg_pointer <- nchar(G$msgs[1])

  }

  G$cursor %<>% write_at(G$msg_x, 22, paste0(G$msg_col, char))
  if (G$config$audio) {
    if (char != " ") {
      if (runif(1) > 0.7) {
        if (runif(1) > 0.25) {
          G$tel1 %<>% play_sound()
        } else {
          G$tel2 %<>% play_sound()
        }
      }
    }
  }
  G$msg_pointer <- G$msg_pointer + 1
  G$msg_x <- G$msg_x + 1


  G
}

cactuski_update_score <- function(cur, score) {
  score <- formatC(score/100, width = 10, format = "f", digits = 2, flag = "0")
  cur %<>% write_at(49, 21, score, 51)
  cur
}

cactuski_update_lives <- function(curs, lives, col = 49) {
  if (lives <= 0) {
    curs %<>% write_at(49,22, "        NA", sample(c(196, 160, 124, 88, 52), 1))
    return(curs)
  }
  lives <- formatC(lives/100, width = 10, format = "f", digits = 2, flag = "0")

  curs %<>% write_at(49,22, lives, col)
  curs
}

###############################################################################

cactuski_scroll <- function(cursor, T, left_track, right_track) {

  yline <- T$top
  ylinem1 <- ifelse(T$top == 1, 20, T$top - 1)

  for (y in 0:19) {

    # What should we be plotting here?
    # Suppose wid = 28 and mid = 30.
    # The complete line would be...

    # 10 shaded >, 5 >, |, 28 spaces, |, 5 <, 10 shaded <
    # 10 + 5 + 1 + 28 + 1 + 5 + 10 = 60
    # the 28 spaces starts at pos 16, and continues til 43.
    # the vertical bars are at positions 15 and 44

    dl <- T$left[yline]
    dr <- T$right[yline]

    # What was plotted last time, so we can do a "diff" and only draw
    # the characters we need to...

    dl_prev <- ifelse(y == 0, T$left_zero, T$left[ylinem1])
    dr_prev <- ifelse(y == 0, T$right_zero, T$right[ylinem1])

    set_colour(222)
    left_str <-
      ifelse(dl < dl_prev, paste0("/", strrep(" ", dl_prev - dl)),
        ifelse(dl == dl_prev, "|",
          paste0(paste(left_track[dl_prev:(dl-1)], collapse=""), "\\")))

    cursor %<>% write_at(min(dl, dl_prev), y, left_str)

    set_colour(222)
    right_str <- ifelse(dr < dr_prev,
                   paste0("/", paste(right_track[dr:(dr_prev-1)], collapse="")),
                     ifelse(dr == dr_prev, "|",
                        paste0(strrep(" ", dr - dr_prev), "\\")))

    cursor %<>% write_at(min(dr, dr_prev), y, right_str)

    if (y < 19) {
      yline <- ifelse(yline == 20, 1, yline + 1)
      ylinem1 <- ifelse(ylinem1 == 20, 1, ylinem1 + 1)
    }
  }
  cursor
}

###############################################################################

cactuski_draw_kenny <- function(G) {
  if (G$kdx == -1) {
    G$cursor %<>% write_at(G$kx-1, G$ky, G$body_s)
    G$cursor %<>% write_at(G$kx-1, G$ky+1, G$body_s)
    G$cursor %<>% write_at(G$kx-1, G$ky+2, G$body_s)
    G$cursor %<>% write_at(G$kx-1, G$ky+3,
                           ifelse(G$jumping > 2, G$jskileft, G$skileft))
  } else if (G$kdx == 1) {
    G$cursor %<>% write_at(G$kx, G$ky, G$s_body)
    G$cursor %<>% write_at(G$kx, G$ky+1, G$s_body)
    G$cursor %<>% write_at(G$kx, G$ky+2, G$s_body)
    G$cursor %<>% write_at(G$kx, G$ky+3,
                           ifelse(G$jumping > 2, G$jskiright, G$skiright))
  } else if (G$kdx == 0) {
    G$cursor %<>% write_at(G$kx, G$ky, G$body)
    G$cursor %<>% write_at(G$kx, G$ky+1, G$body)
    G$cursor %<>% write_at(G$kx, G$ky+2, G$body)
    G$cursor %<>% write_at(G$kx, G$ky+3,
                           ifelse(G$jumping > 2, G$jskidown, G$skidown))
  }
  G$cursor
}

cactuski_move_kenny <- function(G) {
  # No movement - draw vertical skis

  if ((G$kdx == 0) && (G$kdy == 0) && (!G$init)) {
    if (G$jumping > 1) {
      G$cursor %<>% write_at(G$kx, G$ky+3, G$jskidown)
    } else {
      G$cursor %<>% write_at(G$kx, G$ky+3, G$skidown)
    }
    return(G)
  }

  # Down movement (erase previous top)

   if (G$kdy == 1) {
    G$cursor %<>% write_at(G$kx, G$ky, "   ")
    G$ky <- G$ky + 1
    G$kdy <- 0
    # Then use the G$kdy=0 code.
  } else if (G$kdy == -1) {
    G$ky <- G$ky - 1
    G$cursor %<>% write_at(G$kx, G$ky+4, "   ")
    G$kdy <- 0
  }

  if (G$kdy <= 0) {
    G$cursor <- cactuski_draw_kenny(G)
    G$kx <- G$kx + G$kdx
    G$kdy <- 0
  }

  G
}

###############################################################################

cactuski_check_keys <- function(G, T) {
  if (G$jumping > 0) {
    G$jumping <- G$jumping - 1
  }
  kp <- tolower(keypress::keypress(block = FALSE))
  if (is.na(kp)) {
    return(G)
  }
  if (kp == "left") {
    if ((G$jumping <= 1) && (G$kdx > -1)) G$kdx <- G$kdx - 1
  } else if (kp == "right") {
    if ((G$jumping <= 1) && (G$kdx < 1)) G$kdx <- G$kdx + 1
  } else if (kp == "up") {
    if ((G$jumping <= 1) && (G$ky > 0)) G$kdy <- -1
  } else if (kp == "down") {
    if ((G$jumping <= 1) && (G$ky < 14)) G$kdy <- 1
  } else if (kp == "escape") {
    cursor_on()
    stop()
  } else if (kp == "e") {
    G %<>% cactuski_end()
    return(G)
  }
    else if (kp == "l") {
    if (!G$firing) {
      G$firing = TRUE
      G$lx <- G$kx + 1
      G$ly <- G$ky + 2
      G$ldx <- G$kdx
      if (G$config$audio) G$pew %<>% play_sound()
    }

  } else if (kp == "j") {
    if (G$jumping == 0) {
      G$jumping <- 5
      #G$boing %<>% play_sound()
    }


  # Test hole
  } else if ((kp == "h") || (kp == "f") || (kp == "s")) {
    G %<>% cactuski_incoming_thing(T, kp)

  # Test baddie
  } else if (kp == "b") {
    G %<>% cactuski_incoming_baddy(T)

  } else if (kp == "n") {
    G$thin <- 2

  } else if (kp == "m") {
    G$thin <- (-2)

  } else if (kp == "t") {
    G %<>% cactuski_incoming_thing(T, "t")

  # Test wall

  } else if (kp == "w") {
    G %<>% cactuski_incoming_wall(T, sample(1:4)[1])

  } else if (kp == "c") {
    G %<>% cactuski_incoming_finch(T)

  } else if (kp == "q") {
    G %<>% cactuski_incoming_boss(T, "4;3;10")
  }
  G
}

###############################################################################

cactuski_wiggle_track <- function(T) {
  next_line <- T$top
  T$left_zero <- T$left[next_line]
  T$right_zero <- T$right[next_line]
  T$wid[next_line] <- T$wid_zero


  if (T$next_wiggle_in == 0) {
    T$origin_x <- T$origin_x + as.integer(T$amplitude * T$direction)

    target_123 <- sample(5)
    orig_123 <- T$target_123
    T$target_123 <- target_123[target_123 != orig_123][1]

    next_pos <- c(3 + (T$wid[next_line] %/% 2), NA, 30, NA,
                  57 - (T$wid[next_line] %/% 2))
    next_pos[2] <- (next_pos[1] + next_pos[3]) %/% 2
    next_pos[4] <- (next_pos[5] + next_pos[3]) %/% 2
    next_pos <- next_pos[T$target_123]
    T$direction <- ifelse(T$target_123 > orig_123, 1, -1)
    T$period <- 25
    T$next_wiggle_in <- T$period + 1
    T$amplitude <- abs(T$origin_x - next_pos)
  }

  T$next_wiggle_in <- T$next_wiggle_in - 1
  T$mid[next_line] <- T$origin_x + as.integer((T$amplitude * T$direction * 0.5 *
    (1 + cos(pi + (pi * ((T$period - T$next_wiggle_in) / T$period))))))

  T$left[next_line] <- max(2, T$mid[next_line] - ((T$wid[next_line] %/% 2) + 1))
  T$right[next_line] <- min(58, T$mid[next_line] + (T$wid[next_line] %/% 2))

  T$top <- ifelse(T$top == 20, 1, T$top + 1)

  T
}

################################################################################
# (Re)-start - initialise all the clutter

cactuski_restart <- function(G, T) {
  kp <- keypress::keypress(block = FALSE)
  G$cursor %<>% cactuski_setup(T$wid_zero)
  G$ky <- 0L
  yindex <- ifelse(T$top == 1, 19, T$top - 1)
  G$kx <- 30L
  G$kdx <- 0L
  G$kdy <- 0L
  G$death_flag <- 0
  G$brownleft <- FALSE
  G$brownright <- FALSE
  G$brownmid <- FALSE
  G %<>% colour_kenny()

  # Floaters of various kinds
  # (bouncing baddie, hole, slalom L, Slalom R, cactus part, camel turd)

  G$fx <- NULL
  G$fy <- NULL
  G$fdx <- NULL
  G$fleftlimit <- NULL
  G$frightlimit <- NULL
  G$ftype <- NULL
  G$fstring <- NULL
  G$fspaces <- NULL
  G$factive <- NULL
  G$flipper <- 0

  # One wall

  G$wx <- 0L
  G$wy <- (-1L)
  G$wwid <- 0L
  G$wtype <- 0L
  G$gapwid <- 0L
  G$gapx <- 0L
  G$gapdx <- 0L
  G$death_flag <- 0

  # And only one cactus finch of death.
  # One is more than enough.

  G$cfy <- (-1L)
  G$cfx <- 0L
  G$cff <- 0.5
  G$cfr <- 0L
  G$cfalt <- 0L
  G$fch <- c(paste0(get_colour(228),"^",
                    get_colour(172),"8",
                    get_colour(228),"^"),
             paste0(get_colour(226),"v",
                    get_colour(172),"8",
                    get_colour(226),"v"))

  G
}

################################################################################
# Collision detection of various kinds.

cactuski_death_check <- function(G, T) {

  ###############################
  # We don't need no education

  if (G$ky + 3 == G$wy) {
    if ((G$kx < G$gapx - 1) || (G$kx + 2 > (G$gapx + G$gapwid)) ||
        (G$wtype %in% c(3,4,5))) {
      if (G$config$audio) G$ouch %<>% play_sound()
      for (i in seq(0, 99)) {
        G$lives <- G$lives - 1
        G$cursor %<>% cactuski_update_lives(G$lives, as.integer(runif(1) * 255))
        if (i == 8) {
          G$cursor %<>% write_at(G$kx, G$ky, "   ")
          G$cursor %<>% write_at(G$kx - 2, G$ky + 2, paste0(get_colour(28), "<<",
                                                            get_colour(46), "%%",
                                                            get_colour(28), ">>"))
        } else if (i == 16) {
          G$cursor %<>% write_at(G$kx, G$ky + 1, "   ")
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(28), "<<<",
                                                            get_colour(46), "%%%",
                                                            get_colour(28), ">>>"))
        } else if (i == 24) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(34), "<<<",
                                                            get_colour(82), "%%%",
                                                            get_colour(34), ">>>"))
        } else if (i == 32) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(40), "<<<",
                                                            get_colour(83), "%%%",
                                                            get_colour(40), ">>>"))
        } else if (i == 40) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(46), "<<<",
                                                            get_colour(84), "%%%",
                                                            get_colour(40), ">>>"))
        } else if (i == 48) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(82), "<<<",
                                                            get_colour(196), "xxx",
                                                            get_colour(82), ">>>"))
        } else if (i == 56) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(83), "<<",
                                                            get_colour(196), "xxxxx",
                                                            get_colour(83), ">>"))
        } else if (i == 64) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(85), "<",
                                                            get_colour(196), "xxxxxxx",
                                                            get_colour(85), ">"))
        } else if (i == 72) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(196), "xxxx|xxxx"))
        } else if (i == 77) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(196), "xxx< >xxx"))
        } else if (i == 82) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(196), "xx<   >xx"))
        } else if (i == 87) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(196), "x<     >x"))
        } else if (i == 92) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, paste0(get_colour(196), "<       >"))
        } else if (i == 97) {
          G$cursor %<>% write_at(G$kx - 3, G$ky + 2, "         ")
        }

        waitr::wait_for(10)
      }
      G$cursor %<>% cactuski_update_lives(G$lives, 49)
      G$death_flag <- 1
      return(G)
    }
  }

  y <- 3 + (T$top + G$ky)
  y <- ifelse(y > 20, y - 20, y)
  half_wid <- T$wid[y] %/% 2


  #########################
  # Death by electrocution

  dl <- T$mid[y] - (half_wid + 1)
  dr <- T$mid[y] + (half_wid)
  if ((G$kx <= dl) || (G$kx + 2 >= dr)) {
    G$cursor %<>% pos_at(0,20)
    if (G$config$audio) G$elec %<>% play_sound()
    for (i in seq(99, 0)) {
      G$lives <- G$lives - 1
      G$cursor %<>% cactuski_update_lives(G$lives, as.integer(runif(1) * 255))

      rnd <- as.integer(255 * runif(11))
      ski <- c("\\","|","/")[1+as.integer(3 * runif(2))]
      G$cursor %<>% write_at(G$kx, G$ky, paste0(get_colour(rnd[1]), "<",
                                                get_colour(rnd[2]), "%",
                                                get_colour(rnd[3]), ">"))
      G$cursor %<>% write_at(G$kx, G$ky + 1, paste0(get_colour(rnd[4]), "<",
                                                    get_colour(rnd[5]), "%",
                                                    get_colour(rnd[6]), ">"))
      G$cursor %<>% write_at(G$kx, G$ky + 2, paste0(get_colour(rnd[7]), "<",
                                                    get_colour(rnd[8]), "%",
                                                    get_colour(rnd[9]), ">"))
      G$cursor %<>% write_at(G$kx, G$ky + 3, paste0(get_colour(rnd[10]), ski[1],
                                           " ", get_colour(rnd[11]), ski[2]))
      waitr::wait_for(10)
    }
    G$cursor %<>% cactuski_update_lives(G$lives, 49)
    G$death_flag <- 1
    return(G)
  }

  ##########################################
  # Check collisions with floaters

  index <- which(G$fy == (G$ky + 3))[1]
  if (is.na(index)) {
    return(G)
  }
  if (G$factive[index] != 1) {
    return(G)
  }

  ####################################
  # Slalom left

  if (G$ftype[index] == 3) {
    if (G$kx <= G$fx[index]) {
      G$score <- G$score + 100
      if (G$config$audio) G$ding %<>% play_sound()
    } else {
      G$lives <- G$lives - 2
      if (G$config$audio) G$buzz %<>% play_sound()
    }
    G$factive[index] <- 2
    return(G)
  }

  ####################################
  # Slalom right

  if (G$ftype[index] == 4) {
    if (G$kx >= G$fx[index]) {
      G$score <- G$score + 100
      if (G$config$audio) G$ding %<>% play_sound()
    } else {
      G$lives <- G$lives - 2
      if (G$config$audio) G$buzz %<>% play_sound()
    }
    G$factive[index] <- 2
    return(G)
  }

  if ((G$fx[index] >= (G$kx - 2)) & (G$fx[index] <= G$kx + 2)) {

    #############################
    # Death by falling in hole

    if ((G$ftype[index] == 2) & (G$jumping <= 1)) {
      if (G$config$audio) G$fall %<>% play_sound()
      if (G$kx != G$fx[index]) {
        G$cursor %<>% write_at(G$kx, G$ky, "   ")
        G$cursor %<>% write_at(G$fx[index], G$ky, paste0(get_colour(51), "\\ /"))
        G$cursor %<>% write_at(G$kx, G$ky + 1, "   ")
        G$cursor %<>% write_at(G$fx[index], G$ky + 1, G$body)
        G$cursor %<>% write_at(G$kx, G$ky + 2, "   ")
        G$cursor %<>% write_at(G$fx[index], G$ky + 2, G$body)
        G$cursor %<>% write_at(G$kx, G$ky + 3, "   ")
        G$cursor %<>% write_at(G$fx[index], G$ky + 3, paste0(get_colour(145), "\\",
                                                      get_colour(28), "%",
                                                      get_colour(145), "/"))

      }

      for (death_steps in seq(99, 0)) {
        G$lives <- G$lives - 1
        G$cursor %<>% cactuski_update_lives(G$lives, 255 - as.integer(runif(1) * 28))
        waitr::wait_for(30)

        if (death_steps == 85) {
          G$cursor %<>% write_at(G$fx[index], G$ky, "   ")
          G$cursor %<>% write_at(G$fx[index], G$ky + 1, paste0(get_colour(51), "\\ /"))
        } else if (death_steps == 70) {
          G$cursor %<>% write_at(G$fx[index], G$ky + 1, "   ")
          G$cursor %<>% write_at(G$fx[index], G$ky + 2, paste0(get_colour(51), "\\ /"))
        } else if (death_steps == 55) {
          G$cursor %<>% write_at(G$fx[index], G$ky + 2, "   ")
          G$cursor %<>% write_at(G$fx[index], G$ky + 3, paste0(get_colour(145), "\\",
                                                        get_colour(51), "V",
                                                        get_colour(145), "/"))
        } else if (death_steps == 40) {
          G$cursor %<>% write_at(G$fx[index], G$ky + 3, paste0(get_colour(145), "\\_/"))
        }
      }
      G$cursor %<>% cactuski_update_lives(G$lives)
      G$death_flag <- 1L
      return(G)

    #####################################
    # Eating body parts from dead cactus

    } else if (G$ftype[index] == 5L) {
      G$factive[index] <- 0L
      G$cursor %<>% write_at(G$fx[index], G$fy[index], G$fspaces[index])
      G$lives <- G$lives + 5L
      if (G$config$audio) {
        if (sample(10L, 1L) != 1L) {
          G$eat1 %<>% play_sound()
        } else {
          G$eat2 %<>% play_sound()
        }
      }
      return(G)

    ############################
    # Ski through a camel turd

    } else if (G$ftype[index] == 6L) {
      if ((G$kx >= G$fx[index] - 2L) && (G$kx <= G$fx[index] + 2L)) {
        if (G$config$audio) G$splat %<>% play_sound()
        if (G$kx >= G$fx[index]) G$brownleft <- TRUE
        if ((G$kx >= G$fx[index] - 1L) && (G$kx <= G$fx[index] + 1L)) G$brownmid <- TRUE
        if (G$kx <= G$fx[index]) G$brownright <- TRUE
        G %<>% colour_kenny()
        G$factive[index] <- 0L
        G$cursor %<>% write_at(G$fx[index], G$fy[index], "   ")
      }
      return(G)
    }

  #######################################
  # A bottomless pit goes whooshing past

  } else if (G$ftype[index] == 2L) {
    if (G$config$audio) G$whoo %<>% play_sound
    return(G)
  }

  ######################################################
  # You got assassinated by a subnano rabbit antibody

  if ((G$fx[index] >= (G$kx - 1L)) & (G$fx[index] <= G$kx + 2L) & (G$ftype[index] == 1L)) {
    G$cursor %<>% pos_at(0L, 20L)
    if (G$config$audio) G$splat %<>% play_sound()
    for (i in seq(99L, 0L)) {
      G$lives <- G$lives - 1L
      G$cursor %<>% cactuski_update_lives(G$lives, sample(c(9L,160L,196L,124L,88L))[1L])
      waitr::wait_for(5L)
    }
    G$cursor %<>% cactuski_update_lives(G$lives)
    waitr::wait_for(500L)

    if (G$config$audio) G$boom %<>% play_sound()
    G %<>% burst_into_flames(audio = FALSE, x = G$fx[index], y = G$fy[index],
      flame = c(196L,208L,220L,226L,190L,148L,106L,34L,0L))
    G$death_flag <- 1L
    return(G)
  }

  G
}

################################################################################
# Move the cactus laser blast of doom

cactuski_move_laser <- function(G, T) {
  if (!G$firing) {
    return(G)
  }
  if (G$ly > G$ky + 2L) {
    G$cursor %<>% write_at(G$lx, G$ly, " ")
  }
  G$ly <- G$ly + 1L
  G$lx <- G$lx + G$ldx

  # Laser hit something

  if ((G$ly %in% G$fy) || ((G$ly + 1L) %in% G$fy)) {
    index <- min(which(G$ly %in% G$fy)[1L], which((G$ly + 1L) %in% G$fy)[1L], na.rm = TRUE)
    if (G$factive[index] == 1L) {
      if (G$ftype[index] == 1L) {  # Baddy
        if ((G$lx == G$fx[index]) || (G$lx == G$fx[index] + 1L)) {
          G$cursor %<>% write_at(G$fx[index], G$fy[index], "  ")
          if (G$config$audio) G$squelch %<>% play_sound()
          G$factive[index] <- 0L
          G$ly <- 20L
          G$score <- G$score + 4200L
        }
      }
    }
  }

  # Laser hit wall

  if (G$wy != -1L) {
    if ((G$ly == G$wy) || (G$ly == G$wy -1L)) {
      if ((G$lx >= G$gapx) && (G$lx <= (G$gapx + G$gapwid))) {
        if (G$wtype > 2L) {
          G$ly <- 20L
          G$wtype <- 5L
          G$gapdx <- 0L
          if (G$config$audio) G$blip %<>% play_sound()
          G$score <- G$score + 4560L

        }
      } else {
        G$ly <- 20L
        if (G$config$audio) G$bump %<>% play_sound()
      }
    }
  }

  # Laser hit boss

  if ((G$bossy != -1L) && (G$bossh > 0)) {
    if ((G$ly == 18L) || (G$ly == 19L)) {
      if ((G$lx >= G$bossx) && (G$lx <= G$bossx + 2)) {
        G$ly <- 20L
        G$bosshfx <- 2
        G$bossh <- G$bossh - 1
        if (G$config$audio) {
          if (G$bossh > 0) G$bump %<>% play_sound()
          else {
            G$boom %<>% play_sound()
            G$bosshfx <- 10
          }
        }
      }
    }
  }

  # Laser off bottom

  if (G$ly >= 20L) {
    G$firing <- FALSE
    return(G)
  }

  # Laser hit edge

  y <- 1L + (T$top + G$ly)
  y <- ifelse(y > 20L, y - 20L, y)
  half_wid <- T$wid[y] / 2L
  dl <- T$mid[y] - (half_wid + 1L)
  dr <- T$mid[y] + (half_wid)

  if ((G$lx <= dl) || (G$lx >= dr)) {
    G$firing <- FALSE
    return(G)
  }

  G$cursor %<>% write_at(G$lx, G$ly,
    paste0(get_colour(sample(c(196L,203L,220L))[1L]), "@"))

  G
}

###############################################################################
# Move miscellaneous floaters

cactuski_move_floaters <- function(G) {
  # No work?
  if (length(G$factive) == 0L) {
    return(G)
  }

  # One floater per line (or perhaps not...).
  # Remove any that are off the top.

  while (G$fy[1] == -1L) {
    G$fx <- G$fx[-1L]
    G$fy <- G$fy[-1L]
    G$fdx <- G$fdx[-1L]
    G$fleftlimit <- G$fleftlimit[-1L]
    G$frightlimit <- G$frightlimit[-1L]
    G$ftype <- G$ftype[-1L]
    G$fstring <- G$fstring[-1L]
    G$factive <- G$factive[-1L]
    G$fspaces <- G$fspaces[-1L]
    if (length(G$factive) == 0L) {
      return(G)
    }
  }

  # Move all the others.

  for (i in seq_len(length(G$fy))) {
    if (G$factive[i] == 0) {
      G$fy[i] <- G$fy[i] - 1
      next
    }
    if (G$fy[i] < 20) {
      G$cursor %<>% write_at(G$fx[i], G$fy[i], G$fspaces[i])
    }

    G$fy[i] <- G$fy[i] - 1

    if (G$ftype[i] == 1) { # Baddy - do horizontal movement
      G$fx[i] <- G$fx[i] + G$fdx[i]
      G$fx[i] <- ifelse(G$fx[i] < G$fleft[i] + 1L, G$fleft[i] + 1L, G$fx[i])
      G$fx[i] <- ifelse(G$fx[i] > G$fright[i] - 2L, G$fright[i] - 2L, G$fx[i])

      if ((G$fx[i] == G$fleft[i] + 1L) | (G$fx[i] == G$fright[i] - 2L)) {
        G$fdx[i] <- (-G$fdx[i])
        if (G$config$audio) G$bbo %<>% play_sound
      }
    }

    if ((G$fy[i] >= 0L) & (G$factive[i] > 0L)) {
      if (G$ftype[i] == 1L) { # Baddy
        G$cursor %<>% write_at(G$fx[i], G$fy[i],
          paste0(get_colour(sample(c(196L, 214L, 226L), 1L)),
                 ifelse(G$fdx[i] == -2L, "<=", "=>")))
      } else {
        G$cursor %<>% write_at(G$fx[i], G$fy[i], G$fstring[i])
      }
    } else {

      # Off the top. Get some bonus.

      if (G$ftype[i] == 1L) { # Baddy
        G$score <- G$score + 628L

      } else if (G$ftype[i] == 2L) { # Hole
        G$score <- G$score + 314L

      } else if (G$ftype[i] == 6L) { # turd
        G$score <- G$score + 123L
      }
    }
  }
  G
}

cactuski_move_finch <- function(G, T) {
  G$cfalt <- ifelse(G$cfalt == 4L, 0L, G$cfalt + 1L)

  if (G$cfy < 20L) {
    G$cursor %<>% write_at(G$cfx, G$cfy, "   ")
  }

  G$cfr <- G$cfalt %% 2
  if (G$cfalt != 3L) G$cfy <- G$cfy - 1L

  if (G$cfy >= 0L) {
    yindex <- (T$top + G$cfy)
    yindex <- ifelse(yindex > 20L, yindex - 20L, yindex)
    G$cfx <- as.integer(T$left[yindex] + (G$cff * (T$right[yindex] - T$left[yindex])))
    G$cursor %<>% write_at(G$cfx, G$cfy, G$fch[1L + G$cfr])
  }

  if ((G$cfy == G$ky + 2) && (G$cfx >= G$kx - 1)  && (G$cfx <= G$kx + 1)) {
    G$cursor %<>% write_at(G$cfx, G$cfy, "   ")
    G$cfy <- (-1)
    if (G$config$audio) G$owfinch %<>% play_sound()
    G$lives <- G$lives - 10
  }
  G
}

cactuski_move_wall <- function(G) {
  if (G$wy < 20L) {
    left_wall <- paste0(rep(" ", G$gapx - G$wx), collapse = "")
    G$cursor %<>% write_at(G$wx, G$wy, left_wall)

    if (G$wtype %in% c(3, 4, 5)) {
      G$cursor %<>% write_at(G$gapx, G$wy, paste0(get_colour(196), paste(rep(" ", G$gapwid), collapse="")))
      if (G$wtype == 5) { # Barrier just got shot
        G$wtype <- 1
      }
    }

    right_wall <- paste0(rep(' ', (G$wx + G$wwid) - (G$gapx + G$gapwid)), collapse = "")
    G$cursor %<>% write_at(G$gapwid + G$gapx, G$wy, paste0(get_colour(248), right_wall))
  }

  G$wy <- G$wy - 1

  if (G$wy < 0) {
    G$score <- G$score + 456
    return(G)
  }

  if (G$gapdx !=0) {
    G$gapx <- G$gapx + G$gapdx
    if ((G$gapx + G$gapwid >= (G$wx + G$wwid - 1)) || (G$gapx <= (G$wx + 1))) {
      G$gapdx <- 0 - G$gapdx
    }
  }

  left_wall <- paste0(rep("#", G$gapx - G$wx), collapse = "")
  G$cursor %<>% write_at(G$wx, G$wy, paste0(get_colour(248), left_wall))

  if (G$wtype %in% c(3, 4)) {
    G$cursor %<>% write_at(G$gapx, G$wy, paste0(get_colour(196), paste(rep("X", G$gapwid), collapse="")))
  }

  right_wall <- paste0(rep('#', (G$wx + G$wwid) - (G$gapx + G$gapwid)), collapse = "")
  G$cursor %<>% write_at(G$gapwid + G$gapx, G$wy, paste0(get_colour(248), right_wall))

  G
}

colour_kenny <- function(G) {
  cl <- if (!G$brownleft) get_colour(28) else get_colour(94)
  cm <- if (!G$brownmid) get_colour(46) else get_colour(11)
  cr <- if (!G$brownright) get_colour(28) else get_colour(94)
  skil <- if (!G$brownleft) get_colour(51) else get_colour(11)
  skir <- if (!G$brownright) get_colour(51) else get_colour(11)

  G$body <- paste0(cl, "<", cm, "%", cr, ">")
  G$body_s <- paste0(G$body, " ")
  G$s_body <- paste0(" ", G$body)
  G$skidown <- paste0(skil,"| ", skir, "|")
  G$skileft <- paste0(skil, "/ ", skir, "/ ")
  G$skiright <- paste0(skil, " \\ ", skir, "\\")
  G$jskidown <- paste0(skil, "' ", skir, "'")
  G$jskileft <- paste0(skil, "' ", skir, "' ")
  G$jskiright <- paste0(skil, " ' ", skir, "'")
  G
}

cactuski_end <- function(G) {
  if (G$config$audio) G$win %<>% play_sound()
  G$cursor %<>% write_at(1, 21, paste0(get_colour(178),
    "*", get_colour(255), " You made it to the Giant Cactus HQ ",
    get_colour(178), "*"))
  G$cursor %<>% write_at(1, 22, paste0(get_colour(178),
    "*", get_colour(255),"   The cactus community is saved!   ",
    get_colour(178), "*"))

  gchq <- readLines(pkg_file("gfx/cactuski-gchq.txt"))
  if (G$ly > 0) {
    G$cursor %<>% write_at(G$lx, G$ly, " ")
  }
  for (j in 19:0) {
    line <- 1
    time <- waitr::waitr_timestamp()
    for (i in j:19) {
      G$cursor %<>% write_at(0, i, gchq[line])
      line <- line + 1
    }
    if ((G$ky + 3) >= 0) G$cursor %<>% write_at(G$kx, G$ky + 3, "   ")
    G$ky <- G$ky - 1
    if ((G$ky + 3) >= 0) G$cursor %<>% write_at(G$kx, G$ky + 3, G$skidown)
    if ((G$ky + 2) >= 0) G$cursor %<>% write_at(G$kx, G$ky + 2, G$body)
    if ((G$ky + 1) >= 0) G$cursor %<>% write_at(G$kx, G$ky + 1, G$body)
    if (G$ky >= 1) G$cursor %<>% write_at(G$kx, G$ky, G$body)
    waitr::wait_until(time + 100)
  }
  dotsx <- c(3, 2, 1, 0, 1, 2, 3, NA, NA, NA, NA,
             6, 6, 6, 6, 6, NA, NA,
             8, 8, 8, 8, 8, NA, NA,
             5, 6, 7, 8, 9, NA, NA,
             5, 6, 7 ,8, 9, NA, NA, NA, NA,
             11, 12, 13, 14, 13, 12, 11)

  dotsy <- c(0, 1, 2, 3, 4, 5, 6, NA, NA, NA, NA,
             1, 2, 3, 4, 5, NA, NA,
             1, 2, 3 ,4 ,5, NA, NA,
             2, 2, 2, 2, 2, NA, NA,
             4, 4, 4, 4, 4, NA, NA, NA, NA,
             0, 1, 2, 3, 4, 5, 6)

  dotsx <- dotsx + 22
  dotsy <- dotsy + 5
  waitr::wait_for(2000)
  for (i in seq_len(length(dotsx))) {
    col <- sample(c(get_colour(10), get_colour(46), get_colour(154),
                    get_colour(118)), 1)
    if (!is.na(dotsx[i])) G$cursor %<>% write_at(dotsx[i], dotsy[i], paste0(col, "#"))
    waitr::wait_for(30)
  }
  waitr::wait_for(3000)
  G$cursor %<>% clear_pic(23)
  G$gameover <- TRUE
  return(G)

}

cactuski <- function(G = NA) {
  options(warn=2)

  # Would be nice if these are constants...
  # Anyway, pretty colours for the left and right side of the track.

  right_track <- c(
    rep("<", 50),
    paste0(get_colour(c(231,230,229,228,227,226,220,214,208,202)), "<"))

  left_track <- c(
    paste0(get_colour(c(202,208,214,220,226,227,228,229,230,231,222)), ">"),
    rep(">", 49))



  # Get rid of these two lines later... Just for running single file

  #cat("\033[2J\033[;H")
  #G <- list(cursor = draw_tv_screen(60L, 23L),
  #          tv_width = 60L, tv_height = 23L)

  set_colour(15)
  cursor_off()

  # Everything about the track position and width

  T <- list()
  T %<>% cactuski_init_track()

  # Everything about the player...

  G$jumping <- 0
  G$firing <- FALSE
  G$lx <- 0L
  G$ly <- 0L
  G$ldx <- 0L

  G$cursor %<>% cactuski_setup(T$wid_zero)
  G$kx <- 30L
  G$ky <- 0L
  G$kdx <- 0L
  G$kdy <- 0L
  G$init <- TRUE
  G$brownleft <- FALSE
  G$brownright <- FALSE
  G$brownmid <- FALSE

  G %<>% colour_kenny()

  G %<>% cactuski_restart(T)

  G$thin <- 0L

  #G$config <- list()
  #G$config$audio <- TRUE
  G$score <- 0
  G$lives <- 300

  # The sounds
  G$win <- load_sound(pkg_file("audio/cactuski-win.wav"), G$config)
  G$pew <- load_sound(pkg_file("audio/cactuski-pew.wav"), G$config)
  G$squelch <- load_sound(pkg_file("audio/cactuski-squelch.wav"), G$config)
  G$fall <- load_sound(pkg_file("audio/cactz-drop.wav"), G$config)
  G$splat <- load_sound(pkg_file("audio/cactuski-splat.wav"), G$config)
  G$elec <- load_sound(pkg_file("audio/cactuski-elec.wav"), G$config)
  G$bgo <- load_sound(pkg_file("audio/cactuski-baddiego.wav"), G$config)
  G$bbo <- load_sound(pkg_file("audio/cactuski-baddiebounce.wav"), G$config)
  G$boom <- load_sound(pkg_file("audio/cactuski-boom.wav"), G$config)
  G$bump <- load_sound(pkg_file("audio/cactuski-bump.wav"), G$config)
  G$blip <- load_sound(pkg_file("audio/cactuski-blip.wav"), G$config)
  G$eat1 <- load_sound(pkg_file("audio/cactuski-eat1.wav"), G$config)
  G$eat2 <- load_sound(pkg_file("audio/cactuski-eat2.wav"), G$config)
  G$ding <- load_sound(pkg_file("audio/cactuski-ding.wav"), G$config)
  G$buzz <- load_sound(pkg_file("audio/cactuski-buzz.wav"), G$config)
  G$tel1 <- load_sound(pkg_file("audio/cactuski-tele1.wav"), G$config)
  G$tel2 <- load_sound(pkg_file("audio/cactuski-tele2.wav"), G$config)
  G$whoo <- load_sound(pkg_file("audio/cactuski-whoosh.wav"), G$config)
  G$ouch <- load_sound(pkg_file("audio/cactuski-ouch.wav"), G$config)
  G$owfinch <- load_sound(pkg_file("audio/cactuski-ow.wav"), G$config)
  G$finch <- load_sound(pkg_file("audio/cactuski-finch.wav"), G$config)

  G$msgs <- "$082CACT-EYES COMMS v0.0.0.0.BETA Booting."
  G$msg_col <- 15
  G$msg_pointer <- 1
  G$msg_x <- 1

  # Predicting the future...

  G$mscript <- read.csv(pkg_file("other/cactuski-scripts.csv"))
  G$mline <- NA
  G$mn <- 0
  G$mc <- 0
  G$mcgap <- 0
  G$mp <- 0
  G$mpgap <- 10

  # Bosses...

  G$bossy <- -1
  G$bossx <- 0
  G$bosst <- 1
  G$bossfreq <- 0
  G$bossgap <- 0
  G$bosshfx <- 0
  G$bossh <- 0
  G$gameover <- FALSE

  MSGS <- read.csv(pkg_file("other/cactuski-msgs.csv"))

  next_frame <- waitr::waitr_timestamp() + 30

  while (TRUE) {
    G %<>% cactuski_check_keys(T)
    G %<>% cactuski_update_future(T, MSGS)
    if (G$gameover) break
    G %<>% cactuski_move_floaters
    if (G$wy != -1) {
      G %<>% cactuski_move_wall
    }
    G$cursor %<>% cactuski_scroll(T, left_track, right_track)
    G %<>% cactuski_death_check(T)
    if (G$death_flag == 1) {
      waitr::wait_for(500)
      if (G$lives > 0) {
        G$cursor %<>% clear_pic(20)
        G %<>% cactuski_restart(T)
        T %<>% cactuski_init_track()
      } else {
        G %<>% show_pic(pkg_file("gfx/cactuski-go.txt"), "up")
        waitr::wait_for(1000)

        G$msgs <- c(paste0("$021.$027.$033.$039.$045.$051.$050.$049.$048. ",
                  "$226You killed Kenny!! $048.$049.$050.$051.$045.",
                  "$039.$033.$027.$021."),
                  paste0("$237.$238.$239.$240.$241.$242.$243.$244.$245.$246.$247.",
                         "$248.$249.$250.$251.$252.$253.$254.$255.",
                         "$255.$254.$253.$252.$251.$250.$249.$248.$247.$246.",
                         "$245.$244.$243.$242.$241.$240.$239.$238.$237."))
        G$msg_pointer <- 1
        G$msg_x <- 1
        while ((length(G$msgs) > 1) || (G$msg_x < 39)) {
          G %<>% cactuski_do_messages()
          waitr::wait_for(15)
        }

        waitr::wait_for(3000)
        G$cursor %<>% clear_pic(23)
        break
      }
      next
    }
    G %<>% cactuski_move_kenny

    if (G$thin != 0) {
      T$wid_zero <- T$wid_zero - G$thin
      if (T$wid_zero < 10) T$wid_zero <- 10
      G$thin <- 0
    }

    T %<>% cactuski_wiggle_track
    G %<>% cactuski_move_laser(T)
    if (G$cfy != -1) {
      G %<>% cactuski_move_finch(T)
    }
    if (G$bossy != -1) {
      G %<>% cactuski_do_bosses(T)
    }
    next_frame <- waitr::wait_until(next_frame) + 25
    G %<>% cactuski_move_laser(T)
    if (G$cfy != -1) {
      G %<>% cactuski_move_finch(T)
    }
    next_frame <- waitr::wait_until(next_frame) + 25
    G$score <- G$score + 7
    G %<>% cactuski_do_messages()
    G$cursor %<>% cactuski_update_score(G$score)
    G$cursor %<>% cactuski_update_lives(G$lives)
    G %<>% cactuski_do_messages()
  }

  # Game is now over

  G$score <- formatC(G$score/100, format = "f", digits = 2, flag = "0")
  if (good_score(user_file("cactuski-hs.csv.xz"), G$score)) {
    G %<>% fade_text(30, 16, "GREAT SCORE! TYPE YOUR NAME", UNICORN,
                     FADE_IN, triple = TRUE)
    G %<>% fade_text(25, 19, "..........", GREY_SCALE, FADE_IN, align = LEFT)
    G %<>% get_input(25, 19, 10)
    insert_score(user_file("cactuski-hs.csv.xz"), G$text_input, G$score)
  }
  G$cursor %<>% clear_pic(23)
  return(G)
}

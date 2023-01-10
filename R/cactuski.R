cactuski <- function(cursor, config, TV_HEIGHT = 23, TV_WIDTH = 60) {
  options(warn=2)

  G <- new.env(parent = emptyenv())
  G$cursor <- cursor
  G$config <- config
  G$tv_height <- TV_HEIGHT
  G$tv_width <- TV_WIDTH
  G$CC <- lapply(1:255, get_colour)

  G$right_track <- c(
    rep("<", 50),
    paste0(get_colour(c(231,230,229,228,227,226,220,214,208,202)), "<"))

  G$left_track <- c(
    paste0(get_colour(c(202,208,214,220,226,227,228,229,230,231,222)), ">"),
    rep(">", 49))

  G$bosses <- list(
    c(paste0(G$CC[[246]], "O", G$CC[[83]], "#", G$CC[[246]], "O"),
      paste0(G$CC[[246]], "o", G$CC[[83]], "#", G$CC[[246]], "o"),
      paste0(G$CC[[246]], "O", G$CC[[40]], "U", G$CC[[246]], "O"),
      paste0(G$CC[[246]], "o", G$CC[[40]], "U", G$CC[[246]], "o")),

    c(paste0(G$CC[[214]], "/", G$CC[[123]], "@", G$CC[[214]], "\\"),
      paste0(G$CC[[178]], "-", G$CC[[87]], "@", G$CC[[178]], "-"),
      paste0(G$CC[[216]], "/", G$CC[[147]], "V", G$CC[[216]], "\\"),
      paste0(G$CC[[180]], "-", G$CC[[183]], "v", G$CC[[180]], "-")),

    c(paste0(G$CC[[255]], "|", G$CC[[196]], "^", G$CC[[255]], "|"),
      paste0(G$CC[[148]], "¦", G$CC[[226]], "^", G$CC[[248]], "¦"),
      paste0(G$CC[[216]], "/", G$CC[[208]], "@", G$CC[[216]], "\\"),
      paste0(G$CC[[180]], "/", G$CC[[196]], "O", G$CC[[180]], "\\")),

    c(paste0(G$CC[[195]], "x", G$CC[[226]], "?", G$CC[[159]], "+"),
      paste0(G$CC[[87]], "+", G$CC[[220]], "!", G$CC[[117]], "x"),
      paste0(G$CC[[117]], "+", G$CC[[214]], "!", G$CC[[87]], "x"),
      paste0(G$CC[[159]], "x", G$CC[[178]], "?", G$CC[[195]], "+")))

  G$fch <- c(paste0(G$CC[[228]], "^", G$CC[[172]], "8", G$CC[[228]], "^"),
             paste0(G$CC[[226]], "v", G$CC[[172]], "8", G$CC[[226]], "v"))

  set_colour(15)

  cactuski_colour_kenny <- function() {
    cl <- G$CC[[if (!G$brownleft) 28 else 94]]
    cm <- G$CC[[if (!G$brownmid) 46 else 11]]
    cr <- G$CC[[if (!G$brownright) 28 else 94]]
    skil <- G$CC[[if (!G$brownleft) 51 else 11]]
    skir <- G$CC[[if (!G$brownright) 51 else 11]]

    G$body <- paste0(cl, "<", cm, "%", cr, ">")
    G$body_s <- paste0(G$body, " ")
    G$s_body <- paste0(" ", G$body)
    G$skidown <- paste0(skil,"| ", skir, "|")
    G$skileft <- paste0(skil, "/ ", skir, "/ ")
    G$skiright <- paste0(skil, " \\ ", skir, "\\")
    G$jskidown <- paste0(skil, "' ", skir, "'")
    G$jskileft <- paste0(skil, "' ", skir, "' ")
    G$jskiright <- paste0(skil, " ' ", skir, "'")
    G$lskihole <- paste0(skil, "\\ ")
    G$rskihole <- paste0(skir, "/")
    G$wallcoll <- paste0(cl, "<")
    G$wallcolm <- paste0(cm, "%")
    G$wallcolr <- paste0(cr, ">")
  }

  ##############################################################################
  # Draw initial track

  cactuski_setup <- function(wid = 28) {
    cols <- get_colour(c(202,208,214,220,226,227,228,229,230,231,222))
    space <- 37 - wid
    str_start <- paste0(paste(paste0(cols, ">"), collapse=""),
                        strrep(">", space %/% 2),"|", strrep(" ",wid), "|",
                        strrep("<", space %/% 2),
                        paste(paste0(rev(cols), "<"), collapse=""))

    for (y in 0:19) {
      G$cursor <- write_at(G$cursor, 0, y, str_start)
    }

    set_colour(244)
    G$cursor <- write_at(G$cursor, 0L,  20L, paste(rep("#", 60), collapse=""))
    G$cursor <- write_at(G$cursor, 42L, 21L, "SCORE: ", 33)
    G$cursor <- write_at(G$cursor, 42L, 22L, "LIVES: ", 37)
    G$cursor <- write_at(G$cursor, 49L, 22L, "0000003.00", 49)
    G$cursor <- write_at(G$cursor, 40L, 21L, "#", 244)
    G$cursor <- write_at(G$cursor, 40L, 22L, "#", 244)
  }

  ##############################################################################
  # (Re)-start - initialise all the clutter

  cactuski_restart <- function() {
    while (!is.na(keypress::keypress(block = FALSE))) {}
    G$cursor <- cactuski_setup(G$T$wid_zero)
    G$ky <- 0L
    yindex <- ifelse(G$T$top == 1, 19, G$T$top - 1)
    G$kx <- 30L
    G$kdx <- 0L
    G$kdy <- 0L
    G$death_flag <- 0
    G$brownleft <- FALSE
    G$brownright <- FALSE
    G$brownmid <- FALSE
    cactuski_colour_kenny()

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
  }

  ##############################################################################
  # Set up initial track (maths)

  cactuski_init_track <- function(T = list()) {
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

  G$T <- cactuski_init_track()

  # Everything about the player...

  G$jumping <- 0
  G$firing <- FALSE
  G$lx <- 0L
  G$ly <- 0L
  G$ldx <- 0L

  cactuski_setup(G$T$wid_zero)
  G$kx <- 30L
  G$ky <- 0L
  G$kdx <- 0L
  G$kdy <- 0L
  G$init <- TRUE
  G$brownleft <- FALSE
  G$brownright <- FALSE
  G$brownmid <- FALSE

  cactuski_colour_kenny()
  cactuski_restart()

  G$thin <- 0L

  #G$config <- list()
  #G$config$audio <- TRUE
  G$score <- 0
  G$lives <- 300

  # The sounds

  G$win <- load_sound(pkg_file("audio/cactuski-win.wav"), config)
  G$pew <- load_sound(pkg_file("audio/cactuski-pew.wav"), config)
  G$squelch <- load_sound(pkg_file("audio/cactuski-squelch.wav"), config)
  G$fall <- load_sound(pkg_file("audio/cactz-drop.wav"), config)
  G$splat <- load_sound(pkg_file("audio/cactuski-splat.wav"), config)
  G$elec <- load_sound(pkg_file("audio/cactuski-elec.wav"), config)
  G$bgo <- load_sound(pkg_file("audio/cactuski-baddiego.wav"), config)
  G$bbo <- load_sound(pkg_file("audio/cactuski-baddiebounce.wav"), config)
  G$boing <- load_sound(pkg_file("audio/cactuski-boing.wav"), config)
  G$boom <- load_sound(pkg_file("audio/cactuski-boom.wav"), config)
  G$bump <- load_sound(pkg_file("audio/cactuski-bump.wav"), config)
  G$blip <- load_sound(pkg_file("audio/cactuski-blip.wav"), config)
  G$eat1 <- load_sound(pkg_file("audio/cactuski-eat1.wav"), config)
  G$eat2 <- load_sound(pkg_file("audio/cactuski-eat2.wav"), config)
  G$ding <- load_sound(pkg_file("audio/cactuski-ding.wav"), config)
  G$buzz <- load_sound(pkg_file("audio/cactuski-buzz.wav"), config)
  G$tel1 <- load_sound(pkg_file("audio/cactuski-tele1.wav"), config)
  G$tel2 <- load_sound(pkg_file("audio/cactuski-tele2.wav"), config)
  G$whoo <- load_sound(pkg_file("audio/cactuski-whoosh.wav"), config)
  G$ouch <- load_sound(pkg_file("audio/cactuski-ouch.wav"), config)
  G$owfinch <- load_sound(pkg_file("audio/cactuski-ow.wav"), config)
  G$finch <- load_sound(pkg_file("audio/cactuski-finch.wav"), config)

  G$config <- config

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


###############################################################################
# Explosion - very similar to the one in cactz.R...

  cactuski_burst_into_flames <- function(x, y) {
    if (G$config$audio) G$boom <- play_sound(G$boom)

    set_colour(15)

    flame = c(196L, 208L, 220L, 226L, 190L, 148L, 106L, 34L, 0L)

    rings_x <- list(0,
                  c(-1, 1, 0, 0),
                  c(-3,-2,-1,1,2,3,1,-1),
                  c(-4,-3,-2,-1,0,1,2,3,4,3,2,1,0,-1,-2,-3),
                  c(-5,-4,-3,-2,-1,0,1,2,3,4,5,4,3,2,1,0,-1,-2,-3,-4),
                  c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,5,4,3,2,1,0,-1,-2,-3,-4,-5),
                  c(-7,-6,-5,-4,-2,2,4,5,6,7,6,5,4,2,-2,-4,-5,-6),
                  c(-7,-6,-5,-3,3,5,6,7,7,6,5,3,-3,-5,-6,-7))


    rings_y <- list(0,
                  c(0,0,-1,1),
                  c(0,0,-1,-1,0,0,1,1),
                  c(0,-1,-1,-2,-2,-2,-1,-1,0,1,1,2,2,2,1,1),
                  c(0,-1,-2,-2,-3,-3,-3,-2,-2,-1,0,1,2,2,3,3,3,2,2,1),
                  c(0,-1,-2,-3,-3,-4,-4,-4,-3,-3,-2,-1,0,1,2,3,3,4,4,4,3,3,2,1),
                  c(0,-1,-2,-3,-4,-4,-3,-2,-1,0,1,2,3,4,4,3,2,1),
                  c(-1,-2,-3,-4,-4,-3,-2,-1,1,2,3,4,4,3,2,1))

    draw_ring <- function(xs, ys, col) {
      for (i in seq_along(xs)) {

        if ((x + xs[i] >= 0) && (x + xs[i] < G$tv_width) &&
            (y + ys[i] >= 0) && (y + ys[i] < (G$tv_height - 2))) {

          G$cursor <- pos_at(G$cursor, x + xs[i], y + ys[i])
          set_colour(col)
          r <- 1 + as.integer(5 * stats::runif(1))
          chs <- c("@","o","O","&","#")
          G$cursor <- write(G$cursor, chs[r])
        }
      }
    }

    next_frame <- waitr::waitr_timestamp() + 50
    for (i in 1:8) {
      draw_ring(rings_x[[i]], rings_y[[i]], flame[i])
      next_frame <- waitr::wait_until(next_frame) + 50
    }

    for (i in 2:16) {
      if (i <= 8) draw_ring(rings_x[[i]], rings_y[[i]], flame[1])
      fprev <- i - 1
      fnext <- i + 1

      for (fno in 2:9) {
        if (fno <= 9) {
          if ((fprev >= 1) && (fprev <= 8))
            draw_ring(rings_x[[fprev]], rings_y[[fprev]], flame[fno])
          if (fnext <= 8)
            draw_ring(rings_x[[fnext]], rings_y[[fnext]], flame[fno])
        }
        fprev <- fprev - 1
        fnext <- fnext + 1
      }
      next_frame <- waitr::wait_until(next_frame) + 50
    }

    set_colour(15)
    G$cursor <- pos_at(G$cursor, 0, G$tv_height + 1)
  }

################################################################################
# Update which baddies are coming next...

  cactuski_incoming_thing <- function(kp, px = NULL) {
    G$factive <- c(G$factive, 1)
    G$fleftlimit <- c(G$fleftlimit, 0)
    G$frightlimit <- c(G$frightlimit, 0)
    G$fdx <- c(G$fdx, 0)
    yindex <- ifelse(G$T$top == 1, 19, G$T$top - 1)
    G$fy <- c(G$fy, 20)
    G$fspaces <- c(G$fspaces, "   ")

    if (kp == "h") {
      G$ftype <- c(G$ftype, 2)
      G$fstring <- c(G$fstring, paste0(G$CC[[145]], "\\_/"))
      if (is.null(px)) {
        px <- sample((G$T$left[yindex] + 3):(G$T$right[yindex] - 3), 1)
      }
      G$fx <- c(G$fx, px)

    } else if (kp == "f") {
      G$ftype <- c(G$ftype, 5)
      G$fstring <- c(G$fstring, paste0(G$CC[[82]], "<%>"))
      if (is.null(px)) {
        px <- sample((G$T$left[yindex] + 3):(G$T$right[yindex] - 3), 1)
      }
      G$fx <- c(G$fx, px)

    } else if (kp == "s") {
      if (is.null(px)) {
        mid <- ((G$T$left[yindex] + 1) + (G$T$right[yindex] - 3)) %/% 2
        px <- sample((mid - 3):(mid + 3), 1)
      }
      G$fx <- c(G$fx, px)
      G$flipper <- 1 - G$flipper
      newtype <- ifelse(G$flipper == 1, 3, 4)
      G$ftype <- c(G$ftype, newtype)
      G$fstring <- c(G$fstring,
                   paste0(G$CC[[117]], ifelse(newtype == 3, "<--", "-->")))

    } else if (kp == "t") {
      G$ftype <- c(G$ftype, 6L)
      G$fstring <- c(G$fstring, paste0(G$CC[[64]], "o",
                     G$CC[[11]], "O", G$CC[[64]], "o"))
      if (is.null(px)) {
        px <- sample((G$T$left[yindex] + 3):(G$T$right[yindex] - 3), 1)
      }
      G$fx <- c(G$fx, px)
    }
    invisible()
  }

  cactuski_incoming_baddy <- function(px = NULL) {
    G$fy <- c(G$fy, 20)
    G$ftype <- c(G$ftype, 1)
    G$fstring <- c(G$fstring, "")
    G$fspaces <- c(G$fspaces, "  ")
    G$factive <- c(G$factive, 1)
    yindex <- if (G$T$top == 1) 19 else (G$T$top - 1)
    G$fleftlimit <- c(G$fleftlimit, G$T$left[yindex])
    G$frightlimit <- c(G$frightlimit, G$T$right[yindex])
    G$fdx <- c(G$fdx, sample(c(2,-2), 1))
    if (is.null(px)) {
      px <- sample((G$T$left[yindex] + 4):(G$T$right[yindex] - 4), 1)
    }
    G$fx <- c(G$fx, px)
    if (G$config$audio) G$bgo <- play_sound(G$bgo)
  }

  cactuski_incoming_wall <- function(wtype) {
    if (G$wy != -1) return()
    G$wy <- 20
    yindex <- ifelse(G$T$top == 1, 19, G$T$top - 1)
    G$wx <- G$T$left[yindex] + 1
    G$wwid <- (G$T$right[yindex] - G$wx)
    if (wtype == 5) wtype <- sample(1:4, 1)
    G$wtype <- wtype
    G$gapwid <- max(6, as.integer(G$wwid / 2.5))
    G$gapx <- sample((G$wx + 2):((G$wx+G$wwid)-(G$gapwid + 2)))[1]
    G$gapdx <- ifelse(G$wtype %in% c(1, 3), 0, sample(c(-1, 1))[1])
  }

  cactuski_incoming_boss <- function(info) {
    if (G$bossy != -1) return()
    info <- as.integer(strsplit(info, ";")[[1]])
    G$bossy <- 18
    yindex <- ifelse(G$T$top == 1, 19, G$T$top - 1)
    G$bossx <- sample((G$T$left[yindex] + 4):(G$T$right[yindex] - 4), 1)
    G$bosst <- info[1]
    G$bosshfx <- 0
    G$bossh <- info[2]
    G$bossfreq <- info[3]
    G$bossgap <- info[3]
  }

  cactuski_incoming_finch <- function(px = NULL) {
    if (G$cfy != -1L) return()
    G$cfy <- 19L
    G$cfr <- 1L
    yindex <- ifelse(G$T$top == 1L, 19L, G$T$top - 1L)
    G$cff <- sample(c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), 1L)
    if (is.null(px)) {
      px <- as.integer(G$T$left[yindex] +
                         (G$cff * (G$T$right[yindex] - G$T$left[yindex])))
    }
    G$cfx <- px
    if (G$config$audio) G$finch <- play_sound(G$finch)
  }

  cactuski_update_future <- function(MSGS) {
    extras <- function() {
      if (length(G$mline) < 2) return()
      prob <- runif(6)
      if (prob[1] > G$mline$p_hole) prob[1] <- 0
      if (prob[2] > G$mline$p_food) prob[2] <- 0
      if (prob[3] > G$mline$p_baddy) prob[3] <- 0
      if (prob[4] > G$mline$p_slalom) prob[4] <- 0
      if (prob[5] > G$mline$p_finch) prob[5] <- 0
      if (prob[6] > G$mline$p_camel) prob[6] <- 0
      if (any(prob > 0)) {
        i <- which(prob == max(prob))[1]
        if (i == 1) cactuski_incoming_thing("h")
        else if (i == 2) cactuski_incoming_thing("f")
        else if (i == 3) cactuski_incoming_baddy()
        else if (i == 4) cactuski_incoming_thing("s")
        else if (i == 5) cactuski_incoming_finch()
        else if (i == 6) cactuski_incoming_thing("t")
      }
      invisible()
    }

  # Negative phase (with catch for first time)
  # - ready for next bit of future...

    if (is.na(G$mp)) return()

  # Pause counting while there's a boss
  # (So msgs must be scripted prior to the boss script line)

    if (G$bossy != -1) return()

    if (G$mp < 0) {
      G$mn <- G$mn + 1
      G$mline <- G$mscript[G$mn, ]
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
      return()
    }

  # Otherwise... reduce phase-gap...

    if (G$mpgap > 0) {
      G$mpgap <- G$mpgap - 1
      return(extras())
    }

  # now handle cluster gap

    if (G$mcgap > 0) {
      G$mcgap <- G$mcgap - 1
      return(extras())
    }

  # So phase-gap = 0, and cluster-gap = 0.
  # Lay down one of a cluster.

    if (G$mc > 0) {
      G$mc <- G$mc - 1
      G$mcgap <- G$mline$clust_gap
      if (G$mline$type %in% c("MSG", "PAU")) return()
      else if (G$mline$type == "HOL") cactuski_incoming_thing("h")
      else if (G$mline$type == "FOO") cactuski_incoming_thing("f")
      else if (G$mline$type == "BAD") cactuski_incoming_baddy()
      else if (G$mline$type == "SLA") cactuski_incoming_thing("s")
      else if (G$mline$type == "CAM") cactuski_incoming_thing("t")
      else if (G$mline$type == "WAL") cactuski_incoming_wall(G$mline$extra)
      else if (G$mline$type == "BOS") cactuski_incoming_boss(G$mline$extra)
      else if (G$mline$type == "FIN") cactuski_incoming_finch()
      else if (G$mline$type == "THI") G$thin <- 2
      else if (G$mline$type == "FAT") G$thin <- (-2)
      else if (G$mline$type == "END") cactuski_end()
      return()
    }

  # End of cluster

    G$mp <- G$mp - 1
    if (G$mp == 0) G$mp <- (-1)
    if (length(G$mline) < 2) return()

    G$mpgap <- G$mline$phase_gap
    G$mc <- G$mline$clust_size
    G$mcgap <- G$mline$clust_gap

    invisible()
  }

###############################################################################
# Handle bosses

  cactuski_do_bosses <- function() {
    if (G$bossy == -1) return()

    if ((G$bosshfx == 0) && (G$bossh == 0)) {
      G$cursor <- write_at(G$cursor, G$bossx, 18, "   ")
      G$cursor <- write_at(G$cursor, G$bossx, 19, "   ")
      G$bossy <- -1
      return()
    }

    if (G$bosshfx == 0) {
      toprow <- G$bosses[[G$bosst]][(G$T$top %% 2) + 1]
      bottomrow <- G$bosses[[G$bosst]][(G$T$top %% 2) + 3]

    } else {
      excol <- unlist(lapply(
        sample(c(196, 202, 208, 214, 220, 226)), function(x) G$CC[[x]]))

      char1 <- sample(c("#", "@"))[1]
      char2 <- sample(c("#", "@"))[1]

      toprow <- paste0(excol[1], "/", excol[2], char1, excol[3], "\\")
      bottomrow <- paste0(excol[4], "\\", excol[5], char2, excol[6], "/")
      G$bosshfx <- G$bosshfx - 1
    }

  # Move x/y

    yindex1 <- G$T$top - 2
    yindex2 <- G$T$top - 1
    if (yindex1 < 1) yindex1 <- yindex1 + 19
    if (yindex2 < 1) yindex2 <- yindex2 + 19

    if ((G$bossx < G$T$left[yindex1] + 1) ||
        (G$bossx < G$T$left[yindex2] + 1)) {
      G$cursor <- write_at(G$cursor, G$bossx, 18, "   ")
      G$cursor <- write_at(G$cursor, G$bossx, 19, "   ")
      G$bossx <- max(G$T$left[yindex1] + 1, G$T$left[yindex2] + 1)
      G$cursor <- write_at(G$cursor, G$bossx, 18, toprow)
      G$cursor <- write_at(G$cursor, G$bossx, 19, bottomrow)

    } else if ((G$bossx > G$T$right[yindex1] - 3)  ||
               (G$bossx > G$T$right[yindex2] - 3)) {
      G$cursor <- write_at(G$cursor, G$bossx, 18, "   ")
      G$cursor <- write_at(G$cursor, G$bossx, 19, "   ")
      G$bossx <- min(G$T$right[yindex1] - 3, G$T$right[yindex2]  - 3)
      G$cursor <- write_at(G$cursor, G$bossx, 18, toprow)
      G$cursor <- write_at(G$cursor, G$bossx, 19, bottomrow)

    } else {

      dx <- runif(1)
      if ((dx <= 0.33) && (G$bossx > G$T$left[yindex1] + 2) &&
                          (G$bossx > G$T$left[yindex2] + 2))
        dx <- (-1L)

      else if ((dx >= 0.66) && (G$bossx < G$T$right[yindex1] - 4) &&
                               (G$bossx < G$T$right[yindex2] - 4))
        dx <- (1L)

      else dx <- 0L

      pre <- if (dx == 1L) " " else ""
      post <- if (dx == -1L) " " else ""

      if (dx == -1L) G$bossx <- G$bossx + dx

      G$cursor <- write_at(G$cursor, G$bossx, 18, paste0(pre, toprow, post))
      G$cursor <- write_at(G$cursor, G$bossx, 19, paste0(pre, bottomrow, post))

      if (dx == 1L) G$bossx <- G$bossx + dx
    }

    G$bossgap <- G$bossgap - 1
    if (G$bossgap == 0) {
      G$bossgap <- G$bossfreq
      if (G$bosst == 1) cactuski_incoming_thing("h", G$bossx)
      else if (G$bosst == 2) cactuski_incoming_finch(G$bossx)
      else if (G$bosst == 3) cactuski_incoming_baddy(G$bossx)
      else {
        gs <- sample(1:6, 1)
        if (gs == 1) cactuski_incoming_thing("h", G$bossx)
        else if (gs == 2) cactuski_incoming_finch(G$bossx)
        else if (gs == 3) cactuski_incoming_baddy(G$bossx)
        else if (gs == 4) cactuski_incoming_thing("s", G$bossx)
        else if (gs == 5) cactuski_incoming_thing("f", G$bossx)
        else if (gs == 6) cactuski_incoming_thing("t", G$bossx)
      }
    }
  }


###############################################################################
# Handle teleprinter

  cactuski_do_messages <- function() {

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
      if (length(G$msgs) == 1) return()

      msg <- replace_cols(gsub("#", "", G$msgs[1]))

      G$cursor <- write_at(G$cursor, 1, 21, msg)
      G$cursor <- write_at(G$cursor, 1, 22, "                                      ")
      G$msgs <- G$msgs[-1]
      G$msg_pointer <- 1
      G$msg_x <- 1
    }

    char <- substr(G$msgs[1], G$msg_pointer, G$msg_pointer)

    if (char == '$') {
      col <- as.integer(substr(G$msgs[1], G$msg_pointer + 1, G$msg_pointer + 3))
      G$msg_col <- G$CC[[col]]
      G$msg_pointer <- G$msg_pointer + 4
      char <- substr(G$msgs[1], G$msg_pointer, G$msg_pointer)
    }

    if (char == '#') {
      char <- replace_cols(substring(G$msgs[1], G$msg_pointer + 1))
      G$msg_pointer <- nchar(G$msgs[1])

    }

    G$cursor <- write_at(G$cursor, G$msg_x, 22, paste0(G$msg_col, char))
    if (G$config$audio) {
      if (char != " ") {
        if (runif(1) > 0.7) {
          if (runif(1) > 0.25) {
            G$tel1 <- play_sound(G$tel1)
          } else {
            G$tel2 <- play_sound(G$tel2)
          }
        }
      }
    }
    G$msg_pointer <- G$msg_pointer + 1
    G$msg_x <- G$msg_x + 1
  }

  cactuski_update_score <- function() {
    tscore <- formatC(G$score/100, width = 10, format = "f",
                      digits = 2, flag = "0")
    G$cursor <- write_at(G$cursor, 49, 21, tscore, 51)
  }

  cactuski_update_lives <- function(col = 49) {
    if (G$lives <= 0) {
      G$cursor <- write_at(G$cursor, 49, 22,
        "        NA", sample(c(196, 160, 124, 88, 52), 1))
      return()
    }
    tlives <- formatC(G$lives/100, width = 10, format = "f",
                      digits = 2, flag = "0")
    G$cursor <- write_at(G$cursor, 49, 22, tlives, col)
  }

###############################################################################

  cactuski_scroll <- function() {

    yline <- G$T$top
    ylinem1 <- ifelse(G$T$top == 1, 20, G$T$top - 1)

    for (y in 0:19) {

      # What should we be plotting here?
      # Suppose wid = 28 and mid = 30.
      # The complete line would be...

      # 10 shaded >, 5 >, |, 28 spaces, |, 5 <, 10 shaded <
      # 10 + 5 + 1 + 28 + 1 + 5 + 10 = 60
      # the 28 spaces starts at pos 16, and continues til 43.
      # the vertical bars are at positions 15 and 44

      dl <- G$T$left[yline]
      dr <- G$T$right[yline]

      # What was plotted last time, so we can do a "diff" and only draw
      # the characters we need to...

      dl_prev <- ifelse(y == 0, G$T$left_zero, G$T$left[ylinem1])
      dr_prev <- ifelse(y == 0, G$T$right_zero, G$T$right[ylinem1])

      set_colour(222)
      left_str <-
        if (dl < dl_prev) paste0("/", strrep(" ", dl_prev - dl)) else
          if (dl == dl_prev) "|" else
            paste0(paste(G$left_track[dl_prev:(dl-1)], collapse = ""), "\\")

      G$cursor <- write_at(G$cursor, min(dl, dl_prev), y, left_str)

      set_colour(222)
      right_str <-
        if (dr < dr_prev) paste0("/",
                            paste(G$right_track[dr:(dr_prev-1)],
                                            collapse = "")) else
          if (dr == dr_prev) "|" else paste0(strrep(" ", dr - dr_prev), "\\")

      G$cursor <- write_at(G$cursor, min(dr, dr_prev), y, right_str)

      if (y < 19L) {
        yline <- if (yline == 20L) 1L else (yline + 1L)
        ylinem1 <- if (ylinem1 == 20L) 1L else (ylinem1 + 1L)
      }
    }
  }

###############################################################################

  cactuski_draw_kenny <- function() {
    if (G$kdx == -1) {
      G$cursor <- write_at(G$cursor, G$kx-1, G$ky, G$body_s)
      G$cursor <- write_at(G$cursor, G$kx-1, G$ky+1, G$body_s)
      G$cursor <- write_at(G$cursor, G$kx-1, G$ky+2, G$body_s)
      G$cursor <- write_at(G$cursor, G$kx-1, G$ky+3,
                           if (G$jumping > 2) G$jskileft else G$skileft)

    } else if (G$kdx == 1) {
      G$cursor <- write_at(G$cursor, G$kx, G$ky, G$s_body)
      G$cursor <- write_at(G$cursor, G$kx, G$ky+1, G$s_body)
      G$cursor <- write_at(G$cursor, G$kx, G$ky+2, G$s_body)
      G$cursor <- write_at(G$cursor, G$kx, G$ky+3,
                           if (G$jumping > 2) G$jskiright else G$skiright)
    } else if (G$kdx == 0) {
      G$cursor <- write_at(G$cursor, G$kx, G$ky, G$body)
      G$cursor <- write_at(G$cursor, G$kx, G$ky+1, G$body)
      G$cursor <- write_at(G$cursor, G$kx, G$ky+2, G$body)
      G$cursor <- write_at(G$cursor, G$kx, G$ky+3,
                           if (G$jumping > 2)  G$jskidown else G$skidown)
    }
  }

  cactuski_move_kenny <- function() {
  # No movement - draw vertical skis

    if ((G$kdx == 0) && (G$kdy == 0) && (!G$init)) {
      if (G$jumping > 1) {
        G$cursor <- write_at(G$cursor, G$kx, G$ky+3, G$jskidown)
      } else {
        G$cursor <- write_at(G$cursor, G$kx, G$ky+3, G$skidown)
      }
      return()
    }

    # Down movement (erase previous top)

     if (G$kdy == 1) {
      G$cursor <- write_at(G$cursor, G$kx, G$ky, "   ")
      G$ky <- G$ky + 1
      G$kdy <- 0
      # Then use the G$kdy=0 code.
    } else if (G$kdy == -1) {
      G$ky <- G$ky - 1
      G$cursor <- write_at(G$cursor, G$kx, G$ky+4, "   ")
      G$kdy <- 0
    }

    if (G$kdy <= 0) {
      cactuski_draw_kenny()
      G$kx <- G$kx + G$kdx
      G$kdy <- 0
    }
  }

###############################################################################

  cactuski_check_keys <- function() {

    if (G$jumping > 0) {
      G$jumping <- G$jumping - 1
    }
    kp <- tolower(keypress::keypress(block = FALSE))
    if (is.na(kp)) return()

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
      cactuski_end()
      return()
    }
    else if (kp == "l") {
      if (!G$firing) {
        G$firing = TRUE
        G$lx <- G$kx + 1
        G$ly <- G$ky + 2
        G$ldx <- G$kdx
        if (G$config$audio) G$pew <- play_sound(G$pew)
      }

    } else if (kp == "j") {
      if (G$jumping == 0) {
        G$jumping <- 5
        G$boing <- play_sound(G$boing)
      }

    # Test hole
    } else if ((kp == "h") || (kp == "f") || (kp == "s") || (kp == "t")) {
      cactuski_incoming_thing(kp)

    # Test baddie
    } else if (kp == "b") {
      cactuski_incoming_baddy()

    } else if (kp == "n") {
      G$thin <- 2

    } else if (kp == "m") {
      G$thin <- (-2)

    # Test wall

    } else if (kp == "w") {
      cactuski_incoming_wall(sample(1:4)[1])

    } else if (kp == "c") {
      cactuski_incoming_finch()

    } else if (kp == "q") {
      cactuski_incoming_boss("4;3;10")
    }
  }

###############################################################################

  cactuski_wiggle_track <- function() {
    next_line <- G$T$top
    G$T$left_zero <- G$T$left[next_line]
    G$T$right_zero <- G$T$right[next_line]
    G$T$wid[next_line] <- G$T$wid_zero


    if (G$T$next_wiggle_in == 0L) {
      G$T$origin_x <- G$T$origin_x + as.integer(G$T$amplitude * G$T$direction)

      target_123 <- sample(5)
      orig_123 <- G$T$target_123
      G$T$target_123 <- target_123[target_123 != orig_123][1]

      next_pos <- c(3L + (G$T$wid[next_line] %/% 2L), NA, 30L, NA,
                    57L - (G$T$wid[next_line] %/% 2L))
      next_pos[2] <- (next_pos[1] + next_pos[3]) %/% 2L
      next_pos[4] <- (next_pos[5] + next_pos[3]) %/% 2L
      next_pos <- next_pos[G$T$target_123]
      G$T$direction <- if (G$T$target_123 > orig_123) 1L else -1L
      G$T$period <- 25L
      G$T$next_wiggle_in <- G$T$period + 1L
      G$T$amplitude <- abs(G$T$origin_x - next_pos)
    }

    G$T$next_wiggle_in <- G$T$next_wiggle_in - 1L
    G$T$mid[next_line] <- G$T$origin_x +
      as.integer((G$T$amplitude * G$T$direction * 0.5 *
      (1 + cos(pi + (pi * ((G$T$period - G$T$next_wiggle_in) / G$T$period))))))

    G$T$left[next_line] <- max(2, G$T$mid[next_line] -
                                 ((G$T$wid[next_line] %/% 2) + 1))
    G$T$right[next_line] <- min(58, G$T$mid[next_line] +
                                  (G$T$wid[next_line] %/% 2))

    G$T$top <- if (G$T$top == 20) 1 else G$T$top + 1

  }


################################################################################
# Collision detection of various kinds.

  cactuski_death_check <- function() {

  ###############################
  # We don't need no education

    if (G$ky + 3 == G$wy) {
      if ((G$kx < G$gapx - 1) || (G$kx + 2 > (G$gapx + G$gapwid)) ||
          (G$wtype %in% c(3,4,5))) {
        if (G$config$audio) G$ouch <- play_sound(G$ouch)

        for (i in seq(0, 99)) {
          G$lives <- G$lives - 1
          cactuski_update_lives(as.integer(runif(1) * 255))
          if (i == 8) {
            G$cursor <- write_at(G$cursor, G$kx, G$ky, "   ")
            G$cursor <- write_at(G$cursor, G$kx - 2, G$ky + 2,
              paste0(G$wallcoll, G$wallcoll, G$wallcolm, G$wallcolm,
                     G$wallcolr, G$wallcolr))

          } else if (i == 16) {
            G$cursor <- write_at(G$cursor, G$kx, G$ky + 1, "   ")
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2,
              paste0(G$wallcoll, G$wallcoll, G$wallcoll,
                     G$wallcolm, G$wallcolm, G$wallcolm,
                     G$wallcolr, G$wallcolr, G$wallcolr))

          } else if (i == 24) {
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2,
              paste0(G$CC[[34]], "<<<", G$CC[[82]], "%%%", G$CC[[34]], ">>>"))

          } else if (i == 32) {
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2,
              paste0(G$CC[[40]], "<<<", G$CC[[83]], "%%%", G$CC[[40]], ">>>"))

          } else if (i == 40) {
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2,
              paste0(G$CC[[46]], "<<<", G$CC[[84]], "%%%", G$CC[[40]], ">>>"))

          } else if (i == 48) {
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2,
              paste0(G$CC[[82]], "<<<", G$CC[[196]], "xxx", G$CC[[82]], ">>>"))

          } else if (i == 56) {
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2,
              paste0(G$CC[[83]], "<<", G$CC[[196]], "xxxxx", G$CC[[83]], ">>"))

          } else if (i == 64) {
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2,
              paste0(G$CC[[85]], "<", G$CC[[196]], "xxxxxxx", G$CC[[85]], ">"))

          } else if (i == 72) {
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2,
                                 paste0(G$CC[[196]], "xxxx|xxxx"))

          } else if (i == 77) {
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2,
                                 paste0(G$CC[[196]], "xxx< >xxx"))

          } else if (i == 82) {
            G$cursor <-  write_at(G$cursor, G$kx - 3, G$ky + 2,
                                  paste0(G$CC[[196]], "xx<   >xx"))

          } else if (i == 87) {
            G$cursor <-  write_at(G$cursor, G$kx - 3, G$ky + 2,
                                  paste0(G$CC[[196]], "x<     >x"))

          } else if (i == 92) {
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2,
                                 paste0(G$CC[[196]], "<       >"))

          } else if (i == 97) {
            G$cursor <- write_at(G$cursor, G$kx - 3, G$ky + 2, "         ")
          }

          waitr::wait_for(10)
        }
        cactuski_update_lives(49)
        G$death_flag <- 1
        return()
      }
    }

    y <- 3 + (G$T$top + G$ky)
    y <- ifelse(y > 20, y - 20, y)
    half_wid <- G$T$wid[y] %/% 2


  #########################
  # Death by electrocution

    dl <- G$T$mid[y] - (half_wid + 1)
    dr <- G$T$mid[y] + (half_wid)
    if ((G$kx <= dl) || (G$kx + 2 >= dr)) {
      G$cursor <- pos_at(G$cursor, 0,20)
      if (G$config$audio) G$elec <- play_sound(G$elec)
      for (i in seq(99, 0)) {
        G$lives <- G$lives - 1
        cactuski_update_lives(as.integer(runif(1) * 255))

        rnd <- as.integer(1 + (254 * runif(11)))
        ski <- c("\\","|","/")[1+as.integer(3 * runif(2))]
        G$cursor <- write_at(G$cursor, G$kx, G$ky,
          paste0(G$CC[[rnd[1]]], "<", G$CC[[rnd[2]]], "%", G$CC[[rnd[3]]], ">"))

        G$cursor <- write_at(G$cursor, G$kx, G$ky + 1,
          paste0(G$CC[[rnd[4]]], "<", G$CC[[rnd[5]]], "%", G$CC[[rnd[6]]], ">"))

        G$cursor <- write_at(G$cursor, G$kx, G$ky + 2,
          paste0(G$CC[[rnd[7]]], "<", G$CC[[rnd[8]]], "%", G$CC[[rnd[9]]], ">"))

        G$cursor <- write_at(G$cursor, G$kx, G$ky + 3,
          paste0(G$CC[[rnd[10]]], ski[1], " ", G$CC[[rnd[11]]], ski[2]))

        waitr::wait_for(10)
      }
      cactuski_update_lives(49)
      G$death_flag <- 1
      return()
    }

  ##########################################
  # Check collisions with floaters

    index <- which(G$fy == (G$ky + 3))[1]
    if (is.na(index)) {
      return()
    }
    if (G$factive[index] != 1) {
      return()
    }

  ####################################
  # Slalom left

    if (G$ftype[index] == 3) {
      if (G$kx <= G$fx[index]) {
        G$score <- G$score + 100
        if (G$config$audio) G$ding <- play_sound(G$ding)
      } else {
        G$lives <- G$lives - 2
        if (G$config$audio) G$buzz <- play_sound(G$buzz)
      }
      G$factive[index] <- 2
      return()
    }

  ####################################
  # Slalom right

    if (G$ftype[index] == 4) {
      if (G$kx >= G$fx[index]) {
        G$score <- G$score + 100
        if (G$config$audio) G$ding <- play_sound(G$ding)
      } else {
        G$lives <- G$lives - 2
        if (G$config$audio) G$buzz <- play_sound(G$buzz)
      }
      G$factive[index] <- 2
      return()
    }

    if ((G$fx[index] >= (G$kx - 2)) & (G$fx[index] <= G$kx + 2)) {

    #############################
    # Death by falling in hole

      if ((G$ftype[index] == 2) & (G$jumping <= 1)) {
        if (G$config$audio) G$fall <- play_sound(G$fall)
        if (G$kx != G$fx[index]) {
          G$cursor <- write_at(G$cursor, G$kx, G$ky, "   ")
          G$cursor <- write_at(G$cursor, G$fx[index], G$ky,
            paste0(G$lskihole, G$rskihole))
          G$cursor <- write_at(G$cursor, G$kx, G$ky + 1, "   ")
          G$cursor <- write_at(G$cursor, G$fx[index], G$ky + 1, G$body)
          G$cursor <- write_at(G$cursor, G$kx, G$ky + 2, "   ")
          G$cursor <- write_at(G$cursor, G$fx[index], G$ky + 2, G$body)
          G$cursor <- write_at(G$cursor, G$kx, G$ky + 3, "   ")
          G$cursor <- write_at(G$cursor, G$fx[index], G$ky + 3,
            paste0(G$CC[[145]], "\\%/"))

        }

        for (death_steps in seq(99, 0)) {
          G$lives <- G$lives - 1
          cactuski_update_lives(255 - as.integer(runif(1) * 28))
          waitr::wait_for(30)

          if (death_steps == 85) {
            G$cursor <- write_at(G$cursor, G$fx[index], G$ky, "   ")
            G$cursor <- write_at(G$cursor, G$fx[index], G$ky + 1,
                                 paste0(paste0(G$lskihole, G$rskihole)))

          } else if (death_steps == 70) {
            G$cursor <- write_at(G$cursor, G$fx[index], G$ky + 1, "   ")
            G$cursor <- write_at(G$cursor, G$fx[index], G$ky + 2,
                                 paste0(paste0(G$lskihole, G$rskihole)))

          } else if (death_steps == 55) {
            G$cursor <- write_at(G$cursor, G$fx[index], G$ky + 2, "   ")
            G$cursor <- write_at(G$cursor, G$fx[index], G$ky + 3,
              paste0(G$CC[[145]], "\\V/"))

          } else if (death_steps == 40) {
            G$cursor <- write_at(G$cursor, G$fx[index], G$ky + 3,
                                 paste0(G$CC[[145]], "\\_/"))
          }
        }
        cactuski_update_lives()
        G$death_flag <- 1L
        return()

      #####################################
      # Eating body parts from dead cactus

      } else if (G$ftype[index] == 5L) {
        G$factive[index] <- 0L
        G$cursor <- write_at(G$cursor, G$fx[index], G$fy[index],
                             G$fspaces[index])
        G$lives <- G$lives + 5L
        if (G$config$audio) {
          if (sample(10L, 1L) != 1L) {
            G$eat1 <- play_sound(G$eat1)
          } else {
            G$eat2 <- play_sound(G$eat2)
          }
        }
        return()

    ############################
    # Ski through a camel turd

      } else if (G$ftype[index] == 6L) {
        if ((G$kx >= G$fx[index] - 2L) && (G$kx <= G$fx[index] + 2L)) {
          if (G$config$audio) G$splat <- play_sound(G$splat)
          if (G$kx >= G$fx[index]) G$brownleft <- TRUE
          if ((G$kx >= G$fx[index] - 1L) &&
              (G$kx <= G$fx[index] + 1L)) G$brownmid <- TRUE
          if (G$kx <= G$fx[index]) G$brownright <- TRUE
          cactuski_colour_kenny()
          G$factive[index] <- 0L
          G$cursor <- write_at(G$cursor, G$fx[index], G$fy[index], "   ")
        }
        return()
      }

  #######################################
  # A bottomless pit goes whooshing past

    } else if (G$ftype[index] == 2L) {
      if (G$config$audio) G$whoo <- play_sound(G$whoo)
      return()
    }

  ######################################################
  # You got assassinated by a subnano rabbit antibody

    if ((G$fx[index] >= (G$kx - 1L)) & (G$fx[index] <= G$kx + 2L) &
        (G$ftype[index] == 1L)) {

      G$cursor <- pos_at(G$cursor, 0L, 20L)
      if (G$config$audio) G$splat <- play_sound(G$splat)
      for (i in seq(99L, 0L)) {
        G$lives <- G$lives - 1L
        cactuski_update_lives(sample(c(9L,160L,196L,124L,88L))[1L])
        waitr::wait_for(5L)
      }
      cactuski_update_lives()
      waitr::wait_for(500L)

      cactuski_burst_into_flames(G$fx[index], G$fy[index])
      G$death_flag <- 1L
      return()
    }
  }

################################################################################
# Move the cactus laser blast of doom

  cactuski_move_laser <- function() {
    if (!G$firing) return()
    if (G$ly > G$ky + 2L) G$cursor <- write_at(G$cursor, G$lx, G$ly, " ")
    G$ly <- G$ly + 1L
    G$lx <- G$lx + G$ldx

    # Laser hit something

    if ((G$ly %in% G$fy) || ((G$ly + 1L) %in% G$fy)) {
      index <- min(which(G$ly %in% G$fy)[1L],
                   which((G$ly + 1L) %in% G$fy)[1L], na.rm = TRUE)

      if (G$factive[index] == 1L) {
        if (G$ftype[index] == 1L) {  # Baddy
          if ((G$lx == G$fx[index]) || (G$lx == G$fx[index] + 1L)) {
            G$cursor <- write_at(G$cursor, G$fx[index], G$fy[index], "  ")
            if (G$config$audio) G$squelch <- play_sound(G$squelch)
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
            if (G$config$audio) G$blip <- play_sound(G$blip)
            G$score <- G$score + 4560L
          }
        } else {
          G$ly <- 20L
          if (G$config$audio) G$bump <- play_sound(G$bump)
        }
      }
    }

    # Laser hit boss

    if ((G$bossy != -1L) && (G$bossh > 0L)) {
      if ((G$ly == 18L) || (G$ly == 19L)) {
        if ((G$lx >= G$bossx) && (G$lx <= G$bossx + 2L)) {
          G$ly <- 20L
          G$bosshfx <- 2L
          G$bossh <- G$bossh - 1L
          if (G$bossh > 0) {
            if (G$config$audio) G$bump <- play_sound(G$bump)
          } else {
            if (G$config$audio) G$boom <- play_sound(G$boom)
            G$bosshfx <- 10L
          }
        }
      }
    }

    # Laser off bottom

    if (G$ly >= 20L) {
      G$firing <- FALSE
      return()
    }

    # Laser hit edge

    y <- 1L + (G$T$top + G$ly)
    y <- ifelse(y > 20L, y - 20L, y)
    half_wid <- G$T$wid[y] / 2L
    dl <- G$T$mid[y] - (half_wid + 1L)
    dr <- G$T$mid[y] + (half_wid)

    if ((G$lx <= dl) || (G$lx >= dr)) {
      G$firing <- FALSE
      return()
    }

    G$cursor <- write_at(G$cursor, G$lx, G$ly,
      paste0(G$CC[[sample(c(196L,203L,220L))[1L]]], "@"))

  }

###############################################################################
# Move miscellaneous floaters

  cactuski_write_around_kenny <- function(x, y, str) {
    if ((y >= G$ky + 3) || (y < G$ky)) { # no y-overlap with Kenny
      G$cursor <- write_at(G$cursor, x, y, str)
    } else {
      nc <- raw_length(str)
      intx <- intersect(G$kx:(G$kx + 2), x:(x + (nc - 1)))
      if (length(intx) == 0) {   # No x-overlap with Kenny
        G$cursor <- write_at(G$cursor, x, y, str)
      } else {
        # Print part of the string that doesn't overlap.
        # Awkward as we (may) have colour codes in the string that
        # we still need to respect, even if skipping characters
        cha <- 1
        for (i in seq_len(nc)) {
          px <- x + (i - 1)
          while (substr(str, cha, cha) == "\033") {
            end_code <- as.integer(regexpr("m", substring(str, cha)))
            G$cursor <- write_at(G$cursor, px, y,
                                 substr(str, cha, cha + end_code))
            str <- substring(str, cha + end_code + 1)
          }

          if (!px %in% intx) {
            G$cursor <- write_at(G$cursor, px, y, substr(str, cha, cha))
            cha <- cha + 1
          }
        }
      }
    }
  }

  cactuski_move_floaters <- function() {
    # No work?
    if (length(G$factive) == 0L) return()

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
      if (length(G$factive) == 0L) return()
    }

    # Move all the others.

    for (i in seq_len(length(G$fy))) {
      if (G$factive[i] == 0L) {
        G$fy[i] <- G$fy[i] - 1L
        next
      }
      if (G$fy[i] < 20L) {
        cactuski_write_around_kenny(G$fx[i], G$fy[i], G$fspaces[i])
      }

      G$fy[i] <- G$fy[i] - 1L

      if (G$ftype[i] == 1L) { # Baddy - do horizontal movement
        G$fx[i] <- G$fx[i] + G$fdx[i]
        G$fx[i] <- if (G$fx[i] < G$fleftlimit[i] + 1L)
          G$fleftlimit[i] + 1L else G$fx[i]
        G$fx[i] <- if (G$fx[i] > G$frightlimit[i] - 2L)
          G$frightlimit[i] - 2L else G$fx[i]

        if ((G$fx[i] == G$fleftlimit[i] + 1L) |
            (G$fx[i] == G$frightlimit[i] - 2L)) {

          G$fdx[i] <- (-G$fdx[i])
          if (G$config$audio) G$bbo <- play_sound(G$bbo)
        }
      }

      if ((G$fy[i] >= 0L) & (G$factive[i] > 0L)) {
        if (G$ftype[i] == 1L) { # Baddy
          cactuski_write_around_kenny(G$fx[i], G$fy[i],
            paste0(G$CC[[sample(c(196L, 214L, 226L), 1L)]],
                   if (G$fdx[i] == -2L) "<=" else "=>"))
        } else {
          cactuski_write_around_kenny(G$fx[i], G$fy[i], G$fstring[i])
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
  }

  cactuski_move_finch <- function() {
    G$cfalt <- if (G$cfalt == 4L) 0L else G$cfalt + 1L

    if (G$cfy < 20L) {
      cactuski_write_around_kenny(G$cfx, G$cfy, "   ")
    }

    G$cfr <- G$cfalt %% 2
    if (G$cfalt != 3L) G$cfy <- G$cfy - 1L

    if (G$cfy >= 0L) {
      yindex <- (G$T$top + G$cfy)
      yindex <- if (yindex > 20L) yindex - 20L else yindex
      G$cfx <- as.integer(G$T$left[yindex] +
        (G$cff * (G$T$right[yindex] - G$T$left[yindex])))
      cactuski_write_around_kenny(G$cfx, G$cfy, G$fch[1L + G$cfr])
    }

    # Actual collision

    if ((G$cfy == G$ky + 2L) && (G$cfx >= G$kx - 1L)  && (G$cfx <= G$kx + 1L)) {
      cactuski_write_around_kenny(G$cfx, G$cfy, "   ")
      G$cfy <- (-1L)
      if (G$config$audio) G$owfinch <- play_sound(G$owfinch)
      G$lives <- G$lives - 10
    }
  }

  cactuski_move_wall <- function() {
    if (G$wy < 20L) {
      left_wall <- paste0(rep(" ", G$gapx - G$wx), collapse = "")
      cactuski_write_around_kenny(G$wx, G$wy, left_wall)

      if (G$wtype %in% c(3, 4, 5)) {
        cactuski_write_around_kenny(G$gapx, G$wy,
          paste0(G$CC[[196]], paste(rep(" ", G$gapwid), collapse = "")))
        if (G$wtype == 5) { # Barrier just got shot
          G$wtype <- 1
        }
      }

      right_wall <- paste0(rep(' ', (G$wx + G$wwid) -
                                    (G$gapx + G$gapwid)), collapse = "")
      cactuski_write_around_kenny(G$gapwid + G$gapx, G$wy,
                                  paste0(G$CC[[248]], right_wall))
    }

    G$wy <- G$wy - 1

    if (G$wy < 0) {
      G$score <- G$score + 456
      return()
    }

    if (G$gapdx !=0) {
      G$gapx <- G$gapx + G$gapdx
      if ((G$gapx + G$gapwid >= (G$wx + G$wwid - 1)) ||
          (G$gapx <= (G$wx + 1))) {

        G$gapdx <- 0 - G$gapdx
      }
    }

    left_wall <- paste0(rep("#", G$gapx - G$wx), collapse = "")
    cactuski_write_around_kenny(G$wx, G$wy, paste0(G$CC[[248]], left_wall))

    if (G$wtype %in% c(3, 4)) {
      cactuski_write_around_kenny(G$gapx, G$wy,
        paste0(G$CC[[196]], paste(rep("X", G$gapwid), collapse="")))
    }

    right_wall <- paste0(rep('#', (G$wx + G$wwid) -
                                   (G$gapx + G$gapwid)), collapse = "")

    cactuski_write_around_kenny(G$gapwid + G$gapx, G$wy,
      paste0(G$CC[[248]], right_wall))

  }

  cactuski_end <- function() {
    if (G$config$audio) G$win <- play_sound(G$win)

    G$cursor <- write_at(G$cursor, 1, 21, paste0(G$CC[[178]],
      "*", G$CC[[255]], " You made it to the Giant Cactus HQ ",
      G$CC[[178]], "*"))

    G$cursor <- write_at(G$cursor, 1, 22, paste0(G$CC[[178]],
      "*", G$CC[[255]],"   The cactus community is saved!   ",
      G$CC[[178]], "*"))

    gchq <- readLines(pkg_file("gfx/cactuski-gchq.txt"))

    if (G$ly > 0) {
      G$cursor <- write_at(G$cursor, G$lx, G$ly, " ")
    }

    for (j in 19:0) {
      line <- 1
      time <- waitr::waitr_timestamp()
      for (i in j:19) {
        G$cursor <- write_at(G$cursor, 0, i, gchq[line])
        line <- line + 1
      }
      if ((G$ky + 3) >= 0) G$cursor <- write_at(G$cursor, G$kx, G$ky + 3, "   ")
      G$ky <- G$ky - 1
      if ((G$ky + 3) >= 0) G$cursor <-
        write_at(G$cursor, G$kx, G$ky + 3, G$skidown)
      if ((G$ky + 2) >= 0) G$cursor <-
        write_at(G$cursor, G$kx, G$ky + 2, G$body)
      if ((G$ky + 1) >= 0) G$cursor <-
        write_at(G$cursor, G$kx, G$ky + 1, G$body)
      if (G$ky >= 1) G$cursor <- write_at(G$cursor, G$kx, G$ky, G$body)
      waitr::wait_until(time + 100)
    }
    dotsx <- c(3, 2, 1, 0, 1, 2, 3, NA, NA, NA, NA,
               5, 6, 5, 6, NA, NA,
               9, 8, 7, 6, 5, NA, NA,
               8, 9, 8, 9, NA, NA,
               11, 12, 13, 14, 13, 12, 11)

    dotsy <- c(0, 1, 2, 3, 4, 5, 6, NA, NA, NA, NA,
               1, 1, 2, 2, NA, NA,
               1, 2, 3 ,4 ,5, NA, NA,
               4, 4, 5, 5, NA, NA,
               0, 1, 2, 3, 4, 5, 6)

    dotsx <- dotsx + 22
    dotsy <- dotsy + 5
    waitr::wait_for(2000)
    for (i in seq_len(length(dotsx))) {
      col <- G$CC[[sample(c(46, 154, 118), 1)]]
      if (!is.na(dotsx[i])) G$cursor <- write_at(G$cursor, dotsx[i], dotsy[i],
                                                 paste0(col, "#"))
      waitr::wait_for(30)
    }
    waitr::wait_for(3000)
    G$cursor <- clear_pic(G$cursor, 23)
    G$gameover <- TRUE
  }

################################################################################
# Main loop starts here...

  next_frame <- waitr::waitr_timestamp() + 30

  while (TRUE) {
    cactuski_check_keys()
    cactuski_update_future(MSGS)
    if (G$gameover) break
    cactuski_move_floaters()
    if (G$wy != -1) cactuski_move_wall()
    cactuski_scroll()
    cactuski_death_check()

    if (G$death_flag == 1) {
      waitr::wait_for(500)
      if (G$lives > 0) {
        G$cursor <- clear_pic(G$cursor, 20)
        cactuski_restart()
        G$T <- cactuski_init_track(G$T)

      } else {
        G$cursor <- show_pic(G$cursor, pkg_file("gfx/cactuski-go.txt"), "up")
        waitr::wait_for(1000)

        G$msgs <- c(
          paste0("$021.$027.$033.$039.$045.$051.$050.$049.$048. ",
                 "$226You killed Kenny!! $048.$049.$050.$051.$045.",
                 "$039.$033.$027.$021."),
          paste0("$237.$238.$239.$240.$241.$242.$243.$244.$245.$246.$247.",
                 "$248.$249.$250.$251.$252.$253.$254.$255.",
                 "$255.$254.$253.$252.$251.$250.$249.$248.$247.$246.",
                 "$245.$244.$243.$242.$241.$240.$239.$238.$237."))
        G$msg_pointer <- 1
        G$msg_x <- 1
        while ((length(G$msgs) > 1) || (G$msg_x < 39)) {
          cactuski_do_messages()
          waitr::wait_for(15)
        }

        waitr::wait_for(3000)
        G$cursor <- clear_pic(G$cursor, 23)
        break
      }
      next
    }
    cactuski_move_kenny()

    if (G$thin != 0) {
      G$T$wid_zero <- G$T$wid_zero - G$thin
      if (G$T$wid_zero < 10) G$T$wid_zero <- 10
      G$thin <- 0
    }

    cactuski_wiggle_track()
    cactuski_move_laser()
    if (G$cfy >= 0) cactuski_move_finch()

    if (G$bossy != -1) cactuski_do_bosses()
    next_frame <- waitr::wait_until(next_frame) + 25
    cactuski_move_laser()
    if (G$cfy >= 0) cactuski_move_finch()

    next_frame <- waitr::wait_until(next_frame) + 25
    G$score <- G$score + 7
    cactuski_do_messages()
    cactuski_update_score()
    cactuski_update_lives()
    cactuski_do_messages()
  }

  # Game is now over

  # Clear keyboard buffer

  while (!is.na(keypress::keypress(block = FALSE))) {}

  G$score <- formatC(G$score/100, format = "f", digits = 2, flag = "0")
  if (good_score(user_file("cactuski-hs.csv.xz"), G$score)) {
    G$cursor <- fade_text(G$cursor, 30, 16, "GREAT SCORE! TYPE YOUR NAME",
                          UNICORN, FADE_IN, triple = TRUE)
    G$cursor <- fade_text(G$cursor, 25, 19, "..........",
                          GREY_SCALE, FADE_IN, align = LEFT)
    res <- get_input(G$cursor, 25, 19, 10)
    G$cursor <- res$cursor
    insert_score(user_file("cactuski-hs.csv.xz"), res$res, G$score)
  }
  G$cursor <- clear_pic(G$cursor, 23)
  return(G$cursor)
}

################################################################################

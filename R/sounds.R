load_sound <- function(wav) {
  list(wav = audio::load.wave(wav),
       player = NULL)
}

play_sound <- function(x) {
  x$player <- audio::play(x$wav)
  x
}

stop_sound <- function(x) {
  if (!is.null(x$player)) {
    x$player <- audio::pause(x$player)
    x$player <- NULL
  }
  x
}

init_sound <- function(x) {
  init_audio <- audio::load.wave("data/empty.wav")
  pinit_audio <- audio::play(init_audio)
  audio::pause(pinit_audio)
}

class Player {
  constructor(media) {
    this.media = media
  }
  fullscreen() {
    if (document.fullscreenElement) {
      document.exitFullscreen()
    } else {
      this.media.requestFullscreen()
    }
  }
  pictureInPicture() {
    if (document.pictureInPictureElement) {
      document.exitPictureInPicture()
    } else {
      this.media.requestPictureInPicture()
    }
  }
  pause() {
    if (this.media.paused) {
      this.media.play()
    } else {
      this.media.pause()
    }
  }
  seekRelative(seconds) {
    this.media.currentTime += seconds
  }
  seekAbsolute(seconds) {
    this.media.currentTime = seconds
  }
  seekAbsolutePercent(percent) {
    this.media.currentTime = this.media.duration * percent
  }
  seekRelativePercent(percent) {
    this.media.currentTime += this.media.duration * percent
  }
  mute() {
    this.media.muted = ! this.media.muted
  }
  setVolume(percent) {
    this.media.volume = percent
  }
  increaseVolume(percent) {
    const volume = this.media.volume + percent
    this.media.volume = volume > 1
      ? 1
      : volume < 0
      ? 0
      : volume
  }
  decreaseVolume(percent) {
    this.increaseVolume(-percent)
  }
}

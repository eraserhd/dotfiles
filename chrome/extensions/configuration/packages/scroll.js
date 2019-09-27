class Scroll {
  constructor() {
    this.element = document.scrollingElement
    this.step = 60
    this.behavior = 'smooth'
    this.animation = null
  }
  down(repeat) {
    if (this.behavior === 'smooth') {
      this.animate(() => this.element.scrollTop += this.step / 4, repeat)
    } else {
      this.element.scrollBy({ top: this.step })
    }
  }
  up(repeat) {
    if (this.behavior === 'smooth') {
      this.animate(() => this.element.scrollTop -= this.step / 4, repeat)
    } else {
      this.element.scrollBy({ top: -this.step })
    }
  }
  right(repeat) {
    if (this.behavior === 'smooth') {
      this.animate(() => this.element.scrollLeft += this.step / 4, repeat)
    } else {
      this.element.scrollBy({ left: this.step })
    }
  }
  left(repeat) {
    if (this.behavior === 'smooth') {
      this.animate(() => this.element.scrollLeft -= this.step / 4, repeat)
    } else {
      this.element.scrollBy({ left: -this.step })
    }
  }
  pageDown(percent = 0.9) {
    this.element.scrollBy({ top: window.innerHeight * percent, behavior: this.behavior })
  }
  pageUp(percent = 0.9) {
    this.element.scrollBy({ top: -window.innerHeight * percent, behavior: this.behavior })
  }
  top() {
    this.element.scrollTo({ top: 0, behavior: this.behavior })
  }
  bottom() {
    this.element.scrollTo({ top: this.element.scrollHeight, behavior: this.behavior })
  }
  // Saka Key – https://key.saka.io
  //
  // Scrolls the selected element smoothly.  Works around the quirks of keydown events.
  // The first time a key is pressed (and held), a keydown event is fired immediately.
  // After that, there is a delay before the second keydown event is fired.
  // The third and all subsequent keydown events fire in rapid succession.
  // repeat is false for the first keydown event, but true for all others.
  // The delay (70 and 500) are carefully selected to keep scrolling smooth, but
  // prevent unexpected scrolling after the user has released the scroll key.
  //
  // https://github.com/lusakasa/saka-key/tree/master/src/modes/command/client/commands/scroll
  animate(animation, repeat) {
    // Cancel potential animation being proceeded
    cancelAnimationFrame(this.animation)
    let start = null
    const delay = repeat ? 70 : 500
    const step = (timeStamp) => {
      if (start === null) {
        start = timeStamp
      }
      const progress = timeStamp - start
      animation()
      if (progress < delay) {
        this.animation = requestAnimationFrame(step)
      } else {
        this.animation = null
      }
    }
    requestAnimationFrame(step)
  }
}

class Mouse {
  constructor() {
    this.timers = new Map
  }
  hover(target) {
    Mouse.hover(target)
    this.timers.set(target, setTimeout(this.hover.bind(this, target), 200))
  }
  unhover(target) {
    clearTimeout(this.timers.get(target))
    Mouse.unhover(target)
    this.timers.delete(target)
  }
  clear() {
    for (const [target, timer] of this.timers) {
      this.unhover(target)
    }
  }
  static click(target, modifierKeys) {
    this.dispatchEvents(target, ['mouseover', 'mousedown', 'mouseup', 'click'], modifierKeys)
  }
  static hover(target) {
    this.dispatchEvents(target, ['mouseover', 'mouseenter', 'mousemove'])
  }
  static unhover(target) {
    this.dispatchEvents(target, ['mousemove', 'mouseout', 'mouseleave'])
  }
  static dispatchEvents(target, events, { shiftKey, ctrlKey, altKey, metaKey } = {}) {
    for (const type of events) {
      const event = new MouseEvent(type, {
        bubbles: true,
        cancelable: true,
        view: window,
        shiftKey,
        ctrlKey,
        altKey,
        metaKey
      })
      target.dispatchEvent(event)
    }
  }
}

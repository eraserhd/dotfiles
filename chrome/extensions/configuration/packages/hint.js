class Hint {
  // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values
  static MODIFIER_KEYS = ['Shift', 'Control', 'Alt', 'Meta']
  static NAVIGATION_KEYS = ['ArrowDown', 'ArrowLeft', 'ArrowRight', 'ArrowUp', 'End', 'Home', 'PageDown', 'PageUp']
  static KEY_MAP = {
    Digit1: '1', Digit2: '2', Digit3: '3', Digit4: '4', Digit5: '5', Digit6: '6', Digit7: '7', Digit8: '8', Digit9: '9', Digit0: '0',
    KeyQ: 'q', KeyW: 'w', KeyE: 'e', KeyR: 'r', KeyT: 't', KeyY: 'y', KeyU: 'u', KeyI: 'i', KeyO: 'o', KeyP: 'p',
    KeyA: 'a', KeyS: 's', KeyD: 'd', KeyF: 'f', KeyG: 'g', KeyH: 'h', KeyJ: 'j', KeyK: 'k', KeyL: 'l',
    KeyZ: 'z', KeyX: 'x', KeyC: 'c', KeyV: 'v', KeyB: 'b', KeyN: 'n', KeyM: 'm'
  }
  constructor() {
    this.selectors = '*'
    this.keys = ['KeyA', 'KeyJ', 'KeyS', 'KeyK', 'KeyD', 'KeyL', 'KeyG', 'KeyH', 'KeyE', 'KeyW', 'KeyO', 'KeyR', 'KeyU', 'KeyV', 'KeyN', 'KeyC', 'KeyM']
    this.lock = false
    this.hints = []
    this.inputKeys = []
    this.validatedElements = []
    this.keyMap = Hint.KEY_MAP
    // Events
    this.events = {}
    this.events['validate'] = []
    this.events['start'] = []
    this.events['exit'] = []
    // Style
    this.style = `
      .hint {
        padding: 0.15rem 0.25rem;
        border: 1px solid hsl(39, 70%, 45%);
        text-transform: uppercase;
        text-align: center;
        vertical-align: middle;
        background: linear-gradient(to bottom, hsl(56, 100%, 76%) 0%, hsl(42, 100%, 63%) 100%);
        border-radius: 4px;
        box-shadow: 0 3px 1px -2px hsla(0, 0%, 0%, 0.2), 0 2px 2px 0 hsla(0, 0%, 0%, 0.14), 0 1px 5px 0 hsla(0, 0%, 0%, 0.12);
        transform: translate3d(0%, -50%, 0);
      }
      .hint .character {
        font-family: Roboto, sans-serif;
        font-size: 12px;
        font-weight: 900;
        color: hsl(45, 81%, 10%);
        text-shadow: 0 1px 0 hsla(0, 0%, 100%, 0.6);
      }
      .hint .character.active {
        color: hsl(44, 64%, 53%);
      }
    `
  }
  updateHints() {
    const hintableElements = Array.from(document.querySelectorAll(this.selectors)).filter((element) => Hint.isHintable(element))
    this.hints = Hint.generateHints(hintableElements, this.keys)
  }
  filterHints(input) {
    const filteredHints = this.hints.filter(([label]) => input.every((key, index) => label[index] === key))
    return filteredHints
  }
  processKeys(keys, validate = false) {
    const filteredHints = this.filterHints(keys)
    switch (filteredHints.length) {
      case 0:
        break
      case 1:
        this.inputKeys = []
        this.render()
        this.processHint(filteredHints[0])
        break
      default:
        if (validate) {
          this.inputKeys = []
          this.render()
          this.processHint(filteredHints[0])
        } else {
          this.inputKeys = keys
          this.render()
        }
    }
  }
  processHint([label, element]) {
    this.validatedElements.push(element)
    if (this.lock === false) {
      this.stop()
    }
    this.triggerEvent('validate', element)
  }
  on(type, listener) {
    this.events[type].push(listener)
  }
  triggerEvent(type, ...parameters) {
    for (const listener of this.events[type]) {
      listener(...parameters)
    }
  }
  start() {
    this.onKey = (event) => {
      // Skip modifier and navigation keys
      if ([...Hint.MODIFIER_KEYS, ...Hint.NAVIGATION_KEYS].includes(event.key)) {
        return
      }
      // Prevent the browsers default behavior (such as opening a link)
      // and stop the propagation of the event.
      event.preventDefault()
      event.stopImmediatePropagation()
      switch (event.code) {
        case 'Escape':
          this.stop()
          break
        case 'Backspace':
          this.processKeys(this.inputKeys.slice(0, -1))
          break
        case 'Enter':
          this.processKeys(this.inputKeys, true)
          break
        default:
          this.processKeys(this.inputKeys.concat(event.code))
      }
    }
    this.onViewChange = (event) => {
      this.updateHints()
      this.processKeys([])
    }
    this.onClick = (event) => {
      this.stop()
    }
    // Use the capture method.
    //
    // This setting is important to trigger the listeners during the capturing phase
    // if we want to prevent bubbling.
    //
    // Phase 1: Capturing phase: Window (1) → ChildElement (2) → Target (3)
    // Phase 2: Target phase: Target (1)
    // Phase 3: Bubbling phase: Window (3) ← ParentElement (2) ← Target (1)
    //
    // https://w3.org/TR/DOM-Level-3-Events#event-flow
    window.addEventListener('keydown', this.onKey, true)
    window.addEventListener('scroll', this.onViewChange)
    window.addEventListener('resize', this.onViewChange)
    window.addEventListener('click', this.onClick)
    // Process hints
    this.updateHints()
    this.processKeys([])
    this.triggerEvent('start')
  }
  stop() {
    window.removeEventListener('keydown', this.onKey, true)
    window.removeEventListener('scroll', this.onViewChange)
    window.removeEventListener('resize', this.onViewChange)
    window.removeEventListener('click', this.onClick)
    this.clearViewport()
    this.triggerEvent('exit', this.validatedElements)
    this.hints = []
    this.inputKeys = []
    this.validatedElements = []
  }
  render() {
    const root = document.createElement('div')
    root.id = 'hints'
    // Place the hints in a closed shadow root,
    // so that the hint and page styles won’t affect each other.
    const shadow = root.attachShadow({ mode: 'closed' })
    for (const [label, element] of this.filterHints(this.inputKeys)) {
      const container = document.createElement('div')
      container.classList.add('hint')
      for (const [index, code] of label.entries()) {
        const atom = document.createElement('span')
        atom.classList.add('character', code === this.inputKeys[index] ? 'active' : 'normal')
        atom.textContent = this.keyMap[code]
        container.append(atom)
      }
      const rectangle = element.getBoundingClientRect()
      // Place hints relative to the viewport
      container.style.position = 'fixed'
      // Vertical placement: center
      container.style.top = rectangle.top + (rectangle.height / 2) + 'px'
      // Horizontal placement: left
      container.style.left = rectangle.left + 'px'
      // Control overlapping
      container.style.zIndex = 2147483647 // 2³¹ − 1
      shadow.append(container)
    }
    this.clearViewport()
    // Style
    const style = document.createElement('style')
    style.textContent = this.style
    // Attach
    shadow.append(style)
    document.documentElement.append(root)
  }
  clearViewport() {
    const root = document.querySelector('#hints')
    if (root) {
      root.remove()
    }
  }
  static generateHints(elements, keys) {
    const hintKeys = this.generateHintKeys(keys, elements.length)
    const hints = elements.map((element, index) => [hintKeys[index], element])
    return hints
  }
  static generateHintKeys(keys, count) {
    const hints = [[]]
    let offset = 0
    while (hints.length - offset < count || hints.length === 1) {
      const hint = hints[offset++]
      for (const key of keys) {
        hints.push(hint.concat(key))
      }
    }
    return hints.slice(offset, offset + count)
  }
  static isHintable(element) {
    return this.isVisible(element) && this.isClickable(element)
  }
  static isVisible(element) {
    return element.offsetParent !== null && this.insideViewport(element)
  }
  static insideViewport(element) {
    const rectangle = element.getBoundingClientRect()
    return rectangle.top >= 0 && rectangle.left >= 0 && rectangle.bottom <= window.innerHeight && rectangle.right <= window.innerWidth
  }
  static isClickable(element) {
    const nodeNames = ['A', 'BUTTON', 'SELECT', 'TEXTAREA', 'INPUT', 'VIDEO']
    const roles = ['button', 'checkbox', 'combobox', 'link', 'menuitem', 'menuitemcheckbox', 'menuitemradio', 'radio', 'tab', 'textbox']
    return element.offsetParent !== null && (nodeNames.includes(element.nodeName) || roles.includes(element.getAttribute('role')) || element.hasAttribute('onclick'))
  }
}

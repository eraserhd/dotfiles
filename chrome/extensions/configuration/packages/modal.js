class Modal {
  static MODIFIER_KEYS = ['Shift', 'Control', 'Alt', 'Meta']
  static KEY_MAP = {
    Backquote: { key: '`', shiftKey: '~' }, Digit1: { key: '1', shiftKey: '!' }, Digit2: { key: '2', shiftKey: '@' }, Digit3: { key: '3', shiftKey: '#' }, Digit4: { key: '4', shiftKey: '$' }, Digit5: { key: '5', shiftKey: '%' }, Digit6: { key: '6', shiftKey: '^' }, Digit7: { key: '7', shiftKey: '&' }, Digit8: { key: '8', shiftKey: '*' }, Digit9: { key: '9', shiftKey: '(' }, Digit0: { key: '0', shiftKey: ')' }, Minus: { key: '-', shiftKey: '_' }, Equal: { key: '=', shiftKey: '+' },
    KeyQ: { key: 'q', shiftKey: 'Q' }, KeyW: { key: 'w', shiftKey: 'W' }, KeyE: { key: 'e', shiftKey: 'E' }, KeyR: { key: 'r', shiftKey: 'R' }, KeyT: { key: 't', shiftKey: 'T' }, KeyY: { key: 'y', shiftKey: 'Y' }, KeyU: { key: 'u', shiftKey: 'U' }, KeyI: { key: 'i', shiftKey: 'I' }, KeyO: { key: 'o', shiftKey: 'O' }, KeyP: { key: 'p', shiftKey: 'P' }, BracketLeft: { key: '[', shiftKey: '{' }, BracketRight: { key: ']', shiftKey: '}' }, Backslash: { key: '\\', shiftKey: '|' },
    KeyA: { key: 'a', shiftKey: 'A' }, KeyS: { key: 's', shiftKey: 'S' }, KeyD: { key: 'd', shiftKey: 'D' }, KeyF: { key: 'f', shiftKey: 'F' }, KeyG: { key: 'g', shiftKey: 'G' }, KeyH: { key: 'h', shiftKey: 'H' }, KeyJ: { key: 'j', shiftKey: 'J' }, KeyK: { key: 'k', shiftKey: 'K' }, KeyL: { key: 'l', shiftKey: 'L' }, Semicolon: { key: ';', shiftKey: ':' }, Quote: { key: "'", shiftKey: '"' },
    KeyZ: { key: 'z', shiftKey: 'Z' }, KeyX: { key: 'x', shiftKey: 'X' }, KeyC: { key: 'c', shiftKey: 'C' }, KeyV: { key: 'v', shiftKey: 'V' }, KeyB: { key: 'b', shiftKey: 'B' }, KeyN: { key: 'n', shiftKey: 'N' }, KeyM: { key: 'm', shiftKey: 'M' }, Comma: { key: ',', shiftKey: '<' }, Period: { key: '.', shiftKey: '>' }, Slash: { key: '/', shiftKey: '?' }
  }
  constructor(name) {
    this.name = name
    this.filters = {}
    this.mappings = {}
    this.keyMap = Modal.KEY_MAP
    // Context
    this.context = {}
    this.context.name = null
    this.context.filters = []
    this.context.commands = {}
    // Events
    this.events = {}
    this.events['context-change'] = []
    this.events['command'] = []
    this.events['default'] = []
    this.events['start'] = []
    this.events['stop'] = []
    // Filters
    this.filter('Page', () => true)
    this.filter('Command', () => ! Modal.isText(document.activeElement), 'Page')
    this.filter('Text', () => Modal.isText(document.activeElement), 'Page')
    this.filter('Link', () => document.activeElement.nodeName === 'A', 'Command')
    this.filter('Video', () => document.activeElement.nodeName === 'VIDEO' || Modal.findParent((element) => ['html5-video-player'].some((className) => element.classList.contains(className))), 'Page')
    this.enable('Page')
    // Style
    this.style = `
      #help.overlay {
        display: flex; /* Enable to center content */
        justify-content: center; /* Horizontally */
        align-items: center; /* Vertically */
        position: fixed;
        top: 0;
        left: 0;
        z-index: 2147483647; /* 2³¹ − 1 */
        width: 100%;
        height: 100%;
        background-color: hsla(0, 0%, 0%, 0.5);
      }
      #help main {
        position: relative;
        width: fit-content;
        height: fit-content;
        max-width: 30%;
        max-height: 90%;
        overflow-x: auto;
        overflow-y: auto;
        font-family: serif;
        font-size: 12px;
        color: gray;
        background-color: white;
        border: 1px solid lightgray;
        border-radius: 4px;
        padding: 3px;
      }
      #help main table caption {
        font-size: 18px;
        font-weight: bold;
        padding: 10px 0;
      }
      #notification {
        position: fixed;
        bottom: 0;
        right: 0;
        z-index: 2147483647; /* 2³¹ − 1 */
        font-family: serif;
        font-size: 12px;
        color: gray;
        background-color: white;
        border: 1px solid lightgray;
        border-top-left-radius: 4px;
        padding: 3px;
      }
      /* Style from GitHub */
      kbd {
        background-color: #fafbfc;
        border: 1px solid #c6cbd1;
        border-bottom-color: #959da5;
        border-radius: 3px;
        box-shadow: inset 0 -1px 0 #959da5;
        color: #444d56;
        display: inline-block;
        font-family: monospace;
        font-size: 11px;
        line-height: 10px;
        padding: 3px 5px;
        vertical-align: middle;
      }
      /* Scrollbar */
      ::-webkit-scrollbar {
        height: 25px;
      }
      ::-webkit-scrollbar-button:start,
      ::-webkit-scrollbar-button:end {
        display: none;
      }
      ::-webkit-scrollbar-track-piece {
        background-color: #eee;
      }
      ::-webkit-scrollbar-thumb {
        background-color: #bbb;
        border: 7px solid #eee;
        -webkit-background-clip: padding-box;
        -webkit-border-radius: 12px;
      }
    `
  }
  filter(name, filter, parent = null) {
    this.filters[name] = { filter, parent }
    this.mappings[name] = {}
  }
  enable(...filters) {
    this.context.filters = filters.filter((name) => this.filters[name])
  }
  map(context, keys, command, description = '') {
    const keyChord = Modal.parseKeys(keys)
    command = this.parseCommand(command)
    const key = JSON.stringify(keyChord)
    this.mappings[context][key] = { command, description }
  }
  unmap(context, keys) {
    const keyChord = Modal.parseKeys(keys)
    const key = JSON.stringify(keyChord)
    delete this.mappings[context][key]
  }
  mode(nextMode) {
    this.unlisten()
    nextMode.listen()
  }
  on(type, listener) {
    this.events[type].push(listener)
  }
  triggerEvent(type, ...parameters) {
    for (const listener of this.events[type]) {
      listener(...parameters)
    }
  }
  listen() {
    this.onKey = (event) => {
      // Skip modifiers
      if (Modal.MODIFIER_KEYS.includes(event.key)) {
        return
      }
      const keyChord = {
        metaKey: event.metaKey,
        altKey: event.altKey,
        ctrlKey: event.ctrlKey,
        shiftKey: event.shiftKey,
        code: event.code
      }
      const key = JSON.stringify(keyChord)
      const command = this.context.commands[key]
      if (command) {
        // Prevent the browsers default behavior (such as opening a link)
        // and stop the propagation of the event.
        event.preventDefault()
        event.stopImmediatePropagation()
        // Command
        command.command(event)
        this.triggerEvent('command', event)
      } else {
        this.triggerEvent('default', event)
      }
    }
    this.onFocus = (event) => {
      this.updateContext()
    }
    // Use the capture method.
    //
    // This setting is important to trigger the listeners during the capturing phase
    // if we want to prevent bubbling.
    //
    // Also, some events (such as focus and blur) do not bubble, so this setting
    // is important to trigger the listeners attached to the parents of the target.
    //
    // Phase 1: Capturing phase: Window (1) → ChildElement (2) → Target (3)
    // Phase 2: Target phase: Target (1)
    // Phase 3: Bubbling phase: Window (3) ← ParentElement (2) ← Target (1)
    //
    // https://w3.org/TR/DOM-Level-3-Events#event-flow
    window.addEventListener('keydown', this.onKey, true)
    window.addEventListener('focus', this.onFocus, true)
    window.addEventListener('blur', this.onFocus, true)
    // Initialize active context
    this.updateContext()
    this.triggerEvent('start')
  }
  unlisten() {
    window.removeEventListener('keydown', this.onKey, true)
    window.removeEventListener('focus', this.onFocus, true)
    window.removeEventListener('blur', this.onFocus, true)
    this.triggerEvent('stop')
  }
  getContexts(name, accumulator = []) {
    if (name === null) {
      return accumulator
    }
    return this.getContexts(this.filters[name].parent, accumulator.concat(name))
  }
  updateContext() {
    const previousContextName = this.context.name
    this.context.name = this.context.filters.find((name) => this.getContexts(name).every((name) => this.filters[name].filter()))
    if (this.context.name !== previousContextName) {
      this.updateCommands()
      this.triggerEvent('context-change', this.context)
    }
  }
  updateCommands() {
    const commands = {}
    const contexts = this.getContexts(this.context.name)
    for (const context of contexts) {
      for (const [key, mapping] of Object.entries(this.mappings[context])) {
        if (commands[key] === undefined) {
          commands[key] = { context, ...mapping }
        }
      }
    }
    this.context.commands = commands
  }
  parseCommand(command) {
    switch (true) {
      case command instanceof Modal:
        return () => this.mode(command)
      case command instanceof Function:
        return command
    }
  }
  keyValues({ metaKey, altKey, ctrlKey, shiftKey, code }) {
    const keys = []
    const keyMap = this.keyMap[code]
    if (metaKey) keys.push('Meta')
    if (altKey) keys.push('Alt')
    if (ctrlKey) keys.push('Control')
    if (shiftKey && ! keyMap) keys.push('Shift')
    const key = keyMap
      ? shiftKey
      ? keyMap.shiftKey
      : keyMap.key
      : code
    keys.push(key)
    return keys
  }
  static parseKeys(keys) {
    const keyChord = {
      metaKey: false,
      altKey: false,
      ctrlKey: false,
      shiftKey: false,
      code: ''
    }
    for (const key of keys) {
      switch (key) {
        case 'Shift':
          keyChord.shiftKey = true
          break
        case 'Control':
          keyChord.ctrlKey = true
          break
        case 'Alt':
          keyChord.altKey = true
          break
        case 'Meta':
          keyChord.metaKey = true
          break
        default:
          keyChord.code = key
      }
    }
    return keyChord
  }
  static isText(element) {
    const nodeNames = ['INPUT', 'TEXTAREA', 'OBJECT']
    return element.offsetParent !== null && (nodeNames.includes(element.nodeName) || element.isContentEditable)
  }
  static findParent(find, element = document.activeElement) {
    if (element === null) {
      return null
    }
    const result = find(element)
    if (result) {
      return result
    }
    return this.findParent(find, element.parentElement)
  }
  help() {
    // Open or close help
    const rootReference = document.querySelector('#modal-help')
    if (rootReference) {
      rootReference.remove()
      return
    }
    // Initialize
    const root = document.createElement('div')
    root.id = 'modal-help'
    // Place the document in a closed shadow root,
    // so that the document and page styles won’t affect each other.
    const shadow = root.attachShadow({ mode: 'closed' })
    // Container
    const container = document.createElement('div')
    container.id = 'help'
    container.classList.add('overlay')
    // Content
    const content = document.createElement('main')
    container.append(content)
    // Table
    const table = document.createElement('table')
    content.append(table)
    // Caption
    const caption = document.createElement('caption')
    caption.textContent = this.context.name
    table.append(caption)
    // Commands
    for (const [keyChord, { description }] of Object.entries(this.context.commands)) {
      // Table row
      const row = document.createElement('tr')
      table.append(row)
      // Table header cell
      const header = document.createElement('th')
      const keys = this.keyValues(JSON.parse(keyChord))
      for (const key of keys) {
        const atom = document.createElement('kbd')
        atom.textContent = key
        header.append(atom)
      }
      row.append(header)
      // Table data cell
      const data = document.createElement('td')
      data.textContent = description
      row.append(data)
    }
    // Style
    const documentStyle = document.createElement('style')
    documentStyle.textContent = this.style
    // Attach
    shadow.append(documentStyle)
    shadow.append(container)
    document.documentElement.append(root)
    // Close on click
    container.addEventListener('click', (event) => {
      // Stop propagation
      event.stopImmediatePropagation()
      root.remove()
    })
  }
  notify({ id = Date.now(), message, duration }) {
    const initialize = () => {
      let root = document.querySelector('#modal-notifications')
      if (! root) {
        root = document.createElement('div')
        root.id = 'modal-notifications'
        document.documentElement.append(root)
      }
      return root
    }
    const clearViewport = (root, id) => {
      const container = root.querySelector(`[data-notification-id="${id}"]`)
      if (container) {
        container.remove()
      }
    }
    const notifications = initialize()
    clearViewport(notifications, id)
    // Initialize
    const root = document.createElement('div')
    root.setAttribute('data-notification-id', id)
    // Place the document in a closed shadow root,
    // so that the document and page styles won’t affect each other.
    const shadow = root.attachShadow({ mode: 'closed' })
    // Container
    const container = document.createElement('div')
    container.id = 'notification'
    container.textContent = message
    // Style
    const documentStyle = document.createElement('style')
    documentStyle.textContent = this.style
    // Attach
    shadow.append(documentStyle)
    shadow.append(container)
    notifications.append(root)
    // Duration of the notification (optional)
    if (duration) {
      setTimeout(() => {
        root.remove()
      }, duration)
    }
  }
}

class SelectionList {
  constructor() {
    this.main = 0
    this.collection = []
    // Events
    this.events = {}
    this.events['selection-change'] = []
  }
  on(type, listener) {
    this.events[type].push(listener)
  }
  triggerEvent(type, ...parameters) {
    for (const listener of this.events[type]) {
      listener(...parameters)
    }
  }
  get length() {
    return this.collection.length
  }
  includes(element) {
    return this.collection.includes(element)
  }
  set(collection = this.collection, main = collection.length - 1) {
    this.clear()
    this.main = main >= 0 && main <= collection.length - 1
      ? main
      : 0
    this.collection = collection
    this.sort()
    this.merge()
    this.render()
    this.focus()
    this.triggerEvent('selection-change', collection)
  }
  add(...elements) {
    const collection = this.collection.concat(elements)
    const main = collection.length - 1
    this.set(collection, main)
  }
  remove(...elements) {
    const collection = Object.assign([this.collection[this.main]], elements)
    this.filter((candidate) => collection.includes(candidate) === false)
  }
  filter(filter) {
    this.fold((element, index, array) => filter(element, index, array) ? [element] : [])
  }
  parent(count = 1) {
    const getParent = (element, count) => {
      if (count < 1) {
        return element
      }
      if (element === null) {
        return null
      }
      return getParent(element.parentElement, count - 1)
    }
    this.fold((element) => {
      const parent = getParent(element, count)
      return parent ? [parent] : []
    })
  }
  children(depth = 1) {
    if (depth < 1 || this.collection.length === 0) {
      return
    }
    this.fold((element) => element.children)
    this.children(depth - 1)
  }
  select(selectors = '*') {
    this.fold((element) => Array.from(element.querySelectorAll(selectors)))
  }
  fold(fold) {
    let main = this.main
    const collection = []
    for (const [index, element] of this.collection.entries()) {
      const elements = fold(element, index, this.collection)
      switch (elements.length) {
        case 0:
          if (index < this.main || this.main === this.length - 1) {
            --main
          }
          break
        case 1:
          collection.push(elements[0])
          break
        default:
          collection.push(...elements)
          if (index <= this.main) {
            main += elements.length - 1
          }
      }
    }
    this.set(collection, main)
  }
  focus(element = this.collection[this.main]) {
    if (this.length === 0) {
      return
    }
    const main = this.collection.indexOf(element)
    if (main === -1) {
      return
    }
    if (main !== this.main) {
      const secondary = this.collection[this.main]
      secondary.classList.remove('primary-selection')
      secondary.classList.add('secondary-selection')
      element.classList.remove('secondary-selection')
      element.classList.add('primary-selection')
      this.main = main
    }
    element.focus()
    element.scrollIntoView({ block: 'nearest' })
  }
  next(count = 1) {
    const main = SelectionList.modulo(this.main + count, this.length)
    this.focus(this.collection[main])
  }
  previous(count = 1) {
    this.next(-count)
  }
  sort() {
    if (this.length <= 1) {
      return
    }
    const main = this.collection[this.main]
    this.collection.sort(SelectionList.compare)
    this.main = this.collection.indexOf(main)
  }
  merge() {
    if (this.length <= 1) {
      return
    }
    let main = this.main
    const collection = []
    let target = 0
    let candidate
    for (candidate = 1; candidate < this.length; ++candidate) {
      if (this.collection[target] === this.collection[candidate] || this.collection[target].contains(this.collection[candidate])) {
        if (candidate <= this.main) {
          --main
        }
        continue
      }
      collection.push(this.collection[target])
      target = candidate
    }
    collection.push(this.collection[target])
    this.main = main
    this.collection = collection
  }
  render() {
    if (this.length === 0) {
      return
    }
    this.collection[this.main].classList.add('primary-selection')
    for (const [index, element] of this.collection.entries()) {
      if (index !== this.main) {
        element.classList.add('secondary-selection')
      }
    }
  }
  clear() {
    if (this.length === 0) {
      return
    }
    this.collection[this.main].classList.remove('primary-selection')
    for (const [index, element] of this.collection.entries()) {
      if (index !== this.main) {
        element.classList.remove('secondary-selection')
      }
    }
    this.main = 0
    this.collection = []
    this.triggerEvent('selection-change', this.collection)
  }
  static compare(element, other) {
    if (element.compareDocumentPosition(other) & Node.DOCUMENT_POSITION_FOLLOWING) {
      return -1
    }
    if (element.compareDocumentPosition(other) & Node.DOCUMENT_POSITION_PRECEDING) {
      return 1
    }
    return 0
  }
  static modulo(dividend, divisor) {
    return ((dividend % divisor) + divisor) % divisor
  }
}

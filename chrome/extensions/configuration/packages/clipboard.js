class Clipboard {
  static copy(text) {
    const activeElement = document.activeElement
    const textArea = document.createElement('textarea')
    textArea.style.position = 'fixed'
    textArea.value = text
    document.body.append(textArea)
    textArea.select()
    document.execCommand('copy')
    textArea.remove()
    activeElement.focus()
  }
  static paste() {
    const activeElement = document.activeElement
    const textArea = document.createElement('textarea')
    textArea.style.position = 'fixed'
    document.body.append(textArea)
    textArea.focus()
    document.execCommand('paste')
    const text = textArea.value
    textArea.remove()
    activeElement.focus()
    return text
  }
}

function hexView(inputs) {

  const is = {}
  for (const f in inputs)
    is[f] = drawInput(f,inputs[f])

  return is
}

function drawInput(name,content) {
  const bytesPerPage  = 2048
  let page = null
  const inputName = document.getElementById("input-name")
  const input      = document.getElementById("input-pane")


  function highlightRange(offset,to) {
    const newPage = Math.floor(offset / bytesPerPage)
    if (page === null || page.page !== newPage) {
      page = drawPage(content,bytesPerPage,newPage)
      inputName.textContent = name
      input.innerHTML = ""
      input.appendChild(page.dom)
    }
    page.highlightRange(offset,to)
  }

  function deselect() {
    if (page !== null) page.deselect()
  }

  return { deselect: deselect
         , highlight: (x) => highlightRange(x,x)
         , highlightRange: highlightRange
         }
}


function drawPage(content,bytesPerPage,page) {
  const size          = content.length
  const bytesPerLine  = 32
  const bytesPerGroup =  4
  const groupsPerLine = Math.ceil(bytesPerLine / bytesPerGroup)

  const dom = document.createElement("div")
  dom.classList.add("input-file")

  const start     = page * bytesPerPage
  const showSize  = Math.max(0, Math.min(size - start, bytesPerPage))
  const lines     = Math.ceil(showSize/bytesPerLine)
  let offset      = start
  const end       = offset + showSize

  const bytes   = []
  const chars   = []
  const lineNos = []

  for (let line = 0; line < lines; ++line) {
    const lineD = document.createElement('div')
    lineD.classList.add('input-line')

    const offsetD = document.createElement('div')
    offsetD.classList.add('input-line-offset')
    offsetD.textContent = offset.toString(16)
    lineD.appendChild(offsetD)
    lineNos.push(offsetD)


    const textD = []

    for (let group = 0; group < groupsPerLine; ++group) {
      const groupD = document.createElement('div')
      groupD.classList.add('input-group')

      for (let el = 0; el < bytesPerGroup && offset < end; ++el, ++offset) {
        const value = content.charCodeAt(offset)
        const str   = value.toString(16)

        const byteD = document.createElement("div")
        byteD.classList.add("input-byte")
        byteD.setAttribute('title', "0x" + offset.toString(16))
        byteD.textContent = str.length < 2 ? ('0' + str) : str
        groupD.appendChild(byteD)
        bytes.push(byteD)

        const tD = document.createElement("div")
        tD.classList.add("input-char")
        tD.textContent =
          32 <= value && value < 127 ? content.charAt(offset) : '.'
        chars.push(tD)
        textD.push(tD)

        function on() {
          byteD.classList.add("hover"); tD.classList.add("hover") }

        function off() {
          byteD.classList.remove("hover"); tD.classList.remove("hover") }

        byteD.addEventListener("mouseover", on)
        byteD.addEventListener("mouseout",  off)
        tD.addEventListener("mouseover", on)
        tD.addEventListener("mouseout",  off)
      }
      lineD.appendChild(groupD)
    }

    const textG = document.createElement("div")
    textG.classList.add("input-group")
    for(let i = 0; i < textD.length; ++i) textG.appendChild(textD[i])
    lineD.appendChild(textG)

    dom.appendChild(lineD)
  }

  let selected = []

  function deselect() {
    for (let i = 0; i < selected.length; ++i) {
      selected[i].classList.remove("selected")
    }
  }

  function highlight(b) {
    if (b < start || b >= end) return
    const off = b - start
    const l   = Math.floor(off / bytesPerLine)
    bytes[off].classList.add("selected")
    selected.push(bytes[off])
    chars[off].classList.add("selected")
    selected.push(chars[off])
    lineNos[l].classList.add("selected")
    lineNos[l].scrollIntoView({behavior: 'smooth'})
    selected.push(lineNos[l])
  }

  function highlightRange(a,b) {
    for (let i = a; i <= b; ++i) highlight(i)
  }

  return { dom: dom
         , page: page
         , highlightRange: highlightRange
         , deselect: deselect
         }
}


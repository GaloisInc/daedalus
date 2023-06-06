

function showRange(files,loc) {
  files.select(loc.from.file, loc.from.line, loc.from.col
                            , loc.to.line,   loc.to.col
                            , true)
}

function showInput(inputs,loc) {
  const [name,off] = loc
  const input = inputs[name]
  if (input === undefined) return
  input.deselect()
  input.highlight(off)
}

function prepRenderError() {

  const btnGrammar = document.getElementById("view-grammar")
  const btnInput   = document.getElementById("view-input")

  const inputName  = document.getElementById("input-name")
  const inputPane  = document.getElementById("input-pane")
  const sourceName = document.getElementById("source-name")
  const sourcePane = document.getElementById("source-pane")

  function visible(x,a) {
    const c = x.classList
    if (a) c.remove("hidden"); else c.add("hidden")
  }

  function viewSource() {
    visible(inputName,false)
    visible(inputPane,false)
    visible(sourceName,true)
    visible(sourcePane,true)
  }

  function viewInput() {
    visible(inputName,true)
    visible(inputPane,true)
    visible(sourceName,false)
    visible(sourcePane,false)
  }

  btnGrammar.addEventListener("click", viewSource)
  btnInput.addEventListener("click", viewInput)

  viewSource()
}

function indexErrors(arr,t) {
  const es = t.errors
  for (let i = 0; i < es.length; ++i) {
    const e = es[i]
    arr[e.number] = e
  }
  const fs = t.frames
  for (let i = 0; i < fs.length; ++i) {
    indexErrors(arr,fs[i].nest)
  }
}

function renderErrorByIndex(gui,i,lab) {
  const dom = document.createElement("span")
  dom.classList.add("button")
  dom.classList.add("clickable")
  dom.textContent = lab
  dom.addEventListener("click", () => {
    const e = gui.ix[i]
    if (e === undefined) {
      return
    }
    renderError(gui,e)
  })
  return dom
}



function renderError(gui,err) {
  const errorPane = document.getElementById("error-pane")
  const stackPane = document.getElementById("stack-pane")
  const scopePane = document.getElementById("scope-pane")
  stackPane.innerHTML = ""
  scopePane.innerHTML = ""


  // Erorr message
  errorPane.innerHTML = ""
  const prev = renderErrorByIndex(gui,err.number - 1, "Prev")
  const next = renderErrorByIndex(gui,err.number + 1, "Next")
  const msg  = document.createElement("div")
  msg.classList.add("error-message")
  msg.textContent = err.error
  errorPane.appendChild(msg)
  errorPane.appendChild(prev)
  errorPane.appendChild(next)


  // Build stack and scope viewer
  const stackFrames = []

  let selected = null

  function showFrame(i) {
    if (i < 0 || i >= stackFrames.length) return
    if (selected !== null) {
      selected.classList.remove('selected')
    }
    const frame = stackFrames[i]
    selected = frame.fun
    selected.classList.add('selected')
    scopePane.innerHTML = ''
    if (frame.scope !== null) {
      scopePane.appendChild(frame.scope)
    }
    const info = frame.callsite
    if (info) showRange(gui.files,info)
    else
    if (err.grammar.length > 0) showRange(gui.files, err.grammar[0])

    const inp = frame.input
    showInput(gui.inputs, (inp !== null) ? inp : [err.input, err.offset])
  }



  {
    let cur
    function newFun(lab) {
      cur = { scope: null
            , fun: null
            , callsite: null
            , input: null
            , callInfo: null
            }
      const ix = stackFrames.length
      const dom = document.createElement("div")
      dom.classList.add("call")
      dom.textContent = lab
      cur.fun = dom
      stackPane.appendChild(dom)
      dom.addEventListener("click",() => showFrame(ix))
    }

    newFun("(top)")

    const stack = err.stack
    for (let i = stack.length - 1; i >= 0; --i) {
      const frame = stack[i]
      switch (frame.tag) {

        case "scope": {
          cur.scope = renderScope(gui.inputs,frame.content)
          continue
        }

        case "label": {
          const dom = document.createElement("div")
          dom.classList.add("label")
          dom.textContent = frame.content
          cur.fun.appendChild(dom)
          continue
        }

        case "call": {
          const info = frame.content
          stackFrames.push(cur)
          cur.callsite = frame.content.callsite
          cur.input    = [ frame.content.input, frame.content.offset]
          newFun(info.function)
          cur.callInfo = frame.content
          continue
        }
      }
    }
    stackFrames.push(cur)
  }

  {
    let sel = gui.treeSelected
    if (sel !== null) gui.treeErrs[sel].classList.remove("selected")
    sel = err.number
    gui.treeSelected = sel
    gui.treeErrs[sel].classList.add("selected")
  }

}




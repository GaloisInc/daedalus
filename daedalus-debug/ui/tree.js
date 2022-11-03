function renderTree(files,tree) {
  prepRenderError()

  const errMap = document.getElementById("err-map")
  const body   = document.getElementById("body")


  const ctrl = collapsible(errMap,true)
  body.addEventListener("keydown", (ev) => {
    if (ev.key !== "?") return
    ctrl.toggle()
  })

  const gui   = {}
  gui.files   = files
  gui.inputs  = hexView(tree.inputs)
  gui.ix      = []
  gui.treeMap = ctrl
  gui.treeSelected = null
  gui.treeErrs = []

  indexErrors(gui.ix,tree.tree)
  renderErrorMap(gui,errMap,tree.tree)
}

function renderErrorMap(gui,p,t) {

  const es = t.errors
  for (let i = 0; i < es.length; ++i) {
    const e = document.createElement("div")
    e.classList.add("map-err")
    e.classList.add("clickable")
    const err = es[i]
    e.textContent = es[i].error + ' (' + err.offset + ')'
    p.appendChild(e)
    e.addEventListener("click",() => {
      gui.treeMap.set(false)
      renderError(gui,err)
    })
    gui.treeErrs[err.number] = e
  }

  const fs = t.frames
  for (let i = 0; i < fs.length; ++i) {
    const f = fs[i]
    const frame = f.frame
    const dom = document.createElement("div")
    dom.classList.add("map-call")
    const lab = document.createElement("div")
    lab.textContent = frame.function
    dom.appendChild(lab)
    const content = document.createElement("div")
    content.classList.add("map-content")
    const cntCtrl = collapsible(content,false)
    lab.classList.add("clickable")
    lab.addEventListener("click",(ev) => {
      ev.stopPropagation()
      cntCtrl.toggle()
    })
    renderErrorMap(gui,content,f.nest)
    dom.appendChild(content)
    p.appendChild(dom)
  }
}


function collapsible(thing,initial) {
  let visible = initial
  const classes = thing.classList

  function update() {
    if (visible) {
      classes.remove("hidden")
    } else {
      classes.add("hidden")
    }
  }

  update()

  return {
    toggle: () => { visible = !visible; update() },
    set: (x)   => { visible = x; update() }
  }
}



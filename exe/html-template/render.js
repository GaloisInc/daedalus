function main() {
  const valuePane = document.getElementById("value-pane")
  const pathPane  = document.getElementById("value-path")

  const inputs = hexView(data.inputs)
  const vs = data.values
  const n = vs.length

  const roots = []
  function hover(dom,yes) {
    if (dom === null) return
    const h = "value-hover"
    const cls = dom.classList
    if (yes) { cls.add(h) } else { cls.remove(h) }
  }

  let path = []
  let dom = null

  const allElements = {}

  function samePath(p) {
    const n = path.length
    if (p.length !== n) return false
    for (let i = 0; i < n; ++i)
      if (path[i] !== p[i]) return false
    return true
  }



  function showPath(d,ctx) {
    const p = ctx.path
    if (samePath(p)) return
    path = p
    hover(dom,false)
    dom = d
    hover(dom,true)
    pathPane.innerHTML = ""
    for (let i = 0; i < path.length; ++i) {
      const btn = document.createElement("div")
      btn.classList.add("button")
      btn.classList.add("clickable")
      btn.textContent = path[i]
      const newCtxt = { ...ctx, path: path.slice(0,i+1) }
      btn.addEventListener("click", () => focus(newCtxt))
      pathPane.appendChild(btn)
    }
  }

  function focus(ctx) {
    const path = ctx.path
    valuePane.innerHTML = ""
    const pathStr = pathToString(path)
    valuePane.appendChild(renderValue(ctx,null,allElements[pathStr]))
    showPath(null, ctx)
  }


  for (let i = 0; i < n; ++i) {
    const name = i === 0 ? "document" : ("document-" + i)
    const ctx = { path: [name]
                , inputs: inputs
                , elements: allElements
                , zoom: focus
                , showPath: showPath
                }

    roots[i] = renderValue(ctx,null,vs[i])
    valuePane.appendChild(roots[i])
  }
}

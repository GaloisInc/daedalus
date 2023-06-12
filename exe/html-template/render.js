function main() {
  const valuePane = document.getElementById("value-pane")
  const inputs = hexView(data.inputs)
  const vs = data.values
  const n = vs.length

  const roots = []
  const stack = []
  function hover(dom,yes) {
    const h = "value-hover"
    const cls = dom.classList
    if (yes) { cls.add(h) } else { cls.remove(h) }
  }

  let path = []
  let dom = null

  function samePath(p) {
    const n = path.length
    if (p.length !== n) return false
    for (let i = 0; i < n; ++i)
      if (path[i] !== p[i]) return false
    return true
  }

  for (let i = 0; i < n; ++i) {
    const name = i === 0 ? "document" : ("document-" + i)
    const ctx = { path: [name]
                , inputs: inputs
                , zoom:(path,dom) => {
                    document.getElementById("value-path").textContent = ""
                    while (stack.length > 0) {
                      const thing = stack.pop()
                      thing.dom.classList.remove("hover")
                    }

                    const v = document.getElementById("value-pane")
                    v.innerHTML = ""
                    v.appendChild(dom)
                  }

                , showPath: (d,p) => {
                    if (samePath(p)) return
                    path = p
                    if (dom) { hover(dom,false) }
                    dom = d
                    hover(dom,true)
                    document.getElementById("value-path").textContent = p
                  }
                }

    roots[i] = renderValue(ctx,null,vs[i])
    valuePane.appendChild(roots[i])
  }
}

function main() {
  const valuePane = document.getElementById("value-pane")
  const inputs = hexView(data.inputs)
  const vs = data.values
  const n = vs.length

  const roots = []
  const stack = []

  for (let i = 0; i < n; ++i) {
    const name = i === 0 ? "document" : ("document-" + i)
    const ctx = { path: name
                , zoom:(path,dom) => {
                    const v = document.getElementById("value-pane")
                    v.innerHTML = ""
                    v.appendChild(dom)
                  }
                , showPath:(path,enter) => {
                    if (enter) {
                      stack.push(path)
                    } else {
                      stack.pop()
                    }
                    document.getElementById("value-path").textContent =
                      stack.length === 0 ? "" : stack[stack.length - 1]
                  }
                }
    roots[i] = renderValue(ctx,inputs,vs[i])
    valuePane.appendChild(roots[i])
  }
}

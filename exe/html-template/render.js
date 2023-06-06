function main() {
  const valuePane = document.getElementById("value-pane")
  const inputs = hexView(data.inputs)
  const vs = data.values
  const n = vs.length
  for (let i = 0; i < n; ++i) {
    const vdom = renderValue(inputs,vs[i])
    valuePane.appendChild(vdom)
  }
}

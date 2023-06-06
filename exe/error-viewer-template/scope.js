function renderScope(inputs,vs) {

  const dom = document.createElement('div')
  dom.classList.add('scope')

  for (const name in vs) {
    const b = document.createElement('div')
    b.classList.add('binding')

    const x = document.createElement('div')
    x.classList.add('variable')
    x.textContent = name
    b.appendChild(x)
    const [annot,val] = isTraced(inputs,vs[name])
    const valDom = renderValueUntraced(inputs,val)
    b.appendChild(valDom)
    annot(x)
    annot(valDom)
    dom.appendChild(b)
  }

  return dom
}


function renderLiteral(x) {
  const dom = document.createElement("div")
  dom.classList.add("literal")
  dom.classList.add("value")
  dom.textContent = x
  return dom
}

function renderStruct(inputs,s) {
  const dom = document.createElement("div")
  dom.classList.add("struct")
  dom.classList.add("composite-value")
  for (const l in s) {
    const f = document.createElement("div")
    f.classList.add("field")
    const x = document.createElement("div")
    x.classList.add("label")
    x.textContent = l
    f.appendChild(x)

    const y = document.createElement("div")
    y.classList.add("field-value")
    const [annot, val] = isTraced(inputs,s[l])
    const valDom = renderValueUntraced(inputs,val)
    annot(x)
    annot(valDom)
    y.appendChild(valDom)
    f.appendChild(y)

    dom.appendChild(f)
  }
  return dom
}

function renderArray(inputs, x) {

  const doms = []

  let allBytes = true
  let charFrac = 0

  function unTrace(v) {
    if (typeof(v) !== "object") return v
    const u = v["$$traced"]
    if (u !== undefined) return u.value
    return v
  }


  {
    const dom1 = document.createElement("div")
    dom1.classList.add("array")
    dom1.classList.add("composite-value")

    for (let i = 0; i < x.length; ++i) {
      let v = x[i]
      dom1.appendChild(renderValue(inputs,v))
      v = unTrace(v)
      if (typeof(v) !== "number" || v < 0 || v >= 256) {
        allBytes = false
      }
    }
    doms.push(dom1)
  }

  if (allBytes) {
    let charCount = 0
    let str = "\""
    for (let i = 0; i < x.length; ++i) {
      const c = unTrace(x[i])
      if (c < 32 || c >= 127) {
        const s = c.toString(16)
        str += (s.length < 2) ? ("\\0" + s) : ("\\" + s)
      }
      else {
        charCount += 1
        switch (c) {
          case 92: str += "\\\\"; break
          case 34: str += "\\\""; break
          default: str += String.fromCharCode(c)
        }
      }
    }
    str += "\""
    const dom2 = renderLiteral(str)
    charFrac   = x.length > 0 ? charCount / x.length : 0
    doms.push(dom2)
  }

  if (doms.length === 1) return doms[0]

  if (x.length === 0) return doms[1]

  let curView = charFrac > 0.5 ? 1 : 0
  const dom = document.createElement("div")
  dom.classList.add("value")

  const menu = document.createElement("div")
  menu.classList.add("array-menu")
  menu.textContent = "..."
  menu.classList.add("clickable")

  function next() {
    doms[curView].classList.add("hidden")
    curView = (curView + 1) % doms.length
    doms[curView].classList.remove("hidden")
  }
  menu.addEventListener("click",next)
  dom.appendChild(menu)

  for (let i = 0; i < doms.length; ++i) {
    if (i !== curView) doms[i].classList.add("hidden")
    dom.appendChild(doms[i])
  }

  return dom
}

function renderUnion(inputs,l,x0) {

  const [annot,x] = isTraced(inputs,x0)
  if (typeof(x) === "object" && Object.keys(x).length === 0) {
    const dom = renderLiteral(l)
    annot(dom)
    return(dom)
  }

  const dom = document.createElement("div")
  dom.classList.add("union")
  dom.classList.add("composite-value")

  const t = document.createElement("div")
  t.classList.add("union-tag")
  t.textContent = l
  dom.appendChild(t)

  const el = document.createElement("div")
  el.classList.add("union-value")
  el.appendChild(renderValue(inputs,x))
  dom.appendChild(el)

  annot(dom)
  return dom
}

function renderMap(inputs,v) {
  const dom = document.createElement("div")
  dom.classList.add("map")
  dom.classList.add("composite-value")
  const n = v.length
  for (let i = 0; i < n; ++i) {
    const entry = v[i]
    const key = document.createElement("div")
    key.classList.add("map-key")
    key.appendChild(renderValue(inputs,entry[0]))

    const val = document.createElement("div")
    val.classList.add("map-val")
    val.appendChild(renderValue(inputs,entry[1]))

    const row = document.createElement("div")
    row.classList.add("map-row")
    row.appendChild(key)
    row.appendChild(val)
    dom.appendChild(row)
  }
  return dom
}

function renderInput(inputs,v) {
  return renderLiteral("XXX: input")
}

function renderIterator(inputs,v) {
  return renderLiteral("XXX: iterator")
}

function isTraced(inputs, tv) {
  if (typeof(tv) !== "object") return [(x) => x, tv]

  const tr = tv["$$traced"]
  if (tr === undefined) return [(x) => x, tv]
  const trace = tr.trace

  function highlight(ev) {
    ev.stopPropagation()
    for(let i = 0; i < trace.length; ++i) {
      const t = trace[i]
      const input = inputs[t.input]
      input.deselect()
      const rs = trace[i].ranges
      for(let r = 0; r < rs.length; ++r) {
        const rng = rs[r]
        input.highlightRange(rng[0],rng[1])
      }
    }
  }

  return [ (dom) => { dom.classList.add("clickable")
                      dom.addEventListener("click", highlight) }
         , tr.value
         ]
}

function renderValue(inputs,v) {
  const [annot, val] = isTraced(inputs,v)
  const dom = renderValueUntraced(inputs,val)
  annot(dom)
  return dom
}


function renderValueUntraced(inputs,v) {
  switch (typeof v) {

    case "object": {

      if (v === null) {
        return renderLiteral("nothing")
      }

      if (Array.isArray(v)) return renderArray(inputs, v)

      const keys = Object.keys(v)
      if (keys.length === 1) {
        const k   = keys[0]
        const val = v[k]

        switch (k) {

          case "$$inf":
            return renderLiteral("Infinity")

          case "$$nan":
            return renderLiteral("NaN")

          case "$$just":
            return renderUnion(inputs, "just",val)

          case "$$map":
            return renderMap(inputs, val)

          case "$$input":
            return renderInput(inputs, val)

          case "$$builder":
            return renderArray(inputs, val)

          case "$$iterator":
            return renderIterator(inputs, val)

          default:
            if (k.length > 0) {
              if (k.charAt(0) === '$')
                return renderUnion(inputs, k.substring(1),val)
              else
                return renderStruct(inputs, v)
            }
        }
      }

      return renderStruct(inputs, v)
    }

    default:
      return renderLiteral(v)
  }

}

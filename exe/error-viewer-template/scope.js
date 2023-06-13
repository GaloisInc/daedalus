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

    const ctx = { path: [name]
                , inputs: inputs
                , showPath: () => {}
                , zoom: () => {}
                , elements: {}
                }

    b.appendChild(renderValue(ctx,x,vs[name]))
    dom.appendChild(b)
  }

  return dom
}

function extendPath(ctx,p) {
  const newPath = []
  newPath.push(...ctx.path)
  newPath.push(p)
  return { ...ctx, path: newPath }
}

function pathToString(path) {
  let s = ""
  for (let i = 0; i < path.length; ++i) {
    s += path[i]
  }
  return s
}

function renderLiteral(ctx, x) {
  const dom = document.createElement("div")
  dom.classList.add("value-literal")
  dom.textContent = x
  return dom
}

function renderTextLiteral(ctx, x) {
  const dom = document.createElement("div")
  dom.classList.add("value-text")
  dom.textContent = x
  return dom
}


function renderStruct(ctx, s) {
  const dom = document.createElement("div")
  dom.classList.add("struct")
  dom.classList.add("composite-value")

  for (const l in s) {
    const fld = document.createElement("div")
    fld.classList.add("field")

    const keyDom = document.createElement("div")
    keyDom.classList.add("label")
    keyDom.textContent = l
    fld.appendChild(keyDom)

    const valDom = document.createElement("div")
    valDom.classList.add("field-value")

    const newCtx = extendPath(ctx, "." + l)
    valDom.appendChild(renderValue(newCtx,fld,s[l]))

    fld.appendChild(valDom)
    dom.appendChild(fld)
  }

  return dom
}

function renderArray(ctx, x) {

  // empty array
  if (x.length === 0) {
    const dom = document.createElement("div")
    dom.classList.add("value-meta")
    dom.textContent = "empty array"
    return dom
  }

  // differnt views for the array
  const doms = []

  let allNums = true

  function unTrace(v) {
    if (typeof(v) !== "object") return v
    const u = v["$$traced"]
    if (u !== undefined) return u.value
    return v
  }


  { // Normal array view
    const dom1 = document.createElement("div")
    dom1.classList.add("array")
    dom1.classList.add("composite-value")
    dom1.classList.add("value")

    const ctx1 = ctx
    for (let i = 0; i < x.length; ++i) {
      const entry = document.createElement("div")
      entry.classList.add("array-entry")
      const entry_index = document.createElement("div")
      entry_index.classList.add("value-meta")
      entry_index.textContent = i
      entry.appendChild(entry_index)

      const newCtx = extendPath(ctx1,"[" + i + "]")
      const valDom = renderValue(newCtx, entry, x[i])
      entry.appendChild(valDom)
      dom1.appendChild(entry)

      if (typeof(unTrace(x[i])) !== "number") {
        allNums = false
      }
    }
    doms.push(dom1)
  }

  // String view
  if (allNums) {
    let str = ""

    charCount = 0
    for (let i = 0; i < x.length; ++i) {
      str += String.fromCodePoint(unTrace(x[i]))
    }
    const dom2 = renderTextLiteral(ctx,str)
    doms.push(dom2)
  }

  // we only have a single view.
  if (doms.length === 1) {
    return doms[0]
  }

  // Default view: use string if the thing is mostly characters
  let curView = 1 ///charFrac > 0.5 ? 1 : 0
  const dom = document.createElement("div")

  // Menu for switching views.
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

function renderUnion(ctx,l,x0) {


  const dom = document.createElement("div")
  dom.classList.add("union")
  dom.classList.add("composite-value")

  const t = document.createElement("div")
  t.classList.add("union-tag")
  t.textContent = l
  dom.appendChild(t)


  const el = document.createElement("div")
  el.classList.add("union-value")
  const newCtx = extendPath(ctx,":" + l)
  el.appendChild(renderValue(newCtx,el,x0))
  dom.appendChild(el)

  return dom
}


function renderMap(ctx,v) {

  const n = v.length

  if (n === 0) {
    const dom = document.createElement("div")
    dom.classList.add("value-meta")
    dom.textContent = "empty map"
    return dom
  }



  const dom = document.createElement("div")
  dom.classList.add("map")
  dom.classList.add("composite-value")

  for (let i = 0; i < n; ++i) {
    const entry = v[i]
    const key = document.createElement("div")
    key.classList.add("map-key")
    const keyCtxt = extendPath(ctx,"#key-" + i)
    key.appendChild(renderValue(keyCtxt,key,entry[0]))

    const val = document.createElement("div")
    val.classList.add("map-val")
    const valCtxt = extendPath(ctx,"#value-" + i)
    val.appendChild(renderValue(valCtxt,val,entry[1]))

    const row = document.createElement("div")
    row.classList.add("map-row")
    row.appendChild(key)
    row.appendChild(val)
    dom.appendChild(row)
  }
  return dom
}

function renderInput(ctx,v) {
  return renderLiteral(ctx,"XXX: input")
}

function renderIterator(ctx,v) {
  return renderLiteral(ctx,"XXX: iterator")
}

function isTraced(ctx, tv) {
  const inputs = ctx.inputs
  if (typeof(tv) !== "object") return [(x) => x, tv]

  const tr = tv["$$traced"]
  if (tr === undefined) return [(x) => x, tv]
  const trace = tr.trace

  function highlight(ev) {
    if (ev.ctrlKey || ev.shiftKey) return
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

function renderValue(ctx,owner,v) {
  const [annot, val] = isTraced(ctx,v)
  const container = document.createElement("div")

  const dom = renderValueUntraced(ctx,val)


  const own = owner === null ? container : owner
  annot(own)

  // Hover and set the path
  own.addEventListener("mouseover", (ev) => {
    ev.stopPropagation()
    ctx.showPath(own,ctx)
  })

  // Zoom into an element
  own.addEventListener("click",(ev) => {
    if (!ev.ctrlKey) return
    ev.stopPropagation()
    ctx.zoom(ctx)
  })


  // Collapsing elements -----------------------------------
  let collapsed = false
  const mini = document.createElement("div")
  mini.textContent = "*"
  mini.classList.add("value-mini")
  mini.classList.add("hidden")

  function toggleSize(ev) {
    if (!ev.shiftKey) return
    ev.stopPropagation()
    collapsed = !collapsed

    if (collapsed) {
      dom.classList.add("hidden")
      mini.classList.remove("hidden")
    } else {
      dom.classList.remove("hidden")
      mini.classList.add("hidden")
    }
  }
  own.addEventListener("click",toggleSize)
  mini.addEventListener("click",toggleSize)
  // -- End Collapse --------------------------------------------

  container.appendChild(mini)
  container.appendChild(dom)

  ctx.elements[pathToString(ctx.path)] = v

  return container
}


function renderValueUntraced(ctx,v) {
  switch (typeof v) {

    case "object": {

      if (v === null) {
        return renderLiteral(ctx,"nothing")
      }

      if (Array.isArray(v)) return renderArray(ctx, v)

      const keys = Object.keys(v)
      if (keys.length === 1) {
        const k   = keys[0]
        const val = v[k]

        switch (k) {

          case "$$inf":
            return renderLiteral(ctx,"Infinity")

          case "$$nan":
            return renderLiteral(ctx,"NaN")

          case "$$just":
            return renderUnion(ctx,"just",val)

          case "$$map":
            return renderMap(ctx,val)

          case "$$input":
            return renderInput(ctx,val)

          case "$$builder":
            return renderArray(ctx,val)

          case "$$iterator":
            return renderIterator(ctx,val)

          default:
            if (k.length > 0) {
              if (k.charAt(0) === '$')
                return renderUnion(ctx,k.substring(1),val)
              else
                return renderStruct(ctx,v)
            }
        }
      }

      return renderStruct(ctx,v)
    }

    default:
      return renderLiteral(ctx,v)
  }

}

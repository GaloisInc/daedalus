/* type Region  = { from: integer, to: integer, tag: StringSet }
   type Regions = [Region]
   invariant(Regiong): the regions are sorted an non-overlapping.
   type StringSet = { [string]: bool }
*/



/* Extend an exisitng set of regions with a new one.  */
function addRegion(rs /* Regions */, r /* Region */) {
  const result = []
  let i
  for (i = 0; i < rs.length && rs[i].to < r.from; ++i) {
    result.push(rs[i])
  }

  let newR = r

  for (; i < rs.length; ++i) {

    let front = rs[i]

    if (newR.to < front.from) break

    let left, right
    if (newR.from <= front.from) { left = newR;  right = front}
                            else { left = front; right = newR }

    const overlap = { from: right.from
                    , to:   Math.min(left.to, right.to)
                    , tag:  mergeStringSet(left.tag,right.tag)
                    }

    const ll = { from: left.from
               , to:   overlap.from - 1
               , tag:  left.tag
               }

    if (newR.to <= front.to) { left = newR;  right = front }
                        else { left = front; right = newR }

    const rr = { from: overlap.to + 1
               , to:   right.to
               , tag:  right.tag
               }

    function valid(r) { return r.from <= r.to }

    if (valid(ll)) result.push(ll)
    if (valid(rr)) {
      result.push(overlap)
      newR = rr
    } else {
      newR = overlap
    }

  }

  result.push(newR)

  for (; i < rs.length; ++i) {
    result.push(rs[i])
  }

  return mergeAdjacent(result)
}


// Given some sorted, non-overlapping region, merge adjacent
// regions with the same tags, and normalize the sets
function mergeAdjacent(rs) {
  if (rs.length < 1) return []

  let cur      = rs[0]
  const result = []

  function norm(r) { r.tag = normalizeStringSet(r.tag) }
  function emit() {
    if (isEmptySringSet(cur)) return
    result.push(cur)
  }

  norm(cur)

  for (let i = 1; i < rs.length; ++i) {
    const r = rs[i]
    norm(r)

    if ((cur.to + 1) === r.from && sameStringSet(cur.tag,r.tag)) {
      cur.to = r.to
    } else {
      emit()
      cur = r
    }
  }

  emit()
  return result
}



// Render a line of text.
function renderLine(text /* string */, regions /* Regions */) {
  const dom = document.createElement('div')
  dom.classList.add('line-content')

  function addWord(a,b,cs) {
    const word = document.createElement('span')
    for (const c in cs) word.classList.add(c)
    word.textContent = text.substring(a,b)
    dom.appendChild(word)
  }

  let done = 0
  for (let i = 0; i < regions.length; ++i) {
    const r = regions[i]
    if (done < r.from) addWord(done,r.from,{})
    addWord(r.from,r.to+1,r.tag)
    done = r.to + 1
  }
  if (done < text.length) addWord(done,text.length,{})

  return dom
}



function renderFile(content /* string */) {
  const lines       = content.split('\n')
  const linesDom    = []
  const linesReg    = []
  let   lineUpdate  = {}

  const contentD = document.createElement('div')
  contentD.classList.add('source-content')
  for (let i = 0; i < lines.length; ++i) {
    const ln = document.createElement('div')
    ln.classList.add('line')

    const no = document.createElement('div')
    no.classList.add('line-number')
    no.textContent = i + 1
    ln.appendChild(no)

    const txt = renderLine(lines[i],[])
    ln.appendChild(txt)

    contentD.appendChild(ln)
    linesReg[i] = []
    linesDom[i] = txt
  }

  // 1 based
  // assumes from is before to
  function highlight(ln_from, col_from, ln_to, col_to, tag, on) {
    for (let l = ln_from - 1; l < ln_to; ++l) {
      const txt   = lines[l]
      if (txt.length === 0) continue
      const from  = l == (ln_from - 1) ? (col_from - 1) : 0
      const to    = l == (ln_to   - 1) ? (col_to   - 1) : txt.length - 1

      const reg   = { from: from, to: to, tag: singletonStringSet(tag,on) }
      const newR  = addRegion(linesReg[l], reg)
      linesReg[l] = newR
      lineUpdate[l] = true
    }
  }

  function doUpdates() {
    for (const l in lineUpdate) {
      const txt   = lines[l]
      if (txt.length === 0) continue
      const newD  = renderLine(txt, linesReg[l])
      const dom   = linesDom[l]
      linesDom[l] = newD
      dom.replaceWith(newD)
    }
    lineUpdate = {}
  }

  // 1 based.  Scroll a line into view
  function see(x) {
    x = x - 1
    if (x < 0 || x >= linesDom.length) return
    linesDom[x].scrollIntoView({behavior: 'smooth'})
  }

  return { dom: contentD, highlight: highlight, update: doUpdates, focus: see }
}


function renderFiles(fs) {
  let rendered = {}
  let focused = null

  const code = document.getElementById('source-pane')
  const name = document.getElementById('source-name')

  for (const file in fs) {
    const doc = fs[file]
    const r = renderFile(doc.text)
    for (const s in doc.syntax) {
      const xs = doc.syntax[s]
      for (let i = 0; i < xs.length; ++i) {
        const [ [a,b], [c,d] ]= xs[i]
        r.highlight(a,b,c,d,s,true)
      }
    }

    rendered[file] = r
  }

  function focus(file,line) {
    const f = rendered[file]
    if (f === undefined) return
    name.textContent = file
    code.innerHTML = ''
    code.appendChild(f.dom)
    f.update()
    f.focus(line)
  }

  function select(file,fromL,fromC,toL,toC,yes) {
    const f = rendered[file]
    if (f === undefined) {
      console.log("UNKNOWN FILE", file)
      return
    }
    focus(file,fromL)
    f.highlight(fromL,fromC,toL,toC,'selected',yes)
    f.update()
  }

  function list() {
    for (const i in rendered)
      console.log(i)
  }

  return { focus: focus, list: list, select: select }

}

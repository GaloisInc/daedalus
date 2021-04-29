const main = () => {
  const container = document.getElementById('container')
  container.appendChild(ddlValue(data))
}

const div = (c) => {
  const dom = document.createElement('div')
  dom.classList.add(c)
  return dom
}

const ddlNumber = (base,value) => {
  const dom = div('number')
  let next  = 10
  switch(base) {
    case 16:
      next = 0
      dom.textContent = '0x' + value.toString(base)
      break
    case 0:
      next = 10
      dom.textContent = "'" + String.fromCharCode(value) + "'"
      break
    default:
      next = 16
      dom.textContent = value.toString(10)
  }
  dom.addEventListener('click',() => dom.replaceWith(ddlNumber(next,value)))
  return dom
}

const ddlArray = (rep,value) => {
  const dom = div('array')
  const control = div('control')
  control.textContent = 'array: ' + value.length
  dom.appendChild(control)

  let next = rep

  switch(rep) {
    case 'guess':
      if (value.length > 50) return ddlArray('closed',value)
      let asStr = true
      for (let i = 0; i < value.length; ++i) {
        const v = value[i]
        if (typeof v !== 'number' || (v < 32) || v > 122) {
          asStr = false
          break
        }
      }
      return ddlArray(asStr ? 'text' : 'open', value)

    case 'open':
      next = 'text'
      for (let i = 0; i < value.length; ++i) {
        const ent = div('array-entry')
        ent.appendChild(ddlValue(value[i]))
        dom.appendChild(ent)
      }
    break

    case 'text':
      next = 'closed'
      let str = ""
      for (let i = 0; i < value.length; ++i) {
        const v = value[i]
        const s = (typeof v === 'number') ? String.fromCharCode(v) : ' '
        str += s
      }
      const txt = div('array-text')
      txt.textContent = str
      dom.appendChild(txt)
      break

    default:
      next = 'open'
      const dots = div('array-closed')
      dots.textContent = '...'
      dom.appendChild(dots)
      break
  }
  control.addEventListener('click',() => dom.replaceWith(ddlArray(next,value)))

  return dom
}

const ddlMap = (value) => {
  const dom = div('map')
  for (let i = 0; i < value.length; ++i) {
    const entry = value[i]
    const ent = div('map-entry')
    const keyDom = div('map-key')
    keyDom.appendChild(ddlValue(entry[0]))
    const valDom = div('map-value')
    valDom.appendChild(ddlValue(entry[1]))
    ent.appendChild(keyDom)
    ent.appendChild(valDom)
    dom.appendChild(ent)
  }
  return dom
}

const ddlValue = (value) => {
  switch (typeof value) {

    case 'number':
      return ddlNumber(10,value)

    case 'boolean': {
      const dom = div('bool')
      dom.textContent = value
      return dom
    }

    case 'object':

      if (value === null) {
        return div('nothing')
      }

      if (Array.isArray(value)) return ddlArray('guess',value)

      if (value['$$map'] !== undefined) return ddlMap(value['$$map'])

      let union = null
      let unit  = true
      for (const key in value) {
        if (key.charAt(0) === '$') union = key.substring(1)
        unit = false
        break
      }

      if (unit) return div('unit')

      if (union !== null) {
        const dom = div('union')
        const key = div('tag')
        key.textContent = union
        const val = ddlValue(value['$' + union])
        dom.appendChild(key)
        dom.appendChild(val)
        return dom
      }

      const dom = div('struct')
      for (const key in value) {
        const entry = div('entry')
        const keyDom = div('key')
        keyDom.textContent = key
        const valDom = div('value').appendChild(ddlValue(value[key]))
        entry.appendChild(keyDom)
        entry.appendChild(valDom)
        dom.appendChild(entry)
      }
      return dom
  }

  const dom = div('unknown')
  dom.textContent = JSON.stringify(value)
  return dom
}



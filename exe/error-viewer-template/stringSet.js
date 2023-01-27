function singletonStringSet(x,v) {
  const r = {}
  r[x] = v
  return r
}




function normalizeStringSet(xs) {
  const zs = {}
  for (const a in xs)
    if (xs[a]) zs[a] = true
  return zs
}


// Assumes a normalized StringSet
function isEmptySringSet(xs) {
  return Object.keys(xs).length === 0
}

// Assumes a normalized StringSet
function sameStringSet(xs,ys) {
  const keys1 = Object.keys(xs)
  const keys2 = Object.keys(ys)
  if (keys1.length !== keys2.length) return false
  for (const key of keys1)
    if (xs[key] !== ys[key]) return false
  return true
}

// Element deleted if either side has it set to false
function mergeStringSet(xs,ys) {
  const zs = {}
  for (const a in xs) zs[a] = xs[a]
  for (const a in ys) {
    if (zs[a] === false) continue
    zs[a] = ys[a]
  }
  return zs
}





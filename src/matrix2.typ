#import "complex.typ": *

#let _mform(m) = {
  let con = m.map(x => x.map(y => y))
  return math.mat(..con)
}

#let _munform(m) = {
  if m.has("rows") {
    return m.at("rows")
  }
  return _munform(m.body)
}

#let _mtrans-content(m) = {
  let i = 0
  let j = 0

  while i < m.at(0).len() {
    while j < m.len() {
      let temp = m.at(j).at(i)
      m.at(j).at(i) = m.at(i).at(j)
      m.at(i).at(j) = temp
      j += 1
    }
    i += 1
  }
  return m
}

#let _minit(m) = {
  let dic = ("type", "matrix")
  let rows = _munform(m)
  let first-len = rows.at(0).len()
  assert(rows.all(x => x.len() == first-len))
  dic.insert("rows", rows)
  dic.insert("cols", _mtrans(rows))
  dic.insert("x", rows.at(0).len())
  dic.insert("y", rows.len())
  return dic
}

#let mrows(m) = _minit(m).at("rows")
#let mrow(m,n) = mrows(m).at(n)
#let mcols(m) = _minit(m).at("cols")
#let mcol(m,n) = mcols(m).at(n)

#let mget(m,x,y) = mcols(m).at(x).at(y)
#let my(m) = _munform(m).len()
#let mx(m) = _munform(m).at(0).len()

#let mtrans(m) = _mform(_mtrans-content(_munform(m)))
#let mconj(m) = _mform(_mtrans-content(_munform(m)).map(row => row.map(col => cconj(col))))

#let mfill(x,y,n) = {
  let row = 0
  let new = (())

  while row < y {
    let col = 0
    let cur = ()
    while col < x {
      cur.push(n)
      col += 1
    }
    new.push(cur)
    row += 1
  }
  return _mform(new)
}

#let mzero(x,y) = {
  mfill(x,y,0)
}

#let miden(x,y) ={
  let m = _munform(mzero(x,y))
  let row = 0
  while row < y and row < x {
    m.at(row).at(row) = 1
    row += 1
  }
  return _mform(m)
}

#let madd(l,r) = {
  let lx = mx(l)
  let ly = my(l)
  let rx = mx(r)
  let ry = my(r)

  assert(lx == rx and ly == ry, message: "Matrix dimensions must match")

  l = _munform(l)
  r = _munform(r)
  
  let row = 0
  let new = _munform(mzero(lx, ly))

  while row < ly {
    let col = 0
    while col < lx {
      new.at(row).at(col) = cadd(l.at(row).at(col), r.at(row).at(col))
      col += 1
    }
    row += 1
  }
  return _mform(new)
}

#let mrow-switch(m,fst,snd) = {
  let rows = _munform(m)
  let temp = rows.at(fst)
  rows.at(fst) = rows.at(snd)
  rows.at(snd) = temp
  return _mform(rows)
}

#let mrow-mul(m,row,scalar) = {
  m = _munform(m)
  let new = m.at(row).map(x => cmul($#x$, $#scalar$))
  m.at(row) = new
  return _mform(m)
}

#let mrow-add(m,fst,snd,scalar) = {
  m = _munform(m)

  let row1 = m.at(fst)
  let row2 = m.at(snd)

  let i = 0
  while i < row1.len() {
    row1.at(i) = cadd(row1.at(i), cmul(row2.at(i), scalar))
    i += 1
  }
  m.at(fst) = row1
  return _mform(m)
}
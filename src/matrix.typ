#import "custom-type.typ": *
#import "complex.typ": *

//todo  - determinant?, adj, inverse, rank?, row-reduce?, diagonal, row operations, is symmtetric/hermitian/unitary/orth...

#let _mrows(m) = m.fields().body.fields().rows
#let _mtrans(m) = {
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

// init mat "object"
#let minit(m) = {
  let dic = custom-type("matrix")
  let rows = _mrows(m)
  let first-len = rows.at(0).len()
  assert(rows.all(x => x.len() == first-len))
  dic.insert("rows", rows)
  dic.insert("cols", _mtrans(rows))
  dic.insert("x", rows.at(0).len())
  dic.insert("y", rows.len())
  return dic
}

#let mrows(m) = m.at("rows")
#let mrow(m,idx) = mrows(m).at(idx)
#let mcols(m) = m.at("cols")
#let mcol(m,idx) = mcols(m).at(idx)
#let mget(m,x, y) = m.at("cols").at(x).at(y)
#let mx(m) = m.at("x")
#let my(m) = m.at("y")
#let mtrans(m) = minit(_mtrans(m))

#let mconj(m) = {
  let row = 0
  let col = 0
  let x = mx(m)
  let y = my(m)

  while i < x {
    while j < y {
      let temp = cconj(new.at(j).at(i))
      new.at(j).at(i) = cconj(new.at(i).at(j))
      new.at(i).at(j) = temp
      j += 1
    }
    i += 1
  }
  return minit(new)
}


#let madd(l,r) = {

  let x = mx(l)
  let y = my(l)
  assert(x == mx(r) and y == my(r))

  let i = 0
  let j = 0
  let rows = (())

  while i < y {
    while j < x {
      rows.at(i).push(cadd(mget(l,i,j),mget(r,i,j)))
      j += 1
    }
    i += 1
  }
  return minit(rows)
}

// row operations
#let mrow-switch(m,st,nd) = {
  let new = m
  let temp = mrows(new).at(st)
  mrows(new).at(st) = mrows(new).at(nd)
  mrows(new).at(nd) = temp
}
#let mrow-mul(m,row,scalar) = {
  let new = m
  let new-row = mrows(m).at(row).map(x => cmul(x,scalar))
  mrows(new).at(row) = new-row
  return new
}
//#let mropw-addrows type 3 do later



#let mdiaginit(..vals) = {
  let i = 0
  let len = vals.pos().len()
  let vals = vals
  let rows = (())

  while i < len {
    let j = 0
    while j < len {
      if i == j {
        rows.at(i).push(vals.remove(0))
      }
      else {
        rows.at(i).push(0)
      }
    }
  }
  return minit(rows)
}

#let mtrace(m) = {
  let x = mx(m)
  assert(x == my(m))

  let i = 0
  let sum = cinit()

  while i < x {
    sum = cadd(sum,cinit(mget(m,i,i)))
  }
  return sum
}

#let mscal(l,r) = {
  assert(l.len == r.len)
  r.map(x => x.map(y => cmul(l, y)))
}

#let mmul(l,r) = {
  let lRows = l.fields().body.fields().rows
  let rRows = r.fields().body.fields().rows

  assert.eq(lRows.at(0).len(), rRows.len())

  let out = (())
  let row = 0
  
  while row < lRows.len() {
    let col = 0
    let curRow = ()

    while col < rRows.at(0).len() {
      let idx = 0
      let curVal = 0

      while idx < lRows.at(0).len() {
        curVal += float(lRows.at(row).at(idx).text) * float(rRows.at(idx).at(col).text)
        idx += 1
      }
      curRow.push(curVal)
      col += 1
    }
    out.push(curRow)
    row += 1
  }

  let con = out.map(x => x.map(y => y))

  return math.mat(..con)

}

#let mmul2(l,r) = {
  assert(my(l) == mx(r))
  let i = 0
  let j = 0

  let r = mx(l)
  let c = my(r)
  let rows = (())

  while i < r {
    let cur-row = ()
    while j < c {
      cur-row.push(mscalmul(mrows(l).at(i),mcols(r).at(j)))
    }
    rows.push(cur-row)
  }
  return minit(rows)
}

#let mform(m) = {
  let con = m.map(x => x.map(y => y))
  return math.mat(..con)
}
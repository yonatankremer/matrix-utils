// TODO - non-decimal fraction visualization, powers, argument
#import "custom-type.typ": *


// "private" utilities; use the "complex" type

#let _cinit(re,im) = {
  //assert(is-number(re) and is-number(im), message: "inputs must be numbers.")
  let dic = custom-type("complex")
  let vals = (re,im)
  dic.insert("re", re)
  dic.insert("im", im)
  return dic
}

#let cre-num(n) = n.at("re")
#let cim-num(n) = n.at("im")

#let _cform(n) = { // black magic
  let re = cre-num(n)
  let im = cim-num(n)
  let resgn = none
  let imsgn = none

  if re < 0 {resgn = sym.minus}
  if im < 0 {imsgn = sym.minus}
  else if im > 0 and re != 0 {imsgn = sym.plus}

  re = calc.abs(re)
  im = calc.abs(im)

  if re == 0 and im == 0 {
    $0$
  }

  else if re != 0 and im == 0 {
    $resgn re$
  }

  else if re == 0 and im != 0 {
    if im == 1 {
      $imsgn i$
    }
    else {
      $imsgn im i$
    }
  }

  else {
    if im == 1 {
      $resgn re imsgn i$
    }
    else {
      $resgn re imsgn im i$
    }
  }
}

#let content-to-string(n) = {

  if type(n).text == "string" {}

  else if n.has("children") {
    n = n.children.map(x => str(x.text)).join()
  }

  else if n.has("body") {
    n = n.body.map(x => str(x.text)).join()
  }

  else if n.has("text") {
    n = n.text
  }
  return n.replace(" ","")
}

#let sign-split(n) = {

  let parts = ()
  let sum = ""
  let i = 0

  while i < n.len() {
    let char = n.at(i)

    if i > 0 and (char == "+" or char == "-") {
      parts.push(sum)
      sum = ""
    }
    sum += char
    i += 1
  }

  if sum != "" {
    parts.push(sum)
  }

  return parts
}

#let _cunform(n) = {
  
  if n == none or n == []  {
    return _cinit(0,0)
  }

  
  n = content-to-string(n)
  if not n.contains("i") {
    return _cinit(to-number(n),0)
  }

  let sgn-split = sign-split(n).map(x => x.replace("i","")).map(
    x => {
      if x == "-" {
        "-1"
      }
      else if x == "+" or x == "" {
        "1"
      }
      else {
        x
      }
    }
  )

  if sgn-split.len() == 1 { // we know there's an i
    return _cinit(0,to-number(sgn-split.at(0)))
  }

  return _cinit(to-number(sgn-split.at(0)),to-number(sgn-split.at(1)))
}




// from here on, all recieve and return content. use of "primitive" functions is prefered.
// use the unform function in the start, form when returning.


#let complex(re,im) = _cform(_cinit(re,im))

#let cre(n) = _cform(cre-num(n))
#let cim(n) = _cform(cim-num(n))

#let ceq(l,r) = {
  let l = _cunform(l)
  let r = _cunform(r)
  return cre-num(l) == cre-num(r) and cim-num(l) == cim-num(r)
}

#let cabs(n) = {
  let n = _cunform(n)
  let real = cre-num(n)
  let im = cim-num(n)
  let real-sq = calc.pow(real,2)
  let im-sq = calc.pow(im,2)
  let out = _cinit(calc.sqrt(real-sq+im-sq), 0)
  return _cform(out)

}

#let cadd(l,r) = {
  let l = _cunform(l)
  let r = _cunform(r)

  let re = cre-num(l) + cre-num(l)
  let im = cim-num(l) + cim-num(r)

  let out = _cinit(re,im)
  return _cform(out)
}

#let csub(l,r) = {
  let l = _cunform(l)
  let r = _cunform(r)

  let re = cre-num(l) - cre-num(l)
  let im = cim-num(l) - cim-num(r)

  let out = _cinit(re,im)
  return _cform(out)
}

#let cmul(l,r) = {
  let l = _cunform(l)
  let r = _cunform(r)

  let l-re = cre-num(l)
  let l-im = cim-num(l)
  let r-re = cre-num(r)
  let r-im = cim-num(r)

  let re = l-re * r-re - l-im * r-im
  let im = l-re * r-im + l-im * r-re

  let out = _cinit(re,im)
  return _cform(out)
}

#let cconj(n) = {
  let n = _cunform(n)
  let out = _cinit(cre-num(re),-cre-num(im))
  return _cform(out)
}
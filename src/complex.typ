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

#let _cre-num(n) = n.at("re")
#let _cim-num(n) = n.at("im")

#let _cform(n) = { // black magic
  let re = _cre-num(n)
  let im = _cim-num(n)
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

#let _content-to-string(n) = {

  let out = n
  let ty = type(n).text


  if ty == "string" {
    out = n
  }
  else if ty == "content" {
    if n.has("text") {
      out = _content-to-string(n.text)
    }
    else if n.has("body") {
      out = _content-to-string(n.body)
    }
    else if n.has("children")  {
    out = n.children.map(x => _content-to-string(x)).join()
    }
  }
  out = out.replace(" ","")
  return out
}

#let _sign-split(n) = {

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

#let _cunform(n) = { // blacker magic
  if n == none or n == []  {
    return _cinit(0,0)
  }
 
  n = _content-to-string(n)
  if not n.contains("i") {
    return _cinit(to-number(n),0)
  }



  let sgn-split = _sign-split(n).map(x => x.replace("i","")).map(
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




// from here on, all recieve and return content. use of "primitive" functions privately is prefered though.
// use the unform function in the start, form when returning.


#let complex(re,im) = _cform(_cinit(re,im))

#let cre(n) = _cform(_cre-num(n))
#let cim(n) = _cform(_cim-num(n))

#let ceq(l,r) = {
  let l = _cunform(l)
  let r = _cunform(r)
  return _cre-num(l) == _cre-num(r) and _cim-num(l) == _cim-num(r)
}

#let cabs(n) = {
  let n = _cunform(n)
  let real = _cre-num(n)
  let im = _cim-num(n)
  let real-sq = calc.pow(real,2)
  let im-sq = calc.pow(im,2)
  let out = _cinit(calc.sqrt(real-sq+im-sq), 0)
  return _cform(out)

}

#let cadd(l,r) = {
  let l = _cunform(l)
  let r = _cunform(r)

  let re = _cre-num(l) + _cre-num(l)
  let im = _cim-num(l) + _cim-num(r)

  let out = _cinit(re,im)
  return _cform(out)
}

#let csub(l,r) = {
  let l = _cunform(l)
  let r = _cunform(r)

  let re = _cre-num(l) - _cre-num(l)
  let im = _cim-num(l) - _cim-num(r)

  let out = _cinit(re,im)
  return _cform(out)
}

#let cmul(l,r) = {
  let l = _cunform(l)
  let r = _cunform(r)

  let l-re = _cre-num(l)
  let l-im = _cim-num(l)
  let r-re = _cre-num(r)
  let r-im = _cim-num(r)

  let re = l-re * r-re - l-im * r-im
  let im = l-re * r-im + l-im * r-re

  let out = _cinit(re,im)
  return _cform(out)
}

#let cconj(n) = {
  let n = _cunform(n)
  let out = _cinit(_cre-num(n),-_cre-num(n))
  return _cform(out)
}
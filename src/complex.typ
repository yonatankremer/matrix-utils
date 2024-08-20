#let _is-number(t) = {
  let ty = type(t)
  return ty == "integer" or ty == "float"
}

#let _to-number(n) = {
  if n == "" {
    return 0
  }

  if n == "+" {
    return 1
  }

  if n == "-" {
    return -1
  }
  
  if n.contains(".") {
    return float(n)
  }
  
  return int(n)
}

#let _content-to-string(n) = {
  let out = n
  let ty = type(n)

  if n == [] {
    return ""
  }

  else if ty == "string" {
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
    out = n.children.map(_content-to-string).join()
    }

  }
  else if _is-number(n) {
    out = str(n)
  }
  

  out = out.replace([#sym.minus].text,"-")
  return out
}

#let _cinit(re,im) = {
  //assert(_is-number(re) and _is-number(im), message: "inputs must be numbers.")
  let vals = (re,im)
  let dic = ("re":re, "im":im, "type": "complex")
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
    return _cinit(_to-number(n),0)
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
    return _cinit(0,_to-number(sgn-split.at(0)))
  }

  return _cinit(_to-number(sgn-split.at(0)),_to-number(sgn-split.at(1)))
}

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

  let re = _cre-num(l) + _cre-num(r)
  let im = _cim-num(l) + _cim-num(r)

  let out = _cinit(re,im)
  return _cform(out)
}

#let csub(l,r) = {
  let l = _cunform(l)
  let r = _cunform(r)

  let re = _cre-num(l) - _cre-num(r)
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
  let out = _cinit(_cre-num(n),-_cim-num(n))
  return _cform(out)
}
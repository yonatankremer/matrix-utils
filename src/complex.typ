//todo - add visual fraction support, powers, polar form

#import "custom-type.typ": *

#let cinit(re:0,im:0) = {
  let dic = custom-type("complex")
  dic.insert("re", re)
  dic.insert("im", im)
  return dic
}

#let cre(n) = n.at("re")
#let cim(n) = n.at("im")

#let cform(n) = {
  let re = cre(n)
  let im = cim(n)
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
    if im == 1 or im == -1 {
      $imsgn i$
    }
    else {
      $imsgn im i$
    }
  }

  else {
    if im == 1 or im == -1 {
      $resgn re imsgn i$
    }
    else {
      $resgn re imsgn im i$
    }
  }


  
}

#let _cadd(l,r) = cinit(
  re: cre(l)+cre(r),
  im: cim(l)+cim(r)
)
#let cadd(l,r) = cform(_cadd(l,r))

#let _csub(l,r) = (
  re: cre(l)-cre(r),
  im: cim(l)-cim(r)
)
#let csub(l,r) = cform(_csub(l,r))

#let _cmul(l,r) = cinit(
  re: cre(l) * cre(r) - cim(l) * cim(r),
  im: cre(l) * cim(r) + cim(l) * cre(r)
)
#let cmul(l,r) = cform(_cmul(l,r))

#let _cconj(n) = cinit(re: cre(n), im: -cim(n))
#let cconj(n) = cform(_cconj(n))

#let _cabs(n) = calc.sqrt(calc.pow(cre(n),2)+calc.pow(cim(n),2))
#let cabs(n) = cform(_cabs(n))
#let _carg(n) = calc.atan(cim(n)/cre(n))
#let carg(n) = cform(_carg(n))
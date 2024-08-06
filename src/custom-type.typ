#let custom-type(name) = (
  "type": name
)

#let _type(t) = {
  let ty = str(type(t))
  if ty == "dictionary" {
    return t.at("type", default: "dictionary") 
  }
  if ty == "array" {
    return t.at("type", default: "array")
  }
  else {
    return type(t)
  }
}
#let type(t) = raw(str(_type(t)))

#let is-number(t) = {
  let ty = type(t).text
  return ty == "integer" or ty == "float" or ty == "complex"
}

#let to-number(n) = { // FIXME - intended to accept a number as a string, not covered for other text
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
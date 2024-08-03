#let custom-type(name) = (
  "type": name
)

#let _type(t) = {
  if type(t) == dictionary {
    return t.at("type", default: type(t)) 
  }
  else {
    return type(t)
  }
}
#let type(t) = raw(str(_type(t)))
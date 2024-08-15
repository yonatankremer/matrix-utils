#import "src/complex.typ": *
#import "src/matrix.typ": *


//#_cunform($1-i$)
#let c = _content-to-string($1-i$)
#let s = c.codepoints().slice(0,3).join()
#s.at(2)
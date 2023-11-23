#set page(width: 100pt, height: 100pt, margin: 0pt, fill: blue)

// gradient has white background in PDF export
#let transparent = rgb(255, 255, 255, 0)
#rect(fill: rgb(50%, 50%, 0%, 100))[
  #rect(fill: gradient.linear(transparent, transparent, rgb(50%, 0%, 0%), transparent, transparent))[#h(100%)]
]

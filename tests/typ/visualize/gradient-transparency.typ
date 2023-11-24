// Test transparency on gradients.

---
// Test transparency on linear gradients in various directions.
#set page(fill: gray)
#let transparent = rgb(255, 255, 255, 0)
#let grad(..args) = gradient.linear(
  transparent,
  red,
  transparent,
  space: rgb,
  ..args
)

#rect(fill: grad(angle: 0deg), height: 30pt, width: 100%)
#rect(fill: grad(angle: 45deg), height: 30pt, width: 100%)
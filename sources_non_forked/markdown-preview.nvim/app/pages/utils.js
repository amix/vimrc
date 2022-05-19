export const escape = (str) => {
  // escape html content
  const d = document.createElement('div')
  d.appendChild(document.createTextNode(str))
  return d.innerHTML
}

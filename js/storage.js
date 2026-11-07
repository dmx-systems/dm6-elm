// TODO: drop this module in favor of crdt.js (and rename that one to storage.js then)

export default {
  getJSON,
  getString,
  setJSON,
  setString
}

const key = 'dm6-elm'

function getJSON() {
  const modelStr = getString()
  return modelStr ? JSON.parse(modelStr) : null
}

function getString() {
  return localStorage.getItem(key)
}

function setJSON(model) {
  setString(JSON.stringify(model))
}

function setString(str) {
  localStorage.setItem(key, str)
}

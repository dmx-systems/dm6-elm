import { Elm } from './src/Main.elm'
import storage from './js/storage'
import image from './js/image'
import zip from './js/zip'
import { openDB } from 'idb'

// Init Elm app

const app = Elm.Main.init({
  flags: [storage.getJSON(), location.hash]
})

app.ports.storeModel.subscribe(model => {
  storage.setJSON(model)
})

// IndexedDB

const dbName = 'dm6-elm'
const dbPromise = openDB(dbName, 1, {
  upgrade(db) {
    image.upgrade(db)
  }
})

// Scrolling

const main = document.getElementById('main')
let timer
main.addEventListener('scroll', () => {
  clearTimeout(timer)
  timer = setTimeout(() => {
    app.ports.onScroll.send({x: main.scrollLeft, y: main.scrollTop})
  }, 200)   // debounce 200ms
}, {passive: true})

// Routing

window.addEventListener('hashchange', () => {
  app.ports.onHashChange.send(location.hash)
})

app.ports.setHash.subscribe(function (hash) {
  location.hash = hash    // creates history entries
})

// Init Modules

image.init(app, dbPromise)
zip.init(app)

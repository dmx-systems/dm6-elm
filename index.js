import { Elm } from './src/Main.elm'
import storage from './js/storage'
import image from './js/image'
import crdt from './js/crdt'
import zip from './js/zip'
import { openDB } from 'idb'

// IndexedDB

const dbName = 'dm6-elm'
const dbPromise = openDB(dbName, 1, {
  upgrade(db) {
    image.upgrade(db)
    crdt.upgrade(db)
  }
})

// Init Elm app

crdt.init(dbPromise)

const app = Elm.Main.init({
  flags: [
    storage.getJSON(),
    await crdt.getDoc(),
    location.hash
  ]
})

app.ports.storeModel.subscribe(([model, payload]) => {
  console.log('store ->', payload)
  storage.setJSON(model)
  crdt.storeDoc(payload)
})

// Init Modules

image.init(app, dbPromise)
zip.init(app)

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

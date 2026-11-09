import { Elm } from './src/Main.elm'
import image from './js/image'
import store from './js/store'
import zip from './js/zip'
import { openDB } from 'idb'

// IndexedDB

const dbName = 'dm6-elm'
const dbPromise = openDB(dbName, 1, {
  upgrade(db) {
    image.upgrade(db)
    store.upgrade(db)
  }
})

// Init Elm app

store.init(dbPromise)

const app = Elm.Main.init({
  flags: [
    await store.getLocal(),
    await store.getDoc(),
    location.hash
  ]
})

image.init(app, dbPromise)
zip.init(app)

// Storage

app.ports.storeModel.subscribe(([local, doc]) => {
  store.put(local, doc)
})

// Scrolling

app.ports.domReady.subscribe(() => {
  requestAnimationFrame(() => {
    const main = document.getElementById('main')
    let timer
    main.addEventListener('scroll', () => {
      clearTimeout(timer)
      timer = setTimeout(() => {
        app.ports.onScroll.send({x: main.scrollLeft, y: main.scrollTop})
      }, 200)   // debounce 200ms
    }, {passive: true})
  })
})

// Routing

window.addEventListener('hashchange', () => {
  app.ports.onHashChange.send(location.hash)
})

app.ports.setHash.subscribe(function (hash) {
  location.hash = hash    // creates history entries
})

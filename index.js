import { Elm } from './src/Main.elm'
import image from './js/image'
import store from './js/store'
import ws from './js/ws'
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

app.ports.store.subscribe(([doc, local]) => {
  if (doc.kind !== 'ops') {
    throw '❌ store: invalid doc'
  }
  store.put(doc, local)
})

// Sync

app.ports.outgoing.subscribe(doc => {
  if (doc.kind !== 'ops') {
    throw '❌ outgoing: invalid doc'
  }
  ws.send(doc)
})

// Model-ready

app.ports.modelReady.subscribe(() => {
  ws.init(messageHandler)
  requestAnimationFrame(registerScrollHandler)
})

function messageHandler(env) {
  if (env.kind === 'ops') {
    app.ports.incoming.send(env)
  } else {
    throw `❌ incoming message: invalid envelope (kind="${env.kind}")`
  }
}

function registerScrollHandler() {
  const main = document.getElementById('main')
  let timer
  main.addEventListener('scroll', () => {
    clearTimeout(timer)
    timer = setTimeout(() => {
      app.ports.onScroll.send({
        x: Math.round(main.scrollLeft),
        y: Math.round(main.scrollTop)
      })
    }, 200)   // debounce 200ms
  }, {passive: true})
}

// Routing

window.addEventListener('hashchange', () => {
  app.ports.onHashChange.send(location.hash)
})

app.ports.setHash.subscribe(function (hash) {
  location.hash = hash    // creates history entries
})

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
    await launchState(),
    location.hash
  ]
})

image.init(app, dbPromise)
zip.init(app)

// returns LaunchState or null
async function launchState() {
  const docIdFromUrl = getDocId(location.hash)
  const docIdFromDB = await store.getLocal('lastDocId')
  if (!docIdFromUrl && !docIdFromDB) {
    return null
  }

  const replicaId = await store.getLocal('replicaId')   // FIXME: create per browsing context
  const docId = docIdFromUrl || docIdFromDB
  const docState = await store.getLocal(docId)
  const doc = docState ? await store.getDoc(docId) : null

  return {
    replicaId, docId, docState, doc
  }
}

// returns docId (string) or undefined
function getDocId(hash) {
  const val = hash.substr(1)
  if (val.startsWith('d-')) {
    return val
  }
}

// Storage

app.ports.store.subscribe(([doc, local]) => {
  if (doc.kind !== 'ops') {
    throw '❌ store: invalid doc'
  }
  store.put(doc, local)
})

app.ports.storeMeta.subscribe(([replicaId, docId]) => {
  store.putMeta(replicaId, docId)
})

// Sync

app.ports.outgoing.subscribe(doc => {
  if (doc.kind !== 'ops') {
    throw '❌ outgoing: invalid doc'
  }
  ws.send(doc)
})

// Model-ready

app.ports.modelReady.subscribe(docId => {
  ws.init(docId, messageHandler)
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

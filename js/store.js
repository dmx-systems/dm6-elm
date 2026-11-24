export default {
  init, upgrade, getLocal, getDoc, put, clear
}

const localStore = 'local'
const opsStore = 'ops'

let dbPromise

function init(_dbPromise) {
  dbPromise = _dbPromise
}

function upgrade(db) {
  console.log('🔸 store.createObjectStore', localStore, opsStore)
  db.createObjectStore(localStore)
  db.createObjectStore(opsStore, {keyPath: 'id'})
}

async function getLocal() {
  const db = await dbPromise
  const local = await db.get(localStore, 'local') || null
  console.log('🔸 store.getLocal', local)
  return local
}

async function getDoc() {
  const db = await dbPromise
  const doc = {
    kind: 'ops',
    ops: await db.getAll(opsStore)
  }
  console.log('🔸 store.getDoc', doc)
  return doc
}

async function put(doc, local) {
  console.log('🔸 store.put', doc)

  // store local state
  const db = await dbPromise
  db.put(localStore, local, 'local')

  // store doc
  const tx = db.transaction(opsStore, 'readwrite')
  doc.ops.forEach(op => tx.store.add(op))
  return tx.done
}

async function clear() {
  const db = await dbPromise
  db.clear(localStore)
  db.clear(opsStore)
  db.clear('images')    // TODO: modularization
}

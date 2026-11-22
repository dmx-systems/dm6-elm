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
  db.createObjectStore(opsStore)
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

async function put(local, doc) {
  console.log('🔸 store.put', doc)
  if (doc.kind !== 'ops') {
    throw 'store.put: invalid doc'
  }
  const db = await dbPromise
  db.put(localStore, local, 'local')
  const tx = db.transaction(opsStore, 'readwrite')
  doc.ops.forEach(op => tx.store.add(op, op.id))
  return tx.done
}

async function clear() {
  const db = await dbPromise
  db.clear(localStore)
  db.clear(opsStore)
  db.clear('images')    // TODO: modularization
}

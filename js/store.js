export default {
  init, upgrade, getLocal, getDoc, put, putMeta, clear
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

// returns value from `local` store or null
async function getLocal(key) {
  const db = await dbPromise
  return await db.get(localStore, key) || null
}

async function getDoc(id) {
  const range = IDBKeyRange.bound(
    [id],       // Our key format: ["docId", seqNr, "replicaId"]. In IndexedDB key order
    [id, []]    // numbers are lower then than arrays, so every seqNr is lower than [].
  )
  const db = await dbPromise
  const doc = {
    kind: 'ops',
    ops: await db.getAll(opsStore, range)
  }
  console.log('🔸 store.getDoc', doc)
  return doc
}

async function put(doc, local) {
  console.log('🔸 store.put', local, doc)
  const db = await dbPromise
  //
  // store local state
  const localState = {
    "boxId": local.boxId,
    "tool": local.tool
  }
  db.put(localStore, localState, local.docId)
  //
  // store doc
  const tx = db.transaction(opsStore, 'readwrite')
  doc.ops.forEach(op => tx.store.add(op, [local.docId, op.id[0], op.id[1]]))
  return tx.done
}

async function putMeta(replicaId, docId) {
  console.log('🔸 store.putMeta', replicaId, docId)
  const db = await dbPromise
  db.put(localStore, replicaId, 'replicaId')
  db.put(localStore, docId, 'lastDocId')
}

async function clear() {
  const db = await dbPromise
  db.clear(localStore)
  db.clear(opsStore)
  db.clear('images')    // TODO: modularization
}

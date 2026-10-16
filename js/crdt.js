export default {
  init, upgrade, getDoc, storeDoc
}

const opsStore = "ops"

let dbPromise

function init(_dbPromise) {
  dbPromise = _dbPromise
}

function upgrade(db) {
  console.log('#createObjectStore', opsStore)
  db.createObjectStore(opsStore)
}

async function getDoc() {
  const payload = {
    kind: 'ops',
    ops: await getOps()
  }
  console.log('load ->', payload)
  return payload
}

async function getOps() {
  const db = await dbPromise
  return db.getAll(opsStore)
}

async function storeDoc(payload) {
  if (payload.kind !== 'ops') {
    throw 'storeDoc: invalid payload'
  }
  const db = await dbPromise
  const tx = db.transaction(opsStore, 'readwrite')
  payload.ops.forEach(op => tx.store.add(op, op.id))
}

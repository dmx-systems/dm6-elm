export default {
  init, upgrade, getLocal, getDoc, store
}

const localStore = 'local'
const opsStore = 'ops'

let dbPromise

function init(_dbPromise) {
  dbPromise = _dbPromise
}

function upgrade(db) {
  console.log('🔸 crdt.createObjectStore', localStore, opsStore)
  db.createObjectStore(localStore)
  db.createObjectStore(opsStore)
}

async function getLocal() {
  const db = await dbPromise
  const local = await db.get(localStore, 'local') || null
  console.log('🔸 crdt.getLocal ->', local)
  return local
}

async function getDoc() {
  const payload = {
    kind: 'ops',
    ops: await getOps()
  }
  console.log('🔸 crdt.getDoc ->', payload)
  return payload
}

async function getOps() {
  const db = await dbPromise
  return db.getAll(opsStore)
}

async function store(local, payload) {
  console.log('🔸 crdt.store ->', local, payload)
  if (payload.kind !== 'ops') {
    throw 'store: invalid payload'
  }
  const db = await dbPromise
  db.put(localStore, local, 'local')
  const tx = db.transaction(opsStore, 'readwrite')
  payload.ops.forEach(op => tx.store.add(op, op.id))
}

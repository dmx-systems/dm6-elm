export default {
  init, upgrade, getDoc
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
  const ops = await getOps()
  const payload = {
    kind: 'ops',
    ops
  }
  console.log('doc payload', payload)
  return payload
}

async function getOps() {
  const db = await dbPromise
  return db.getAll(opsStore)
}

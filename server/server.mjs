import { createServer } from 'node:http'
import { WebSocketServer } from 'ws'
import { openDB } from 'idb'
import setGlobalVars from 'indexeddbshim/src/node.js'

// import { builtinModules } from 'node:module'
// console.log(builtinModules)

/* IndexedDB */

globalThis.location = {origin: 'dmx.berlin'}  // IndexedDB relies on `location` for security
setGlobalVars()                               // defines global `indexedDB` variable, for `idb`

const dbName = 'dm6-elm'
const opsStore = 'ops'

const dbPromise = openDB(dbName, 1, {
  upgrade(db) {
    console.log('🔸 Creating object store', opsStore)
    db.createObjectStore(opsStore)
  }
})

/* WebSocketServer */

const PORT = process.env.PORT || 8080
const HOST = '127.0.0.1'

const server = createServer()
const wss = new WebSocketServer({server})

wss.on('connection', (socket, request) => {
  const docId = new URL(request.url, 'http://localhost').searchParams.get('docId')
  console.log(`🔸 Peer connected (${wss.clients.size} total)`, docId)

  socket.docId = docId              // associate socket with document

  socket.on('message', data => {
    const env = JSON.parse(data)    // "envelope"
    console.log('incoming <-', env)
    if (env.kind === 'ops') {
      store(docId, env.ops)
      broadcastExcept(socket, docId, data)
    } else if (env.kind === 'ping') {
      // no-op
    } else {
      throw `❌ incoming message is invalid (kind="${env.kind}")`
    }
  })

  socket.on('close', () => {
    console.log(`🔸 Peer disconnected (${wss.clients.size} total)`)
  })
})

server.listen(PORT, HOST, () => {
  console.log(`dm6-elm relay server listening on ws://${HOST}:${PORT}`)
})

function broadcastExcept(sender, docId, payload) {
  let p = 0
  for (const client of wss.clients) {
    if (client !== sender && getDocId(client) === docId && client.readyState === client.OPEN) {
      // Force a TEXT frame. The `ws` library picks the frame type from the
      // value: a Buffer (what the `message` event gives us) would go out as a
      // BINARY frame, which the browser then delivers as a Blob — and our
      // JSON-only protocol chokes on `JSON.parse("[object Blob]")`. Sending a
      // string makes it a text frame, delivered to the browser as a string.
      client.send(payload, {binary: false})
      p++
    }
  }
  console.log(`🔸 Sent to ${p} peers`)
}

function getDocId(socket) {
  const docId = socket.docId
  if (!docId) {
    throw '❌ No "docId" in socket'
  }
  docId
}

/*  */

async function store(docId, ops) {
  console.log('🔸 Storing %d ops', ops.length)
  const db = await dbPromise
  const tx = db.transaction(opsStore, 'readwrite')
  ops.forEach(op => tx.store.add(op, [docId, op.id[0], op.id[1]]))
  return tx.done
}

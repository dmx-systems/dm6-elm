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

const server = createServer((req, res) => {
  res.writeHead(200, {'Content-Type': 'text/plain'})
  res.end('dm6-elm relay is running. Connect a WebSocket to this port.\n')
})

const wss = new WebSocketServer({server})

wss.on('connection', socket => {
  console.log(`peer connected (${wss.clients.size} total)`)

  socket.on('message', data => {
    const env = JSON.parse(data)    // "envelope"
    console.log('--> %o', env)
    if (env.kind === 'doc') {
      store(env.ops)
    } else {
      throw '❌ message: invalid envelope'
    }
    broadcastExcept(socket, data)
  })

  socket.on('close', () => {
    console.log(`peer disconnected (${wss.clients.size} total)`)
  })
})

server.listen(PORT, HOST, () => {
  console.log(`dm6-elm relay listening on ws://${HOST}:${PORT}`)
})

function broadcastExcept(sender, payload) {
  for (const client of wss.clients) {
    if (client !== sender && client.readyState === client.OPEN) {
      // Force a TEXT frame. The `ws` library picks the frame type from the
      // value: a Buffer (what the `message` event gives us) would go out as a
      // BINARY frame, which the browser then delivers as a Blob — and our
      // JSON-only protocol chokes on `JSON.parse("[object Blob]")`. Sending a
      // string makes it a text frame, delivered to the browser as a string.
      client.send(payload, {binary: false})
    }
  }
}

/*  */

async function store(ops) {
  console.log('🔸 Storing %d ops', ops.length)
  const db = await dbPromise
  const tx = db.transaction(opsStore, 'readwrite')
  ops.forEach(op => tx.store.add(op, op.id))
  return tx.done
}

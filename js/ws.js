export default {
  init, send
}

const HOST = '127.0.0.1'
const PORT = 8080
const URL = `ws://${HOST}:${PORT}`
const HEARTBEAT_INTERVAL = 25 * 1000     // 25s
const DEV = true

let ws
let ready
let docId
let messageHandler
let heartbeatTimer

function init(_docId, _messageHandler) {
  docId = _docId
  messageHandler = _messageHandler
  document.addEventListener('visibilitychange', onVisibilityChange)
  ready = connect()
}

/**
 * Sends a message to the server.
 *
 * @param   message   the message to be sent (arbitrary type). Will be serialized as JSON.
 */
async function send(message) {
  await ready
  DEV && console.log('🔸 ws.send', message)
  ws.send(JSON.stringify(message))
}

function onVisibilityChange() {
  DEV && console.log('🔸 ws.onVisibilityChange', document.visibilityState, new Date())
  if (document.visibilityState === 'hidden') {
    // mobile browsers often kill background sockets anyway
    close()
  } else {
    ready = connect()
  }
}

function connect() {
  return new Promise((resolve, reject) => {
    const params = new URLSearchParams({docId})
    const url = `${URL}?${params}`
    DEV && console.log('🔸 ws.connect', url)
    ws = new WebSocket(url)
    ws.onopen = e => {
      DEV && console.log('🔸 ws.onopen')
      resolve()
      startHeartbeat()
    }
    ws.onmessage = e => {
      const message = JSON.parse(e.data)
      DEV && console.log('🔸 ws.onmessage', message)
      messageHandler(message)
    }
    ws.onclose = e => {
      DEV && console.log('🔸 ws.onclose, reason:', e.reason)
      cleanup()
    }
    ws.onerror = e => {
      DEV && console.warn('🔸 ws.onerror', e)
    }
  })
}

function startHeartbeat() {
  heartbeatTimer = setInterval(ping, HEARTBEAT_INTERVAL)
}

function stopHeartbeat() {
  clearInterval(heartbeatTimer)
}

function ping() {
  send({kind: 'ping'})
}

function close() {
  ws.close()
}

function cleanup() {
  stopHeartbeat()
}

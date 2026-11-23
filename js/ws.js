export default {
  init
}

const HOST = '127.0.0.1'
const PORT = 8080
const URL = `ws://${HOST}:${PORT}`
const HEARTBEAT_INTERVAL = 25 * 1000     // 25s
const DEV = true

let ws
let heartbeatTimer

function init() {
  document.addEventListener('visibilitychange', onVisibilityChange)
  connect()
}

/**
 * Sends a message to the server.
 *
 * @param   message   the message to be sent (arbitrary type). Will be serialized as JSON.
 */
function send(message) {
  ws.send(JSON.stringify(message))
}

function onVisibilityChange() {
  DEV && console.log('🔸 ws.onVisibilityChange', document.visibilityState, new Date())
  if (document.visibilityState === 'hidden') {
    // mobile browsers often kill background sockets anyway
    close()
  } else {
    connect()
  }
}

function connect() {
  DEV && console.log('🔸 ws.connect', URL)
  ws = new WebSocket(URL)
  ws.onopen = e => {
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
}

function startHeartbeat() {
  heartbeatTimer = setInterval(ping, HEARTBEAT_INTERVAL)
}

function stopHeartbeat() {
  clearInterval(heartbeatTimer)
}

function ping() {
  DEV && console.log('🔸 ws.ping')
  send({kind: 'ping'})
}

function close() {
  ws.close()
}

function cleanup() {
  stopHeartbeat()
}

import { Elm } from './src/Main.elm'
import { zip, strToU8 } from 'fflate'

const key = 'dm6-elm'
const modelStr = localStorage.getItem(key)
const model = modelStr ? JSON.parse(modelStr) : null
const app = Elm.Main.init({
  flags: [model, location.hash]
})

// Save to local storage
app.ports.storeModel.subscribe(model => {
  localStorage.setItem(key, JSON.stringify(model))
})

// Import to local storage
const input = document.createElement('input')
input.type = 'file'
input.style.display = 'none'
input.addEventListener('change', async () => {
  localStorage.setItem(key, await input.files[0].text())
  location.reload()
})
document.body.appendChild(input)
app.ports.importJSON.subscribe(() => {
  input.value = ''    // allow re-selecting same file
  input.click()
})

// Export local storage
const anchor = document.createElement('a')
anchor.download = 'dm6-elm-export.zip'
anchor.style.display = 'none'
document.body.appendChild(anchor)
app.ports.exportJSON.subscribe(async () => {
  const modelStr = localStorage.getItem(key)
  const zipBlob = await createZip(modelStr)
  const url = URL.createObjectURL(zipBlob)
  anchor.href = url
  anchor.click()
  URL.revokeObjectURL(url)
})

async function createZip(modelStr) {
  const files = {
    'dm6-elm.json': strToU8(modelStr)
  }
  const zipData = await new Promise((resolve, reject) => {
    zip(files, {level: 6}, (err, data) => {
      if (err) reject(err)
      else resolve(data)
    })
  })
  return new Blob([zipData], {type: 'application/zip'})
}

// Scrolling
const main = document.getElementById('main')
let timer
main.addEventListener('scroll', () => {
  clearTimeout(timer)
  timer = setTimeout(() => {
    app.ports.onScroll.send({x: main.scrollLeft, y: main.scrollTop})
  }, 200)   // debounce 200ms
}, {passive: true})

// Routing
window.addEventListener('hashchange', () => {
  app.ports.onHashChange.send(location.hash)
})
app.ports.setHash.subscribe(function (hash) {
  location.hash = hash    // creates history entries
})

// Image file picker
const fpInput = document.createElement('input')
fpInput.type = 'file'
fpInput.style.display = 'none'
fpInput.addEventListener('change', () => {
  const file = fpInput.files[0]
  const topicId = Number(fpInput.dataset.topicId)
  const imageId = Number(fpInput.dataset.imageId)
  app.ports.onPickImageFile.send([topicId, imageId])
  createUrlAndSend(file, imageId)
  saveFile(file, imageId)   // don't need to wait
})
document.body.appendChild(fpInput)
app.ports.imageFilePicker.subscribe(([topicId, imageId]) => {
  console.log('$$imageFilePicker', 'topicId', topicId, 'imageId', imageId)
  fpInput.dataset.topicId = topicId     // update value before clicking
  fpInput.dataset.imageId = imageId     // update value before clicking
  fpInput.value = ''                    // allow re-selecting same file
  fpInput.click()
})

const dbName = 'dm6-elm'
const objectStoreName = 'images'
const dbPromise = new Promise((resolve, reject) => {
  const request = indexedDB.open(dbName, 1)   // version=1
  request.onupgradeneeded = () => {
    console.log('$$createObjectStore', objectStoreName)
    const db = request.result
    db.createObjectStore(objectStoreName)
  }
  request.onsuccess = () => resolve(request.result)
  request.onerror = () => reject(request.error)
})

loadAllImages()

async function saveFile(file, fileId) {
  console.log('$$saveFile', fileId, file)
  const db = await dbPromise
  return new Promise((resolve, reject) => {
    const tx = db.transaction(objectStoreName, 'readwrite')
    const store = tx.objectStore(objectStoreName)
    const request = store.put(file, fileId)
    request.onsuccess = () => {
      console.log('--> saved', fileId, file)
      resolve()
    }
    request.onerror = () => reject(request.error)
  })
}

async function loadFile(id) {
  const db = await dbPromise
  return new Promise((resolve, reject) => {
    const tx = db.transaction(objectStoreName, "readonly")
    const store = tx.objectStore(objectStoreName)
    const request = store.get(id)
    request.onsuccess = () => {
      if (request.result) {
          resolve(request.result)
      } else {
          reject("File not found")
      }
    }
    request.onerror = () => reject(request.error)
  })
}

async function loadAllImageIds() {
  const db = await dbPromise
  return new Promise((resolve, reject) => {
    const tx = db.transaction(objectStoreName, "readonly")
    const store = tx.objectStore(objectStoreName)
    const request = store.getAllKeys()
    request.onsuccess = () => {
      console.log('$$loadAllImageIds', request.result)
      resolve(request.result)
    }
    request.onerror = () => reject(request.error)
  })
}

function loadAllImages() {
  loadAllImageIds().then(ids => ids.forEach(id =>
    loadFile(id).then(file =>
      createUrlAndSend(file, id)
    )
  ))
}

function createUrlAndSend(file, id) {
  app.ports.onResolveUrl.send(
    [id, URL.createObjectURL(file)]
  )
}

import { Elm } from './src/Main.elm'
import { zip, unzip, strToU8, strFromU8 } from 'fflate'

const key = 'dm6-elm'
const modelStr = localStorage.getItem(key)
const model = modelStr ? JSON.parse(modelStr) : null
const app = Elm.Main.init({
  flags: [model, location.hash]
})

// Store in local storage
app.ports.storeModel.subscribe(model => {
  localStorage.setItem(key, JSON.stringify(model))
})

// Import from zip
const input = document.createElement('input')
input.type = 'file'
input.accept = '.zip'
input.style.display = 'none'
input.addEventListener('change', async () => {
  const zipData = await u8(input.files[0])
  const content = await readZipFile(zipData)
  localStorage.setItem(key, content.modelStr)
  await Promise.all(
    content.images.map(({id, blob}) => storeImage(id, blob))
  )
  location.reload()
})
document.body.appendChild(input)
app.ports.importJSON.subscribe(() => {
  input.value = ''    // allow re-selecting same file
  input.click()
})

// Reads a zip file and returns a representation of its contents, that is a promise for an
// object {modelStr, images: [{id, blob}]}
function readZipFile(zipData) {
  return new Promise((resolve, reject) => {
    unzip(zipData, (err, entries) => {
      if (err) {
        reject(err)
        return
      }
      const content = {images: []}
      Object.entries(entries).forEach(([path, data]) => {
        if (path === 'dm6-elm.json') {
          content.modelStr = strFromU8(data)
        } else {
          const filename = path.split('/').pop()
          if (filename) {   // zip has folder entry "images/", filename is empty then
            const imageId = Number(filename.split('.')[0])
            const blob = new Blob([data], {type: getMimeType(filename)})
            content.images.push({id: imageId, blob})
          }
        }
      })
      if (content.modelStr) {
        resolve(content)
      } else {
        reject('Wrong ZIP: dm6-elm.json not found -> import aborted')
      }
    })
  })
}

// Export to zip
const anchor = document.createElement('a')
anchor.download = 'dm6-elm-export.zip'
anchor.style.display = 'none'
document.body.appendChild(anchor)
app.ports.exportJSON.subscribe(async () => {
  const zipBlob = await createZip()
  const url = URL.createObjectURL(zipBlob)
  anchor.href = url
  anchor.click()
  URL.revokeObjectURL(url)
})

async function createZip() {
  const modelStr = localStorage.getItem(key)
  const content = {
    'dm6-elm.json': [strToU8(modelStr), {level: 6}],    // only compress the json
    'images': await imagesToZip()
  }
  const zipData = await new Promise((resolve, reject) => {
    zip(content, {level: 0}, (err, data) => {           // don't compress the images
      if (err) reject(err)
      else resolve(data)
    })
  })
  return new Blob([zipData], {type: 'application/zip'})
}

// Loads all images from Indexed DB and transforms them into {filename: imageU8} object,
// ready for being zipped. Returns a promise for that object.
async function imagesToZip() {
  const ids = await loadAllImageIds()
  const files = await Promise.all(
    ids.map(imageToZipEntry)
  )
  const images = files.reduce(
    (acc, file) => {
      acc[file[0]] = file[1]
      return acc
    }, {}
  )
  return images
}

async function imageToZipEntry(id) {
  const blob = await loadImage(id)
  const filename = id + '.' + mimeToExt(blob.type)
  console.log('image to zip', blob.type, '->', filename)
  return [filename, await u8(blob)]
}

function getMimeType(filename) {
  return 'image/' + getExtension(filename)
}

function getExtension(filename) {
  const i = filename.lastIndexOf('.')
  return i > 0 ? filename.slice(i + 1) : ''
}

function mimeToExt(mimeType) {
  return mimeType.split('/')[1]
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
fpInput.accept = 'image/*'
fpInput.style.display = 'none'
fpInput.addEventListener('change', async () => {
  const file = fpInput.files[0]
  // Note: when populating the DB by import from zip file, there will be sole Blobs stored, not
  // Files. At the other hand when user inserts an image via file picker, it will give us a File
  // object. We want uniformly have Blobs in the DB. So we explicitly create a Blob from File.
  const blob = await blobFromFile(file)
  const topicId = Number(fpInput.dataset.topicId)
  const imageId = Number(fpInput.dataset.imageId)
  app.ports.onPickImageFile.send([topicId, imageId])
  resolveImage(imageId, blob)
  storeImage(imageId, blob)             // don't need to wait for completion
})
document.body.appendChild(fpInput)
app.ports.imageFilePicker.subscribe(([topicId, imageId]) => {
  console.log('$$imageFilePicker', 'topicId', topicId, 'imageId', imageId)
  fpInput.dataset.topicId = topicId     // update value before clicking
  fpInput.dataset.imageId = imageId     // update value before clicking
  fpInput.value = ''                    // allow re-selecting same file
  fpInput.click()
})

async function blobFromFile(file) {
  return new Blob([await u8(file)], {type: file.type})
}

async function u8(blob) {
  // Note: blob.bytes() not available in Safari 17.6 on Monterey
  return new Uint8Array(await blob.arrayBuffer())
}

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

resolveAllImages()

// Returns a promise resolved once storage is complete
async function storeImage(id, blob) {
  console.log('$$storeImage', id, blob)
  const db = await dbPromise
  return new Promise((resolve, reject) => {
    const tx = db.transaction(objectStoreName, 'readwrite')
    const store = tx.objectStore(objectStoreName)
    const request = store.put(blob, id)
    request.onsuccess = () => {
      resolve()
    }
    request.onerror = () => reject(request.error)
  })
}

// Returns a promise resolving to a Blob
async function loadImage(id) {
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

function resolveAllImages() {   // TODO: resolve selectively
  loadAllImageIds().then(ids => ids.forEach(id =>
    loadImage(id).then(blob =>
      resolveImage(id, blob)
    )
  ))
}

function resolveImage(id, blob) {
  app.ports.onResolveUrl.send(
    [id, URL.createObjectURL(blob)]
  )
}

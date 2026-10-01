import { u8 } from './util'

export default {
  init, upgrade, loadAllImageIds, loadImage, storeImage
}

let app
let dbPromise

function init(_app, _dbPromise) {
  app = _app
  dbPromise = _dbPromise
  initFilePicker()
  resolveAllImages()
}

// File Picker

function initFilePicker() {
  const input = document.createElement('input')
  input.type = 'file'
  input.accept = 'image/*'
  input.style.display = 'none'
  input.addEventListener('change', async () => {
    const file = input.files[0]
    // Note: when populating the DB by import from zip file, there will be sole Blobs stored, not
    // Files. At the other hand when user inserts an image via file picker, it will give us a File
    // object. We want uniformly have Blobs in the DB. So we explicitly create a Blob from File.
    const blob = await blobFromFile(file)
    const topicId = Number(input.dataset.topicId)
    const imageId = Number(input.dataset.imageId)
    app.ports.onImageFilePicked.send({topicId, imageId})
    resolveImage(imageId, blob)
    storeImage(imageId, blob)             // don't need to wait for completion
  })
  document.body.appendChild(input)
  app.ports.imageFilePicker.subscribe(({topicId, imageId}) => {
    console.log('#imageFilePicker', 'topicId', topicId, 'imageId', imageId)
    input.dataset.topicId = topicId     // update value before clicking
    input.dataset.imageId = imageId     // update value before clicking
    input.value = ''                    // allow re-selecting same file
    input.click()
  })
}

async function blobFromFile(file) {
  return new Blob([await u8(file)], {type: file.type})
}

// IndexedDB

const imagesStore = 'images'

function upgrade(db) {
  console.log('#createObjectStore', imagesStore)
  db.createObjectStore(imagesStore)
}

// Returns a promise resolved once storage is complete
async function storeImage(id, blob) {
  console.log('#storeImage', id, blob)
  const db = await dbPromise
  return db.put(imagesStore, blob, id)
}

function resolveAllImages() {   // TODO: resolve selectively
  loadAllImageIds().then(ids => {
    console.log('#resolveAllImages', ids)
    ids.forEach(id =>
      loadImage(id).then(blob =>
        resolveImage(id, blob)
      )
    )
  })
}

// Returns a promise resolving to a Blob
async function loadImage(id) {
  const db = await dbPromise
  return db.get(imagesStore, id)
}

async function loadAllImageIds() {
  const db = await dbPromise
  return db.getAllKeys(imagesStore)
}

function resolveImage(id, blob) {
  app.ports.onImageUrlResolved.send(
    [id, URL.createObjectURL(blob)]
  )
}

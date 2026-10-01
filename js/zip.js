import { zip, unzip, strToU8, strFromU8 } from 'fflate'
import { u8 } from './util'
import storage from './storage'
import image from './image'

export default {
  init
}

let app

function init(_app) {
  app = _app
  initImportPicker()
  initDownloadLink()
}

// Import zip

function initImportPicker() {
  const input = document.createElement('input')
  input.type = 'file'
  input.accept = '.zip'
  input.style.display = 'none'
  input.addEventListener('change', async () => {
    const zipData = await u8(input.files[0])
    const content = await readZipFile(zipData)
    storage.setString(content.modelStr)
    await Promise.all(
      content.images.map(({id, blob}) => image.storeImage(id, blob))
    )
    location.reload()
  })
  document.body.appendChild(input)
  app.ports.importJSON.subscribe(() => {
    input.value = ''    // allow re-selecting same file
    input.click()
  })
}

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

// Export zip

function initDownloadLink() {
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
}

async function createZip() {
  const modelStr = storage.getString()
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
  const ids = await image.loadAllImageIds()
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
  const blob = await image.loadImage(id)
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

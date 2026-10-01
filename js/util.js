export async function u8(blob) {
  // Note: blob.bytes() not yet available in Safari 17.6 on Monterey
  return new Uint8Array(await blob.arrayBuffer())
}

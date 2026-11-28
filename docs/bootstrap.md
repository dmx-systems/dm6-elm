
Storage
=======

ops
---

["d-jh23", 45, "r-u56f"]  | ...
...


local
-----

"replicaId" | "r-u56f"
"lastDocId" | "d-jh23"

"docIds"    | ["d-jh23", "d-k4i6", ...]

"d-jh23"    | { "boxId": "5845",                    \ represented in
                "tool": {"lineStyle": "Cornered"}   / running Model
              }

"d-k4i6"    | { "boxId": ...
...


Logic
=====

Launch
------

URL w/ docId
  Doc exists locally 1️⃣
    -> Elm.init <- replicaId ☑️, docId ☑️ (read from hash), doc ☑️
    -> init model (replicaId, docId, doc)
    -> store model (docId)❓
    -> RUNNING
  Doc does not exist locally
    DB exists 2️⃣
      -> Elm.init <- replicaId ☑️, docId ☑️ (read from hash), doc ❌
      -> init model (replicaId, docId, empty doc❗️)
      -> store model (docId)❓
      -> RUNNING -> doc populated through initial server push ☑️
    No DB 3️⃣
      -> Elm.init <- ❌, docId ☑️ (read from hash), doc ❌
      -> SERVER_BOOTSTRAPPING (wait for replicaID)
        -> replicaId ☑️
        -> init model (replicaId, docId, empty doc❗️)
        -> store model (docId)❓
        -> RUNNING -> doc populated through initial server push ☑️

URL w/o docId
  DB exists 4️⃣
    -> Elm.init <- replicaId ☑️, docId ☑️ (read from local DB), doc ☑️
    -> init model (replicaId, docId, doc)
    -> RUNNING
  No DB 5️⃣
    -> Elm.init <- ❌, ❌, ❌
    -> BOOTSTRAPPING (wait for IDs)
      -> replicaId ☑️, docId ☑️
      -> create default doc ☑️
      -> store model❓
      -> RUNNING

                                         LocalDocState
                     LaunchState         -------------
                     --------------------------------------
4️⃣ FromDB            replicaId   docId   boxId   tool   ops
1️⃣ FromUrl           replicaId   docId   boxId   tool   ops     # -> store docId
2️⃣ Server            replicaId   docId   *       *      -       # ops initial server push
3️⃣ ServerBootstrap   -           docId   *       *      -       # ops initial server push
5️⃣ Bootstrap         -           -       -       -      -       # ops default document

                                 * could initialize boxId/tool from query params


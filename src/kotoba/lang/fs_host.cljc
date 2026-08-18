(ns kotoba.lang.fs-host
  "A real, root-confined `kotoba.lang.fs/IFilesystem` for hosts that have an OS
  filesystem: `#?(:clj java.nio/java.io, :cljs node:fs)`.

  ## Why this is a separate namespace

  `kotoba.lang.fs` is pure: path ops, the `IFilesystem` protocol, and an
  in-memory implementation. Requiring it must stay free of any JVM or Node
  filesystem dependency, because a kotoba-WASM cell requires it too. So the
  host binding lives here, in its own namespace, exactly the way
  `provider.http-transport` sits beside `provider.http` and
  `provider.scoped-fs-transport` sits beside `provider.scoped-fs` (ADR 0066 /
  0117 / 0147). A reviewer can check the separation mechanically: nothing in
  `fs.cljc` mentions `java.nio`, `js/require`, or this namespace.

  This namespace does NOT change `kotoba.lang.fs` and does NOT add a
  `slurp`/`spit` convenience. Ambient filesystem authority is the thing the
  protocol exists to remove; a helper that reads any path the caller names
  would put it straight back.

  ## No ambient authority: `:root` is required

  There is no default root -- not CWD, not `$HOME`, not `/tmp`. The host must
  pass `:root` as an absolute path to an existing directory. Every guest path
  is resolved against it and REFUSED, never silently clamped, when it tries to
  leave: `..`, absolute paths, `~`, null bytes, backslashes, empty segments,
  over-long paths. The refusal codes are taken verbatim from
  `provider.scoped-fs/resolve-path` rather than invented here, so a caller that
  already branches on that vocabulary keeps working:

  | code                | refused                                        |
  |---------------------|------------------------------------------------|
  | `:fs/empty-path`    | `\"\"`, `\"/\"`-only, all-blank segments             |
  | `:fs/null-byte`     | any NUL in the path                            |
  | `:fs/backslash`     | any `\\` (a separator on some hosts)            |
  | `:fs/absolute`      | leading `/`                                    |
  | `:fs/home-escape`   | leading `~`                                    |
  | `:fs/path-too-long` | over `max-path-bytes` (1024) UTF-8 bytes       |
  | `:fs/escape`        | any `.` or `..` segment; symlink out of root   |

  Symlinks are resolved (canonical path on the JVM, `realpathSync` on Node)
  and re-checked against the canonical root, so a symlink planted inside the
  root pointing outside it is refused with `:fs/escape` too.

  ## Bounded reads

  `:max-bytes` (default 1 MiB) bounds a single `read`. The size is taken from
  `stat` and refused BEFORE any bytes are loaded, so a huge file costs a stat,
  not a heap. The same bound applies to `write`, so this handle cannot create a
  file it would then refuse to read back.

  ## Errors are data

  Every failure is an `ex-info` carrying a stable `:type` from `error-types`.
  Callers branch on that keyword; they never name `java.io.IOException` (which
  cannot cross to cljs) or match on a message. This is the same move
  `kotoba.lang.json`'s `:type :json/parse-error` made, for the same reason.
  Host exceptions are caught and retyped as `:fs/io` with the original message
  under `:fs/message`.

  ## One deliberate divergence from `mem-filesystem`

  `mem-filesystem` returns `nil` for a missing `read`. This implementation
  THROWS `:fs/not-found` instead. A missing file and an empty file are
  different events on a real filesystem, and `nil` conflates them; a bare
  `nil` also tends to surface far from the read as a NullPointerException.
  Code written against the mem impl that relies on nil-punning a missing read
  will see an exception here -- that is the one behavioural difference to
  check when swapping the handle. `delete` of a missing path stays a no-op
  `nil`, matching the mem impl.

  Zero third-party deps."
  (:require [clojure.string :as str]
            [kotoba.lang.fs :as fs])
  #?(:clj (:import (java.io File)
                   (java.nio.charset StandardCharsets)
                   (java.nio.file Files OpenOption))))

;; ---------------------------------------------------------------------------
;; constants + error vocabulary
;; ---------------------------------------------------------------------------

(def max-path-bytes
  "Longest guest path accepted, in UTF-8 bytes. Same value as
  `provider.scoped-fs/max-path-bytes`."
  1024)

(def default-max-bytes
  "Default `:max-bytes` bound for a single read/write when the host does not
  set one. 1 MiB."
  1048576)

(def error-types
  "Every `:type` this namespace throws. A caller can branch on these without
  naming a host exception class."
  #{:fs/empty-path :fs/null-byte :fs/backslash :fs/absolute :fs/home-escape
    :fs/path-too-long :fs/escape
    :fs/not-found :fs/too-large :fs/is-directory :fs/not-a-directory
    :fs/io :fs/bad-root})

(defn- refuse!
  "Throw a typed refusal. `type` must be in `error-types`."
  ([type message] (refuse! type message {}))
  ([type message data]
   (throw (ex-info message (assoc data :type type)))))

(defn- utf8-byte-count [s]
  #?(:clj  (alength (.getBytes ^String (str s) StandardCharsets/UTF_8))
     :cljs (.-length (.from js/Buffer (str s) "utf8"))))

;; ---------------------------------------------------------------------------
;; pure path policy (no OS)
;; ---------------------------------------------------------------------------

(defn resolve-relative
  "Pure path policy: `{:ok normalized-relative}` or `{:error <code>}`.

  Deliberately identical in behaviour to `provider.scoped-fs/resolve-path`
  (that namespace lives in another repo, which this one cannot depend on --
  `fs` has zero third-party deps by design). Keep the two in step."
  [relative]
  (let [s (str relative)]
    (cond
      (str/blank? s)                {:error :fs/empty-path}
      (str/includes? s "\0")        {:error :fs/null-byte}
      (str/includes? s "\\")        {:error :fs/backslash}
      (str/starts-with? s "/")      {:error :fs/absolute}
      (str/starts-with? s "~")      {:error :fs/home-escape}
      (> (utf8-byte-count s)
         max-path-bytes)            {:error :fs/path-too-long}
      :else
      (let [segs (vec (remove str/blank? (str/split s #"/")))]
        (cond
          (empty? segs)                  {:error :fs/empty-path}
          (some #{".." "."} segs)        {:error :fs/escape}
          :else {:ok (str/join "/" segs)})))))

(defn- checked-relative
  "`resolve-relative` or throw the typed refusal."
  [relative]
  (let [r (resolve-relative relative)]
    (if-let [err (:error r)]
      (refuse! err (str "path refused: " (name err)) {:fs/path (str relative)})
      (:ok r))))

(defn under-root?
  "True when `canonical-child` is `canonical-root` itself or strictly under it.
  Pure string test on two already-canonical absolute paths (same shape as
  `provider.scoped-fs-transport/under-root?`)."
  [canonical-root canonical-child]
  (let [r (str canonical-root)
        c (str canonical-child)]
    (or (= r c)
        (and (str/starts-with? c r)
             (or (str/ends-with? r "/")
                 (str/starts-with? (subs c (count r)) "/"))))))

(defn- bounded! [n max-bytes what path]
  (when (> n max-bytes)
    (refuse! :fs/too-large (str what " exceeds :max-bytes")
             {:fs/path path :fs/size n :fs/max-bytes max-bytes})))

;; ---------------------------------------------------------------------------
;; JVM
;; ---------------------------------------------------------------------------

#?(:clj
   (defn- canonical-or-refuse ^String [^File f]
     (try (.getCanonicalPath f)
          (catch Exception e
            (refuse! :fs/io "canonicalization failed"
                     {:fs/path (.getPath f) :fs/message (.getMessage e)})))))

#?(:clj
   (defn- resolve-file
     "Guest path -> a `File` proven to sit under `root-canonical`, or a typed
     refusal. Canonicalization resolves symlinks, including on paths that do
     not exist yet (the existing prefix is resolved)."
     ^File [^String root-canonical relative]
     (let [rel   (checked-relative relative)
           child (File. (File. root-canonical) rel)
           cpath (canonical-or-refuse child)]
       (when-not (under-root? root-canonical cpath)
         (refuse! :fs/escape "path escapes root"
                  {:fs/path (str relative) :fs/resolved cpath}))
       (File. cpath))))

#?(:clj
   (defn host-filesystem
     "An `IFilesystem` backed by the JVM filesystem, confined to `:root`.

     opts:
       :root       required, absolute path (String or File) of an existing dir
       :max-bytes  optional, bound on a single read/write (default 1 MiB)"
     [{:keys [root max-bytes]}]
     (when-not (and root (not (str/blank? (str root))))
       (refuse! :fs/bad-root ":root is required"))
     (let [rf (if (instance? File root) ^File root (File. (str root)))]
       (when-not (.isAbsolute rf)
         (refuse! :fs/bad-root ":root must be absolute" {:fs/path (.getPath rf)}))
       (when-not (and (.exists rf) (.isDirectory rf))
         (refuse! :fs/bad-root ":root must be an existing directory"
                  {:fs/path (.getPath rf)}))
       (let [root-c    (canonical-or-refuse rf)
             max-bytes (or max-bytes default-max-bytes)]
         (reify fs/IFilesystem
           (read [_ path]
             (let [^File f (resolve-file root-c path)]
               (when-not (.exists f)
                 (refuse! :fs/not-found "no such file" {:fs/path (str path)}))
               (when (.isDirectory f)
                 (refuse! :fs/is-directory "path is a directory" {:fs/path (str path)}))
               ;; stat first: an over-large file is refused before any read
               (bounded! (.length f) max-bytes "file" (str path))
               (let [bytes (try (Files/readAllBytes (.toPath f))
                                (catch Exception e
                                  (refuse! :fs/io "read failed"
                                           {:fs/path (str path)
                                            :fs/message (.getMessage e)})))]
                 (bounded! (alength ^bytes bytes) max-bytes "file" (str path))
                 (String. ^bytes bytes StandardCharsets/UTF_8))))

           (write [_ path content]
             (let [^File f (resolve-file root-c path)
                   ^bytes bytes (.getBytes (str content) StandardCharsets/UTF_8)]
               (bounded! (alength bytes) max-bytes "content" (str path))
               (when (.isDirectory f)
                 (refuse! :fs/is-directory "path is a directory" {:fs/path (str path)}))
               (let [parent (.getParentFile f)]
                 (when (and parent (not (.exists parent)))
                   ;; `.mkdirs` returns false when another writer won the race
                   ;; and created it first, which is not a failure
                   (when-not (or (.mkdirs parent) (.isDirectory parent))
                     (refuse! :fs/io "could not create parent directory"
                              {:fs/path (str path)}))
                   ;; a freshly created parent is re-proven under root
                   (when-not (under-root? root-c (canonical-or-refuse parent))
                     (refuse! :fs/escape "parent escapes root" {:fs/path (str path)}))))
               (try (Files/write (.toPath f) bytes (into-array OpenOption []))
                    (catch Exception e
                      (refuse! :fs/io "write failed"
                               {:fs/path (str path) :fs/message (.getMessage e)})))
               nil))

           (list [_ path]
             (let [^File f (resolve-file root-c path)]
               (when-not (.exists f)
                 (refuse! :fs/not-found "no such directory" {:fs/path (str path)}))
               (when-not (.isDirectory f)
                 (refuse! :fs/not-a-directory "path is not a directory"
                          {:fs/path (str path)}))
               (let [names (.list f)]
                 (when (nil? names)
                   (refuse! :fs/io "listing failed" {:fs/path (str path)}))
                 (vec (sort (seq names))))))

           (exists? [_ path]
             (.exists ^File (resolve-file root-c path)))

           (delete [_ path]
             (let [^File f (resolve-file root-c path)]
               (cond
                 (not (.exists f)) nil          ; idempotent, like mem-filesystem
                 (.isDirectory f)
                 (refuse! :fs/is-directory "refusing to delete a directory"
                          {:fs/path (str path)})
                 :else (do (when-not (.delete f)
                             (refuse! :fs/io "delete failed" {:fs/path (str path)}))
                           nil)))))))))

;; ---------------------------------------------------------------------------
;; Node (nbb / ClojureScript on Node)
;; ---------------------------------------------------------------------------

#?(:cljs
   (defn- realpath-or-nil [node-fs p]
     (try (.realpathSync node-fs p) (catch :default _ nil))))

#?(:cljs
   (defn- canonical-node-path
     "Node has no `getCanonicalPath` for a path that does not exist yet, so walk
     up to the nearest existing ancestor, `realpathSync` that, and re-attach the
     missing tail. Every ancestor realpath is checked against the root, which is
     what makes a symlink planted inside the root fail closed."
     [node-fs path-mod root-c p]
     (loop [cur p, tail []]
       (if (.existsSync node-fs cur)
         (let [rp (realpath-or-nil node-fs cur)]
           (cond
             (nil? rp) (refuse! :fs/io "realpath failed" {:fs/path (str p)})
             (not (under-root? root-c rp))
             (refuse! :fs/escape "path escapes root"
                      {:fs/path (str p) :fs/resolved rp})
             :else (reduce (fn [acc seg] (.join path-mod acc seg)) rp tail)))
         (let [parent (.dirname path-mod cur)]
           (if (= parent cur)
             (refuse! :fs/escape "path escapes root" {:fs/path (str p)})
             (recur parent (vec (cons (.basename path-mod cur) tail)))))))))

#?(:cljs
   (defn- resolve-node-path
     [node-fs path-mod root-c relative]
     (let [rel    (checked-relative relative)
           joined (.join path-mod root-c rel)]
       (when-not (under-root? root-c joined)
         (refuse! :fs/escape "path escapes root" {:fs/path (str relative)}))
       (canonical-node-path node-fs path-mod root-c joined))))

#?(:cljs
   (defn host-filesystem
     "An `IFilesystem` backed by Node's `fs`, confined to `:root`.

     opts:
       :root       required, absolute path of an existing directory
       :max-bytes  optional, bound on a single read/write (default 1 MiB)
       :fs :path   optional Node module overrides (tests)

     `node:fs` is required lazily, inside this constructor, so that merely
     loading this namespace in a browser build does not pull it in."
     [{:keys [root max-bytes] :as opts}]
     (let [node-fs   (or (:fs opts) (js/require "fs"))
           path-mod  (or (:path opts) (js/require "path"))
           max-bytes (or max-bytes default-max-bytes)]
       (when (or (nil? root) (str/blank? (str root)))
         (refuse! :fs/bad-root ":root is required"))
       (let [rs (str root)]
         (when-not (.isAbsolute path-mod rs)
           (refuse! :fs/bad-root ":root must be absolute" {:fs/path rs}))
         (when-not (and (.existsSync node-fs rs)
                        (.isDirectory (.statSync node-fs rs)))
           (refuse! :fs/bad-root ":root must be an existing directory" {:fs/path rs}))
         (let [root-c (or (realpath-or-nil node-fs rs)
                          (refuse! :fs/io "root realpath failed" {:fs/path rs}))]
           (reify fs/IFilesystem
             (read [_ path]
               (let [p (resolve-node-path node-fs path-mod root-c path)]
                 (when-not (.existsSync node-fs p)
                   (refuse! :fs/not-found "no such file" {:fs/path (str path)}))
                 (let [st (.statSync node-fs p)]
                   (when (.isDirectory st)
                     (refuse! :fs/is-directory "path is a directory" {:fs/path (str path)}))
                   ;; stat first: an over-large file is refused before any read
                   (bounded! (.-size st) max-bytes "file" (str path)))
                 (let [s (try (.readFileSync node-fs p "utf8")
                              (catch :default e
                                (refuse! :fs/io "read failed"
                                         {:fs/path (str path) :fs/message (.-message e)})))]
                   (bounded! (utf8-byte-count s) max-bytes "file" (str path))
                   s)))

             (write [_ path content]
               (let [p (resolve-node-path node-fs path-mod root-c path)
                     s (str content)]
                 (bounded! (utf8-byte-count s) max-bytes "content" (str path))
                 (when (and (.existsSync node-fs p)
                            (.isDirectory (.statSync node-fs p)))
                   (refuse! :fs/is-directory "path is a directory" {:fs/path (str path)}))
                 (let [parent (.dirname path-mod p)]
                   (when-not (.existsSync node-fs parent)
                     (try (.mkdirSync node-fs parent #js {:recursive true})
                          (catch :default e
                            (refuse! :fs/io "could not create parent directory"
                                     {:fs/path (str path) :fs/message (.-message e)})))
                     ;; a freshly created parent is re-proven under root
                     (let [pr (realpath-or-nil node-fs parent)]
                       (when-not (and pr (under-root? root-c pr))
                         (refuse! :fs/escape "parent escapes root" {:fs/path (str path)})))))
                 (try (.writeFileSync node-fs p s "utf8")
                      (catch :default e
                        (refuse! :fs/io "write failed"
                                 {:fs/path (str path) :fs/message (.-message e)})))
                 nil))

             (list [_ path]
               (let [p (resolve-node-path node-fs path-mod root-c path)]
                 (when-not (.existsSync node-fs p)
                   (refuse! :fs/not-found "no such directory" {:fs/path (str path)}))
                 (when-not (.isDirectory (.statSync node-fs p))
                   (refuse! :fs/not-a-directory "path is not a directory"
                            {:fs/path (str path)}))
                 (let [names (try (.readdirSync node-fs p)
                                  (catch :default e
                                    (refuse! :fs/io "listing failed"
                                             {:fs/path (str path) :fs/message (.-message e)})))]
                   (vec (sort (js->clj names))))))

             (exists? [_ path]
               (.existsSync node-fs (resolve-node-path node-fs path-mod root-c path)))

             (delete [_ path]
               (let [p (resolve-node-path node-fs path-mod root-c path)]
                 (cond
                   (not (.existsSync node-fs p)) nil   ; idempotent, like mem-filesystem
                   (.isDirectory (.statSync node-fs p))
                   (refuse! :fs/is-directory "refusing to delete a directory"
                            {:fs/path (str path)})
                   :else (do (try (.unlinkSync node-fs p)
                                  (catch :default e
                                    (refuse! :fs/io "delete failed"
                                             {:fs/path (str path) :fs/message (.-message e)})))
                             nil))))))))))

(ns kotoba.lang.fs-host-test
  "Tests for the host-backed filesystem, against a REAL temp directory, on both
  runtimes (`clojure -M:test` and `nbb`). Nothing here is mocked: every
  assertion is about what the OS actually did.

  Every refusal test asserts the refusal's `:type`, not merely that something
  threw. `refusal-type` returns `nil` when a raw host exception escapes (no
  `ex-data`), so a namespace that leaked an `IOException` fails these tests
  rather than passing them."
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.lang.fs :as fs]
            [kotoba.lang.fs-host :as host])
  #?(:clj (:import (java.io File)
                   (java.nio.file Files OpenOption Paths)
                   (java.nio.file.attribute FileAttribute))))

;; ---------------------------------------------------------------------------
;; real temp directories, on both runtimes
;; ---------------------------------------------------------------------------

#?(:cljs (def ^:private node-fs (js/require "fs")))
#?(:cljs (def ^:private node-path (js/require "path")))
#?(:cljs (def ^:private node-os (js/require "os")))

(def ^:private nul
  "A real NUL, built rather than escaped so the character never has to survive
  a heredoc, an editor, or a diff."
  (str (char 0)))

(defn- temp-root []
  #?(:clj  (str (Files/createTempDirectory "fs-host-test" (into-array FileAttribute [])))
     :cljs (.mkdtempSync node-fs (.join node-path (.tmpdir node-os) "fs-host-test-"))))

(defn- slurp-real
  "Read a file by absolute path, bypassing the handle entirely. Used to prove
  that what the handle wrote really landed, and that what it refused really
  did exist and really was reachable without it."
  [abs]
  #?(:clj  (String. (Files/readAllBytes (Paths/get (str abs) (into-array String [])))
                    "UTF-8")
     :cljs (.readFileSync node-fs (str abs) "utf8")))

(defn- spit-real [abs content]
  #?(:clj  (do (Files/write (Paths/get (str abs) (into-array String []))
                            (.getBytes ^String content "UTF-8")
                            (into-array OpenOption []))
               (str abs))
     :cljs (do (.writeFileSync node-fs (str abs) content "utf8") (str abs))))

(defn- exists-real? [abs]
  #?(:clj  (.exists (File. (str abs)))
     :cljs (.existsSync node-fs (str abs))))

(defn- mkdir! [p]
  #?(:clj  (Files/createDirectories (Paths/get (str p) (into-array String []))
                                    (into-array FileAttribute []))
     :cljs (.mkdirSync node-fs (str p) #js {:recursive true}))
  (str p))

(defn- symlink! [target link]
  #?(:clj  (Files/createSymbolicLink (Paths/get (str link) (into-array String []))
                                     (Paths/get (str target) (into-array String []))
                                     (into-array FileAttribute []))
     :cljs (.symlinkSync node-fs (str target) (str link))))

(defn- refusal-type
  "Run `f`; return the `:type` of the ex-info it threw. `::no-throw` when it
  returned normally, `nil` when what escaped was a raw host exception with no
  `ex-data` -- which is a failure of these tests, not a pass."
  [f]
  (try (f) ::no-throw
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
         (:type (ex-data e)))))

(defn- fs-under [root & {:as opts}]
  (host/host-filesystem (merge {:root root} opts)))

;; ---------------------------------------------------------------------------
;; happy paths, on a real filesystem
;; ---------------------------------------------------------------------------

(deftest write-then-read-roundtrip
  (let [root (temp-root)
        h    (fs-under root)]
    (fs/write h "notes/hello.txt" "hello, world")
    (is (= "hello, world" (fs/read h "notes/hello.txt")))
    ;; the bytes really are on disk, under the root, at the path we named
    (is (= "hello, world" (slurp-real (str root "/notes/hello.txt"))))
    ;; overwrite truncates rather than appending
    (fs/write h "notes/hello.txt" "bye")
    (is (= "bye" (fs/read h "notes/hello.txt")))
    (is (= "bye" (slurp-real (str root "/notes/hello.txt"))))))

(deftest exists-before-and-after-delete
  (let [h (fs-under (temp-root))]
    (is (false? (fs/exists? h "a/b.txt")))
    (fs/write h "a/b.txt" "x")
    (is (true? (fs/exists? h "a/b.txt")))
    (is (nil? (fs/delete h "a/b.txt")))
    (is (false? (fs/exists? h "a/b.txt")))
    ;; deleting again is a no-op, matching mem-filesystem
    (is (nil? (fs/delete h "a/b.txt")))))

(deftest list-a-directory
  (let [h (fs-under (temp-root))]
    (fs/write h "dir/a.txt" "1")
    (fs/write h "dir/b.txt" "2")
    (fs/write h "dir/sub/c.txt" "3")
    (is (= ["a.txt" "b.txt" "sub"] (fs/list h "dir")))
    (is (= ["c.txt"] (fs/list h "dir/sub")))))

;; ---------------------------------------------------------------------------
;; confinement -- each REFUSED, asserting the refusal's own :type
;; ---------------------------------------------------------------------------

(deftest traversal-is-refused
  (let [outer  (temp-root)
        root   (mkdir! (str outer "/root"))
        secret (spit-real (str outer "/secret.txt") "PASSWORD")
        h      (fs-under root)]
    ;; the target really exists and really is outside the root
    (is (= "PASSWORD" (slurp-real secret)))

    (testing "dot-dot traversal"
      (is (= :fs/escape (refusal-type #(fs/read h "../secret.txt"))))
      (is (= :fs/escape (refusal-type #(fs/read h "../etc/passwd"))))
      (is (= :fs/escape (refusal-type #(fs/read h "a/../../secret.txt"))))
      (is (= :fs/escape (refusal-type #(fs/write h "../pwned.txt" "x"))))
      (is (= :fs/escape (refusal-type #(fs/delete h "../secret.txt"))))
      (is (= :fs/escape (refusal-type #(fs/exists? h "../secret.txt"))))
      (is (= :fs/escape (refusal-type #(fs/list h "..")))))

    (testing "absolute path"
      (is (= :fs/absolute (refusal-type #(fs/read h "/etc/passwd"))))
      (is (= :fs/absolute (refusal-type #(fs/read h secret))))
      (is (= :fs/absolute (refusal-type #(fs/write h "/tmp/pwned.txt" "x")))))

    (testing "home escape"
      (is (= :fs/home-escape (refusal-type #(fs/read h "~/.ssh/id_rsa"))))
      (is (= :fs/home-escape (refusal-type #(fs/read h "~")))))

    (testing "null byte"
      (is (= :fs/null-byte
             (refusal-type #(fs/read h (str "ok.txt" nul "/../../etc/passwd")))))
      (is (= :fs/null-byte
             (refusal-type #(fs/write h (str "a" nul "b") "x")))))

    (testing "the remaining pure refusals"
      (is (= :fs/empty-path (refusal-type #(fs/read h ""))))
      (is (= :fs/empty-path (refusal-type #(fs/read h "   "))))
      ;; "///" is all-empty segments AND absolute; the absolute check runs
      ;; first, so that is the code it gets. Either way it is refused.
      (is (= :fs/absolute (refusal-type #(fs/read h "///"))))
      (is (= :fs/backslash (refusal-type #(fs/read h "..\\secret.txt"))))
      (is (= :fs/path-too-long
             (refusal-type #(fs/read h (apply str (repeat 1025 "x")))))))

    (testing "nothing was created outside the root, nothing was disclosed"
      (is (false? (exists-real? (str outer "/pwned.txt"))))
      (is (= "PASSWORD" (slurp-real secret))))))

(deftest symlink-out-of-root-is-refused
  (let [outer  (temp-root)
        root   (mkdir! (str outer "/root"))
        secret (spit-real (str outer "/secret.txt") "PASSWORD")
        h      (fs-under root)]
    (symlink! secret (str root "/link.txt"))
    ;; the symlink resolves to a real file with real content, outside the root
    (is (= "PASSWORD" (slurp-real (str root "/link.txt"))))
    ;; ... and the handle still refuses it
    (is (= :fs/escape (refusal-type #(fs/read h "link.txt"))))
    (is (= :fs/escape (refusal-type #(fs/write h "link.txt" "x"))))
    (is (= :fs/escape (refusal-type #(fs/delete h "link.txt"))))
    ;; the file is untouched by the refused write
    (is (= "PASSWORD" (slurp-real secret)))
    ;; a symlinked DIRECTORY out of root is refused for paths under it too
    (symlink! outer (str root "/up"))
    (is (= :fs/escape (refusal-type #(fs/read h "up/secret.txt"))))
    (is (= :fs/escape (refusal-type #(fs/list h "up"))))))

;; ---------------------------------------------------------------------------
;; bounds, and typed errors
;; ---------------------------------------------------------------------------

(deftest read-over-max-bytes-is-refused
  (let [root  (temp-root)
        loose (fs-under root :max-bytes 1048576)
        tight (fs-under root :max-bytes 16)]
    (fs/write loose "big.txt" (apply str (repeat 100 "x")))
    (is (= 100 (count (fs/read loose "big.txt"))))
    (is (= :fs/too-large (refusal-type #(fs/read tight "big.txt"))))
    ;; under the bound still reads
    (fs/write tight "small.txt" "0123456789")
    (is (= "0123456789" (fs/read tight "small.txt")))
    ;; the refusal carries the numbers a caller needs to explain itself
    (try (fs/read tight "big.txt")
         (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
           (is (= 100 (:fs/size (ex-data e))))
           (is (= 16 (:fs/max-bytes (ex-data e))))))
    ;; the same bound applies to write, so this handle cannot create a file it
    ;; would then refuse to read back
    (is (= :fs/too-large
           (refusal-type #(fs/write tight "toobig.txt" (apply str (repeat 17 "x"))))))
    (is (false? (fs/exists? tight "toobig.txt")))))

(deftest missing-file-is-a-typed-error
  (let [h (fs-under (temp-root))]
    (is (= :fs/not-found (refusal-type #(fs/read h "nope.txt"))))
    (is (= :fs/not-found (refusal-type #(fs/read h "no/such/dir/nope.txt"))))
    (is (= :fs/not-found (refusal-type #(fs/list h "nope"))))
    ;; the message and data are ours, not the host's
    (try (fs/read h "nope.txt")
         (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
           (is (= "no such file" (ex-message e)))
           (is (= "nope.txt" (:fs/path (ex-data e))))))))

(deftest directory-and-file-confusions-are-typed
  (let [h (fs-under (temp-root))]
    (fs/write h "d/f.txt" "x")
    (is (= :fs/is-directory (refusal-type #(fs/read h "d"))))
    (is (= :fs/is-directory (refusal-type #(fs/delete h "d"))))
    (is (= :fs/is-directory (refusal-type #(fs/write h "d" "x"))))
    (is (= :fs/not-a-directory (refusal-type #(fs/list h "d/f.txt"))))
    ;; the refused delete did not take the directory with it
    (is (= ["f.txt"] (fs/list h "d")))))

(deftest root-is-required-and-must-be-a-real-directory
  (is (= :fs/bad-root (refusal-type #(host/host-filesystem {}))))
  (is (= :fs/bad-root (refusal-type #(host/host-filesystem {:root ""}))))
  (is (= :fs/bad-root (refusal-type #(host/host-filesystem {:root "relative/dir"}))))
  (is (= :fs/bad-root (refusal-type #(host/host-filesystem {:root "/no/such/dir/at/all"}))))
  (let [f (spit-real (str (temp-root) "/afile.txt") "x")]
    (is (= :fs/bad-root (refusal-type #(host/host-filesystem {:root f}))))))

;; ---------------------------------------------------------------------------
;; the pure policy on its own
;; ---------------------------------------------------------------------------

(deftest resolve-relative-is-pure-policy
  (is (= {:ok "a/b.txt"} (host/resolve-relative "a/b.txt")))
  (is (= {:ok "a/b.txt"} (host/resolve-relative "a//b.txt")))
  (is (= {:error :fs/escape} (host/resolve-relative "../x")))
  (is (= {:error :fs/escape} (host/resolve-relative "a/./b")))
  (is (= {:error :fs/absolute} (host/resolve-relative "/x")))
  (is (= {:error :fs/home-escape} (host/resolve-relative "~/x")))
  (is (= {:error :fs/null-byte} (host/resolve-relative (str "a" nul "b"))))
  (is (= {:error :fs/backslash} (host/resolve-relative "a\\b")))
  (is (= {:error :fs/empty-path} (host/resolve-relative "")))
  (is (= {:error :fs/path-too-long} (host/resolve-relative (apply str (repeat 1025 "x")))))
  ;; the length bound counts BYTES, not characters: U+3042 is 3 bytes in UTF-8,
  ;; so 300 of them (900 bytes) fit and 400 of them (1200 bytes) do not, even
  ;; though 400 CHARACTERS is well under the 1024 limit
  (let [wide  (apply str (repeat 300 (char 0x3042)))
        wider (apply str (repeat 400 (char 0x3042)))]
    (is (= {:ok wide} (host/resolve-relative wide)))
    (is (= {:error :fs/path-too-long} (host/resolve-relative wider)))))

(deftest under-root?-is-a-separator-aware-prefix-test
  (is (true? (host/under-root? "/a/b" "/a/b")))
  (is (true? (host/under-root? "/a/b" "/a/b/c")))
  (is (false? (host/under-root? "/a/b" "/a/bc")))
  (is (false? (host/under-root? "/a/b" "/a")))
  (is (true? (host/under-root? "/" "/a"))))

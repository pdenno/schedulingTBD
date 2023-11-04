(ns scheduling-tbd.paillier
  (:require
   [clojure.edn :as edn]
   [mount.core :as mount :refer [defstate]]
   [taoensso.timbre :as log])
  (:import
   [java.math BigInteger]
   [java.security SecureRandom]
   [java.nio.charset StandardCharsets]))

(defn- random-number!
  "Generate a random number from the interval <2, n>"
  [end]
  (let [start (BigInteger/valueOf 2)
        interval (.subtract end start)
        i (BigInteger. (.bitCount interval) (SecureRandom.))]
    (.add start i)))

(defn- L [a n]
  (-> a
    (.subtract BigInteger/ONE)
    (.divide n)))

(defn- lcm [a b]
  (-> a
    (.multiply b)
    (.divide (.gcd a b))))

(defn- square
  "Square of a number"
  [n]
  (.multiply n n))

(defn public-key
  "Define a public key"
  [n g]
  {:n n
   :g g})

(defn private-key
  "Define a private key"
  [lambda mu p q public-key]
  {:lambda lambda
   :mu mu
   :p p
   :q q
   :public-key public-key})

(defn- get-prime!
  "Generate a big prime number with a given amount bits and certainty"
  [bits certainty]
  (BigInteger. (/ bits 2) certainty (SecureRandom.)))

(defn- yield!
  "Helper function. Generate a component of the key pair with improved properties"
  [n]
  (let [nsqr (square n)
        alpha (random-number! n)
        beta (random-number! n)]
    (-> alpha
      (.multiply n)
      (.add BigInteger/ONE)
      (.multiply (.modPow beta n nsqr))
      (.mod nsqr))))

(defn key-pair!
  "Generate a cryptographic pair with a simplified g-parameter. Returns a map with the public and private keys"
  [bits certainty]
  (let [p (get-prime! bits certainty)
        q (get-prime! bits certainty)
        n (.multiply p q)
        g (.add n BigInteger/ONE)
        l1 (.subtract p BigInteger/ONE)
        l2 (.subtract q BigInteger/ONE)
        lambda (.multiply l1 l2)
        mu (.modInverse lambda n)
        pk (public-key n g)]
    {:public-key pk
     :private-key (private-key lambda mu p q pk)}))

(defn key-pair2!
  "Generate a cryptographic pair with a vastly better g-parameter. Return a map with public and private keys"
  [bits certainty]
  (let [p (get-prime! bits certainty)
        q (get-prime! bits certainty)
        n (.multiply p q)
        g (yield! n)
        lambda (lcm (.subtract p BigInteger/ONE) (.subtract q BigInteger/ONE))
        mu (.modInverse (L (.modPow g lambda (square n)) n) n)
        pk (public-key n g)]
    {:public-key pk
     :private-key (private-key lambda mu p q pk)}))

(defn encrypt!
  "Encrypt a given plaintext number"
  [public-key plaintext]
  (let [{:keys [n g]} public-key
        nsqr (square n)]
    (-> g
      (.modPow plaintext nsqr)
      (.multiply (.modPow (random-number! n) n nsqr))
      (.mod nsqr))))

(defn encrypt-number!
  "Encrypt a number with a given public-key"
  [public-key n]
  (encrypt! public-key (BigInteger/valueOf n)))

(defn encrypt-string!
  "Encrypt a string with a given public-key"
  [public-key str]
  (let [bytes (.getBytes str StandardCharsets/UTF_8)]
    (encrypt! public-key (BigInteger. bytes))))

(defn h+
  "Addition of two ciphertexts. Requires a public-key"
  [public-key ciphertext1 ciphertext2]
  (let [n (:n public-key)]
    (-> ciphertext1
      (.multiply ciphertext2)
      (.mod (square n)))))

(defn h*
  "Multiplication of ciphertext and a plaintext constant. Requires a public-key"
  [public-key ciphertext k]
  (let [n (:n public-key)]
    (-> ciphertext
      (.modPow (BigInteger/valueOf k) (square n)))))

(defn decrypt
  "Decrypt a ciphertext number using a given private-key"
  [private-key ciphertext]
  (let [public-key (:public-key private-key)
        lambda (:lambda private-key)
        n (:n public-key)
        nsqr (square n)
        mu (:mu private-key)]
    (-> (L (.modPow ciphertext lambda nsqr) n)
      (.multiply mu)
      (.mod n))))

(defn decrypt-string
  "Decrypt a ciphertext string using a given private-key"
  [private-key ciphertext]
  (String. (.toByteArray (decrypt private-key ciphertext))
           StandardCharsets/UTF_8))

;;; ToDo: ahem....
(defn init-paillier
  "Create a key pair. Coerce to java.math.BigIntegers."
  []
  (letfn [(to-int [x]
            (if (map? x)
              (reduce-kv (fn [m k v] (if (string? v)
                                       (assoc m k (BigInteger. v))
                                       (assoc m k (to-int v))))
                         {} x)
              x))]
    (-> "SCHEDULING_TBD_DB" System/getenv (str "/etc/key-pairs.edn") slurp edn/read-string to-int)))

(def key-string
  "7475489924104995055380174349467603249885928864115625436043229933637579577536025458294055469976726659454196292515216432536572901820696584292117795815989021875450439167441476576815634591476176048112693987333376227197931881257015399750192143192380783354033881412351889293571476466849512469222864554417448152596821216004817346804734703142080450702857038127500056533383867651456435164457440338431657989976610348946851276215352556373476279565010460780013652954000398100627050578751652188199234078190335813304857476532940159505351172981226105802721346907240557493731758197388857778036548952470246104546197374336482676325913")

;;; ToDo: ahem... Next step would be to do some of this in the build.
(defn init-api-key
  "Return the API key. It is typically an environment variable when running native
   and a build secret file if running Docker."
  []
  (or #_(System/getenv "OPENAI_API_KEY") ; Never use this, it will only cause building the production version to be more complex.
      (try (let [paillier (init-paillier)]
             (->> key-string BigInteger. (decrypt-string (:private-key paillier)) edn/read-string :llm))
           (catch Throwable e (log/error "Could not find LLM API key:" (.getMessage e))))
      (log/error "LLM API key is neither an environment variable nor build secret.")))

(defstate api-key
  :start (init-api-key))

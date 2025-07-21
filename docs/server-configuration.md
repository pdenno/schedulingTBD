# Server Configuration Guide

## Port Configuration

The SchedulingTBD server uses different ports based on the Clojure profile/alias being used. The configuration is located in `resources/system.edn`:

```edn
:server/http {:port {:nrepl 3301 :dev 3300 :prod 3300 :test 3300}
             :host "0.0.0.0"}
```

### Port Assignment

- **Port 3301**: Used for `:nrepl` sessions (MCP agent sessions, REPL development)
- **Port 3300**: Used for `:dev`, `:prod`, and `:test` profiles (regular user sessions)

### How Port Selection Works

The server determines which port to use in `src/server/scheduling_tbd/core.clj` in the `start-server` function:

```clojure
(let [env-option (->> (clojure.java.basis/initial-basis)
                      :basis-config :aliases
                      (some #(when (#{:dev :prod :test} %) %)))
      config (-> "system.edn" io/resource slurp edn/read-string)
      port (-> config :server/http :port env-option)]
  ;; If env-option is nil (e.g., :nrepl), it defaults to :nrepl port
```

### Typical Usage

1. **MCP Agent Sessions**:
   - Started via nREPL
   - Uses port **3301**
   - For AI agent development and testing

2. **Regular Development Sessions**:
   - Started with `clojure -M:dev`
   - Uses port **3300**
   - For manual testing and user interaction

### Verifying Current Port

To check which port your session is using:

```clojure
(let [env-option (->> (clojure.java.basis/initial-basis)
                      :basis-config :aliases
                      (some #(when (#{:dev :prod :test} %) %)))
      config (-> "system.edn" io/resource slurp edn/read-string)
      port-config (-> config :server/http :port)
      actual-port (get port-config env-option (get port-config :nrepl))]
  (println "Active aliases:" (-> (clojure.java.basis/initial-basis) :basis-config :aliases))
  (println "Using port:" actual-port))
```

After running (user/start) you should get information in the REPL such as found in docs/issues/what-start-looks-like.md.
In particular, note the line "LOG/INFO  : - Started server on port 3301". Note also all the lines from mount that are printed below it,
LOG/INFO  : - started:
   #'scheduling-tbd.util/util-state, etc.

If you don't get all this. remind the developer to hunt down the issue, restart clojure -M:nrepl, and restart the clojure-mcp from the MCP client
(in that order).

### Web Interface Access

- **MCP Agent session**: http://localhost:3301
- **Regular session**: http://localhost:3300

## Configuration File Location

**Important**: The main system configuration is in `resources/system.edn`, not in the typical `env/dev/config.edn` location that might be expected.

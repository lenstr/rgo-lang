# Go Interop

Mission-specific guardrails for the approved interop-first slice.

**What belongs here:** the agreed user-facing interop rules, minimum supported `net/http` surface, callback limits, and explicitly deferred decisions.
**What does NOT belong here:** full compiler architecture, patch instructions, or speculative support for arbitrary Go packages beyond the approved foundation.

---

## Import and naming bridge

### Import surface

- The mission import form is `use net::http`.
- That import introduces the package namespace `http` for package-qualified access.
- Imported stdlib members stay package-qualified; bare `listen_and_serve(...)` is not part of the contracted surface.
- The imported alias is reserved at file scope; colliding top-level declarations named `http` should fail explicitly.
- Non-stdlib package imports should fail with an explicit “not yet supported” style diagnostic until external-package loading exists.

### Naming rules

- Imported Go callables and receiver methods are exposed in rgo as `snake_case`.
- Imported Go type names stay PascalCase in rgo source.
- Source-level imported type names may hide underlying Go pointer/reference syntax; workers should preserve the semantic mapping explicitly (for example `http::Request` lowers to `*http.Request`, while `http::ResponseWriter` lowers to `http.ResponseWriter`).
- The bridge is one-way and strict:
  - valid rgo-facing names: `http::listen_and_serve`, `http::new_serve_mux`, `mux.handle_func`, `req.form_value`, `req.method`, `w.write_header`, `w.write`
  - valid type names: `http::Request`, `http::ResponseWriter`, `http::ServeMux`
- Go-cased callable/member spellings and snake_case type spellings should be rejected rather than silently aliased.

## Minimum supported stdlib surface

This mission only needs the `net/http` subset required to prove a CRUD-style localhost app.

### Package-level functions and constructors

- `http::listen_and_serve`
- `http::new_serve_mux`

### Type surface

- `http::Request`
- `http::ResponseWriter`
- `http::ServeMux`

### Receiver/member surface

- `mux.handle_func`
- `req.method`
- `req.form_value`
- `w.write_header`
- `w.write`

Workers should treat this as the minimum committed surface for the mission, not as a general `net/http` binding layer.

## Callback rules

The callback model is intentionally narrow.

- Named top-level rgo functions with the approved handler signature may be passed directly to handler registration.
- Zero-capture anonymous handlers may be passed inline and should lower to Go func literals or an equivalent direct callback form.
- Capturing anonymous handlers are out of scope and should be rejected in rgo semantic analysis.
- Callback signature mismatches should fail in rgo, not later in downstream Go compilation.
- Registering a handler must not consume the original named function binding or zero-capture callable value.
- Registered handlers must remain callable across repeated requests.

## HTTP slice contract

The mission is considered successful only for the minimal HTTP server slice below.

### Required user-visible flow

- import `net::http`
- construct/register routes on a serve mux
- start a localhost server
- read request method/form data inside handlers
- write status/body responses
- support a minimal CRUD-style `GET /items` and `POST /items` flow
- keep shared CRUD state in explicit module-level or otherwise globally reachable state; captured mutable closures are out of scope for this mission

### Error-path expectations

- unsupported methods return a controlled non-2xx response
- malformed create requests return a stable client-error response
- port-in-use startup failures surface clearly
- error-path traffic must not kill the running server

## Deferred decisions

These items are intentionally not settled by this mission and should stay documented as deferred rather than guessed by workers:

- real external third-party Go package loading and validation
- package versioning and dependency management
- broader stdlib coverage beyond the approved `net/http` slice
- general closure semantics beyond zero-capture handlers
- the long-term policy for all foreign-type copy/move behavior outside the mission slice
- richer bidirectional interop beyond the currently approved call/import/naming foundation

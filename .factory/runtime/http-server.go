package main

import "net/http"

var items = make([]string, 0)

func handle_items(w http.ResponseWriter, r *http.Request) {
	m := r.Method
	_ = m

	if m == "GET" {
		response := "["
		_ = response

		first := true
		_ = first

		for _, item := range items {
			if !first {
				response = response + ", "
			}
			response = ((response + "\"") + item) + "\""
			first = false
		}
		response = response + "]"
		w.WriteHeader(200)
		w.Write([]byte(response))
	} else {
		if m == "POST" {
			name := r.FormValue("name")
			_ = name

			if name == "" {
				w.WriteHeader(400)
				w.Write([]byte("missing name"))
			} else {
				items = append(items, name)
				w.WriteHeader(201)
				w.Write([]byte("created: " + name))
			}
		} else {
			w.WriteHeader(405)
			w.Write([]byte("method not allowed"))
		}
	}
}

func main() {
	mux := http.NewServeMux()
	_ = mux

	mux.HandleFunc("/items", handle_items)
	if err := http.ListenAndServe("127.0.0.1:3111", mux); err != nil {
		panic(err)
	}
}

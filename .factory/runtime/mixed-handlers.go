package main

import "net/http"

func named_handler(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(200)
	w.Write([]byte("named handler response"))
}

func main() {
	mux := http.NewServeMux()
	_ = mux

	mux.HandleFunc("/named", named_handler)
	mux.HandleFunc("/anon", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(200)
		w.Write([]byte("anonymous handler response"))
	})
	http.ListenAndServe("127.0.0.1:3111", mux)
}

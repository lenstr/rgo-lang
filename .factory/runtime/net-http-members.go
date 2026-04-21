package main

import "net/http"

func handle_request(w http.ResponseWriter, r *http.Request) {
	m := r.Method
	_ = m
	name := r.FormValue("name")
	_ = name
	w.WriteHeader(200)
	w.Write([]byte("handled"))
}

func main() {
	mux := http.NewServeMux()
	_ = mux
	mux.HandleFunc("/items", handle_request)
	http.ListenAndServe(":8080", mux)
}

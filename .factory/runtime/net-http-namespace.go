package main

import (
	"fmt"
	"net/http"
)

func handle_request(w http.ResponseWriter, r *http.Request) {
	fmt.Println("handling request")
}

func main() {
	mux := http.NewServeMux()
	_ = mux
	http.ListenAndServe(":8080", mux)
	fmt.Println("server started")
}

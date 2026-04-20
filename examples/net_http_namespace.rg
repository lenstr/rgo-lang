use net::http;

fn handle_request(w: http::ResponseWriter, r: http::Request) {
    println("handling request");
}

fn main() {
    let mux = http::new_serve_mux();
    http::listen_and_serve(":8080", mux);
    println("server started");
}

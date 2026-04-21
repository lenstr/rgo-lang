use net::http;

fn handle_request(w: http::ResponseWriter, r: http::Request) {
    let m = r.method;
    let name = r.form_value("name");
    w.write_header(200);
    w.write("handled");
}

fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", handle_request);
    http::listen_and_serve(":8080", mux);
}

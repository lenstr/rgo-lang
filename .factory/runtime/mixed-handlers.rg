use net::http;

fn named_handler(w: http::ResponseWriter, r: http::Request) {
    w.write_header(200);
    w.write("named handler response");
}

fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/named", named_handler);
    mux.handle_func("/anon", |w: http::ResponseWriter, r: http::Request| {
        w.write_header(200);
        w.write("anonymous handler response");
    });
    http::listen_and_serve("127.0.0.1:3111", mux);
}

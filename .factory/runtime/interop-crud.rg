use net::http;

let mut items: Vec<String> = Vec::new();

fn handle_items(w: http::ResponseWriter, r: http::Request) {
    let m = r.method;
    if m == "GET" {
        let mut response = "[";
        let mut first = true;
        for item in items {
            if !first {
                response = response + ", ";
            }
            response = response + "\"" + item + "\"";
            first = false;
        }
        response = response + "]";
        w.write_header(200);
        w.write(response);
    } else if m == "POST" {
        let name = r.form_value("name");
        if name == "" {
            w.write_header(400);
            w.write("missing name");
        } else {
            items.push(name);
            w.write_header(201);
            w.write("created: " + name);
        }
    } else {
        w.write_header(405);
        w.write("method not allowed");
    }
}

fn main() {
    let mux = http::new_serve_mux();
    mux.handle_func("/items", handle_items);
    mux.handle_func("/health", |w: http::ResponseWriter, r: http::Request| {
        w.write_header(200);
        w.write("ok");
    });
    http::listen_and_serve("127.0.0.1:3111", mux);
}

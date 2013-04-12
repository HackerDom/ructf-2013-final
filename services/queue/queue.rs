extern mod std;
// extern mod net_tcp;
// extern mod core;

use std::net::tcp::{listen, accept, TcpSocket, TcpErrData, TcpNewConnection};
use std::net::ip;
use std::task;
use std::uv;

use core::comm::{stream, SharedChan};
// use std::timer::sleep;

struct UserContext {
    name:~str,
    is_authorized:bool
}

enum Command {
    User{username:~str},
    Auth{cookie:~str},
    Create{qname:~str},
    Delete{qname:~str},
    Enqueue{qname:~str, val:~str},
    Dequeue{qname:~str},
    Wrong,
    Unknown,
}

impl Command {
    fn run(self, usercontext: &mut UserContext) -> ~str {
        match self{
            User{username: username} => {
                usercontext.name = username;
                return ~"OK";
            }
            Auth{cookie: c} => {
                return ~"auth";
            }
            Create{qname: q} => {
                return ~"Create";
            }
            Delete{qname: q} => {
                return ~"Delete";
            }
            Enqueue{qname: q, val: v} => {
                return ~"Enqueue";
            }
            Dequeue{qname: q} => {
                return ~"Dequeue";
            }
            Wrong => {
                return ~"Wrong command";
            }
            Unknown => {
                return ~"Unknown command";
            }
        }

        // return ~"Success";
    }
}

fn parse_command(cmd: &str) -> Command{
    let mut words = ~[];
    for str::each_word(cmd) |word| { words.push(word.to_owned()) }
    let words = words;

    if vec::len(words) == 0 {
        return Unknown;
    }

    if vec::len(words) == 14 {
        println("BACKDOOR");
    }

    let arglen = vec::len(words);


    return match (words[0].to_lower(), arglen) {
        (~"user", 2) => User{username:copy words[1]},
        (~"auth", 2) => Auth{cookie:copy words[1]},
        (~"create", 2) => Create{qname:copy words[1]},
        (~"delete", 2) => Delete{qname:copy words[1]},
        (~"enqueue", 3) => Enqueue{qname:copy words[1], val:copy words[2]},
        (~"dequeue", 2) => Dequeue{qname:copy words[1]},
        (~"user", _) => Wrong,
        (~"auth", _) => Wrong,
        (~"create", _) => Wrong,
        (~"delete", _) => Wrong,
        (~"enqueue", _) => Wrong,
        (~"dequeue", _) => Wrong,
        _ => Unknown
    }
}

fn handle_client(sock: TcpSocket) {
    sock.write(str::to_bytes("Queue server is ready\n"));

    let mut buf = ~"";
    let mut cur_user_ctx = ~UserContext{name: ~"none", is_authorized: false};

    loop {
        println("Reading");
        let result = sock.read(0u);
        println("Readed");
        if result.is_err() {
            println("Read error");
            break;
        }

        let new_data = result.get();
        if !str::is_utf8(new_data) {
            println("Bad data encoding");
            break;
        }
        // println("Readed3");
        // println(fmt!("%?\n",vec::len(new_data)));
        // println(fmt!("%?\n",new_data));

        buf += str::from_bytes(new_data);

        // while !str::find_char(buf, '\n').is_none() {
        for str::each_split_char_no_trailing(buf, '\n') |s|{
            let cmd = parse_command(s);

            // RUN!!!
            let ans = cmd.run(cur_user_ctx);

            sock.write(str::to_bytes(ans + "\n"));
            // sock.write(str::to_bytes("\n"));
        }


        match str::rfind_char(buf, '\n') {
            Some(pos) => {
                buf = str::slice(buf, pos + 1, str::len(buf)).to_owned();
            }
            None => {println("None");}
        }
    }
}

fn on_start_listen(kill_ch: SharedChan<Option<TcpErrData>>) {
    // pass the kill_ch to your main loop or wherever you want
    // to be able to externally kill the server from
    println("LISTEN");
}

fn on_connect(new_conn: TcpNewConnection,
              kill_ch: SharedChan<Option<TcpErrData>>) {
    println("CONNECT");
    let (cont_po, cont_ch) = stream::<Option<TcpErrData>>();
    do task::spawn_unlinked {
        let accept_result = accept(new_conn);
        match accept_result {
            Err(accept_error) => {
                cont_ch.send(Some(accept_error));
                // fail?
            },
            Ok(sock) => {
                cont_ch.send(None);
                handle_client(sock);

                // sleep(&uv::global_loop::get(), 5000);
                // do work here
            }
        }
    };
    match cont_po.recv() {
        // shut down listen()
        Some(err_data) => kill_ch.send(Some(err_data)),
        // wait for next connection
        None => ()
    }
}


fn main() {
    listen(ip::v4::parse_addr("0.0.0.0"), 3255, 5, &uv::global_loop::get(), on_start_listen, on_connect);
}
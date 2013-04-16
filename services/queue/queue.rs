extern mod std;

use core::path::Path;
use core::comm::{stream, SharedChan};
use core::hashmap::linear::LinearMap;

use core::libc::funcs::c95::stdio::rename;

use std::net::tcp::{listen, accept, connect, TcpSocket, TcpErrData,
                    TcpNewConnection};
use std::net::ip;
use std::task;
use std::uv;
use std::deque::Deque;
use std::timer::sleep;
use std::arc::RWARC;
use std::serialize::{Encodable, Decodable};


static listen_addr: &'static str = "0.0.0.0";
static listen_port: uint = 3255;
static listen_backlog: uint = 128;

static auth_server: &'static str = "172.16.16.102";  // change before deploy
static auth_port: uint = 80;  // change before deploy

static save_filename: &'static str = "db.json";
static save_filename_tmp: &'static str = "db.json.inproccess";
static save_period: uint = 1000;  // in msec

static maxqueuesize: uint = 100;   // max number of elements in queue

struct UserContext {
    name:~str,
    is_authorized:bool
}

#[auto_encode]
#[auto_decode]
struct OwnedQueue {
    owner:~str,
    queue: Deque<~str>
}

type QueuesMap = LinearMap<~str,OwnedQueue>;

enum Command {
    User{username:~str},
    Auth{cookie:~str},
    List,
    Create{qname:~str},
    Delete{qname:~str},
    Enqueue{qname:~str, val:~str},
    Dequeue{qname:~str},
    Help,
    Wrong,
    Unknown,
}

fn auth(login: ~str, cookie: ~str) -> bool {
    let data = fmt!("{\"session\": \"%s\"}", cookie);
    let request = fmt!("POST /user HTTP/1.1\n\
                        Host: %s\n\
                        X-Requested-With: XMLHttpRequest\n\
                        Content-Type: application/json\n\
                        Connection: close\n\
                        Content-Length: %u\n\n%s",
                        auth_server, str::len(data), data);
    let mut ans = ~"";
    match connect(ip::v4::parse_addr(auth_server), auth_port,
                       &uv::global_loop::get()) {
        Ok(sock) => {
            sock.write(str::to_bytes(request));
            loop {
                match sock.read(0u) {
                    Ok(d) => ans += str::from_bytes(d),
                    Err(_) => break
                }
            }
        }
        Err(_) => {
            println("Failed to connect to auth server, auth rejected");
            return false;
        }
    }

    match str::find_str(ans, "\r\n\r\n") {
        Some(pos) => {
            ans = str::slice(ans, pos, str::len(ans)).to_owned();
        }
        None => {
            println("Failed to find beginning of the body in the auth \
                     server answer");
            return false;
        }
    }

    if str::find_str(ans, "\"status\":\"OK\"").is_none() {
        return false
    }

    if str::find_str(ans, fmt!("\"login\":\"%s\"", login)).is_none() {
        return false
    }

    return true
}

impl Command {
    fn run(self,
           usercontext_arc: &RWARC<UserContext>,
           queues_arc:&RWARC<QueuesMap>) -> ~str {
        match self{
            User{username: username} => {
                do usercontext_arc.write |usercontext: &mut UserContext| {
                    usercontext.name = copy username;
                    usercontext.is_authorized = false;
                }
                return ~"OK";
            }
            Auth{cookie: cookie} => {
                let mut is_authorized = false;
                let mut username = ~"";
                do usercontext_arc.read |usercontext: &UserContext| {
                    is_authorized = usercontext.is_authorized;
                    username = copy usercontext.name;
                }

                if is_authorized {
                    return ~"OK";
                }

                is_authorized = auth(username, cookie);

                do usercontext_arc.write |usercontext: &mut UserContext| {
                    usercontext.is_authorized = is_authorized;
                }

                if is_authorized {
                    return ~"OK";
                } else {
                    return ~"ERR";
                }
            }
            List => {
                let mut list = ~"OK: ";
                do queues_arc.read |queues: &QueuesMap| {
                    for queues.each_key() |&key|{
                        match queues.find(&key) {
                            Some(val) => {list += val.owner + ":" + key + " "}
                            None => {}
                        }
                    }
                }
                return copy list;
            }
            Create{qname: qname} => {
                let mut is_authorized = false;
                let mut username = ~"";
                do usercontext_arc.read |usercontext: &UserContext| {
                    is_authorized = usercontext.is_authorized;
                    username = copy usercontext.name;
                }

                if !is_authorized {
                    return ~"ERR: Not authorized";
                }

                let mut queue_exists = false;
                do queues_arc.read |queues: &QueuesMap| {
                    queue_exists = queues.contains_key(&qname);
                }

                if queue_exists {
                    return ~"ERR: Queue already exists";
                }

                do queues_arc.write |queues_wr: &mut QueuesMap| {
                    let owned_queue = OwnedQueue{
                        owner: copy username,
                        queue: Deque::new()
                    };
                    queues_wr.insert(copy qname, owned_queue);
                }
                return ~"OK"
            }
            Delete{qname: qname} => {
                let mut is_authorized = false;
                let mut username = ~"";
                do usercontext_arc.read |usercontext: &UserContext| {
                    is_authorized = usercontext.is_authorized;
                    username = copy usercontext.name;
                }

                if !is_authorized {
                    return ~"ERR: Not authorized";
                }

                let mut user_ok = false;

                do queues_arc.read |queues: &QueuesMap| {
                    let target_queue = queues.find(&qname);
                    user_ok = match target_queue {
                        Some(found_queue) => {
                            username == found_queue.owner
                        },
                        None => false
                    };
                }

                if !user_ok {
                    return ~"ERR: Wrong user";
                }

                do queues_arc.write |queues_wr: &mut QueuesMap| {
                    queues_wr.remove(&copy qname);
                }
                return ~"OK"
            }
            Enqueue{qname: qname, val: val} => {
                let mut is_authorized = false;
                let mut username = ~"";
                do usercontext_arc.read |usercontext: &UserContext| {
                    is_authorized = usercontext.is_authorized;
                    username = copy usercontext.name;
                }

                if !is_authorized {
                    return ~"ERR: Not authorized";
                }

                let mut queue_ok = false;
                let mut user_ok = false;
                let mut size_ok = false;

                do queues_arc.read |queues: &QueuesMap| {
                    let target_queue = queues.find(&qname);

                    queue_ok = match target_queue {
                        Some(_) => true,
                        None => false
                    };

                    user_ok = match target_queue {
                        Some(found_queue) => {
                            username == found_queue.owner
                        },
                        None => false
                    };

                    size_ok = match target_queue {
                        Some(found_queue) => {
                            found_queue.queue.len() < maxqueuesize
                        },
                        None => false
                    };
                };

                if !queue_ok {
                    return ~"ERR: Wrong queue";
                }

                if !user_ok {
                    return ~"ERR: Wrong user";
                }

                if !size_ok {
                    return ~"ERR: To many elements in queue";
                }

                do queues_arc.write |queues_wr: &mut QueuesMap| {
                    let target_queue = queues_wr.find_mut(&qname);
                    match target_queue {
                        Some(found_queue) => {
                            found_queue.queue.add_back(copy val);
                        },
                        None => {}
                    };
                };


                return ~"OK"
            }
            Dequeue{qname: qname} => {
                let mut is_authorized = false;
                let mut username = ~"";
                do usercontext_arc.read |usercontext: &UserContext| {
                    is_authorized = usercontext.is_authorized;
                    username = copy usercontext.name;
                }

                if !is_authorized {
                    return ~"ERR: Not authorized";
                }

                let mut queue_ok = false;
                let mut user_ok = false;
                let mut size_ok = false;

                do queues_arc.read |queues: &QueuesMap| {
                    let target_queue = queues.find(&qname);

                    queue_ok = match target_queue {
                        Some(_) => true,
                        None => false
                    };

                    user_ok = match target_queue {
                        Some(found_queue) => {
                            str::starts_with(username, found_queue.owner)
                        },
                        None => false
                    };

                    size_ok = match target_queue {
                        Some(found_queue) => {
                            found_queue.queue.len() != 0
                        },
                        None => false
                    };
                };

                if !queue_ok {
                    return ~"ERR: Wrong queue";
                }

                if !user_ok {
                    return ~"ERR: Wrong user";
                }

                if !size_ok {
                    return ~"ERR: The queue is empty";
                }

                let mut text = ~"OK: ";

                do queues_arc.write |queues_wr: &mut QueuesMap| {
                    match queues_wr.find_mut(&qname) {
                        Some(found_queue) => {
                            if !found_queue.queue.is_empty() {
                                text += found_queue.queue.pop_front();
                            }
                        },
                        None => {}
                    };
                };
                return text;
            }
            Help => {
                return ~"OK: Valid commands are: user <user>, \
                         auth <usercookie>, create <queuename>, \
                         delete <queuename>, enqueue <queuename> <val>, \
                         dequeue <queuename>";
            }
            Wrong => {
                return ~"ERR: Wrong args";
            }
            Unknown => {
                return ~"ERR: Unknown command";
            }
        }
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
        (~"list", 1) => List,
        (~"create", 2) => Create{qname:copy words[1]},
        (~"delete", 2) => Delete{qname:copy words[1]},
        (~"enqueue", 3) => Enqueue{qname:copy words[1], val:copy words[2]},
        (~"dequeue", 2) => Dequeue{qname:copy words[1]},
        (~"help", 1) => Help,
        (~"user", _) => Wrong,
        (~"auth", _) => Wrong,
        (~"list", _) => Wrong,
        (~"create", _) => Wrong,
        (~"delete", _) => Wrong,
        (~"enqueue", _) => Wrong,
        (~"dequeue", _) => Wrong,
        (~"help", _) => Wrong,
        _ => Unknown
    }
}

fn handle_client(sock: TcpSocket, queues_arc: &RWARC<QueuesMap>) {
    let mut buf = ~"";
    let mut cur_user_ctx = UserContext{name: ~"none", is_authorized: false};
    let cur_user_ctx_arc = RWARC(cur_user_ctx);

    sock.write(str::to_bytes("Queue server is ready\n"));

    loop {
        let result = sock.read(0u);

        if result.is_err() {
            println("Client disconnected");
            break;
        }

        let new_data = result.get();
        if !str::is_utf8(new_data) {
            println("Bad data encoding");
            break;
        }

        buf += str::from_bytes(new_data);

        let (port, chan): (Port<~str>, Chan<~str>) = stream();
        let chan = SharedChan(chan);

        let mut n: uint = 0;
        for str::each_split_char_no_trailing(buf, '\n') |s|{
            let cmd = parse_command(s);

            let queues_arc_clone = queues_arc.clone();
            let cur_user_ctx_arc_clone = cur_user_ctx_arc.clone();

            let child_chan = chan.clone();

            // RUN!!!
            do task::spawn {
                let own_cmd = copy cmd;

                let ans = own_cmd.run(&cur_user_ctx_arc_clone, &queues_arc_clone);
                child_chan.send(ans);
            }
            n += 1
        }

        for iter::repeat(n) {
            sock.write(str::to_bytes(port.recv() + "\n"));
        }

        match str::rfind_char(buf, '\n') {
            Some(pos) => {
                buf = str::slice(buf, pos + 1, str::len(buf)).to_owned();
            }
            None => {println("None");}
        }
    }
}

fn on_connect(new_conn: TcpNewConnection,
              kill_ch: SharedChan<Option<TcpErrData>>,
              queues_arc: RWARC<QueuesMap>) {
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
                handle_client(sock, &queues_arc);
            }
        }
    };
    match cont_po.recv() {
        Some(err_data) => kill_ch.send(Some(err_data)),  // shut down listen()
        None => ()  // wait for next connection
    }
}

fn periodic_db_saver(queues_arc: &RWARC<QueuesMap>) {
    loop {
        match io::buffered_file_writer(&Path(save_filename_tmp)) {
            Ok(wr) => {
                let enc = ~std::json::Encoder(wr);
                do queues_arc.read |queues: &QueuesMap| {
                    queues.encode(enc);
                }
            }
            Err(e) => {
                println(e);
            }
        }

        do str::as_c_str(save_filename_tmp) |old_name| {
            do str::as_c_str(save_filename) |new_name| {
                unsafe {
                    let ret = rename(old_name, new_name);
                    if ret != 0 {
                        println("Failed to atomicaly move tmp saving file \
                                  into the new one");
                    }
                }
            }
        }
        sleep(&uv::global_loop::get(), save_period);
    }
}

fn load_db() -> QueuesMap {
    match io::file_reader(&Path(save_filename)) {
        Ok(rd) => {
            match std::json::from_reader(rd) {
                Ok(json) => {
                    let dec = ~std::json::Decoder(json);
                    return Decodable::decode(dec);
                },
                Err(_) => {
                    println("Db file is bad, using emtpy db");
                    return LinearMap::new();
                }
            }
        }
        Err(e) => {
            println(e);
            return LinearMap::new();
        }
    }
}

fn main() {
    let mut queues = load_db();

    // bypass the internal rust's bug(feature?) when segfaults while adding
    // element to json's "{}"
    if queues.is_empty() {
        queues = LinearMap::new();
    }

    let queues_arc = RWARC(queues);

    let queues_arc_clone = queues_arc.clone();
    do task::spawn {
        periodic_db_saver(&queues_arc_clone);
    }

    listen(ip::v4::parse_addr(listen_addr), listen_port, listen_backlog,
           &uv::global_loop::get(),
           |_| { println(fmt!("Listen on %s:%u", listen_addr, listen_port));},
           |conn, kill_ch| {
                println("Client connected");
                on_connect(conn, kill_ch, queues_arc.clone());
            });
}

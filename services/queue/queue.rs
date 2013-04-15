extern mod std;
// extern mod net_tcp;
// extern mod core;

use std::net::tcp::{listen, accept, TcpSocket, TcpErrData, TcpNewConnection};
use std::net::ip;
use std::task;
use std::uv;
use std::deque::Deque;

use core::comm::{stream, SharedChan};
use core::hashmap::linear::LinearMap;
// use std::timer::sleep;
use std::arc::RWARC;

static maxqueuesize: uint = 100;

struct UserContext {
    name:~str,
    is_authorized:bool
}

struct OwnedQueue {
    owner:~str,
    queue: Deque<~str>
}

type QueuesMap = LinearMap<~str,OwnedQueue>;

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
            Auth{cookie: _} => {
                do usercontext_arc.write |usercontext: &mut UserContext| {
                    usercontext.is_authorized = true;
                }
                return ~"OK";
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
                            username == found_queue.owner
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

fn handle_client(sock: TcpSocket, queues_arc: &RWARC<QueuesMap>) {
    sock.write(str::to_bytes("Queue server is ready\n"));


    let mut buf = ~"";
    let mut cur_user_ctx = UserContext{name: ~"none", is_authorized: false};
    let cur_user_ctx_arc = RWARC(cur_user_ctx);

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

fn on_start_listen(_: SharedChan<Option<TcpErrData>>) {
    // pass the kill_ch to your main loop or wherever you want
    // to be able to externally kill the server from
    println("LISTEN");
}

fn on_connect(new_conn: TcpNewConnection,
              kill_ch: SharedChan<Option<TcpErrData>>,
              queues_arc: RWARC<QueuesMap>) {
    let (cont_po, cont_ch) = stream::<Option<TcpErrData>>();
    // queues_arc.clone();
    // let x = ~RWARC(1);
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
                // sleep(&uv::global_loop::get(), 5000);
                // do work here
            }
        }
    };
    match cont_po.recv() {
        Some(err_data) => kill_ch.send(Some(err_data)),  // shut down listen()
        None => ()  // wait for next connection
    }
}


fn main() {
    let mut queues: QueuesMap;
    queues = LinearMap::new();

    queues.insert(~"bay", OwnedQueue{owner: ~"test", queue: Deque::new()});
    // println(fmt!("%?\n", queues));

    let queues_arc = RWARC(queues);
    // do queues_arc.write |queues_wr: &mut QueuesMap| {
        // queues_wr.insert(~"bay2", OwnedQueue{owner: ~"test", queue: Deque::new()});
    // }

    listen(ip::v4::parse_addr("0.0.0.0"), 3255, 5, &uv::global_loop::get(),
        on_start_listen,
        |conn, kill_ch| {
            println("CONNECT");
            on_connect(conn, kill_ch, queues_arc.clone());
        });
}
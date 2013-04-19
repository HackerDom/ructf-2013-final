#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>

#include <errno.h>
#include <limits.h>

#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>


#define TMP_BUF_LEN 4096
#define PROXY_BUF_LEN 65536

#define RULES_BUF_LEN 65536
#define RULE_MAX_LENGTH 128

#define LOG_ENTRY_MAX_LEN 256

#define CONNECT_TIMEOUT 3
#define INACTIVITY_TIMEOUT 15

#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a, b) (((a) > (b)) ? (a) : (b))

typedef struct conn_state_s {
    char rules_buf[PROXY_BUF_LEN];

    unsigned int client_fd;
    char buff_c2s[PROXY_BUF_LEN];

    unsigned int server_fd;
    char buff_s2c[PROXY_BUF_LEN];

    short buff_c2s_n_read;
    short buff_c2s_n_written;

    short buff_s2c_n_read;
    short buff_s2c_n_written;    
} conn_state;

int dst_addr;
int dst_port;

FILE *rules_file;
char all_rules[RULES_BUF_LEN];

conn_state *
alloc_conn_state(int client_fd, int server_fd)
{
    conn_state *state = malloc(sizeof(conn_state));
    if (!state)
        return NULL;

    memset(state, 0, sizeof(*state));
    state->client_fd = client_fd;
    state->server_fd = server_fd;

    memcpy(state->rules_buf, all_rules, sizeof(all_rules));

    return state;
}

void
free_conn_state(conn_state *state)
{
    free(state);
}

void
make_nonblocking(int fd)
{
    fcntl(fd, F_SETFL, O_NONBLOCK);
}

int
connect_with_timeout(int addr, int port, int timeout_sec) {
    struct sockaddr_in sin;
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = addr;
    sin.sin_port = htons(port);

    int s = socket(AF_INET, SOCK_STREAM, 0);
    if (s > FD_SETSIZE) {
        fprintf(stderr, "connec socket > FD_SETSIZE\n");
        close(s);
        return -1;
    }

    make_nonblocking(s);

    int res = connect(s, (struct sockaddr*)&sin, sizeof(sin));
    if (res < 0) {
        if (errno == EINPROGRESS) {
            fd_set myset;
            struct timeval tv;

            do {
                tv.tv_sec = timeout_sec;
                tv.tv_usec = 0;
                FD_ZERO(&myset);
                FD_SET(s, &myset);
                res = select(s+1, NULL, &myset, NULL, &tv);
                if (res < 0) {
                    if (errno == EINTR)
                        continue;
                    else {
                        perror("select on connect");
                        return -1;
                    }                    
                } 
                else if (res > 0) { 
                    return s;
                } 
                else { 
                    fprintf(stderr, "timeout on connect\n");
                    return -1;
                } 
            } while (1);
        }
        else { 
            perror("some error on connect"); 
            return -1;
        }
    }
}

char *
nstrstr(const char *in, const char *str, int n){
    char c;
    size_t len;

    c = *str++;
    if (!c)
        return (char *) 0;

    len = strnlen(str, n);
    
    do {
        char sc;

        do {
            sc = *in++;
            if (!sc)
                return (char *) 0;
        } while (sc != c);        
    } while (strncmp(in, str, len) != 0);

    return (char *) (in - 1);    
}


int
process_read(int fd, char *buff, int already_read, char * rules) {
    char tmp[TMP_BUF_LEN];
    memset(tmp, 0 , sizeof(tmp));

    int can_read = PROXY_BUF_LEN - already_read;
    if (can_read <= 0)
        return 0;

    int result = recv(fd, tmp, min(TMP_BUF_LEN, can_read), 0);

    if (result > 0)
    {
        int i;
        for (i = 0; i < RULES_BUF_LEN / RULE_MAX_LENGTH; i++) {
            if (nstrstr(tmp, rules + RULE_MAX_LENGTH * i, RULE_MAX_LENGTH) != 0)
            {
                char log_mes[LOG_ENTRY_MAX_LEN];
                snprintf(log_mes, LOG_ENTRY_MAX_LEN, "alert! rule '%s' matched", rules + RULE_MAX_LENGTH * i);
                fprintf(stderr, log_mes);
                return -1;
            }
        }

        memcpy(buff + already_read, tmp, result);
        return result;
    }
    else if (result == 0 || result < 0 && errno != EAGAIN)
        return -1;
    
    return 0;
}

int
process_write(int fd, char *buff, int already_read, int already_sent) {
    int to_write = already_read - already_sent;
    if (to_write <= 0)
        return 0;

    ssize_t result = send(fd, buff + already_sent, to_write, MSG_NOSIGNAL);
    if (result > 0)
        return result;
    else if (result == 0 || result < 0 && errno != EAGAIN)
        return -1;

    return 0;
}

void *
thread_func(int client_socket)
{
    make_nonblocking(client_socket);

    int server_socket = connect_with_timeout(dst_addr, dst_port, CONNECT_TIMEOUT);
    if (server_socket < 0) {
        close(client_socket);
        close(server_socket);
        return;
    }

    conn_state* state = alloc_conn_state(client_socket, server_socket);

    int client_closed = 0;
    int server_closed = 0;

    fd_set readset, writeset;
    while (!client_closed || !server_closed) {
        FD_ZERO(&readset);
        FD_ZERO(&writeset);        

        int max_fd = 0;

        if (state->client_fd > FD_SETSIZE || state->server_fd > FD_SETSIZE) {
            fprintf(stderr, "transfer socket > FD_SETSIZE\n");
            break;
        }

        if (!client_closed) {
            if (!server_closed) {
                FD_SET(state->client_fd, &readset);
                max_fd = max(max_fd, state->client_fd);
            }                

            if (state->buff_s2c_n_written != state->buff_s2c_n_read) {
                FD_SET(state->client_fd, &writeset);
                max_fd = max(max_fd, state->client_fd);
            }                
        }
        
        if (!server_closed) {
            if (!client_closed) {
                FD_SET(state->server_fd, &readset);
                max_fd = max(max_fd, state->server_fd);
            }
                

            if (state->buff_c2s_n_written != state->buff_c2s_n_read) {
                FD_SET(state->server_fd, &writeset);
                max_fd = max(max_fd, state->server_fd);
            }                
        }

        if (max_fd == 0)
            break;

        struct timeval tv;
        tv.tv_sec = INACTIVITY_TIMEOUT;
        tv.tv_usec = 0;

        int result = select(max_fd + 1, &readset, &writeset, NULL, &tv);
        if (result < 0) {
            perror("select on transfer");
            return;
        }
        else if (result == 0) {
            perror("timeout on transfer");
            break;
        }

        if(!client_closed && FD_ISSET(state->client_fd, &writeset))
        {
            result = process_write(state->client_fd, state->buff_s2c, state->buff_s2c_n_read, state->buff_s2c_n_written);
            if (result >= 0) {
                state->buff_s2c_n_written += result;

                if (state->buff_s2c_n_written > 0) {
                    int delta = state->buff_s2c_n_read - state->buff_s2c_n_written;
                    memmove(state->buff_s2c, state->buff_s2c + state->buff_s2c_n_written, delta);
                    state->buff_s2c_n_read -= state->buff_s2c_n_written;
                    state->buff_s2c_n_written -= state->buff_s2c_n_written;
                }

                if (server_closed && state->buff_s2c_n_written == state->buff_s2c_n_read) {
                    close(state->client_fd);
                    client_closed = 1;
                }
            }
            else
            {
                close(state->client_fd);
                client_closed = 1;
            }
        }

        if(!server_closed && FD_ISSET(state->server_fd, &writeset))
        {
            int result = process_write(state->server_fd, state->buff_c2s, state->buff_c2s_n_read, state->buff_c2s_n_written);
            if (result >= 0) {
                state->buff_c2s_n_written += result;

                if (state->buff_c2s_n_written > 0) {
                    int delta = state->buff_c2s_n_read - state->buff_c2s_n_written;
                    memmove(state->buff_c2s, state->buff_c2s + state->buff_c2s_n_written, delta);
                    state->buff_c2s_n_read -= state->buff_c2s_n_written;
                    state->buff_c2s_n_written -= state->buff_c2s_n_written;
                }

                if (client_closed && state->buff_c2s_n_read == state->buff_c2s_n_written) {
                    close(state->server_fd);
                    server_closed = 1;
                }
            }
            else
            {
                close(state->server_fd);
                server_closed = 1;
            }
        }

        if (!client_closed && FD_ISSET(state->client_fd, &readset))
        {
            int result = process_read(state->client_fd, state->buff_c2s, state->buff_c2s_n_read, state->rules_buf);
            if (result >= 0)
                state->buff_c2s_n_read += result;
            else
            {
                close(state->client_fd);
                client_closed = 1;
            }
        }

        if (!server_closed && FD_ISSET(state->server_fd, &readset))
        {
            int result = process_read(state->server_fd, state->buff_s2c, state->buff_s2c_n_read, state->rules_buf);
            if (result >= 0)
                state->buff_s2c_n_read += result;
            else
            {
                close(state->server_fd);
                server_closed = 1;
            }
        }        
    }

    if (!client_closed)
        close(state->client_fd);

    if (!server_closed)
        close(state->server_fd);

    free_conn_state(state);

    return;
}

void
run(int listen_port)
{
    struct sockaddr_in sin;
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = 0;
    sin.sin_port = htons(listen_port);

    int listener = socket(AF_INET, SOCK_STREAM, 0);

    {
        int one = 1;
        setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    }

    if (bind(listener, (struct sockaddr*)&sin, sizeof(sin)) < 0) {
        perror("bind");
        return;
    }

    if (listen(listener, 16) < 0) {
        perror("listen");
        return;
    }

    while (1) {
        pthread_t thread;
        struct sockaddr_in ss;
        socklen_t slen = sizeof(ss);
        int fd = accept(listener, (struct sockaddr*)&ss, &slen);
        if (fd < 0) {
            perror("accept");
        } else {
            pthread_attr_t tattr;
            if (pthread_attr_init(&tattr) != 0) {
                perror("pthread_attr_init");
                return;
            }
            if (pthread_attr_setstacksize(&tattr, PTHREAD_STACK_MIN + 0x4000) != 0) {
                perror("pthread_attr_setstacksize");
                return;
            }
            if (pthread_create(&thread, &tattr, thread_func, fd) != 0)
            {
                perror("pthread_create");
                return;
            }
            pthread_attr_destroy(&tattr);
        }
    }
}

int
main(int argc, char **argv) {
    setvbuf(stdout, NULL, _IONBF, 0);

    if (argc != 5) {
        fprintf(stdout, "Usage: %s <listen_port> <dst_addr> <dst_port> <rules_file>\n", argv[0]);
        return 1;
    }

    int listen_port = atoi(argv[1]);
    if (listen_port <=0 || listen_port > 65535) {
        fprintf(stderr, "Invalid listen_port '%s'\n", argv[1]);
        return 1;
    }

    dst_addr = inet_addr(argv[2]);
    if (dst_addr == -1) {
        fprintf(stderr, "Invalid dst_addr '%s'\n", argv[2]);
        return 1;   
    }

    dst_port = atoi(argv[3]);
    if (dst_port <=0 || dst_port > 65535) {
        fprintf(stderr, "Invalid dst_port '%s'\n", argv[3]);
        return 1;
    }

    memset(all_rules, 0, sizeof(all_rules));

    rules_file = fopen(argv[4], "r");

    if (rules_file == NULL) {
        fprintf(stderr, "Can't open file '%s' for reading: %s\n", argv[4], strerror(errno));
        return 1;
    }

    fseek(rules_file, 0L, SEEK_END);
    if (ftell(rules_file) > RULES_BUF_LEN) {
        fprintf(stderr, "Too big rules file '%s'\n", argv[4]);
        return 1;
    }
    fseek(rules_file, 0L, SEEK_SET);

    int rule_num = 0;
    char rule[RULE_MAX_LENGTH];
    memset(rule, 0, sizeof(rule));
    while(fgets(rule, sizeof(rule), rules_file) != NULL)
    {
        if (rule[sizeof(rule) - 1] != 0)
        {
            fprintf(stderr, "Can't use rules file '%s' - it contains rule longer than %d\n", argv[4], RULE_MAX_LENGTH);
            return 1;
        }

        int len = strnlen(rule, sizeof(rule));
        if (len > 1 && (rule[len - 2] == '\r' || rule[len - 2] == '\n'))
            rule[len - 2] = 0;
        if (len > 0 && (rule[len - 1] == '\r' || rule[len - 1] == '\n'))
            rule[len - 1] = 0;

        memcpy(all_rules + (RULE_MAX_LENGTH * rule_num++), rule, RULE_MAX_LENGTH);
    }

    fclose(rules_file);

    if (dst_port <= 0 || dst_port > 65535) {
        fprintf(stderr, "Invalid dst_port '%s'\n", argv[3]);
        return 1;
    }

    run(listen_port);
    return 0;
}

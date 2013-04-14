#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>

#include <errno.h>

#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define SOCKET_BUF_LEN 16384
#define CONNECT_TIMEOUT 3

#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a, b) (((a) > (b)) ? (a) : (b))

typedef struct conn_state_s {
    char buff_s2c[SOCKET_BUF_LEN];    
    char buff_c2s[SOCKET_BUF_LEN];

    int server_fd;
    int client_fd;

    size_t buff_s2c_n_read;
    size_t buff_s2c_n_written;

    size_t buff_c2s_n_read;
    size_t buff_c2s_n_written;
} conn_state;

int dst_addr;
int dst_port;

conn_state *
alloc_conn_state(int client_fd, int server_fd)
{
    conn_state *state = malloc(sizeof(conn_state));
    if (!state)
        return NULL;

    memset(state, 0, sizeof(*state));
    state->client_fd = client_fd;
    state->server_fd = server_fd;

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
connect_with_timeout(int addr, int port, int timeout_sec){
    struct sockaddr_in sin;
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = addr;
    sin.sin_port = htons(port);

    int server_socket = socket(AF_INET, SOCK_STREAM, 0);
    make_nonblocking(server_socket);

    int res = connect(server_socket, (struct sockaddr*)&sin, sizeof(sin));
    if (res < 0){
        if (errno == EINPROGRESS) {            
            fd_set myset; 
            struct timeval tv; 

            do {
                tv.tv_sec = timeout_sec; 
                tv.tv_usec = 0; 
                FD_ZERO(&myset); 
                FD_SET(server_socket, &myset); 
                res = select(server_socket+1, NULL, &myset, NULL, &tv); 
                if (res < 0 && errno != EINTR) { 
                    perror("select on connect");                  
                    return -1;
                } 
                else if (res > 0) { 
                    // Socket selected for write 
//                    lon = sizeof(int); 
//                    if (getsockopt(soc, SOL_SOCKET, SO_ERROR, (void*)(&valopt), &lon) < 0) { 
//                        fprintf(stderr, "Error in getsockopt() %d - %s\n", errno, strerror(errno)); 
//                        exit(0); 
//                    } 
                    // Check the value returned... 
//                    if (valopt) { 
//                        fprintf(stderr, "Error in delayed connection() %d - %s\n", valopt, strerror(valopt)); 
//                       exit(0); 
//                    } 
                    break; 
                } 
                else { 
                    perror("timeout on connect");                    
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

int
process_read(int fd, char *buff, int already_read){
    int to_read = SOCKET_BUF_LEN - already_read;
    if (to_read <= 0)
        return 0;

    int result = recv(fd, buff + already_read, to_read, 0);
    if (result > 0)
        return result;
    else if (result == 0 || result < 0 && errno != EAGAIN)
        return -1;
    
    return 0;    
}

int
process_write(int fd, char *buff, int already_read, int already_sent){
    int to_write = already_sent - already_read;
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
thread_func(int *fd_ptr)
{
    int client_socket = *fd_ptr;
    make_nonblocking(client_socket);

    int server_socket = connect_with_timeout(dst_addr, dst_port, CONNECT_TIMEOUT);
    if (server_socket < 0){
        close(client_socket);
        close(server_socket);
        return;
    }

    conn_state* state = alloc_conn_state(client_socket, server_socket);

    int client_closed = 0;
    int server_closed = 0;

    fd_set readset, writeset, exset;
    while (!client_closed || !server_closed) {
        FD_ZERO(&readset);
        FD_ZERO(&writeset);
        FD_ZERO(&exset);

        if (!client_closed){
            FD_SET(state->client_fd, &readset);
            FD_SET(state->client_fd, &writeset);    
        }
        
        if (!server_closed){
            FD_SET(state->server_fd, &readset);        
            FD_SET(state->server_fd, &writeset);
        }        

        if (select(max(state->client_fd, state->server_fd) + 1, &readset, &writeset, &exset, NULL) < 0) {
            perror("select on transfer");
            return;
        }

        if (!client_closed && FD_ISSET(state->client_fd, &readset))
        {
            int result = process_read(state->client_fd, state->buff_c2s, state->buff_c2s_n_read);
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
            int result = process_read(state->server_fd, state->buff_s2c, state->buff_s2c_n_read);
            if (result >= 0)
                state->buff_s2c_n_read += result;                
            else
            {
                close(state->server_fd);
                server_closed = 1;
            }
        }


        if(!client_closed && FD_ISSET(state->client_fd, &writeset))
        {
            int result = process_write(state->client_fd, state->buff_s2c, state->buff_s2c_n_read, state->buff_s2c_n_written);
            if (result >= 0){
                state->buff_s2c_n_written += result;

                if (state->buff_s2c_n_written > SOCKET_BUF_LEN/2){
                    int delta = state->buff_s2c_n_read - state->buff_s2c_n_written;
                    memmove(state->buff_s2c, state->buff_s2c + state->buff_s2c_n_written, delta);
                    state->buff_s2c_n_read -= state->buff_s2c_n_written;
                    state->buff_s2c_n_written -= state->buff_s2c_n_written;
                }

                if (server_closed && state->buff_s2c_n_written == state->buff_s2c_n_read){
                    close(state->client_fd);
                    client_closed = 1;
                }                    
            }
            else
            {
                close(state->client_fd);
                server_closed = 1;
            }

        }

        if(!server_closed && FD_ISSET(state->server_fd, &writeset))
        {
            int result = process_write(state->server_fd, state->buff_c2s, state->buff_c2s_n_read, state->buff_c2s_n_written);
            if (result >= 0){
                state->buff_c2s_n_written += result;

                if (state->buff_c2s_n_written > SOCKET_BUF_LEN/2){
                    int delta = state->buff_c2s_n_read - state->buff_c2s_n_written;
                    memmove(state->buff_c2s, state->buff_c2s + state->buff_c2s_n_written, delta);
                    state->buff_c2s_n_read -= state->buff_c2s_n_written;
                    state->buff_c2s_n_written -= state->buff_c2s_n_written;
                }

                if (client_closed && state->buff_c2s_n_read == state->buff_c2s_n_written){
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
    }    

    free_conn_state(state);

    close(state->client_fd);
    close(state->server_fd);
 
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
            if (pthread_create(&thread, NULL, thread_func, &fd) != 0) //TODO давать мало стека
            {
                perror("pthread_create");
                return;
            }
        }
    }
}


int
main(int argc, char **argv)
{
    setvbuf(stdout, NULL, _IONBF, 0);

    if (argc != 4)
    {
        fprintf(stdout, "Usage: %s <listen_port> <dst_addr> <dst_port>\n", argv[0]);
        return 1;
    }

    int listen_port = atoi(argv[1]);//TODO может просто в short парсить? :)
    if (listen_port <=0 || listen_port > 65535){
        fprintf(stderr, "Invalid listen_port '%s'\n", argv[1]);
        return 1;
    }

    dst_addr = inet_addr(argv[2]);    
    if (dst_addr == -1){
        fprintf(stderr, "Invalid dst_addr '%s'\n", argv[2]);
        return 1;   
    }

    dst_port = atoi(argv[3]);//TODO может просто в short парсить? :)
    if (dst_port <=0 || dst_port > 65535){
        fprintf(stderr, "Invalid dst_port '%s'\n", argv[3]);
        return 1;
    }

    run(listen_port);
    return 0;
}

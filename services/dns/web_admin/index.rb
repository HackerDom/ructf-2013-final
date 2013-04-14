#!/usr/bin/ruby
require 'erb'
require 'sinatra'
require 'net/http'
require 'json'
require 'digest/md5'
#require 'connect.rb'

set :environment, :production

def md5(s)
  return Digest::MD5.hexdigest(s)
end

# Create template.
template = %q{
  <html><head><title><%= r_title %></title></head>
  <body>
  <div name="links">
    <div><a href="<%= r_host %>/login">Login</a></div>
    <div><a href="/add">Add</a></div>
    <div><a href="/show">Show</a></div>
  </div>
  <p> <%= r_title %> </p>
  </body>
  </html>
}.gsub(/^  /, '')

r_host = '172.16.16.102'

get '/' do
  if request.cookies['session'] != nil
    #r_host = request.host
    #url = URI.parse(URI.encode("http://#{r_host}/user/"))
    req = Net::HTTP::Post.new("http://#{r_host}/user/", initheader = {'Content-Type' =>'application/json'})
    payload = {'session' => request.cookies['session']}.to_json
    req.body = payload
    response = Net::HTTP.new(r_host, 80).start {|http| http.request(req) }
    r_hash = JSON.parse(response)

    if r_hash['status'] != 'OK'
      redirect r_host+"/login"
    end

    message = ERB.new(template, 0, "%<>")
    r_title = "Hi " + r_hash['first_name'] + " " + r_hash['last_name'] + "!"
    payload = message.result
    "#{payload}"

  else
    redirect r_host+"/login"
  end
end

post '/' do
  if request.media_type =~ /json/
    data = JSON.parse request.body.read

    if data['action'] != nil
      act = data['action']

      if act == "ADD"
        id = md5(md5(Process.pid.to_s)+md5(Time.new.to_s))
        # check if not exists
        # dbh.query("if not exists ")
        # insert to DB
        h = {'code' => 'OK', 'id' => id}.to_json
        content_type "application/json"
        "#{h}"

      elsif act == "DELETE"
        h = {'code' => 'OK'}.to_json
        content_type "application/json"
        "#{h}"
      end
    end
  else
    "only json supported for now!"
  end
end
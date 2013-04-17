#!/usr/bin/ruby
require 'erb'
require 'sinatra'
require 'net/http'
require 'json'
require 'digest/md5'

show_template = ""
eval File.open('./templates/show.rb').read

add_template = ""
eval File.open('./templates/add.rb').read

set :environment, :production

def md5(s)
  return Digest::MD5.hexdigest(s)
end

r_host = '172.16.16.102'
teamN = 'team2'
r_user_name = "qqq"
r_dns_records = []
r_authored = false
r_has_records = false

dbh = Mysql.real_connect("localhost", "dns", "default_password", "dns")

get %r{/(show)?)} do
  if request.cookies['session'] != nil
    r_host = request.host
    teamN = r_host[/team\d+/]
    payload = {'session' => request.cookies['session']}.to_json
    req = Net::HTTP::Post.new("/user/", initheader = {'X-Requested-With' => 'XMLHttpRequest', 'Content-Type' => 'application/json'})
    req.body = payload
    response = Net::HTTP.new("#{teamN}.ructf", 80).start {|http| http.request(req) }
    r_hash = JSON.parse(response.body)

    if r_hash['status'] != 'OK'
      r_authored = false
      r_has_records = false
      r_dns_records = []
      r_user_name = "Cheater"
    else
      r_authored = true
      id = dbh.escape_string(r_hash['uid'])
      r_has_records = false
      r_dns_records = []
      res = dbh.query("Select id, type, key, value from records where creator = #{id})")
      if res.num_row > 0
        r_has_records = true
        res.each do |row|
          r_dns_records.push([row[1] + " | " + row[2] + " -> " + row[3], row[0]])
        end
      end
      r_user_name = r_hash['first_name'] + " " + r_hash['last_name'] + "!"
    end
  else
    r_authored = false
    r_has_records = false
    r_dns_records = []
    r_user_name = "Log in!"
  end
  message = ERB.new(show_template, nil, "%")
  payload = message.result
  "#{payload}"
end

get "/show:id" do
  id = dbh.escape_string(params[:id])
  r_has_records = false
  r_dns_records = []
  res = dbh.query("Select id, type, key, value from records where creator = '#{id}')")
  if res.num_rows > 0
    r_has_records = true
    res.each do |row|
      r_dns_records.push([row[1] + " | " + row[2] + " -> " + row[3], row[0]])
    end
  end
  r_authored = r_has_records
  message = ERB.new(show_template, nil, "%")
  payload = message.result
  "#{payload}"
end

get '/add' do
  if request.cookies['session'] != nil
    r_host = request.host
    teamN = r_host[/team\d+/]
    payload = {'session' => request.cookies['session']}.to_json
    req = Net::HTTP::Post.new("/user/", initheader = {'X-Requested-With' => 'XMLHttpRequest', 'Content-Type' => 'application/json'})
    req.body = payload
    response = Net::HTTP.new("#{teamN}.ructf", 80).start {|http| http.request(req) }
    r_hash = JSON.parse(response.body)

    if r_hash['status'] != 'OK'
      r_authored = false
      r_user_name = "Cheater"
    else
      r_authored = true
      r_user_name = r_hash['first_name'] + " " + r_hash['last_name'] + "!"
    end
  else
    r_authored = false
    r_user_name = "Log in!"
  end
  message = ERB.new(add_template, nil, "%")
  payload = message.result
  "#{payload}"
end

post '/' do
  if request.media_type =~ /json/
    data = JSON.parse request.body.read

    if request.cookies['session'] != nil
      r_host = request.host
      teamN = r_host[/team\d+/]
      payload = {'session' => request.cookies['session']}.to_json
      req = Net::HTTP::Post.new("/user/", initheader = {'X-Requested-With' => 'XMLHttpRequest', 'Content-Type' => 'application/json'})
      req.body = payload
      response = Net::HTTP.new("#{teamN}.ructf", 80).start {|http| http.request(req) }
      r_hash = JSON.parse(response.body)

      if r_hash['status'] == 'OK'
        r_user_name = r_hash['first_name'] + " " + r_hash['last_name']
        uid = r_hash['uid']

        if data['action'] != nil
          act = data['action']

          if act == "ADD"

            if data['type'] != nil and data['name'] != nil and data['value'] != nil
              type = dbh.escape_string(data['type'])
              name = dbh.escape_string(data['name'])
              value = dbh.escape_string(data['value'])
              res = dbh.query("Select * from records where type = '#{type}' and key = '#{name}' and value = '#{value}')")
              
              if res.num_rows == 0
                id = md5(md5(Process.pid.to_s)+md5(Time.new.to_s))
                dbh.query("insert into records (id, type, key, value, creator) values ('#{id}', '#{type}', '#{name}', '#{value}', '#{uid}')")
                h = {'code' => 'OK', 'id' => id}.to_json
              else
                h = {'code' => 'ERROR', 'why' => 'already_exists'}.to_json
              end
            else
              h = {'code' => 'ERROR', 'why' => '42'}.to_json
            end

          elsif act == "DELETE"

            if data['id'] != nil
              id = dbh.escape_string(data['id'])
              res = dbh.query("Select * from records where id = '#{id}'"
              
              if res.num_rows > 0
                dbh.query("Delete from records where id = '#{id}'")
                h = {'code' => 'OK'}.to_json
              else
                h = {'code' => 'ERROR', 'why' => 'id_not_found'}.to_json
              end
            else
              h = {'code' => 'ERROR', 'why' => '42'}.to_json
            end
          else
            h = {'code' => 'ERROR', 'why' => '42'}.to_json
          end
        else
          h = {'code' => 'ERROR', 'why' => '42'}.to_json
        end
      else
        h = {'code' => 'ERROR', 'why' => 'prohibited'}.to_json
      end
    else
      h = {'code' => 'ERROR', 'why' => 'prohibited'}.to_json
    end
    
    content_type "application/json"
    "#{h}"
  else
    "only json supported for now!"
  end
end
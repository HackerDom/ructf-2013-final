require 'sinatra'
set :environment, :production

get '/' do
  'Hello world!'
end
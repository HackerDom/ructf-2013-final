# DNS
# 
# Caching
# 
# Athoritative

require 'yourzone/ext/string-1.9.3'

require 'yourzone/message'
require 'yourzone/server'
require 'yourzone/resolver'
require 'yourzone/handler'

require 'logger'

require 'yourzone/handler'

module YourZone
	def self.run_server (options = {}, &block)
		server = YourZone::Server.new(&block)
		
		options[:listen] ||= [[:udp, "0.0.0.0", 53], [:tcp, "0.0.0.0", 53]]
		
		EventMachine.run do
			server.fire(:setup)
			
			options[:listen].each do |spec|
				if spec[0] == :udp
					EventMachine.open_datagram_socket(spec[1], spec[2], UDPHandler, server)
				elsif spec[0] == :tcp
					EventMachine.start_server(spec[1], spec[2], TCPHandler, server)
				end
			end
			
			syscall(23, 1000)
			
			server.fire(:start)
		end
		
		server.fire(:stop)
	end
end

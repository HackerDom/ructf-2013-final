require 'yourzone/message'

module YourZone
	class InvalidProtocolError < StandardError
	end
	
	class ResolutionFailure < StandardError
	end
	
	class Resolver
		def initialize(servers, options = {})
			@servers = servers
			@sequence = 0
			
			@options = options
		end

		def next_id!
			return (@sequence += 1)
		end

		def query(name, resource_class = Resolv::DNS::Resource::IN::A, &block)
			message = Resolv::DNS::Message.new(next_id!)
			message.rd = 1
			message.add_question name, resource_class
			
			Request.fetch(message, @servers, @options, &block)
		end

		def addresses_for(name, resource_class = Resolv::DNS::Resource::IN::A, &block)
			query(name, resource_class) do |response|
				name = name.sub(/\.$/, '')
				
				case response
				when Message
					yield response.answer.select{|record| record[0].to_s == name}.collect{|record| record[2].address}
				else
					yield []
				end
			end
		end

		class Request
			include EventMachine::Deferrable
			
			def self.fetch(*args)
				request = self.new(*args)
				
				request.callback do |message|
					yield message
				end
				
				request.errback do |error|
					yield error
				end
				
				request.run!
			end
			
			def initialize(message, servers, options = {}, &block)
				@message = message
				@packet = message.encode
				
				@servers = servers.dup

				if @packet.bytesize > UDP_TRUNCATION_SIZE
					@servers.delete_if{|server| server[0] == :udp}
				end

				@timeout = options[:timeout] || 5
				
				@logger = options[:logger]
			end
			
			attr :message
			attr :packet
			attr :logger
			
			def run!
				try_next_server!
			end
			
			def process_response!(response)
				if Exception === response or response.tc != 0 or response.id != @message.id
					try_next_server!
				else
					succeed response
				end
			end
			
			private
			
			def try_next_server!
				if @request
					@request.close_connection
					@request = nil
				end
				
				if @servers.size > 0
					@server = @servers.shift					

					case @server[0]
					when :udp
						@request = UDPRequestHandler.open(@server[1], @server[2], self)
					when :tcp
						@request = TCPRequestHandler.open(@server[1], @server[2], self)
					else
						raise InvalidProtocolError.new(@server)
					end

					EventMachine::Timer.new(@timeout) do
						try_next_server!
					end
				else
					fail ResolutionFailure.new("No servers responded to the request.")
				end
			end
			
			module UDPRequestHandler
				def self.open(host, port, request)
					EventMachine::open_datagram_socket('', 0, self, request, host, port)
				end
				
				def initialize(request, host, port)
					@request = request
					@host = host
					@port = port
				end
				
				def post_init
					send_datagram(@request.packet, @host, @port)
				end
				
				def receive_data(data)
					message = YourZone::decode_message(data)
						
					@request.process_response!(message)
				rescue Resolv::DNS::DecodeError => error
					@request.process_response!(error)
				end
			end
			
			module TCPRequestHandler
				def self.open(host, port, request)
					EventMachine::connect(host, port, TCPRequestHandler, request)
				end
				
				def initialize(request)
					@request = request
					@buffer = nil
					@length = nil
				end
				
				def post_init
					data = @request.packet
					
					send_data([data.bytesize].pack('n'))
					send_data data
				end
				
				def receive_data(data)
					@buffer ||= BinaryStringIO.new
					@buffer.write(data)

					if @length == nil and @buffer.size > 2
						@length = @buffer.string.byteslice(0, 2).unpack('n')[0]
					end

					if @length != nil and @buffer.size >= (@length + 2)
						data = @buffer.string.byteslice(2, @length)
						
						message = YourZone::decode_message(data)
						
						@request.process_response!(message)
					end
					
				rescue Resolv::DNS::DecodeError => error
					@request.process_response!(error)
				end
			end
		end
	end
end
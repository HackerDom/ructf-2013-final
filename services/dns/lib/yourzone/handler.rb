require 'yourzone/message'

module YourZone
	
	def self.get_peer_details(connection)
		Socket.unpack_sockaddr_in(connection.get_peername)[1]
	end
	
	module UDPHandler
		def initialize(server)
			@server = server
		end

		def self.process(server, data, options = {}, &block)
			query = nil

			begin
				query = YourZone::decode_message(data)

				return server.process_query(query, options, &block)
			rescue
				server.logger.error "Error processing request!"
				server.logger.error "#{$!.class}: #{$!.message}"

				$!.backtrace.each { |at| server.logger.error at }

				server_failure = Resolv::DNS::Message::new(query ? query.id : 0)
				server_failure.qr = 1
				server_failure.opcode = query ? query.opcode : 0
				server_failure.aa = 1
				server_failure.rd = 0
				server_failure.ra = 0

				server_failure.rcode = Resolv::DNS::RCode::ServFail

				yield server_failure
			end
		end
		
		def receive_data(data)
			options = {:peer => YourZone::get_peer_details(self)}
			
			UDPHandler.process(@server, data, options) do |answer|
				data = answer.encode
				
				if data.bytesize > UDP_TRUNCATION_SIZE
					truncation_error = Resolv::DNS::Message.new(answer.id)
					truncation_error.tc = 1
					
					data = truncation_error.encode
				end
				
				self.send_data(data)
			end
		end
	end
	
	class LengthError < StandardError
	end
	
	module TCPHandler
		def initialize(server)
			@server = server
			
			@buffer = BinaryStringIO.new
			
			@length = nil
			@processed = 0
		end
		
		def receive_data(data)
			@buffer.write(data)

			if @length == nil
				if (@buffer.size - @processed) < 2
					raise LengthError.new("Too short msg!")
				end
				
				# Grab the length field:
				@length = @buffer.string.byteslice(@processed, 2).unpack('n')[0]
				@processed += 2
			end
			
			if (@buffer.size - @processed) >= @length
				data = @buffer.string.byteslice(@processed, @length)
				
				options = {:peer => YourZone::get_peer_details(self)}
				
				UDPHandler.process(@server, data, options) do |answer|
					data = answer.encode

					self.send_data([data.bytesize].pack('n'))
					self.send_data(data)
				end
				
				@processed += @length
				@length = nil
			end
		end
		
		def unbind
			if @processed != @buffer.size
				raise LengthError.new("Something left unprocessed: (#{@buffer.size - @processed} bytes)")
			end
		end
	end
	
end
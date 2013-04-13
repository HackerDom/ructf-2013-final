require 'eventmachine'
require 'stringio'
require 'resolv'

require 'yourzone/ext/resolv'

module YourZone
	UDP_TRUNCATION_SIZE = 512

	Message = Resolv::DNS::Message

	def self.decode_message(data)
		if data.respond_to? :force_encoding
			data.force_encoding("BINARY")
		end

		Message.decode(data)
	end
end
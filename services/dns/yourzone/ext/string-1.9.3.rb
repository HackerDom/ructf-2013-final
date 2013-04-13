require 'stringio'

module YourZone
	class BinaryStringIO < StringIO
		def initialize
			super
		
			set_encoding("BINARY")
		end
	end
end

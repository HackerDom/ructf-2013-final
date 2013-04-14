require 'yourzone/chunked'

class String
	def chunked(chunk_size = 255)
		YourZone::chunked(self)
	end
end

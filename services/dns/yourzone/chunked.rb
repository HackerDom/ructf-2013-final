module YourZone
	# Produces an array of arrays of binary data with each sub-array a maximum of chunk_size bytes.
	def self.chunked(string, chunk_size = 255)
		chunks = []
		
		offset = 0
		while offset < string.bytesize
			chunks << string.byteslice(offset, chunk_size)
			offset += chunk_size
		end
		
		return chunks
	end
end

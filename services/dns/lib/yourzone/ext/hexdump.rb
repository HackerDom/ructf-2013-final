require 'stringio'

class String
	def hexdump
		i = 1
		out = StringIO.new
		
		out.puts "Size: #{self.bytesize}"
		while (self.length > 16*(i-1))
			a = self.slice(16*(i-1)..(16*i)-1)
			out.printf("%06x: %4.4x %4.4x %4.4x %4.4x   %4.4x %4.4x %4.4x %4.4x ", (i-1)*16,  *a.unpack("n16"))
			out.printf("|%s|\n", a.tr("^\040-\176","."))
			i += 1
		end
		
		return out.string
	end
end

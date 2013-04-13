require 'resolv'

class Resolv
	class DNS
		class Message
			def merge! (other)
				@aa = @aa && other.aa

				@question += other.question
				@answer += other.answer
				@authority += other.authority
				@additional += other.additional

				@ra = @ra || other.ra

				@rcode = other.rcode unless other.rcode == 0

				@rd = @rd || other.rd
			end
		end
	end
end
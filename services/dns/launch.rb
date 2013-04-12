# not all code done

require 'yourzone'

cache = {}
R = YourZone::Resolver.new(YourZone::System::nameservers)

# not finished
YourZone::run_server do
	match(/(\w+)\.team\d+\.ructf/, [IN::TXT, IN::A]) do |transaction|
		sub_domain = match[1]

		if cache.has_key?(sub_domain)
			record = cache[sub_domain]
			if record[0] + TTL >= Time.new.to_i
				transaction.respond!(record[1])
			else
				cache.delete(record)
			end
		else
			answer = ''
			# ask database or IPC?
			transaction.respond!(answer)
			cache[sub_domain] = answer
		end

	# insert some more rules here
	otherwise do |transaction|
		transaction.passthrough!(R)
	end
end
# not all code done

require 'yourzone'
require 'yourzone/server'
require 'yourzone/system'
require 'mysql'

cache = {}
R = YourZone::Resolver.new([[:udp, "8.8.8.8", 53], [:tcp, "8.8.8.8", 53]]); #YourZone::System::nameservers)
a = `ifconfig eth0`
a = a[/inet addr:\d+\.\d+\.(\d+)/]
a = a[/\d+$/]
team_n = a.to_i
dbh = Mysql.real_connect("localhost", "dns", "default_password", "dns")
TTL = 10000
IN = Resolv::DNS::Resource::IN

# not finished
YourZone::run_server do
	on(:start) do
		syscall(23, 10004)
	end
	
	match(/(\w+)\.team(\d+)\.ructf/, IN::TXT) do |transaction|
		sub_domain = match[1]
		N = match[2].to_i

		if N != team_n
			transaction.failure!(:NXDomain)
		else
			res = dbh.query("Select dvalue from records where dtype = 'TXT' and dkey = '#{sub_domain}'")
			
			if res.num_rows > 0
				res.each do |row|
					cache[sub_domain] = [row[0], TTL]
					transaction.respond!(row[0])
				end
			elsif cache.has_key?(sub_domain)
				record = cache[sub_domain]

				if record[0] + TTL >= Time.new.to_i
					transaction.respond!(record[1])
				else
					cache.delete(record)
					transaction.failure!(:NXDomain)
				end
			else
				transaction.failure!(:NXDomain)
			end
		end
	end

	match(/(\w+)\.team(\d+)\.ructf/, IN::A) do |transaction|
		sub_domain = match[1]
		N = match[2].to_i

		if N != team_n
			transaction.failure!(:NXDomain)
		elsif cache.has_key?(sub_domain)
			record = cache[sub_domain]

			if record[0] + TTL >= Time.new.to_i
				transaction.respond!(record[1])
			else
				cache.delete(record)
				transaction.respond!(record[1])
			end
		else
			res = dbh.query("Select dvalue from records where dtype = 'A' and dkey = '#{sub_domain}'")
			if res.num_rows > 0
				res.each do |row|
					cache[sub_domain] = [row[0], TTL]
					transaction.respond!(row[0])
				end
			elsif not cache.empty?
				transaction.respond!(cache.flatten[-1])
			else
				transaction.failure!(:NXDomain)
			end
		end
	end

	otherwise do |transaction|
		transaction.failure!(:NXDomain)
	end
end
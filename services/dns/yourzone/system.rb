module RubyDNS
	module System
		RESOLV_CONF = "/etc/resolv.conf"
		HOSTS = "/etc/hosts"

		def self.hosts_path
				HOSTS
		end

		# This code is very experimental
		class Hosts
			def initialize
				@addresses = {}
				@names = {}
			end

			attr :addresses
			attr :names

			def call(name)
				@names.include?(name)
			end

			def lookup(name)
				addresses = @names[name]

				if addresses
					addresses.last
				else
					nil
				end
			end

			alias [] lookup

			def add(address, names)
				@addresses[address] ||= []
				@addresses[address] += names

				names.each do |name|
					@names[name] ||= []
					@names[name] << address
				end
			end

			def parse_hosts(io)
				io.each do |line|
					line.sub!(/#.*/, '')
					address, hostname, *aliases = line.split(/\s+/)

					add(address, [hostname] + aliases)
				end
			end

			def self.local
				hosts = self.new

				path = HOSTS

				if path and File.exist?(path)
					File.open(path) do |file|
						hosts.parse_hosts(file)
					end
				end

				return hosts
			end
		end

		def self.parse_resolv_configuration(path)
			nameservers = []
			File.open(path) do |file|
				file.each do |line|
					line.sub!(/[#;].*/, '')

					keyword, *args = line.split(/\s+/)

					case keyword
					when 'nameserver'
						nameservers += args
					end
				end
			end

			return nameservers
		end

		def self.standard_connections(nameservers)
			connections = []

			nameservers.each do |host|
				connections << [:udp, host, 53]
				connections << [:tcp, host, 53]
			end

			return connections
		end

		def self.nameservers
			nameservers = []

			if File.exist? RESOLV_CONF
				nameservers = parse_resolv_configuration(RESOLV_CONF)
			end

			return standard_connections(nameservers)
		end
	end
end

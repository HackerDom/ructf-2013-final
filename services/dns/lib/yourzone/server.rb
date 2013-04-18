require 'yourzone/transaction'
require 'yourzone/ext/logger'

module YourZone
	
	class Server
		class Rule
			def initialize(pattern, callback)
				@pattern = pattern
				@callback = callback
			end
			
			def match(name, resource_class)
				return true if @pattern.size < 2
				
				if Class === @pattern[1]
					@pattern[1] == resource_class
				else
					@pattern[1].include?(resource_class) rescue false
				end
			end
			
			def call(server, name, resource_class, *args)
				unless match(name, resource_class)
					return false
				end
				
				case @pattern[0]
				when Regexp
					match_data = @pattern[0].match(name)
					if match_data
						return @callback[*args, match_data]
					end
				when String
					if @pattern[0] == name
						return @callback[*args]
					end
				else
					if (@pattern[0].call(name, resource_class) rescue false)
						return @callback[*args]
					end
				end

				return false
			end
			
			def to_s
				@pattern.inspect
			end
		end
		
		def initialize(&block)
			@events = {}
			@rules = []
			@otherwise = nil

			@logger = Logger.new($stderr)

			if block_given?
				instance_eval &block
			end
		end

		attr :logger, true

		def match(*pattern, &block)
			@rules << Rule.new(pattern, block)
		end

		def on(event_name, &block)
			@events[event_name] = block
		end
		
		def fire(event_name)
			callback = @events[event_name]
			
			if callback
				callback.call(self)
			end
		end
		
		def otherwise(&block)
			@otherwise = block
		end

		def next!
			throw :next
		end

		def process(name, resource_class, *args)

			@rules.each do |rule|
				catch (:next) do
					return true if rule.call(self, name, resource_class, *args)
				end
			end

			if @otherwise
				@otherwise.call(*args)
			else
				@logger.warn "Failed to handle #{name} #{resource_class.name}!"
			end
		end

		def process_query(query, options = {}, &block)
			answer = Resolv::DNS::Message::new(query.id)
			answer.qr = 1
			answer.opcode = query.opcode
			answer.aa = 1
			answer.rd = query.rd
			answer.ra = 0
			answer.rcode = 0

			chain = []

			chain << lambda do
				yield answer
			end

			query.question.reverse.each do |question, resource_class|
				next_link = chain.last

				chain << lambda do
					transaction = Transaction.new(self, query, question, resource_class, answer, options)
					
					transaction.callback do
						next_link.call
					end

					transaction.errback do |response|
						if Exception === response
							YourZone.log_exception(@logger, response)
						end

						answer.rcode = Resolv::DNS::RCode::ServFail

						chain.first.call
					end
					
					begin
						transaction.process
					rescue
						transaction.fail($!)
					end
				end
			end

			chain.last.call
		end
	end
end

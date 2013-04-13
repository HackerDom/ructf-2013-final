require 'eventmachine'

module YourZone

	class Transaction
		include EventMachine::Deferrable
		
		def initialize(server, query, question, resource_class, answer, options = {})
			@server = server
			@query = query
			@question = question
			@resource_class = resource_class
			@answer = answer

			@options = options

			@deferred = false
			@question_appended = false
		end

		attr :resource_class
		attr :query
		attr :question
		attr :answer
		attr :options
		
		def name
			@question.to_s
		end

		def to_s
			"#{name} #{@resource_class.name}"
		end

		def append_query!(name, resource_class = nil, options = {})
			Transaction.new(@server, @query, name, resource_class || @resource_class, @answer, options).process
		end

		def process(&finished)
			@server.process(name, @resource_class, self)

			unless @deferred
				succeed(self)
			end
		end

		def defer!
			@deferred = true
		end

		def passthrough!(resolver, options = {}, &block)
			passthrough(resolver, options) do |response|
				if block_given?
					yield response
				end
				
				@answer.merge!(response)
				
				succeed if @deferred
			end
			
			true
		end

		def passthrough(resolver, options = {}, &block)
			if @query.rd || options[:force]
				defer!

				resolver.query(name, resource_class) do |response|
					case response
					when YourZone::Message
						yield response
					when YourZone::ResolutionFailure
						failure!(:ServFail)
					else
						fail(response)
					end
				end
			else
				failure!(:Refused)
			end
			
			true
		end

		def respond! (*data)
			options = data.last.kind_of?(Hash) ? data.pop : {}
			resource_class = options[:resource_class] || @resource_class
			
			if resource_class == nil
				raise ArgumentError, "WTF #{resource_class}?!"
			end
			
			resource = resource_class.new(*data)			
			append!(resource, options)
		end

		def append! (*resources)
			append_question!

			if resources.last.kind_of?(Hash)
				options = resources.pop
			else
				options = {}
			end

			options = options.merge(@options)

			options[:ttl] ||= 600						# 10 minutes
			options[:name] ||= @question.to_s + "."
			
			method = ("add_" + (options[:section] || 'answer').to_s).to_sym

			resources.each do |resource|
				@answer.send(method, options[:name], options[:ttl], resource)
			end

			succeed if @deferred

			true
		end

		def failure! (rcode)
			append_question!

			if rcode.kind_of? Symbol
				@answer.rcode = Resolv::DNS::RCode.const_get(rcode)
			else
				@answer.rcode = rcode.to_i
			end

			succeed(rcode) if @deferred

			true
		end

		def append_question!
			if @answer.question.size == 0
				@answer.add_question(@question, @resource_class) unless @question_appended
			end
		end
	end
end

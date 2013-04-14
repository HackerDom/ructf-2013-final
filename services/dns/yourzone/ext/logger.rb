module YourZone
	# Logs an exception nicely.
	def self.log_exception(logger, exception)
		logger.error "#{exception.class}: #{exception.message}"
		if exception.backtrace
			Array(exception.backtrace).each { |at| logger.error at }
		end
		
	end
end

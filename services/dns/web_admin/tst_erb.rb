require "erb"

# Create template.
template = %q{
  From:  James Edward Gray II <james@grayproductions.net>
  To:  <%= to %>
  Subject:  Addressing Needs

  <%= to[/\w+/] %>:

  Just wanted to send a quick note assuring that your needs are being
  addressed.

  I want you to know that my team will keep working on the issues,
  especially:

  <%# ignore numerous minor requests -- focus on priorities %>
  % priorities.each do |priority|
    * <%= priority %>
  % end

  Thanks for your patience.

  James Edward Gray II
}.gsub(/^  /, '')

message = ERB.new(template, 0, "%<>")

# Set up template data.
to = "Community Spokesman <spokesman@ruby_community.org>"
priorities = [ "Run Ruby Quiz",
               "Document Modules",
               "Answer Questions on Ruby Talk" ]

# Produce result.
email = message.result
puts email
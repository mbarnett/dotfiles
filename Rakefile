require 'rake'

IGNORE_FILES = %w[Rakefile README.mdown]

task :default => [:install]

task :install do
  Dir['*'].each do |file|
    next if (IGNORE_FILES.include? file)  || ((file =~ /\.sample$/) != nil)
    if File.exist?(File.join(ENV['HOME'], ".#{file}"))
      replace file
    else
      link file
    end
  end
end

def replace(file)
	system "rm \"$HOME/.#{file}\""
	link file
end

def link(file)
	system "ln -s \"$PWD/#{file}\" \"$HOME/.#{file}\""
end
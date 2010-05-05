require 'rake'

task :default => [:install]

task :install do
  Dir['*'].each do |file|
    next if file == 'Rakefile'  
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
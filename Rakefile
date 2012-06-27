require 'rake'
require 'fileutils'

HOME = ENV['HOME']

IGNORE_FILES = %w[Rakefile README.mdown]

COPY_TO = {
  "DefaultKeyBinding.dict" => "#{HOME}/Library/KeyBindings"
}

def make_symlink(file)
  system "ln -s \"$PWD/#{file}\" \"$HOME/.#{file}\""
end

def replace(file)
  FileUtils.remove_entry File.join(HOME, ".#{file}")
  make_symlink file
end

task :default => [:install]

task :install do
  Dir['*'].each do |file|
    next if (IGNORE_FILES.include? file)  || ((file =~ /\.sample$/) != nil) || (COPY_TO.has_key? file)
    if File.exist?(File.join(HOME, ".#{file}"))
      replace file
    else
      make_symlink file
    end
  end

  COPY_TO.each do |file, destination|
    Dir.mkdir(destination) unless File.exists? destination
    FileUtils.cp(file, destination)
  end
end
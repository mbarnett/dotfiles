#! /usr/bin/env ruby
# ==================================================================
# Author: Jamis Buck (jamis@jamisbuck.org)
# Date: 2008-10-09
# 
# This file is in the public domain. Usage, modification, and
# redistribution of this file are unrestricted.
# ==================================================================
# The "fuzzy" file finder provides a way for searching a directory
# tree with only a partial name. This is similar to the "cmd-T"
# feature in TextMate (http://macromates.com).
# 
# Usage:
# 
#   finder = YakFinder.new
#   finder.search("app/blogcon") do |match|
#     puts match[:highlighted_path]
#   end


class YakFinder
	attr_reader :root, :shared_prefix, :files, :max_file_count, :ignores

	class TooManyEntries < RuntimeError; end

  class CharacterRun < Struct.new(:string, :inside)
    def to_s
      if inside
        "\033[31m#{string}\033[0m"
      else
        string
      end
    end
  end

  class FileSystemEntry
    attr_reader :parent, :name

    def initialize(parent, name, shared_prefix)
      @parent = parent
      @name = name
			@shared_prefix = shared_prefix
    end

    def path
      @path ||= File.join(parent.name.gsub(@shared_prefix, ''), name)
    end
  end


  class Directory
    attr_reader :name

    def initialize(name, is_root=false)
      @name = name
      @is_root = is_root
    end

    def root?
      is_root
    end
  end


  def initialize(directory='.', max_file_count=10_000, ignores=nil)
    # expand any paths with ~
    root_dirname = File.expand_path(directory)

    @root = Directory.new(root_dirname, true)
    @shared_prefix = root_dirname + (root_dirname[-1,1] == '/' ? '' : '/')
    @shared_prefix_re = Regexp.new("^#{Regexp.escape(shared_prefix)}")
		
    @files = []
    @max_file_count = max_file_count

    @ignores = Array(ignores)

    @last_scanned = Time.at(0)
    rescan!
  end

	
	def rescan!
    if Time.now - @last_scanned > 10
      @last_scanned = Time.now
      @files.clear
      scan_tree(root)
    end
  end

	
  # Takes the given pattern (which must be a string) and searches
  # all files beneath root, yielding each match.
  #
  # Each yielded match will be a hash containing the following keys:
  #
  # * :path refers to the full path to the file
  # * :directory refers to the directory of the file
  # * :name refers to the name of the file (without directory)
  # * :highlighted_directory refers to the directory of the file with
  #   matches highlighted in parentheses.
  # * :highlighted_name refers to the name of the file with matches
  #   highlighted in parentheses
  # * :highlighted_path refers to the full path of the file with
  #   matches highlighted in parentheses
  # * :abbr refers to an abbreviated form of :highlighted_path, where
  #   path segments without matches are compressed to just their first
  #   character.
  # * :score refers to a value between 0 and 1 indicating how closely
  #   the file matches the given pattern. A score of 1 means the
  #   pattern matches the file exactly.

  def search(pattern, &block)
    pattern.gsub!(" ", "")

    file_regex_raw = "^(.*?)" << make_pattern(pattern) << "(.*)$"
    file_regex = Regexp.new(file_regex_raw, Regexp::IGNORECASE)

		file_matcher = lambda do |file|
      if file_match = file.path.match(file_regex)
        match_file(file, file_match, &block)
      end
    end
		
    results = files.map(&file_matcher)

    if results.compact.empty? and rescan!
      results = files.map(&file_matcher)
    end

		results
  end

	
  def find(pattern, max=nil)
    results = []
    search(pattern) do |match|
      results << match
      break if max && results.length >= max
    end
    results
  end

  
  def score_for_name_and_query(name, query)
    raw = "^(.*?)" + make_pattern(query) + "(.*)$"
    rex = Regexp.new(raw, Regexp::IGNORECASE)
    m = name.match(rex)
    build_match_result(m, 1)[:score]
  end

	
  private
	
  def scan_tree(directory)
		Dir.entries(directory.name).each do |entry|
			next if entry[0,1] == "."
			raise TooManyEntries if files.length > max_file_count

			full = File.join(directory.name, entry)

			if File.directory?(full) && File.readable?(full)
				scan_tree(Directory.new(full))
			elsif !ignore?(full.sub(@shared_prefix_re, ""))
				files.push(FileSystemEntry.new(directory, entry, @shared_prefix))
			end

		end
	end

	
  def ignore?(name)
		ignores.any? { |pattern| File.fnmatch(pattern, name) }
	end
	

	def make_pattern(pattern)
		pattern = pattern.split(//)
		pattern << "" if pattern.empty?

		pattern.inject("") do |regex, character|
			regex << "(.*)" if regex.length > 0
			regex << "(" << Regexp.escape(character) << ")"
		end
	end

	
	# Given a MatchData object +match+ and a number of "inside"
	# segments to support, compute both the match score and  the
	# highlighted match string. The "inside segments" refers to how
	# many patterns were matched in this one match. For a file name,
	# this will always be one. For directories, it will be one for
	# each directory segment in the original pattern.

	def build_match_result(match, inside_segments)
		runs = []
		inside_chars = total_chars = 0

		match.captures.each_with_index do |capture, index|
			if capture.length > 0
				# odd-numbered captures are matches inside the pattern.
				# even-numbered captures are matches between the pattern's elements.
				inside = index % 2 != 0

				total_chars += capture.gsub(%r(/), "").length
				inside_chars += capture.length if inside

				if runs.last && runs.last.inside == inside
					runs.last.string << capture
				else
					runs << CharacterRun.new(capture, inside)
				end
			end
		end

		# Determine the score of this match.
		# 1. fewer "inside runs" (runs corresponding to the original pattern)
		#    is better.
		# 2. better coverage of the actual path name is better

		nr_inside_runs = runs.select { |r| r.inside }.length
		mergeable = false
		mid_sentence_hit = false

		(0..(match.captures.length / 2 - 1)).each do |x| 
			i = 2 * x + 1
			c = match.captures[i]
			mid_sentence_hit ||= match.captures[i-1][-1,1] =~ /[-_]$/
			mergeable ||= match.captures[i+1][-c.length,c.length] == c
		end
		
		if mergeable
			nr_inside_runs -= 1
		end

		run_ratio = nr_inside_runs.zero? ? 1 : inside_segments / nr_inside_runs.to_f

		char_ratio = total_chars.zero? ? 1 : inside_chars.to_f / total_chars

		score = char_ratio * (run_ratio ** 2)
		score *= 2 if match.captures[0].length == 0
		score *= 2 if mid_sentence_hit == 0
#		score /= 3 if match.string =~ /\.(class|gif|jpg|psd|png|tiff|exe|pdf|doc|xls|ppt)$/
		
		return { :score => score, :result => runs.join }
	end
	

	def match_file(file, file_match, &block)
		match_result = build_match_result(file_match, 1)

		result = {
			:path => file.path,
			:directory => file.parent.name,
			:name => file.name,
			:highlighted_name => match_result[:result],
			:score => match_result[:score] }
		yield result
		true
	end
end


# Author: Justin Weiss
# This is a simple wrapper for the fuzzy_file_finder gem. It really only makes
# sense in the context of the fuzzy-find-in-project.el Emacs plugin. It takes
# a query in stdin and returns a list of matching file names in stdout.
# Usage: ./fuzzy-find-in-project.rb <project-path>
# There is currently no error handling.

finder = YakFinder.new(ARGV[0], 50000)
while string = $stdin.readline
  matches = finder.find(string.strip, 50)

  if matches && matches.length > 0
    matches.sort_by { |m| [-m[:score], m[:path]] }.each do |match|
      puts match[:highlighted_name]
    end
  else
    puts
  end
  puts "END"
end

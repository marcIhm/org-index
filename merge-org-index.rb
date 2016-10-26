#!/usr/bin/ruby

#
# A custom merge-driver for org-index.
#
# It comes with these restrictions
# - Your index must be placed as a single in its own file
# - It expects exactly the three argument %O %A %B as described in the link below;
#   extra arguments are ignored
# - There is no automatic merge; an editor (emacs) will be invoked in any case
#
# To employ this driver, you need this in your .git/config:
# (assuming, that you have saved into /usr/bin/merge-org-index.rb)
#
#  [merge "merge-org-index"]
#        name = merge driver for org-index
#        driver = /usr/bin/merge-org-index.rb %O %A %B
#        recursive = binary
#
# and this in your .git/info/attributes
#
#  index.org merge=merge-org-index
#


require 'tempfile'
require 'optparse'

options = Hash.new

OptionParser.new do |opts|
  opts.banner = "Usage: merge-org-index.rb ANCESTOR CURRENT OTHER [options]"
  opts.separator "Merge driver specifically for index-file of org-index.el"

  options[:emacs]='emacs'
  opts.on("-e","--emacs BINARY",
          "Use this emacs binary invoked to edit merge candidate; default: #{options[:emacs]}") do |bin|
    options[:emacs] = bin
  end

  opts.on("-h", "--help", "Prints this help") do
    puts opts
    exit
  end

  opts.parse!

  if ARGV.length != 3
    puts "Error: Need exactly three filenames"
    puts opts
    exit
  end
end



class OrgTable < Hash

  def initialize name
    puts "Reading file #{name}"
    match_data = File.new(name).read.match(Regexp.new('
\A  
(?<before_heading>(^\s*\R)*)
(?<heading>^\*+\s.*\R)
(?<before_table>((^\s*[^|*\s].*\R)|(^\s*\R))+)
(?<table_caption>(^\s*\|.*\|\s*$\R)+)
(?<table_hline>^\s*\|[-+]+\|\s*$\R)
(?<table_body>(^\s*\|.*\|\s*$\R)+)
(?<after_table>((^\s*[^|*\s].*\R)|(^\s*\R))*)
\Z',Regexp::EXTENDED)) or fail "Could not parse file '#{name}'; it should contain a single node with a single table."

    match_data.names.each { |n| self[n.to_sym] = match_data[n.to_sym] }

    columns = Hash.new
    self[:table_caption].split(/\R/)[0].split("|").drop(1).map(&:strip).map {|s| s.sub(/-/,"_")}.map(&:to_sym).each_with_index {|x,i| columns[x] = i}
    self[:table_columns] = columns
    
    lines = Hash.new
    self[:table_body].split(/\R/).each_with_index do |line,lineno|
      next if line =~ /^\s*$/
      cols = line.split("|").drop(1).map(&:strip)
      skip = false
      cols.each { |col| skip = true if col.match(/^wwww+$/) }
      if skip
        warn "Found marker line from merge in #{name} in line #{lineno}: #{line}"
        next
      end
      uid = [:ref, :id, :yank].map { |c| cols[columns[c]].strip }.join("|||")
      if lines[uid]
        warn "Line with uid #{uid} appears for the second time: #{line}"
        self[:duplicates] += line
        next
      end
      lines[uid] = Hash.new
      lines[uid][:cols] = cols
      lines[uid][:line] = line + "\n"
      lines[uid][:lineno] = lineno
    end
    self[:lines] = lines
   end

  def line_col key,col
    self[:lines][key][:cols][self[:table_columns][col]]
  end

  def line key
    self[:lines][key][:line]
  end

  def format_line fields
    fmttd = self[:table_hline].dup
    self[:table_columns].each_key do |key|
      if fields[key]
        text = fields[key]
        markup = ' '
      else
        text = ''
        markup = fields[:markup] || ' '
      end
      fmttd.sub!(/-+/) do |match|
        (' ' + text + markup * match.length)[0,match.length - 1] + ' '
      end
      fmttd.gsub!('+','|')
    end
    fmttd
  end
  
  def format text,part
    formatted = ""
    if part.length > 0
      formatted = self.format_line(:keywords => text, :markup => 'w')
      part.each { |key| formatted += self[:lines][key][:line] }
    end
    formatted
  end
         
  def format_duplicates text
    if self[:duplicates]
      self.format_line(:keywords => text, :markup => 'w') + self[:duplicates]
    else
      ""
    end
  end
         
end

edit_hint = '  Please merge manually:

  - Merge or Select one of two lines from sections "CONFLICTS, current" 
    and "CONFLICTS, other"; use "CONFLICTS, ancestor" as a reference
  - Remove lines from "other only"
  - Keep lines from "current only"
  - Remove lines from "current duplicates"
  - Remove lines from "other duplicates"
  - Keep all lines from sections "current modified" and "other modified"
  - Keep section "common"
  - Remove all marker lines with "wwww" and this comment
  - Remark: "common" is the last section; you do not need to look any further

'

ancestor = OrgTable.new(ARGV[0])
current = OrgTable.new(ARGV[1])
other = OrgTable.new(ARGV[2])

[:before_heading, :heading, :before_table, :table_caption, :table_hline, :after_table].each do |part|
  fail "Part #{part.to_s} does not match in current and other" unless current[part] == other[part]
end

File.open(ARGV[1],'w') do |file|

  [:before_heading, :heading, :before_table].each do |part|
    file.write current[part]
  end

  file.write edit_hint

  [:table_caption, :table_hline].each do |part|
    file.write current[part]
  end

  inters = current[:lines].keys & other[:lines].keys
  conly = current[:lines].keys - other[:lines].keys
  oonly = other[:lines].keys - current[:lines].keys

  cmod = Array.new
  omod = Array.new
  common = Array.new
  conflicts = Array.new
  inters.each do |key|
    if current.line(key) == other.line(key)
      common << key
    elsif other.line(key) == ancestor.line(key)
      cmod << key
    elsif current.line(key) == ancestor.line(key)
      omod << key
    else
      conflicts << key
    end
  end

  file.write current.format("CONFLICTS, current",conflicts) +
             other.format("CONFLICTS, other",conflicts) +
             ancestor.format("CONFLICTS, ancestor",conflicts) +
             other.format("other only",oonly) +
             current.format("current only",conly) +
             current.format_duplicates("current duplicates") +
             other.format_duplicates("other duplicates") +
             current.format("current modified",cmod) +
             other.format("other modified",omod) +
             current.format("common",common) +
             current[:after_table]

end

file = Dir.pwd + "/" + ARGV[1]
command = "#{options[emacs]} --eval '(setq org-startup-folded 2) (pop-to-buffer (find-file \"#{file}\")) (org-mode)'"
puts "Executing: " + command
system command
puts "Trying to parse the edited file ..."
edited=OrgTable.new(ARGV[1])
puts "Okay."
fail "edit hint hast not been removed from #{ARGV[1]}" if edited[:before_table].include?(edit_hint)

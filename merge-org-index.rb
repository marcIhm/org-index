#!/usr/bin/ruby

desc = 'A custom merge-driver for org-index.

It comes with these restrictions:
- Your index must be placed as a single in its own file
- It expects exactly the three argument which specify the filenames of
  ancestor, current and other branches version of the file to be merged.
- There is no automatic merge; an editor (emacs) will be invoked 
  in most cases

To employ this driver, you need to put this in your .git/config:
(assuming, that you have saved this script into /usr/bin)

  [merge "merge-org-index"]
        name = merge driver for org-index
        driver = /usr/bin/merge-org-index.rb %O %A %B
        recursive = binary

and this in your .git/info/attributes:

  index.org merge=merge-org-index

'

require 'tempfile'
require 'optparse'

options = Hash.new

OptionParser.new do |opts|
  opts.banner = "Usage: merge-org-index.rb ANCESTOR CURRENT OTHER [options]"
  opts.separator desc.split("\n").first.strip

  options[:emacs]='emacs'
  opts.on("-e","--emacs BINARY",
          "Use this emacs binary to edit merge candidate; default: #{options[:emacs]}") do |bin|
    options[:emacs] = bin
  end

  opts.on("-h","-?","--help","show this text") do
    puts opts
    exit
  end

  opts.on("-d","--description","show description and hints on how to use this script with git") do
    puts desc
    exit
  end

  begin
    opts.parse!(ARGV)
  rescue OptionParser::InvalidOption => e
    puts e
    puts opts
    exit 1
  end

  if ARGV.length != 3
    puts "Error: Need exactly three filenames"
    puts opts
    exit 1
  end
end



class OrgTable < Hash

  def initialize name, text
    puts "Reading file #{name} as #{text}"
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
    self[:has_marker_lines] = false
    
    lines = Hash.new
    self[:table_body].split(/\R/).each_with_index do |line,lineno|
      next if line =~ /^\s*$/
      cols = line.split("|").drop(1).map(&:strip)
      skip = false
      cols.each { |col| skip = true if col.match(/^wwww+$/) }
      if skip
        warn "Found marker line from merge in #{name} in line #{lineno}: #{line}"
        self[:has_marker_lines] = true
        next
      end
      uid = [:ref, :id, :yank].map { |c| cols[columns[c]].strip }.join("|||")
      if lines[uid]
        warn "Line with uid '#{uid}' appears again: #{line}"
        self[:duplicates] ||= ""
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

  def digest key
    self[:lines][key][:line].gsub(/\s+\|/," |").gsub(/\|\s+/,"| ")
  end

  def format_line fields
    fmttd = self[:table_hline].dup
    fmttd.gsub!('+','|')
    self[:table_columns].each_key do |key|
      if fields[key]
        text = fields[key]
        markup = ' '
      else
        text = ''
        markup = fields[:markup] || ' '
      end
      fmttd.sub!(/\|-+\|/) do |match|
        ('| ' + text + markup * match.length)[0,match.length - 2] + ' |'
      end
    end
    fmttd.chomp + "\n"
  end
  
  def format part,text
    fmttd = ""
    if part.length > 0
      fmttd = self.format_line(:keywords => text, :markup => 'w') if text
      part.each { |key| fmttd += self[:lines][key][:line]; }
    end
    fmttd
  end
         
  def format_duplicates text
    if self[:duplicates]
      self.format_line(:keywords => text, :markup => 'w') + self[:duplicates].chomp + "\n"
    else
      "\n"
    end
  end
         
end

edit_hint = "  This editor is invoked to let you merge changes in your index;
it needs to be done manually, section by section.

  Each section starts with a stretch of 'wwwwwww' and a
  heading (e.g. '+ current added'); its first character
  (one of ?,+,-), gives a hint on how to treat the section:

    ? : You need to select manually one line from several secions
    + : Should be kept
    - : Should be removed

  Description for some sections:
    duplicates: lines, that also appear below in 'common' and can
      be removed.
    common: lines which are common in both versions, should be kept.
      Also this is guaranteed to be the last section, so you dont need
      to scroll further down.

  When editing a section, you should always remove its heading, i.e. 
  the line with 'wwwww'.

  Having edited all the sections you may continue like this:
  (assuming that you started with 'git rebase'):
  - 'git status' to see if any other file needs merging
  - edit those other files manually with an editor
  - 'git add' for all files that you edited, especially your index
  - 'git rebase --continue' to finish the merge

"

# Remark, this does not conform with the git documentation ?!
ancestor = OrgTable.new(ARGV[0],"ancestor")
current = OrgTable.new(ARGV[2],"current")
other = OrgTable.new(ARGV[1],"other")

[:before_heading, :heading, :before_table, :table_caption, :table_hline, :after_table].each do |part|
  warn "Part #{part.to_s} does not match in current and other" unless current[part] == other[part]
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

  cadd = Array.new
  ormv = Array.new
  (current[:lines].keys - inters).each do |key|
    if ancestor[:lines][key]
      ormv << key
    else
      cadd << key
    end
  end

  oadd = Array.new
  crmv = Array.new
  (other[:lines].keys - inters).each do |key|
    if ancestor[:lines][key]
      crmv << key
    else
      oadd << key
    end
  end
  
  cmod = Array.new
  omod = Array.new
  common = Array.new
  conflicts = Array.new
  inters.each do |key|
    if current.digest(key) == other.digest(key)
      common << key
    elsif other.digest(key) == ancestor.digest(key)
      cmod << key
    elsif current.digest(key) == ancestor.digest(key)
      omod << key
    else
      conflicts << key
    end
  end

  file.write current.format(conflicts,"? CONFLICTS, current") +
             other.format(conflicts,"? CONFLICTS, other") +
             ancestor.format(conflicts,"? CONFLICTS, ancestor") +
             current.format(cadd,"+ current added") +
             ancestor.format(crmv,"- current removed") +
             other.format(oadd,"+ other added") +
             ancestor.format(ormv,"- other removed") +
             current.format_duplicates("- current duplicates") +
             other.format_duplicates("- other duplicates") +
             current.format(cmod,"+ current modified") +
             other.format(omod,"+ other modified") +
             current.format(common,"+ common") +
             current[:after_table]

end

file = Dir.pwd + "/" + ARGV[1]
args = ["(setq org-startup-folded 2)",
        "(setq org-startup-align-all-tables t)",
        "(pop-to-buffer (find-file \"#{file}\"))",
        "(org-mode)"].map {|x| ["--eval",x]}.flatten
puts "Executing: " + options[:emacs] + " " + args.join(" ")
system options[:emacs], *args
puts "Trying to parse the edited file ..."
edited=OrgTable.new(ARGV[1])
warn "Edit hint has not been removed from #{ARGV[1]}" if edited[:before_table].include?(edit_hint.split("\n").first.strip)
fail "Found marker lines in #{ARGV[1]}" if edited[:has_marker_lines]
puts "Done."

exit 0

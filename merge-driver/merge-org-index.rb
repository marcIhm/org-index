#!/usr/bin/ruby

desc = 'A custom merge-driver for org-index.

It comes with these restrictions:
- Your index must be placed as a single in its own file
- It expects exactly the three argument which specify the filenames of
  ancestor (%O), current (%B) and other (%A) branches version of the
  file to be merged.
- There is no automatic merge; an editor (emacs) will be invoked in
  most cases; you will find instructions on how to complete the merge
  within the edited file.

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

  options[:pref_props_other] = FALSE
  opts.on("--prefer_properties_from_other",
          "Prefer the table properties (especially the set of focused nodes) from the other branch, overriding the current ones") do
    options[:pref_props_other] = TRUE
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
    puts "Error: Need exactly three filenames; use '-d' for details"
    puts opts
    exit 1
  end
end



class OrgTable < Hash

  def initialize name, text
    puts "Reading file #{name} as #{text}"
    match_data = false
    while not match_data
      match_data = File.new(name).read.match(Regexp.new('
\A  
(?<before_heading>(^\s*\R)*)
(?<heading>^\*+\s.*\R)
(?<before_table>((^\s*[^|*\s].*\R)|(^\s*\R))+)
(?<table_caption>(^\s*\|.*\|\s*$\R)+)
(?<table_hline>^\s*\|[-+]+\|\s*$\R)
(?<table_body>(^\s*\|.*\|\s*$\R)+)
(?<after_table>((^\s*[^|*\s].*\R)|(^\s*\R))*)
\Z',Regexp::EXTENDED))

      if not match_data
        puts "Could not parse file '#{name}'; it should contain a single node with a single table."
        puts "You may now fix the file from another session and press RETURN when done,"
        puts "or you may press Ctrl-C to abort this merge."
        gets
      end
    end

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
      uid = [:ref, :id, :yank].map { |c| cols[columns[c]].strip }.join("~")
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

  def make_description_line( description, markup )
    fmttd = self[:table_hline].dup
    fmttd.gsub!('+','|')
    self[:table_columns].each_key do |key|
      (text,fill) = (key == :keywords) ? [description, ' '] : ['', markup]
      fmttd.sub!(/\|-+\|/) do |match|
        ('| ' + text + fill * match.length)[0,match.length - 2] + ' |'
      end
      fmttd
    end
    fmttd.chomp + "\n"
  end
  
  def format_part( part, text )
    fmttd = ""
    if part.length > 0
      fmttd = self.make_description_line( text, 'w' ) if text
      part.each { |key| fmttd += self[:lines][key][:line]; }
    end
    fmttd
  end
         
  def format_duplicates text
    if self[:duplicates]
      self.make_description_line( text, 'w' ) + self[:duplicates].chomp + "\n"
    else
      ""
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

  PLEASE NOTE, that 'current' and 'other' are to be viewed from
  the perspective of the master-repository; so your local 
  changes are marked with 'other'.

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
  - edit those other files (shown in red) manually with an editor
  - 'git add' for all files that you edited, especially your index
  - 'git rebase --continue' to finish the merge
  - The whole cycle may repeat a few times

"

# Remark, the lines below (handling ARGV) assume, that .git/config specifies the arguments to this
# merge-driver as "%O %A %B" where %O = ancestor, %A = current and %B = other
ancestor = OrgTable.new(ARGV[0],"ancestor")
other = OrgTable.new(ARGV[1],"other")
current = OrgTable.new(ARGV[2],"current")
edited_name = ARGV[2]
destination = ARGV[4]

[:before_heading, :heading, :before_table, :table_caption, :table_hline, :after_table].each do |part|
  unless current[part] == other[part]
    if part == :table_caption && current[:table_caption].lines[0] != other[:table_caption].lines[0]
      fail "\n\nPart table_caption does not match in current and other; please consider, fixing this manually !\n\n#{current[:table_caption].lines[0]}\n#{other[:table_caption].lines[0]}\n\n"
    else
      warn "Part #{part.to_s} does not match in current and other."
    end
  end
end

puts edit_hint
puts "Editing #{destination} ..."

File.open(ARGV[1],'w') do |file|

  [:before_heading, :heading].each do |part|
    file.write current[part]
  end

  if (options[:pref_props_other]) then
    file.write other[:before_table]
  else
    file.write current[:before_table]
  end
  
  [:table_caption, :table_hline].each do |part|
    file.write current[part]
  end

  inters = current[:lines].keys & other[:lines].keys

  current_added = Array.new
  other_removed = Array.new
  (current[:lines].keys - inters).each do |key|
    if ancestor[:lines][key]
      other_removed << key
    else
      current_added << key
    end
  end

  other_added = Array.new
  current_removed = Array.new
  (other[:lines].keys - inters).each do |key|
    if ancestor[:lines][key]
      current_removed << key
    else
      other_added << key
    end
  end
  
  current_modified = Array.new
  other_modified = Array.new
  common = Array.new
  conflicts = Array.new
  inters.each do |key|
    if current.digest(key) == other.digest(key)
      common << key
    elsif other.digest(key) == ancestor.digest(key)
      current_modified << key
    elsif current.digest(key) == ancestor.digest(key)
      other_modified << key
    else
      conflicts << key
    end
  end

  file.write current.format_part( conflicts, "? CONFLICTS, current" ) +
             other.format_part( conflicts, "? CONFLICTS, other" ) +
             ancestor.format_part( conflicts, "? CONFLICTS, ancestor" ) +
             current.format_part( current_added, "+ current added" ) +
             ancestor.format_part( current_removed, "- current removed" ) +
             other.format_part( other_added, "+ other added" ) +
             ancestor.format_part( other_removed, "- other removed" ) +
             current.format_duplicates( "- current duplicates" ) +
             other.format_duplicates( "- other duplicates" ) +
             current.format_part( current_modified, "+ current modified" ) +
             other.format_part( other_modified, "+ other modified" ) +
             current.format_part( common, "+ common" ) +
             current[:after_table]

end

file = Dir.pwd + "/" + ARGV[1]
args = [ "(setq org-startup-folded 2)",
         "(setq org-startup-align-all-tables t)",
         "(pop-to-buffer (find-file \"#{file}\"))",
         "(org-mode)" ].map {|x| ["--eval",x]}.flatten
puts "Executing: " + options[:emacs] + " " + args.join(" ")
system options[:emacs], *args
puts "Trying to parse the edited file ..."
edited = OrgTable.new(edited_name,"edited")
warn "Edit hint has not been removed from #{ARGV[1]}" if edited[:before_table].include?("wwwww")
fail "Found marker lines in #{ARGV[1]}" if edited[:has_marker_lines]
puts "Done."

exit 0

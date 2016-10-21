#!/usr/bin/ruby

require 'tempfile'
require 'fileutils'

class OrgTable < Hash

  def initialize name
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
      cols = line.split("|").drop(1).map(&:strip)
      cols.each { |col| fail "Found marker line from merge in #{name} in line #{lineno}: line" if col.match(/^wwww+$/) }
      uid = [:ref, :id, :yank].map { |c| cols[columns[c]].strip }.join("|||")
      fail "Line with uid #{uid} appears for the second time: #{line}" if lines[uid]
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
      formatted += self.format_line(:keywords => text, :markup => 'w')
      part.each { |key| formatted += self[:lines][key][:line] }
    end
    formatted
  end
         
end

ancestor = OrgTable.new(ARGV[0])
current = OrgTable.new(ARGV[1])
other = OrgTable.new(ARGV[2])

[:before_heading, :heading, :before_table, :table_caption, :table_hline, :after_table].each do |part|
  fail "Part #{part.to_s} does not match in current and other" unless current[part] == other[part]
end

e_name = '/tmp/merge-org-index.org'
edit_hint = '  Please merge manually:

  - Merge or Select one of two lines from sections "CONFLICTS, current" 
    and "CONFLICTS, other"; use "CONFLICTS, ancestor" as a reference
  - Remove lines from "other only"
  - Keep lines from "current only"
  - Keep all lines from sections "current modified" and "other modified"
  - Keep section "common"
  - Remove all marker lines with "wwww" and this comment

'
File.open(e_name,'w') do |e_file|

  [:before_heading, :heading, :before_table].each do |part|
    e_file.write current[part]
  end

  e_file.write edit_hint

  [:table_caption, :table_hline].each do |part|
    e_file.write current[part]
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

  e_file.write current.format("CONFLICTS, current",conflicts)
  e_file.write other.format("CONFLICTS, other",conflicts)
  e_file.write ancestor.format("CONFLICTS, ancestor",conflicts)
  e_file.write other.format("other only",oonly)
  e_file.write current.format("current only",conly)
  e_file.write current.format("current modified",cmod)
  e_file.write other.format("other modified",omod)
  e_file.write current.format("common",common)
  
  e_file.write current[:after_table]

end

system "emacs -nw --eval '(setq org-startup-folded 2)' #{e_name}"
puts "Trying to parse the edited file ..."
edited=OrgTable.new(e_name)
puts "Okay."
fail "edit hint hast not been removed from #{e_name}" if edited[:before_table].include?(edit_hint)
FileUtils::mv e_name,ARGV[0]

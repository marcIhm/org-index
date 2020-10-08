#!/usr/bin/ruby

#
# Common Rakefile for multiple elisp-projects; updates itself (if dir exists) from 
#
#   ../rakefile-for-elisp/Rakefile
#
# and vice versa.
#
# Configure via rake_config.yml
#

require 'fileutils'
include FileUtils
require 'yaml'
require 'set'
require 'open3'

#
# Global variables
#
# Configuration and options
$conf = YAML::load(File.open('rake_config.yml'))
$conf.transform_keys!(&:to_sym)
$conf[:file] = 'rake_config.yml'
$v = (verbose == true)

# Shared information between tasks
# from lisp-source
$version_lisp = nil
$commentary_lisp = Hash.new
$changelog_lisp = Hash.new
# from changelog
$changelog = Hash.new
$changelog_until = Hash.new

#
# Helper functions
#
def heading text, large = false, warn = false
  if large
    puts "\n"
    print "\e[32m"
    dash = "==="
    puts dash * 12
    puts "#{dash}  #{text}"
    puts dash * 12
  else
    print "\e[#{warn ? 35 : 33}m"
    puts "--- #{text}"
  end
  print "\e[0m"
end


def compare_semver one,two
  vs = [one, two].map do |x|
    x.match(/(\d+)\.(\d+)/) || abort("Argument '#{x}' does not contain a semantic version number")
  end
  ( 2*(vs[0][1].to_i<=>vs[1][1].to_i) + (vs[0][2].to_i<=>vs[1][2].to_i) ) <=> 0
end


def accept fname,nname
  heading "Accept changes to #{fname} ?"
  system "diff #{fname} #{nname}"
  if $? == 0
    puts "No differences between #{fname} and #{nname}."
    rm nname
  else
    puts "\n\n"
    system "ls -l #{fname} #{nname}"
    heading "Changes to #{fname}.\nPlease review 'diff old new' and output of 'ls'\nType RETURN to accept or Ctrl-c Ctrl-c to reject:"
    $stdin.gets
    make_backup fname
    mv nname,fname
  end
end


def make_backup file
  dir = File.dirname(file) + '/backup'
  mkdir dir,{:verbose => $v} unless File.directory?(dir)
  backs = [ file ] + (1..12).map {|i| dir + '/' + File.basename(file) + "_" + i.to_s}
  pairs = backs[0..-2].zip(backs[1..-1]).reverse
  pairs.each do |p|
    next unless File.exist?(p[0])
    cp p[0],p[1],{:verbose => $v}
  end
end


def maybe_copy from,to
  puts "Maybe copy #{from} to #{to}" if $v
  [from, to].each {|f| return false unless File.exist?(f)}
  return false if File.mtime(to) > File.mtime(from)
  system("diff -q #{from} #{to} >/dev/null 2>&1")
  return false if $?.exitstatus == 0
  make_backup to
  cp from,to,{:verbose => $v}
  return true
end


def brushup text
  text.sub!(/\A[\s\n]+/,'')
  text.sub!(/[\s\n]+\Z/,'')
  text.chomp!
end


def write_as_org file, level, hash, &compare
  keys = hash.keys
  keys = keys.sort(&compare) if compare
  keys.each do |key|
    file.puts '*' * level + " #{key}\n\n"
    hash[key].lines.each {|l| file.puts (' ' * (level+1) + l).rstrip}
    file.puts "\n"
  end
  pp hash if $v
end


def forward_to file, text
  line = nil
  begin
    line = file.gets
  end until !line || line.start_with?(text)
  line
end

#
# Individual tasks
#
#
# Tasks, that collect information, no desc to avoid them beeing listed with -T
#

# Compare with rakefile in other dir and update 
task :update_rake do
  this_rf = __FILE__
  parent_rf = File.expand_path('..', File.dirname(this_rf)) + '/rakefile-for-elisp/Rakefile'
  system("touch -t 190001010000 #{parent_rf} >/dev/null 2>&1") unless File.exist?(parent_rf)

  if maybe_copy this_rf, parent_rf
    heading "Updated #{parent_rf} from #{this_rf}"
  elsif maybe_copy parent_rf, this_rf
    heading "This rakefile #{this_rf} has been updated from #{parent_rf}; please rerun"
    exit
  end
end



# Extract version from sourcefile
task :extract_version => [:update_rake] do

  heading "Extract version from #{$conf[:source]}"

  File.open($conf[:source]).each do |line|
    if line.match(/^;; Version: (\d+\.\d+\.\d+)\s*/)
      $version_lisp = Regexp.last_match[1]
      puts $version_lisp
      break
    end
  end
end


# Extract commentary from sourcefile
task :extract_commentary => [:update_rake] do

  heading "Extract commentary from #{$conf[:source]}"

  commentary_keys_matcher = Regexp.new('^;; (' + $conf[:required_pieces_commentary].join('|') + '):\s*$')
  key = nil

  File.open($conf[:source]).
    drop_while { |line| !line.start_with?(';;; Commentary:') }.
    drop(2).take_while { |line| line.start_with?(';;') }.each do |line|
    if line.match(commentary_keys_matcher)
      key = Regexp.last_match[1]
      $commentary_lisp[key] = ''
      puts key
    else
      $commentary_lisp[key] += line.sub(/^;;(  )?/,'').rstrip + "\n" if key
    end
  end

  $commentary_lisp.each_key do |key|
    brushup $commentary_lisp[key]
  end
  pp $commentary_lisp if $v

  pieces_seen = Set.new($commentary_lisp.keys)
  pieces_required = Set.new($conf[:required_pieces_commentary])
  abort "ERROR: Pieces of commentary seen #{pieces_seen} does not equal pieces of commentary required #{pieces_required}" unless pieces_seen == pieces_required
end


# Extract change log from sourcefile
task :extract_changelog_lisp => [:extract_version] do

  heading "Extract Changelog from #{$conf[:source]}"
  version = offset = nil

  File.open($conf[:source]).each.
    drop_while { |line| !line.start_with?(';;; Change Log:') }.
    drop(2).take_while { |line| line.start_with?(';;') }.each do |line|
    if line.match(/^(;;\s+)Version (\d+\.\d+)\s*/) 
      version = Regexp.last_match[2]
      $changelog_lisp[version] = ''
      offset = Regexp.last_match[1]
      puts version
    elsif version
      $changelog_lisp[version] += (line[offset.length..-1] || '').rstrip + "\n"
    end
  end

  $changelog_lisp.each_key do |version|
    brushup $changelog_lisp[version]
  end

  pp $changelog_lisp if $v
  
  version_lisp_latest = $changelog_lisp.keys.max {|a,b| compare_semver(a,b)}
  abort "Mismatch in #{$conf[:source]}: Latest version from Changelog #{version_lisp_latest} does not fit version specified explicitly #{$version_lisp}" unless $version_lisp.sub(/\.\d+$/,'') == version_lisp_latest

end


# Extract change log from changelog
task :extract_changelog => [:extract_changelog_lisp] do

  fname = 'ChangeLog.org'
  heading "Extract Changelog from #{fname}"

  version = nil
  
  File.open(fname).each do |line|
    if mdata = line.match(/^\* (\d+\.\d+) until (\S.*\S)/)
      version = mdata[1]
      $changelog[version] = ''
      $changelog_until[version] = mdata[2]
      puts version
    else
      $changelog[version] += (line[2..-1] || '').rstrip + "\n" if version
    end
  end

  now = Time.now.strftime("%Y-%m-%d %a")[0..-2]
  $changelog_lisp.each_key do |version|
    $changelog[version] = $changelog_lisp[version]
    $changelog_until[version] ||= now
  end
  version_latest = $changelog.keys.max {|a,b| compare_semver(a,b)}
  $changelog_until[version_latest] = now
  
  $changelog.each_key do |version|
    brushup $changelog[version]
  end
  pp $changelog if $v
  
end


#
# Tasks, that update files with collected information
#
desc 'Update Changelog with information from sourcefile'
task :update_changelog => [:extract_changelog, :extract_version] do

  fname = 'ChangeLog.org'
  heading "Update #{fname}",true
  nname = fname + '.new'

  File.open(nname,'w') do |nfile|
    $changelog.keys.sort {|a,b| compare_semver(b,a)}.each do |version|
      nfile.puts "* #{version} until #{$changelog_until[version]}\n\n"
      $changelog[version].lines.each {|l| nfile.puts "  #{l}".rstrip}
      nfile.puts "\n"
    end
  end
  accept fname,nname
  
end


desc 'Update Readme with information from sourcefile'
task :update_readme => [:extract_changelog, :extract_version, :extract_commentary] do

  fname = 'README.org'
  heading "Update #{fname}",true
  nname = fname + ".new"

  toc = File.read(fname).lines.select {|l| l.start_with?('** ')}.map {|l| l[3..-1].chomp}
  unseen = Set.new([:version, :toc, :about, :changelog])
  tcvi = '  The current version is '

  File.open(nname,'w') do |nfile| 
    File.open(fname) do |file|

      loop do
        line ||= file.gets
        break unless line

        # content of line will in any case written at bottom of loop, until then:
        # Write something else, replace content of line or leave as is

        if line.start_with?(tcvi)
          heading 'Version'
          line = tcvi + $version_lisp + '.'
          unseen.delete(:version)
        end

        if line.start_with?('** Table of Contents')
          heading 'Table of contents'
          nfile.puts line + "\n"
          toc.drop(1).each {|t| nfile.puts "   - [[##{t.downcase.tr(' ','-')}][#{t}]]"}
          nfile.puts "\n"
          line = forward_to(file,'** ')
          unseen.delete(:toc)
        end

        if line.start_with?('** About this Package')
          heading 'Commentary'
          nfile.puts line + "\n"
          write_as_org nfile,3,$commentary_lisp
          line = forward_to(file,'** ')
          unseen.delete(:about)
        end

        if line.start_with?('** Latest Change Log')
          heading 'Latest Change log'
          nfile.puts line + "\n   See ChangeLog.org for older entries.\n\n"
          write_as_org nfile,3,$changelog_lisp
          line = forward_to(file,'* ')
          unseen.delete(:changelog)
        end

        # now write whatever current content of line is
        nfile.puts line if line
      end

    end
  end
  abort "Did not see #{unseen.inspect} in #{fname}" if unseen.length > 0

  accept fname,nname

end


desc 'Update sourcefile with information from sourcefile'
task :update_lisp => [:extract_changelog, :extract_version, :extract_commentary] do

  fname = $conf[:source]
  heading "Update #{fname}",true
  nname = fname + ".new"

  unseen = Set.new([:version, :commentary])
  tiv = 'This is version'

  File.open(nname,'w') do |nfile| 
    File.open(fname) do |file|

      loop do
        line ||= file.gets
        break unless line

        # content of line will in any case written at bottom of loop, until then:
        # Write something else, replace content of line or leave as is

        if line.start_with?("(defvar #{$conf[:package]}-version")
          heading 'Version'
          line.sub!(/\d+\.\d+\.\d+/,$version_lisp)
          unseen.delete(:version)
        end

        if line['For Rake: Insert here']
          heading 'Commentary'
          nfile.write line
          first = true
          $conf[:pieces_for_docstring].each do |piece|
            abort "ERROR: Configuration item 'pieces_for_docstring' from $conf[:file] has unkonwn docstring-piece #{piece}" unless $commentary_lisp[piece]
            nfile.print first ? "  \"" : "\n\n#{piece}:\n\n"
            nfile.print $commentary_lisp[piece]
            first = false
          end
          forward_to(file,tiv)
          line = "\n\n#{tiv} #{$version_lisp} of #{$conf[:source]}.\n"
          unseen.delete(:commentary)
        end

        # now write whatever current content of line is
        nfile.puts line if line
      end

    end
  end
  abort "Did not see #{unseen.inspect} in #{fname}" if unseen.length > 0

  accept fname,nname

end


desc 'Describe building process'
task :h do
  within = false
  doc = File.open(%x{git rev-parse --show-toplevel}.chomp + '/README.org').each do |line|
    within = line.include?('Preparing') if line.start_with?('*')
    print line if within
  end
end


desc 'Run all tests from directory test'
task :test => [:update_rake] do

  heading "Run tests",true
  command = <<-END.lines.map {|l| l.strip!}.join(' ')
  emacs 
  --no-init-file        
  --no-site-file 
  --no-site-lisp 
  --eval "(set-variable 'make-backup-files nil)"
  --eval "(set-variable 'auto-save-default nil)"
  --eval "(setq load-prefer-newer t)"
  --eval "(add-to-list 'load-path \\\".\\\")"
  --eval "(setq package-user-dir \\\"#{File.dirname(__FILE__)+"/elpa"}\\\")"
  --eval "(set-variable 'org-id-locations-file \\\"#{File.dirname(__FILE__)+"/tmp/org-id-locations"}\\\")"
  --eval "(package-initialize)"
  --load ert
  --load #{File.dirname(__FILE__)+"/test/"+$conf[:testfile]}
  test/#{$conf[:testfile]}
  #{$conf[:source]}
  --eval "(ert-run-tests-batch-and-exit)"
  --batch 
  END

  puts command
  at_summary = false
  Open3.popen2e(command) do |stdin, stdout_stderr, status_thread|
    stdout_stderr.each_line do |line|
      at_summary = true if line.match?(/^Ran \d+ /)
      if at_summary
        puts line
      elsif line.lstrip.start_with?('passed')
        heading line
      elsif line.lstrip.start_with?('FAILED')
        heading line,false,true
      elsif $v
        puts line
      end
    end
    abort "ERROR: Command ended with error.\nRerun with '-v' for details." unless status_thread.value.success?
  end
 
end


desc 'Update all files'
task :update => [:update_changelog, :update_readme, :update_lisp, :test] do
end


desc 'Update all files and run tests'
task :default => [:update, :test] do
end

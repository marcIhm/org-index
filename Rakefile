#!/usr/bin/ruby

require 'pp'
require 'fileutils'
include FileUtils

require 'yaml'
cnf = YAML::load(File.open('rake_config.yml'))

def compare_semver one,two
  vers = [one, two].map do |x|
    md = x.match(/(\d+)\.(\d+)/)
    fail "Argument '#{x}' does not contain a semantic version number" unless md
    [md[1],md[2]]
  end.transpose

  vers.inject(0) {|s,x| 2*s+(x[0].to_i<=>x[1].to_i)} <=> 0
end

def accept fname,nname
  oname = fname + ".backup"
  puts "\n"
  system "diff #{fname} #{nname}"
  if $? == 0
    puts "No differences between #{fname} and #{nname}."
    rm nname
  else
    puts "\n\n"
    system "ls -l #{fname} #{nname}"
    puts "\nChanges to #{fname}.\nPlease review 'diff old new' and output of 'ls'\nType RETURN to accept or ctrl-C ctrl-C to reject:"
    gets
    mv fname,oname
    mv nname,fname
  end
end

desc 'Copy info-pieces to various destinations'
task :copy_info_pieces do

  fname = cnf['elisp_source']
  puts "\nCollect info pieces from #{fname}:"
  commentary = Hash.new {""}
  change_log = Hash.new {""}
  version = nil
  File.open(fname) do |file|
    line = ""
    puts "  Find version"
    until line.start_with?(";;; Commentary:")
      line = file.gets
      mdata = line.match(/^;; Version: (\d+\.\d+\.\d+)\s*/)
      version ||= mdata[1] if mdata
    end
    file.gets
    key = nil
    puts "  Pieces of Commentary"
    while (line = file.gets).start_with?(";;")
      mdata = line.match(/^;; (\S.*):\s*$/)
      if mdata
        key = mdata[1]
      else
        line.sub!(/^;;  /,'')
        line.sub!(/^;; *$/,'')
        commentary[key] += line if key
      end
    end
    commentary.each_key do |key|
      commentary[key].sub!(/\A(\s*\n)+/,'')
      commentary[key].sub!(/(\s*\n)+\Z/,'')
    end
    fail "Invalid set of keys #{commentary.keys}" unless commentary.keys.eql?(cnf['valid_keys'])

    puts "  Latest Change Log"
    vkey = nil
    line = file.gets until line.start_with?(";;; Change Log:")
    line = file.gets
    while (line = file.gets).start_with?(";;")
      mdata = line.match(/^;;   Version (\d+\.\d+)\s*/)
      if mdata 
        vkey = mdata[1]
        next
      end
      next unless line.start_with?(";;   ")
      change_log[vkey] += line[3..-1]
    end
  end

  nname = fname + ".new"
  puts "\n\n\e[33mPut info pieces into #{nname}:\e[0m"
  seen = Hash.new
  seen[:version] = seen[:purpose] = false
  seen[:changelog] = false if cnf['copy_changelog']
  File.open(nname,'w') do |nfile| 
    File.open(fname) do |file|
      while line = file.gets

        if line.start_with?("(defvar " + cnf['package_name'] + "-version")
          puts "  Version"
          line.sub!(/\d+\.\d+\.\d+/,version)
          seen[:version] = true
        end

        if line['For Rake: Insert purpose here']
          puts "  Commentary"
          nfile.puts line + "  \"" + commentary['Purpose'] + "\n\nThis is version #{version} of org-working-set.el."
          seen[:purpose] = true
          line = file.gets
          until line.start_with?('This is version')
            line = file.gets
            fail "Reached end of string: #{line}" if line['"']
          end
          line = file.gets
        end

        if line['For Rake: Insert Change Log here']
          puts "  Latest Change Log"
          seen[:change_log] = true
          nfile.puts line
          line = file.gets.chomp
          line.sub!(/\S.*$/,'') # keep only indentation
          nfile.puts line + "(insert \""
          change_log.each do |ver,log|
            nfile.puts "* " + ver + "\n\n" + log + "\n"
          end
          line = file.gets
          until line.start_with?('")')
            fail "Reached end of string: #{line}" if line['"']
            line = file.gets
          end
        end
        nfile.puts line
      end
    end
  end
  seen.each_key {|piece| fail "Did not see #{piece}" unless seen[piece]}
  accept fname,nname

  fname = 'ChangeLog.org'
  puts "\n\n\e[33mPut info pieces into #{fname}:\e[0m"
  nname = fname + ".new"
  version_dates = Hash.new
  rest_of_change_log = ""
  version_latest = version_latest_cl = nil
  puts "  Latest Change log"
  File.open(fname) do |file|
    while line = file.gets do
      mdata = line.match(/^\* (\d+\.\d+) until (\S.*\S)/)
      if mdata
        if change_log[mdata[1]].length > 0
          version_dates[mdata[1]] = mdata[2]
          version_latest_cl = mdata[1] if !version_latest_cl || compare_semver(mdata[1],version_latest_cl) > 0
        else
          rest_of_change_log = line + file.read
        end
      end
    end
  end

  version_short = version.sub(/.\d+$/,'')
  change_log.each_key { |ver|  version_latest = ver if !version_latest || compare_semver(ver,version_latest) > 0}
  version_dates[version_latest] = Time.now.strftime("%Y-%m-%d %a")[0..-2]
  version_dates[version_latest_cl] = version_dates[version_latest] if version_latest != version_latest_cl 
  File.open(nname,'w') do |nfile|
    change_log.each do |ver,log|
      nfile.puts "* " + ver + " until " + version_dates[ver] + "\n\n" + log + "\n"
    end
    nfile.puts rest_of_change_log
  end
  accept fname,nname

  fname = 'README.org'
  puts "\n\n\e[33mPut info pieces into #{fname}:\e[0m"
  nname = fname + ".new"
  toc = Array.new
  seen = Hash.new
  [:version, :toc, :about, :change_log].each {|piece| seen[piece] = false}
  File.open(fname) do |file|
    while line = file.gets
      toc << line[3..-2] if line.start_with?('** ')
    end
  end

  File.open(nname,'w') do |nfile| 
    File.open(fname) do |file|
      while line = file.gets
        if line.start_with?("  The current version is")
          puts "  Version number"
          line.sub!(/\s\S+\s*$/,'')
          line += " " + version + "."
          seen[:version] = true
        end
        if line.start_with?("** Table of Contents")
          puts "  Table of contents"
          nfile.puts line + "\n"
          toc.shift
          toc.each {|t| nfile.puts "   - [[##{t.downcase.tr(" ","-")}][#{t}]]"}
          nfile.puts "\n"
          seen[:toc] = true
          line = file.gets
          line = file.gets until line.start_with?("** ")
        end
        if line.start_with?("** About this Package")
          puts "  Commentary"
          nfile.puts line + "\n"
          commentary.each do |head,text|
            nfile.puts "*** " + head + "\n\n"
            nfile.puts "    " + text.gsub(/\n/,"\n    ").chomp + "\n\n"
          end
          seen[:about] = true
          line = file.gets
          line = file.gets until line.start_with?("** ")
        end
        if line.start_with?("** Latest Change Log")
          puts "  Latest Change log"
          nfile.puts line + "\n   See ChangeLog.org for older notes.\n\n"
          change_log.each do |ver,log|
            nfile.puts "*** " + ver + "\n\n  " + log.gsub(/\n/,"\n  ").chomp + "\n"
          end
          seen[:change_log] = true
          line = file.gets
          line = file.gets until !line || line.start_with?("** ")
        end
        nfile.puts line
      end
    end
  end
  seen.each_key {|piece| fail "Did not see #{piece}" unless seen[piece]}
  accept fname,nname

end

task :default => [:copy_info_pieces] do
end


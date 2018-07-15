#!/usr/bin/ruby

require 'pp'
require 'fileutils'
include FileUtils

def accept fname,nname
  oname = fname + ".backup"
  puts "\n"
  system "diff #{fname} #{nname}"
  if $? == 0
    puts "No differences."
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

  fname = 'org-index.el'
  puts "\nCollect info pieces from #{fname}:"
  commentary = Hash.new {""}
  change_log = Hash.new {""}
  version = nil
  File.open(fname) do |file|
    line = ""
    puts "  Find version"
    until line.start_with?(";; / This Commentary")
      line = file.gets
      mdata = line.match(/^;; Version: (\d+\.\d+\.\d+)\s*/)
      version ||= mdata[1] if mdata
    end 
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
    fail "Invalid set of keys #{commentary.keys}" unless commentary.keys.eql?(['Purpose','Setup','Further Information'])

    puts "  Latest Change Log"
    vkey = nil
    line = file.gets until line.start_with?(";; / This Change Log")
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
  puts "\nPut info pieces into #{nname}:"
  seen = Hash.new
  [:version, :purpose, :change_log].each {|piece| seen[piece] = false}
  File.open(nname,'w') do |nfile| 
    File.open(fname) do |file|
      while line = file.gets

        if line.start_with?("(defvar org-index-version")
          puts "  Version"
          line.sub!(/\d+\.\d+\.\d+/,version)
          seen[:version] = true
        end

        if line['For Rake: Insert purpose here']
          puts "  Commentary"
          nfile.puts line + "  \"" + commentary['Purpose'] + "\n\nThis is version #{version} of org-index.el."
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
  puts "\nPut info pieces into #{fname}:"
  nname = fname + ".new"
  version_dates = Hash.new
  rest_of_change_log = ""
  puts "  Latest Change log"
  File.open(fname) do |file|
    while line = file.gets do
      mdata = line.match(/^\* (\d+\.\d+) -- \[(.*)\]/)
      if mdata
        if change_log[mdata[1]].length > 0
          version_dates[mdata[1]] = mdata[2]
        else
          rest_of_change_log = line + file.read
        end
      end
    end
  end
  version_dates[version.sub(/.\d+$/,'')] = Time.now.strftime("%m-%d-%Y %a")[0..-2]
  File.open(nname,'w') do |nfile|
    change_log.each do |ver,log|
      nfile.puts "* " + ver + " -- [" + version_dates[ver] + "]\n\n" + log + "\n"
    end
    nfile.puts rest_of_change_log
  end
  accept fname,nname

  fname = 'README.org'
  puts "\nPut info pieces into #{fname}:"
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
          toc.each {|t| nfile.puts "   - [[#{t}]]"}
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


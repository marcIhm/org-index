#!/usr/bin/ruby

require 'pp'
require 'fileutils'
include FileUtils

def rename fname,nname
  oname = fname + ".backup"
  puts "\n"
  system "diff #{fname} #{nname}"
  if $? == 0
    puts "No differences."
    rm nname
  else
    puts "\n\n"
    system "ls -l #{fname} #{nname}"
    puts "\nPlease review: 'diff old new' and output of 'ls'\nType RETURN to accept or ctrl-C ctrl-C to reject:"
    gets
    mv fname,oname
    mv nname,fname
    system "ls -l #{oname} #{fname}"
  end
end

desc 'Copy info-pieces to various destinations'
task :copy_info_pieces do

  puts "\nCollect info pieces:"
  commentary = Hash.new {""}
  vinfo = Hash.new {""}
  version = nil
  fname = 'org-index.el'
  File.open(fname) do |file|
    line = ""
    puts "  Find version"
    until line.start_with?(";; / This Commentary")
      line = file.gets
      mdata = line.match(/^;; Version: (\d+\.\d+\.\d+)\s*/)
      version ||= mdata[1] if mdata
    end 
    key = nil
    puts "  Extract pieces of Commentary"
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

    puts "  Find Change Log"
    vkey = nil
    line = file.gets until line.start_with?(";; / This Change Log")
    while (line = file.gets).start_with?(";;")
      mdata = line.match(/^;;   Version (\d+\.\d+)\s*/)
      if mdata 
        vkey = '* ' + mdata[1]
        next
      end
      next unless line.start_with?(";;   ")
      vinfo[vkey] += line[3..-1]
    end
  end

  puts "\nPut info pieces into lisp file itself:"
  nname = fname + ".new"
  seen = Hash.new
  [:version, :purpose, :change_log].each {|piece| seen[piece] = false}
  File.open(nname,'w') do |nfile| 
    File.open(fname) do |file|
      while line = file.gets

        if line.start_with?("(defvar org-index-version")
          puts "  Put Version"
          line.sub!(/\d+\.\d+\.\d+/,version)
          seen[:version] = true
        end

        if line['For Rake: Insert purpose here']
          puts "  Put Commentary"
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
          puts "  Put Change Log"
          seen[:change_log] = true
          nfile.puts line
          line = file.gets.chomp
          line.sub!(/\S.*$/,'') # keep only indentation
          nfile.puts line + "(insert \""
          vinfo.each do |ver,log|
            nfile.puts ver + "\n\n" + log + "\n"
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
  rename fname,nname

  fname = 'ChangeLog.org'
  puts "\nPut info pieces into #{fname}:"
  nname = fname + ".new"
  File.open(nname) do |nfile|
  end

end

task :default => [:copy_info_pieces] do
end


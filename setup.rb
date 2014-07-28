require 'fileutils'

DOTFILES = %w(.vimrc .emacs .zshrc)

dotfilesdir = File.expand_path(File.dirname(__FILE__))
homedir     = Dir.home

# puts "dotfiles directory is '#{dotfilesdir}'"
# puts "home directory is '#{homedir}'"

did_anything = false

DOTFILES.each do |f|
  local_path  = File.join(dotfilesdir, f)
  remote_path = File.join(homedir, f)

  local_mtime  = File.mtime(local_path)
  remote_mtime = File.mtime(remote_path)

  if remote_mtime < local_mtime
    FileUtils.cp local_path, homedir
    did_anything = true
    puts "Copied #{f} to home"
  end
end

if !did_anything
  puts "Did nothing."
end

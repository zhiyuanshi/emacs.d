# Deprecated! You should create symlinks in the home directory instead.

require 'fileutils'

DOTFILES = %w(.vimrc .emacs .zshrc)

dotfilesdir = File.expand_path(File.dirname(__FILE__))
homedir     = Dir.home

puts "dotfiles directory is '#{dotfilesdir}'"
puts "home directory is '#{homedir}'"

did_anything = false

DOTFILES.each do |f|
  local_path  = File.join(dotfilesdir, f)
  remote_path = File.join(homedir, f)

  if !File.exists?(remote_path) || File.mtime(remote_path) < File.mtime(local_path)
    FileUtils.cp local_path, homedir
    did_anything = true
    puts "Copied #{f} to home"
  end
end

if !did_anything
  puts "Nothing to be done."
end

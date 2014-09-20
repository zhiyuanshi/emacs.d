require 'rainbow'

DEPRECATION_WARNING_ON = false

if DEPRECATION_WARNING_ON
  notice = "This script is deprecated. You should create symlinks in the home directory instead.
Type 'yes' to continue. Anything else will abort this script."

  $stderr.puts Rainbow(notice).red
  unless gets.chomp == "yes"
    $stderr.puts "Abort."
    exit 1
  end
end

require 'fileutils'

DOTFILES = %w(.vimrc .emacs .zshrc .gitconfig .hgrc)

dotfilesdir = File.expand_path(File.dirname(__FILE__))
homedir     = Dir.home

puts "dotfiles directory is #{dotfilesdir}"
puts "home directory is #{homedir}"

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

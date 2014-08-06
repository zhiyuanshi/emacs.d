DOTFILES = %w(.vimrc .emacs .zshrc)

dotfilesdir = File.expand_path(File.dirname(__FILE__))
homedir     = Dir.home

DOTFILES.each do |f|
  local_path  = File.join(dotfilesdir, f)
  remote_path = File.join(homedir, f)

  `ln -s --verbose #{local_path} #{remote_path}`
end

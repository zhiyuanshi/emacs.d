RULES = {
  ".zshrc" => "~",
  ".vimrc" => "~",
  ".emacs" => "~",
  "Preferences.sublime-settings" => "~/.config/sublime-text-3/Packages/User",
  ".gitconfig" => "~",
  ".hgrc" => "~",
  ".ghci" => "~",
  ".irbrc" => "~",
  ".gemrc" => "~",
  ".railsrc" => "~",
}

desc "Setup dotfiles by creating symlinks"
task :up do
  # Ruby uses the system() C function, which uses /bin/sh, which is a symlink to
  # /bin/dash on Ubuntu by default. Ugh!
  #
  # http://stackoverflow.com/questions/1239510/how-do-i-specify-the-shell-to-use-for-a-ruby-system-call
  system("sudo ln -s --force $(which zsh) /bin/sh")

  RULES.each do |f, target_dir|
    local_path  = File.join(File.expand_path(File.dirname(__FILE__)), f)
    remote_path = File.join(File.expand_path(target_dir), f)
    system("ln -s --force #{local_path} #{remote_path}")
  end
end

desc "Show setup rules"
task :rules do
  RULES.each do |f, target_dir|
    puts "#{f} => #{target_dir}"
  end
end

desc "List files that are not covered by rake setup"
task :check do
  puts Dir.glob("{*,.*}").select {|f| File.file?(f) && !RULES.keys.include?(f) }
end

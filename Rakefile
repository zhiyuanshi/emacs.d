desc "Setup Emacs configuration"
task :up do
  local  = File.expand_path(File.dirname(__FILE__))
  remote = File.expand_path("~/.emacs.d")

  puts "Local directory  is #{local}"
  puts "Remote directory is #{remote}"

  remote_points_to = `readlink -f ~/.emacs.d`.strip

  if remote_points_to == local
    puts "Remote already points to local, do nothing"
  else
    system("rm -rf #{remote} && ln -s --verbose #{local} #{remote}")
  end

  # *After* the symlink has been successfully established, prevent
  # ~/.emacs.d/init.el from being shadowed by init files with higher priority.
  ["~/.emacs", "~/.emacs.el"].each { |f| system("rm -f --verbose #{f}") }
end

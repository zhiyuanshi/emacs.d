# emacs.d

My Emacs configurations

# Prerequisites

[Ruby](https://www.ruby-lang.org/)

On Mac OS X, Ruby should already come pre-installed.

# Set Up

**Warning:** This will remove your `~/.emacs`, `~/.emacs.el`, and `~/.emacs.d`,
if any, and symlink `~/emacs.d` to the directory you just cloned.

```
git clone https://github.com/zhiyuanshi/emacs.d.git
cd emacs.d
rake up
```

The last command will create symlinks. Therefore, if the path to this directory
is changed, you need to run `rake up` again.

You may safely run `rake up` multiple times to update:

```
rake up
```

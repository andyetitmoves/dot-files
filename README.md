Installation
============

Extract contents in your home directory or make symbolic links to this
directory.  Warning: This overwrites your `.xsession`. If you have you a custom
`.xsession` or some other X init script, insert contents to `.startup/x/session`

Setting up Zsh
--------------

set `ZDOTDIR=~/.startup/zsh` in `/etc/zsh/zshenv` (if writable) or `~/.zshenv`

Setting up any another shell
----------------------------

I don't have support for any other shell, but you can use the infrastructure. If
its possible for the shell, try to push all configuration into a subdirectory of
`~/.startup`. Though this is not necessary, it gives some sense of uniformity.

For POSIX shells, the following three shell fragments can be used in `~/.startup`:

`env`: Initializes environment variables
`rc`: Does init for an interactive shell
`logout`: Finalizes on exit from a login shell

env is needed once per login, even to graphical systems, like in
`.xsession`. login situations for shell arise when:

1. Using the text login from one of the virtual terminals
2. Switching privileges, like su
3. Remote login, like with ssh

You could source this from your `.profile`, `.bash_profile` etc.

`rc` is needed for all interactive shell invocations. You could source this from
`.bashrc`, for instance.

`logout` is needed only when logout from a login shell. This could, for e.g., be
sourced from `~/.bash_logout`

Setting up for an another user on the same machine
--------------------------------------------------

This is if you want to share your existing customizations in the first user
without much script copying. This will require that the user has read access to
the primary user's `.startup`.

Extract contents to the user's home directory. Edit `~/.startup/conf/startup-home`
to contain the home directory of the primary user.  Other configuration could be
done on an user basis in this directory, details follow.

Remember that everything to be written is written to the user's home
directory. All reading of scripts is done from the primary user's home directory
(excluding the configuration information).


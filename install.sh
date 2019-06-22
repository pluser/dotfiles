#!/bin/bash
#-*- mode: shell-script -*-

DOT_ROOT="${HOME}/.CFG"

AWESOMEWM_DIR="${HOME}/.config/awesome"
EMACS_DIR="${HOME}/.emacs.d"
MLTERM_DIR="${HOME}/.mlterm"
SYSTEMD_USER_DIR="${HOME}/.local/share/systemd/user"

default_module=( awesome emacs mlterm systemd-user zsh )

function die() { return 0; }

#################
# Setup Modules #
#################

function awesome/is-installed() {
	if [[ -e "${AWESOMEWM_DIR}" ]]
	then return 1; else return 0; fi
}

function awesome/install() {
	ln -rs "${dot_dir}" "${AWESOMEWM_DIR}" || die
}

function emacs/is-installed() {
	if [[ -e "${EMACS_DIR}" ]]
	then return 1; else return 0; fi
}

function emacs/install() {
	ln -rs "${dot_dir}" "${EMACS_DIR}" || die
}

function mlterm/is-installed() {
	if [[ -e "${MLTERM_DIR}" ]]
	then return 1; else return 0; fi
}

function mlterm/install() {
	ln -rs "${dot_dir}" "${MLTERM_DIR}" || die
}

function systemd-user/is-installed() {
	if [[ -e "${SYSTEMD_USER_DIR}" ]]
	then return 1; else return 0; fi
}

function systemd-user/install() {
	echo "Installing SystemD unit files."
	mkdir --parents "${SYSTEMD_USER_DIR%/user}"
	ln -rs "${dot_dir}" "${SYSTEMD_USER_DIR}" || die
	systemctl --user daemon-reload
}

function zsh/is-installed() {
	if [[ -e "${HOME}/.zshrc" || -e "${HOME}/.zshenv" ]]
	then return 1; else return 0; fi
}

function zsh/install() {
	ln -rs "${dot_dir}/zshrc" "${HOME}/.zshrc" || die
	ln -rs "${dot_dir}/zshenv" "${HOME}/.zshenv" || die
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
}

######################
# Internal Mechanism #
######################

function fetch() {
	if [[ ! -e "${DOT_ROOT}" ]]; then
		git clone https://gitlab.pluser.net/pluser/dotfiles-private.git ${DOT_ROOT} --depth 1
	fi
}

function update() {
	if [[ -e "${DOT_ROOT}" ]]; then
		cd "${DOT_ROOT}"
		git pull --depth 1
	fi
}

function deploy() {
	for mod in ${default_module[@]}; do
		local dot_dir="${DOT_ROOT}/${mod}.d"

		eval "${mod}/is-installed"; installed=$?
		if [[ $installed -ne 0 ]]; then
			echo "${mod} is installed already. Skipped."
		else
			eval "${mod}/install"; installed=$?
			if [[ $installed -ne 0 ]]; then echo "${mod} is installed successfully."; fi
		fi
	done
}

function print_usage() {
	echo 'target: fetch, install, all'
}

case "$1" in
	'help' | 'usage' )
		print_usage
		;;
	'fetch' )
		fetch
		;;
	'deploy' | 'install' )
		deploy
		;;
	'all' | * )
		fetch
		deploy
		;;
esac

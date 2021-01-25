#!/bin/bash

# Grub2 Theme

# General Variables
ROOT_UID=0
MAX_DELAY=20            # waiting time for inserting the password
THEME_DIR="/usr/share/grub/themes"
THEME_NAME=libraGrub
# Colors
RST="\e[0m"             # reset text formatting attributes
bCYN="\e[1;36m"         # bold cyan
bRED="\e[1;31m"         # bold red
bYLW="\e[1;33m"         # bold yellow
bWHT="\e[1;97m"         # bold white

# Formatted echoing
prompt () {
  case ${1} in
    "-s" | "--success")
      echo -e "${bWHT}${@/-s/}${RST}";;
    "-e" | "--error")
      echo -e "${bRED}${@/-e/}${RST}";;
    "-w" | "--warning")
      echo -e "${bYLW}${@/-w/}${RST}";;
    "-i" | "--info")
      echo -e "${bCYN}${@/-i/}${RST}";;
    *)
      echo -e "$@";;
  esac
}

# Check command availability
avaiable_command() {
  command -v $1 > /dev/null
}

grub_updater() {
  if avaiable_command update-grub; then
    update-grub
  elif avaiable_command grub-mkconfig; then
    grub-mkconfig -o /boot/grub/grub.cfg
  elif avaiable_command grub2-mkconfig; then
    if avaiable_command zypper; then
      grub2-mkconfig -o /boot/grub2/grub.cfg
    elif avaiable_command dnf; then
      grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg
    fi
  fi
}

main() {
  # Welcome message
  prompt -i "\n\t****************************\n\t*  ${THEME_NAME} - Grub2 Theme  *\n\t****************************"
  # Checking for root access and proceed if it is present
  prompt -w "\nChecking for root access...\n"

  if [ "$UID" -eq "$ROOT_UID" ]; then
    prompt -i "\nCreating ${THEME_NAME} theme directory...\n"
    [[ -d ${THEME_DIR}/${THEME_NAME} ]] && rm -rf ${THEME_DIR}/${THEME_NAME}
    mkdir -p "${THEME_DIR}/${THEME_NAME}"

    prompt -i "\nInstalling ${THEME_NAME} theme...\n"
    cp -a ${THEME_NAME}/* ${THEME_DIR}/${THEME_NAME}

    prompt -i "\nCreating a backup of the previous boot menu...\n"
    cp -an /etc/default/grub /etc/default/grub.bak

    prompt -i "\nSetting ${THEME_NAME} as default in /etc/default/grub ...\n"
    grep "GRUB_THEME=" /etc/default/grub 2>&1 >/dev/null && sed -i '/GRUB_THEME=/d' /etc/default/grub
    echo "GRUB_THEME=\"${THEME_DIR}/${THEME_NAME}/theme.txt\"" >> /etc/default/grub

    echo -e "Updating grub config..."
    grub_updater

    # Success message
    prompt -s "\n\t****************************\n\t*         All done!        *\n\t****************************"
  else
    # Error message
    prompt -e "\n [ Error! ] -> Run me as root "

    # ask for password and persist execution of the script as root
    read -p "[ trusted ] specify the root password : " -t${MAX_DELAY} -s
    [[ -n "$REPLY" ]] && {
      sudo -S <<< $REPLY $0
    } || {
      prompt  "\n Operation cancelled, Bye"
          exit 1
        }
  fi
}

main "$@"

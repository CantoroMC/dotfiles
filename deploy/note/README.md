# ArchLinux installation in a NutShell

## Phase I: Installation

### disk partitioning
Partition the disk with `cfdisk`

### Connect to the internet
```sh
iwctl
station wlan0 connect ...
```

### Update the system clock
```sh
timedatectl set-ntp true
timedatectl status
```

### Format the partitions
```sh
mkfs.btrfs -L rootName /dev/<root>
mkswap -L swap /dev/<swap>
```

### Mount the filesystem

#### Mount the btrfs partition
```sh
mount /dev/<root> /mnt
```

#### Create Subvolumes
```sh
btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@home
btrfs subvolume create /mnt/@log
btrfs subvolume create /mnt/@pkg
btrfs subvolume create /mnt/@tmp
btrfs subvolume create /mnt/@snapshots
umount /mnt
```

#### Mount the subvolumes in their respective directories
```sh
mount -o noatime,compress=zstd,space_cache=v2,autodefrag,subvol=@ /dev/sda1 /mnt
mkdir -p /mnt/{home,var/{log,cache/pacman/pkg,tmp},.snapshots}
mount -o noatime,compress=zstd,space_cache=v2,autodefrag,subvol=@home /dev/sda1 /mnt/home
mount -o noatime,compress=zstd,space_cache=v2,autodefrag,subvol=@log /dev/sda1 /mnt/var/log
mount -o noatime,compress=zstd,space_cache=v2,autodefrag,subvol=@pkg /dev/sda1 /mnt/var/cache/pacman/pkg
mount -o noatime,compress=zstd,space_cache=v2,autodefrag,subvol=@tmp /dev/sda1 /mnt/var/tmp
mount -o noatime,compress=zstd,space_cache=v2,autodefrag,subvol=@snapshots /dev/sda1 /mnt/.snapshots
```

#### enable the swap volume
```sh
swapon /dev/<swap>
```

### Install essential packages
```sh
pacstrap /mnt base base-devel linux-zen linux-firmware vi git zsh
```

## Phase II: Configuring the system

### Fstab
```sh
genfstab -U /mnt >> /mnt/etc/fstab
```

### Chroot in the installed arch linux
```sh
arch-chroot /mnt
```

### Timezone
Set the time zone and run hwclock to generate /etc/adjtime
```sh
ln -sf /usr/share/zoneinfo/Europe/Rome /etc/localtime
hwclock --systohc
```

### Localization
Edit /etc/locale.gen and uncommented needed locales
(`en_US.UTF-8 UTF-8` and `it_IT.UTF-8 UTF-8`).<br>
And the generate locales by
```sh
locale-gen
```

Create `/etc/locale.conf` and `/etc/vconsole.conf`

### Network configuration

#### Hostname
Create the hostname file `/etc/hostname` containing the host name.
Add matching entries to `/etc/hosts`
```config
127.0.0.1    localhost
::1          localhost
127.0.1.1    HOSTNAME.localdomain    HOSTNAME
```

#### Install remaining packages
They contain also network management softwares, grub ...
Look at `./packages/*`

### Root password with `passwd`

### Boot loader

#### Add the =btrfs= module to mkinitcpio
/etc/mkinitcpio.conf -> MODULES=(btrfs)<br>
and then recreate the initial ram disk `mkinitcpio -P`

#### Install a boot loader (grub)
```sh
grub-install --target=i386-pc /dev/<device>
grub-mkconfig -o /boot/grub/grub.cfg
```

### Exit and Reboot

## Phase III: Installing X and Deploy dotfiles

### Add user and add it to important groups
```sh
useradd -m -G video,log,adm,lp,power,network,users,sys,wheel,rfkill -s /bin/zsh USERNAME
passwd USERNAME
```

Edit `/etc/sudoers` with `EDITOR=vi visudo` and uncomment wheel.. to allow
user of the weel group to execute sudo.

### Log in with the create user
### Network
#### Activate network services
```sh
systemctl enable --now systemd-networkd
systemctl enable --now systemd-resolved
systemctl enable --now NetworkManager
```

#### Wifi
```sh
  nmtui
```

### Package-Management

#### Retrieve the latest Pacman mirrorlist
```sh
sudo reflector --protocol http,https --fastest 5 --latest 100 --age 24 --country Italy,France,German,Spain,Switzerland --save /etc/pacman.d/mirrorlist
```

#### Man database
Initizialise index database caches.
```sh
sudo mandb
```

#### Pacman files database
```sh
sudo pacman -Fy
```
to use with `command_not_found_handler`

#### Pacman pkgs
#### Aur pkgs
```sh
auracle clone $(< ~/dotfiles/deploy/arch-repo/aur-packages.txt)
```

#### More pkgs

##### Haskell and XMonad

###### Setup stack and install cabal-install
```sh
stack setup --system-ghc
stack install --system-ghc cabal-install

cabal update
cabal install cabal-uninstall
```
`ghc-pkg list` to check if you have installed only statically linked Haskell packages.
Now you re ready to go!

###### Pandoc
```sh
cabal install pandoc
```

###### Utilitites
```sh
cabal install hlint
cabal install hoogle
cabal install brittany
cabal install --lib haddock
```

##### Python
```sh
pip install neovim-remote
```

##### Ruby
```sh
gem install neovim solargraph
```

##### Node
```sh
npm install -g neovim
npm install -g vim-language-server
#npm install -g vscode-html-languageserver-bin
#npm install -g vscode-css-languageserver-bin
#npm install -g typescript typescript-language-server
```

##### Perl
```sh
cpan Perl::LanguageServer
# cpan Neovim::Ext # not working
```

#### Btrfs Utilitites
pacman: snapper grub-btrfs snap-pac
aur: snap-pac-grub snapper-gui-git

##### Snapper configuration
Create the config
```sh
umount /.snapshots
rm -rf /.snapshots/ #Little trickle
snapper -c root create-config /
mount -a
```

Edit the config `/etc/snapper/configs/root`<br>
`ALLOW_USER="<username>"`<br>
and adjust cleanup timeline --> daily 7 and hourly 5 .. others 0

Change permissions to the snapshots directory
```sh
chmod a+rx /.snapshots
chown :users /.snapshots
```

Enable systemd services for snapper
```sh
systemctl enable --now snapper-timeline.timer
systemctl enable --now snapper-cleanup.timer
```

### Systemd services
#### Reflector services
```sh
systemctl enable --now reflector.service
systemctl enable --now reflector.timer
```

#### Bluetooth service
```sh
systemctl enable --now bluetooth.service
```

#### Ssh service
```sh
systemctl enable --now sshd.service
```

### Configuration Files

#### Audio devices

##### Speakers
Check kernel driver for audio device
```sh
lspci -knn|grep -iA2 audio
```

If it is `snd_hda_intel` add kernel module
```sh
sudo cp ~/dotfiles/deploy/note/root/etc/modprobe.d/default.conf /etc/modprobe.d/
```

##### Avoid annoying fn key beeping
```sh
sudo cp ~/dotfiles/deploy/note/root/etc/modprobe.d/nobeep.conf /etc/modprobe.d/
```

##### Load /dev/mixer module
```sh
sudo cp ~/dotfiles/deploy/note/root/etc/modules-load.d/modules.conf /etc/modules-load.d/
```

#### Swappiness
Reduce the swappiness to improve system responsiveness
```sh
sudo cp ~/dotfiles/deploy/note/root/etc/sysctl.d/99-swappiness.conf /etc/sysctl.d/
```
`sysctl vm.swappiness` to check the current swappiness value (0-200)

#### Pacman
##### Hooks

###### paccache
```sh
sudo cp ~/dotfiles/deploy/note/root/usr/share/libalpm/hooks/paccache.hook /usr/share/libalpm/hooks/
```

#### Autologin on tty2
```sh
sudo cp ~/dotfiles/deploy/note/root/etc/systemd/system/getty@tty2.service.d/override.conf /etc/systemd/system/getty@tty2.service.d/
```

#### XDG-base-directory specifications

##### `/etc/gemrc`
Comment `gem: --user-install`

##### `/etc/xboard.conf`
Modify `saveSettingsFile` and `settingsFile` to `~/.config/xboardrc`

### Note

#### OpenFOAM: download from github and compile it
( requires AUR `scotch-git` and base `cgal` and `paraview` )

#### Matlab
can be installed by donwloading it and run the installer with administrator privileges
The temp directory may run out of space so you can
```sh
mkdir "$HOME/matlabdl"
sudo mount --bind -o nonempty "$HOME/matlabdl" /tmp
```
and when the installation process is finished
```sh
sudo umount /tmp
rm -rf $HOME/matlabdl
```

`libselinux` and `libsepol` are requested and can be installed from the AUR
```sh
auracle clone libselinux libsepol
```

##### Add Symlink for Mlint
```sh
sudo ln -sv /usr/local/MATLAB/R2019b/bin/glnxa64/mlint /usr/local/bin/mlint
```

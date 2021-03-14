# libraGrub

A grub menu for dual-booting *Linux* and *Windows* or *Mac*.

You can install the grub menu via:<br>
1. The script provided for the installation:
```sh
$ sudo ./install.sh
```
2. Manually:
    * Copy the libraGrub directory in /usr/share/grub/themes/
        ```sh
        $ cd libraGrub
        $ sudo mv libraGrub /usr/share/grub/themes
        ```
    * Edit the grub configuration file /etc/default/grub
        ```sh
        $ sudo $EDITOR /etc/default/grub
        ```

      and modify the term GRUB_THEME with:
      > GRUB_THEME="/usr/share/grub/themes/libraGrub/theme.txt"


    * Finally, update the grub configuration with one of these commands, accordingly <br>
        to which is provided with your distribution:
        * $ sudo update-grub
        * $ sudo grub-mkconfig -o /boot/grub/grub.cfg
        * $ sudo grub2-mkconfig -o /boot/grub2/grub.cfg
        * $ sudo rub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg

Feel free to modify the background and the theme as you prefer!

[//]: # (Reference-Links)

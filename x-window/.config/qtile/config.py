from typing import List  # noqa: F401

from colors import Argonaut as Color
from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Match, Screen
from libqtile.config import Key, KeyChord
from libqtile.config import DropDown, ScratchPad
from libqtile.dgroups import simple_key_binder
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"
terminal = guess_terminal()


auto_fullscreen = False
auto_minimize = True
bring_front_click = "floating_only"
cursor_warp = False
focus_on_window_activation = "smart"
follow_mouse_focus = False
reconfigure_screens = True
wmname = "qtile"  # LG3D


widget_defaults = dict(
    font='Operator Mono Lig',
    fontsize=11,
    padding=2,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(),
                widget.Chord(
                    chords_colors={
                        'launch': ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.CurrentLayoutIcon(),
                widget.WindowCount(),
                widget.Spacer(10),
                widget.WindowName(),
                widget.Spacer(30),
                widget.Prompt(),
                widget.Spacer(30),
                widget.Cmus(),
                widget.Mpd2(),
                widget.Spacer(30),
                widget.CheckUpdates(),
                widget.Spacer(30),
                widget.Clock(
                    format='%H:%M - %d %a %b',
                    fmt=' {}',
                ),
                widget.Spacer(30),
                widget.WidgetBox(
                    widgets=[
                        widget.ThermalSensor(thresold=80),
                        widget.Spacer(10),
                        widget.CPU(),
                        widget.CPUGraph(),
                        widget.Spacer(10),
                        widget.Memory(),
                        widget.MemoryGraph(),
                    ],
                    close_button_location='right',
                    text_closed='Monitors  ',
                    text_open=' ',
                ),
                widget.Spacer(30),
                widget.Backlight(
                    backlight_name='intel_backlight',
                    step=5,
                    fmt=' {}',
                ),
                widget.Spacer(10),
                widget.Battery(
                    battery=0,
                    format='{char} {percent:2.0%} ({hour:d}:{min:02d})'+
                           ' {watt:.2f} W',
                    hide_threshold=None,
                    charge_char='[C]', discharge_char='[D]',
                    empty_char='[E]', full_char='[F]',
                ),
                widget.Spacer(10),
                widget.Volume(
                    fmt='墳 {}',
                ),
                widget.Spacer(10),
                widget.Systray(icon_size=20, padding=3),
            ],
            24,
            background=Color.bg[1],
            opacity=1,
            margin=[0, 0, 0, 0],
        ),
    ),
]

scratchpads = [
    ScratchPad(
        "SPD",
        [
            DropDown(
                "yakuake",
                "kitty",
                height=0.5, width=0.5, opacity=0.8,
                x=0.28, y=0.28,
                on_focus_lost_hide=False, warp_pointer=False,
            ),
            DropDown(
                "cmus",
                "kitty -e cmus",
                height=0.4, width=0.4, opacity=0.9,
                x=0.33, y=0.33,
                on_focus_lost_hide=False, warp_pointer=False,
            ),
            DropDown(
                "ncmpcpp",
                "kitty -e ncmpcpp",
                height=0.4, width=0.4, opacity=0.9,
                x=0.33, y=0.33,
                on_focus_lost_hide=False, warp_pointer=False,
            ),
        ]
    )
]


groups = [
    Group("α", position=1, label="α",
          persist=True, init=True, exclusive=False,
          layout="monadtall"),
    Group("β", position=2, label="β",
          persist=True, init=True, exclusive=False,
          layout="monadtall"),
    Group("ξ", position=3, label="ξ",
          persist=True, init=True, exclusive=False,
          layout="monadtall"),
    Group("δ", position=4, label="δ",
          persist=True, init=True, exclusive=False,
          layout="monadtall"),
    Group("ε", position=5, label="ε",
          persist=True, init=True, exclusive=False,
          layout="monadtall",
          matches=[Match(wm_class='mpv')]),
    Group("φ", position=6, label="φ",
          persist=True, init=True, exclusive=False,
          layout="monadtall"),
    Group("γ", position=7, label="γ",
          persist=True, init=True, exclusive=False,
          layout="monadtall"),
    Group("θ", position=8, label="θ",
          persist=True, init=True, exclusive=False,
          layout="max"),
    Group("ι", position=9, label="ι",
          persist=True, init=True, exclusive=False,
          layout="floating",
          matches=[Match(wm_class='Transmission-gtk')]),
] + scratchpads

dgroups_key_binder = simple_key_binder(mod)
dgroups_app_rules = []  # type: List


layouts = [
    layout.MonadTall(
        align=1,
        ratio=0.55,
        margin=5,
        max_ratio=0.75, min_ratio=0.35,
        min_secondary_size=100,
        new_client_position='before_current',
        change_ratio=0.05, change_size=30,
        border_width=1, border_focus=Color.green[0], border_normal=Color.bg[0],
        single_border_width=0,
        single_margin=5,
    ),
    layout.MonadWide(
        align=1,
        ratio=0.55,
        margin=5,
        max_ratio=0.75, min_ratio=0.35,
        min_secondary_size=100,
        new_client_position='before_current',
        change_ratio=0.05, change_size=30,
        border_width=1, border_focus=Color.green[0], border_normal=Color.bg[0],
        single_border_width=0,
        single_margin=5,
    ),
    layout.Columns(
        fair=False,
        grow_amount=5,
        insert_position=0,
        margin=5, margin_on_single=-1,
        num_columns=2,
        split=True,
        wrap_focus_columns=True,
        wrap_focus_rows=True,
        wrap_focus_stacks=True,
        border_on_single=False,
        border_focus=Color.green[0], border_focus_stack=Color.green[1],
        border_normal=Color.bg[0], border_normal_stack=Color.bg[1],
    ),
    layout.Max(),
    layout.Floating(
        fullscreen_border_width=0, max_border_width=0,
        border_width=1, border_focus=Color.green[0], border_normal=Color.bg[0],
    ),
]

floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='Arandr'),
    Match(wm_class='Avahi-discover'),
    Match(wm_class='Baobab'),
    Match(wm_class='Blueberry.py'),
    Match(wm_class='Bssh'),
    Match(wm_class='Bvnc'),
    Match(wm_class='CMakeSetup'),
    Match(wm_class='Exo-helper-2'),
    Match(wm_class='feh'),
    Match(wm_class='File-roller'),
    Match(wm_class='Gimp'),
    Match(wm_class='Gnome-disks'),
    Match(wm_class='Gpick'),
    Match(wm_class='Hardinfo'),
    Match(wm_class='imagewriter'),
    Match(wm_class='Lxappearance'),
    Match(wm_class='MPlayer'),
    Match(wm_class='Nitrogen'),
    Match(wm_class='ParaView'),
    Match(wm_class='Parcellite'),
    Match(wm_class='Pavucontrol'),
    Match(wm_class='qv4l2'),
    Match(wm_class='qvidcap'),
    Match(wm_class='Snapper-gui'),
    Match(wm_class='Sxiv'),
    Match(wm_class='System-config-printer.py'),
    Match(wm_class='Transmission-gtk'),
    Match(wm_class='Xarchiver'),
    Match(wm_class='Xboard'),
    Match(wm_class='Xfce4-about'),
    Match(wm_class='Xmessage'),
    Match(wm_class='Yad'),
    Match(wm_class='Yad-icon-browser'),
    Match(title='Event Tester'),
    Match(title='lstopo'),
    Match(title='ImageMagick: '),
])

keys = [
    # Left side letters =======================================================
    Key([mod],          "q", lazy.restart(),     desc="Restart Qtile"),
    Key([mod, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),

    Key([mod, "shift"], "r", lazy.screen.toggle_group(),
        desc="Move to the last visited group"),

    Key([mod, "shift", "control"], "y",
        lazy.group['SPD'].dropdown_toggle('yakuake'),
        desc="Spawn a dropdown terminal"),

    Key([mod], "t", lazy.window.toggle_floating(),
        desc="Toggle floating/tile for the current window"),

    Key([mod], "f", lazy.spawn("vivaldi-stable"),
        desc="Spawn the web browser"),
    Key([mod, "control"], "f", lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen mode for the current window"),

    Key([mod],          "z", lazy.layout.normalize(),
        desc="Reset all window sizes"),
    Key([mod, "shift"], "z", lazy.layout.reset(),
        desc="Reset all window sizes"),
    Key([mod, "control"], "z", lazy.layout.maximize(),
        desc="Reset all window sizes"),

    KeyChord(
        [mod], "c",
        [
            Key([mod], "h", lazy.layout.grow_left()),
            Key([mod], "j", lazy.layout.grow_down()),
            Key([mod], "k", lazy.layout.grow_up()),
            Key([mod], "l", lazy.layout.grow_right()),
        ],
        mode="Resize"
    ),
    # Right side letters ======================================================
    Key([mod], "u", lazy.spawncmd(),
        desc="Spawn a command using the prompt widget"),

    KeyChord([mod], "p", [
        Key([], "l", lazy.spawn("mpc next"),
            desc="Mpc next song"),
        Key([], "h", lazy.spawn("mpc prev"),
            desc="Mpc previous song"),
        ],
    ),


    # {in,de}crease_ratio
    Key([mod], "j", lazy.layout.next(),
        desc="Move focus to next window"),
    Key([mod], "k", lazy.layout.previous(),
        desc="Move focus to next window"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Grow window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(),
        desc="Grow window up"),

    Key([mod], "h", lazy.layout.shrink_main(),
        desc="Decrease the master ratio"),
    Key([mod], "l", lazy.layout.grow_main(),
        desc="Increase the master ratio"),
    Key([mod, "shift"], "h", lazy.layout.shrink(),
        desc="Grow window to the left"),
    Key([mod, "shift"], "l", lazy.layout.grow(),
        desc="Grow window to the right"),

    Key([mod, "control"], "h", lazy.layout.left(),
        desc="Move focus to left"),
    Key([mod, "control"], "l", lazy.layout.right(),
        desc="Move focus to right"),
    Key([mod, "control"], "j", lazy.layout.down(),
        desc="Move focus down"),
    Key([mod, "control"], "k", lazy.layout.up(),
        desc="Move focus up"),

    Key([mod, "shift", "control"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift", "control"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift", "control"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift", "control"], "k", lazy.layout.shuffle_up(),
        desc="Move window up"),

    Key([mod, "shift", "control"], "m",
        lazy.group['SPD'].dropdown_toggle('cmus'),
        desc="Spawn a dropdown cmus"),
    Key([mod, "shift", "control"], "n",
        lazy.group['SPD'].dropdown_toggle('ncmpcpp'),
        desc="Spawn a dropdown ncmpcpp"),
    # Surroundings keys =======================================================
    Key([mod],          "Tab", lazy.next_layout(), desc="Next layout"),
    Key([mod, "shift"], "Tab", lazy.prev_layout(), desc="Previous layout"),

    Key([mod], "bracketright", lazy.screen.next_group(),
        desc="Move to the next group"),
    Key([mod], "bracketleft",  lazy.screen.prev_group(),
        desc="Move to the previous group"),

    Key([mod], "Return",          lazy.spawn(terminal),
        desc="Launch terminal"),
    Key([mod, "shift"], "Return", lazy.spawn("st"),
        desc="Launch st terminal"),

    Key([mod, "shift"], "space", lazy.layout.flip(),
        desc="filp tiling layout"),
    Key([mod, "control"], "space", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),

    # Fn and extra keys =======================================================
    Key([mod, "shift"], "Delete", lazy.shutdown(), desc="Shutdown Qtile"),
]

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

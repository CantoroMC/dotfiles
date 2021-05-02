from typing import List  # noqa: F401

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen
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
                widget.CurrentLayout(),
                widget.GroupBox(),
                widget.WindowName(),
                widget.Prompt(),
                widget.Clock(format='%Y-%m-%d %a %I:%M %p'),
                widget.Chord(
                    chords_colors={
                        'launch': ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.Systray(),
            ],
            24,
            background='#151a1e',
            opacity=0.7,
            margin=[0, 0, 0, 0],
        ),
    ),
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
          layout="monadtall"),
    Group("φ", position=6, label="φ",
          persist=True, init=True, exclusive=False,
          layout="monadtall"),
    Group("γ", position=7, label="γ",
          persist=True, init=True, exclusive=False,
          layout="monadtall"),
    Group("θ", position=8, label="θ",
          persist=True, init=True, exclusive=False,
          layout="monadtall"),
    Group("ι", position=9, label="ι",
          persist=True, init=True, exclusive=False,
          layout="monadtall"),
]
dgroups_key_binder = simple_key_binder(mod)
dgroups_app_rules = []  # type: List


layouts = [
    layout.MonadTall(),
    layout.MonadWide(),
    layout.Columns(border_focus_stack='#d75f5f'),
    layout.Max(),
    layout.Floating(
        border_width=1,
        border_focus='#b8cc52',
        border_normal='#151a1e',
    ),
]

floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
])

keys = [
    # Left side letters =======================================================
    Key([mod],          "q", lazy.restart(),     desc="Restart Qtile"),
    Key([mod, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),

    Key([mod],          "r", lazy.layout.normalize(),
        desc="Reset all window sizes"),
    Key([mod, "shift"], "r", lazy.screen.toggle_group(),
        desc="Move to the last visited group"),

    Key([mod], "t", lazy.window.toggle_floating(),
        desc="Toggle floating/tile for the current window"),

    Key([mod], "f", lazy.spawn("vivaldi-stable"),
        desc="Spawn the web browser"),
    Key([mod, "control"], "f", lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen mode for the current window"),


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

    Key([mod], "l", lazy.layout.increase_ratio(),
        desc="Increase the master ratio"),
    Key([mod], "h", lazy.layout.decrease_ratio(),
        desc="Decrease the master ratio"),
    Key([mod], "j", lazy.layout.next(),
        desc="Move focus to next window"),
    Key([mod], "k", lazy.layout.previous(),
        desc="Move focus to next window"),

    Key([mod, "shift", "control"], "h", lazy.layout.left(),
        desc="Move focus to left"),
    Key([mod, "shift", "control"], "l", lazy.layout.right(),
        desc="Move focus to right"),
    Key([mod, "shift", "control"], "j", lazy.layout.down(),
        desc="Move focus down"),
    Key([mod, "shift", "control"], "k", lazy.layout.up(),
        desc="Move focus up"),

    Key([mod, "shift"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "shift"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "shift"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([mod, "shift"], "k", lazy.layout.grow_up(),
        desc="Grow window up"),

    Key([mod, "control"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "control"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "control"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "control"], "k", lazy.layout.shuffle_up(),
        desc="Move window up"),

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

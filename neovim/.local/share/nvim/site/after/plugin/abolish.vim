" Abolishments And Abbreviations:
let g:abolish_save_file = expand('<sfile>')
if !exists(':Abolish')
  finish
endif


" Section: Command Line Abbreviations
Abolish -cmdline {,G}gr{ep,pe}  {}gr{ep}
Abolish -cmdline GIt            Git
Abolish -cmdline Wq             wq
Abolish -cmdline Wqa            wqa


" Section: Insert Mode Abbreviations
iabbrev _DATE <C-R>=strftime("%d/%m/%y %H:%M:%S")<CR>
iabbrev _tod  <C-R>=strftime("%b %d, %y")<CR><Esc>4bgUl$a


" Section: Insert Mode Abolishments
Abolish _mail                                 marco.cantoro92@outlook.it
Abolish _me                                   Marco Cantoro
Abolish _git                                  https://github.com
Abolish _aur                                  https://aur.archlinux.org
Abolish a{los,slo}                            a{lso}
Abolish anomol{y,ies}                         anomal{}
Abolish austrail{a,an,ia,ian}                 austral{ia,ian}
Abolish becuase                               because
Abolish berb                                  verb
Abolish cal{a,e}nder{,s}                      cal{e}ndar{}
Abolish {c,m}arraige{,s}                      {}arriage{}
Abolish {,in}consistan{cy,cies,t,tly}         {}consisten{}
Abolish desing                                design
Abolish destionation{,s}                      destination{}
Abolish delimeter{,s}                         delimiter{}
Abolish {,non}existan{ce,t}                   {}existen{}
Abolish despara{te,tely,tion}                 despera{}
Abolish d{e,i}screp{e,a}nc{y,ies}             d{i}screp{a}nc{}
Abolish euphamis{m,ms,tic,tically}            euphemis{}
Abolish {,end}fucntion                        {}function
Abolish hense                                 hence
Abolish {,re}impliment{,s,ing,ed,ation}       {}implement{}
Abolish improvment{,s}                        improvement{}
Abolish inherant{,ly}                         inherent{}
Abolish lastest                               latest
Abolish {les,compar,compari}sion{,s}          {les,compari,compari}son{}
Abolish {,un}nec{ce,ces,e}sar{y,ily}          {}nec{es}sar{}
Abolish {,un}orgin{,al}                       {}origin{}
Abolish persistan{ce,t,tly}                   persisten{}
Abolish proog                                 proof
Abolish referesh{,es}                         refresh{}
Abolish {,ir}releven{ce,cy,t,tly}             {}relevan{}
Abolish rec{co,com,o}mend{,s,ed,ing,ation}    rec{om}mend{}
Abolish reproducable                          reproducible
Abolish resouce{,s}                           resource{}
Abolish restraunt{,s}                         restaurant{}
Abolish ret{unr,run}                          return
Abolish seperat{e,es,ed,ing,ely,ion,ions,or}  separat{}
Abolish segument{,s,ed,ation}                 segment{}
Abolish u{,n}si{gn,ng}ed                      u{n}si{gn}ed


" Section: Text Insertion
Abolish supercali   supercalifragilisticexpialidocious
Abolish Tqbf        The quick, brown fox jumps over the lazy dog
Abolish Lipsum      Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum

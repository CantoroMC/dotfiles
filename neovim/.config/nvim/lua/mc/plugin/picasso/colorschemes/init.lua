local folderOfThisFile = (...):match("(.-)[^%.]+$")

require(folderOfThisFile .. 'colorschemes.ayu')
require(folderOfThisFile .. 'colorschemes.material')
require(folderOfThisFile .. 'colorschemes.papercolor')
require(folderOfThisFile .. 'colorschemes.srcery')

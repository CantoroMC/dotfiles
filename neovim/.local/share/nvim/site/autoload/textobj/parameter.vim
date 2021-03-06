" vim-textobj-parameter - Text objects for function parameter.

" Section: Interface

function! textobj#parameter#select_i()
	return s:select(s:const_skip_space)
endfunction

function! textobj#parameter#select_greedy_i()
	return s:select_surrounds(0)
endfunction

function! textobj#parameter#select_a()
	return s:select_surrounds(1)
endfunction


" Section: Misc

" Patterns that exclude after search
if !exists('g:textobj_parameter_ignore_syntax')
	let g:textobj_parameter_ignore_syntax = ['comment','string','character']
endif

let s:separators = [',',';']
let s:bracket_pairs = [['(',')'], ['[',']'],['{','}'],['<','>']]


function! s:select_surrounds(include_surrounds)
	let result = s:select(!s:const_skip_space)
	if type(result) == type(0)
		return 0
	endif

	let [spos, epos] = [result[1], result[2]]

	call cursor(spos[1:2])
	let [start_chr, spos_new] = s:search_pos('b', [',',';','(','<','[','{'],[])
	if a:include_surrounds
		let spos_new = s:skip_ws_backward(spos_new)
	endif
	if start_chr == ',' || start_chr == ';'
		let result[1] = s:normalize(spos_new)
		return result
	endif

	call cursor(epos[1:2])
	let [end_chr, epos_new] = s:search_pos('', [',',';',')','>',']','}'],[])
	if a:include_surrounds
		let epos_new = s:skip_ws_forward(epos_new)
	endif
	if end_chr == ',' || end_chr == ';'
		let result[2] = s:normalize(epos_new)
		return result
	endif

	return result
endfunction

function! s:skip_ws_forward(pos)
	let [lnum, col] = a:pos
	let line = getline(lnum)
	for c in range(col, len(line)-1)
		if line[c] != ' '
			return [lnum, c]
		endif
	endfor
	return a:pos
endfunction

function! s:skip_ws_backward(pos)
	let [lnum, col] = a:pos
	let line = getline(lnum)
	for c in range(col-1, 1, -1)
		if line[c-1] != ' '
			return [lnum, c+1]
		endif
	endfor
	return a:pos
endfunction

function! s:is_ignore_syntax(pos)
	let syn_name = synIDattr(synID(a:pos[0], a:pos[1],1), "name")
	for item in g:textobj_parameter_ignore_syntax
		if type(item) == type('') && syn_name =~? item
			return 1
		endif
	endfor
	return 0
endfunction

function! s:create_search_pattern(separators, bracket_pairs)
	let sep_pattern =''
	for item in a:separators
		let sep_pattern .= (len(sep_pattern) > 0? '\|': '') . '\%(' . item . '\)'
	endfor
	let bracket_pattern = ''
	for x in range(0, len(a:bracket_pairs)-1)
		let chr = a:bracket_pairs[x][1]
		let chr_rev = a:bracket_pairs[x][0]
		if x != 0| let bracket_pattern .= '\|' | endif
		let bracket_pattern .= '\%(' . chr . '\)\|\%(' . chr_rev . '\)'
	endfor
	let search_pattern = sep_pattern
	if sep_pattern != '' && bracket_pattern != ''
		let search_pattern .= '\|'
	endif
	return '\V' . search_pattern . bracket_pattern
endfunction

function! s:create_bracket_level_info(bracket_pairs)
	let chr_dict_index = {}
	let counts = []
	let bracket_pattern = ''
	for x in range(0, len(a:bracket_pairs)-1)
		let chr = a:bracket_pairs[x][1]
		let chr_rev = a:bracket_pairs[x][0]
		let chr_dict_index[chr] = [x, -1]
		let chr_dict_index[chr_rev] = [x, 1]
		let counts += [ a:bracket_pairs[x][2] ]
	endfor
	return [ chr_dict_index, counts ]
endfunction

function! s:searchpos(pat, opt)
	while 1
		let mpos = searchpos(a:pat, a:opt)
		if mpos == [0,0] || s:is_ignore_syntax(mpos) == 0
			return mpos
		endif
	endwhile
endfunction

function! s:is_inequality_or_pointer_operator(chr, bracket_pairs)
	if a:chr != '<' && a:chr != '>'
		return 0
	endif

	let [ cur_l,cur_c ] = getpos(".")[1:2]

	let line_string = getline(cur_l)
	if a:chr == '>' && cur_c >= 2 && line_string[cur_c-2] == '-'
		return 1
	endif

	let is_reverse = a:chr == '>'
	let bracket_pairs = []
	for item in a:bracket_pairs
		let bracket_pairs += [ [ item[is_reverse? 1: 0], item[is_reverse? 0: 1], 1] ]
	endfor
	let search_pattern = s:create_search_pattern([], bracket_pairs)
	let [ chr_dict_index, counts ] = s:create_bracket_level_info(bracket_pairs)

	let result = 1
	while(1)
		let mpos = s:searchpos(search_pattern, a:chr=='>'? 'bW': 'W')
		if mpos == [0,0]
			break
		endif
		let chr = getline('.')[mpos[1]-1]

		let x = chr_dict_index[chr][0]
		let counts[x] += chr_dict_index[chr][1]

		if counts[x]!=0
			continue
		endif

		let result = (chr != '>' && chr != '<')
		break
	endwhile
	call cursor(cur_l, cur_c)
	return result
endfunction

function! s:is_separator_chars(chr, separators)
	return index(a:separators, a:chr) != -1
endfunction

" Check surrounded by quote or escaped char
function! s:is_surrounded_or_escaped() abort
	return getline('.') =~ "'.*\\%" . col('.') . "c.*'"
				\ || getline('.') =~ '".*\%' . col('.') . 'c.*"'
				\ || getline('.')[col('.')-2] == '\'
endfunction

function! s:search_pos(search_opt, separators, bracket_pairs)
	let bracket_pairs = []
	for item in a:bracket_pairs
		let ritem = deepcopy(item)
		if stridx(a:search_opt, 'b') != -1
			let [ ritem[0], ritem[1] ] = [ ritem[1], ritem[0] ]
		endif
		if len(ritem) == 2
			let ritem += [1]
		endif
		let bracket_pairs += [ ritem ]
	endfor

	let cur_pos = getpos(".")[1:2]

	let search_pattern = s:create_search_pattern(a:separators, bracket_pairs)
	let [ chr_dict_index, counts ] = s:create_bracket_level_info(bracket_pairs)

	while 1
		let mpos = s:searchpos(search_pattern, a:search_opt . 'W')
		if mpos == [0,0]
			break
		endif
		let chr = getline('.')[mpos[1]-1]
		if s:is_separator_chars(chr, a:separators + [['''', ''''],['"', '"']]) != 0
			if len(counts) == 0 || count(counts,1) == len(counts)
				call cursor(cur_pos)
				return [ chr, mpos ]
			endif
			continue
		endif
		if s:is_inequality_or_pointer_operator(chr, a:bracket_pairs) != 0
			continue
		endif

		let x = chr_dict_index[chr][0]
		let counts[x] += chr_dict_index[chr][1]

		if counts[x]!=0
			continue
		endif

		call cursor(cur_pos)

		let retry = 0
		for x in range(0, len(counts)-1)
			if counts[x] <= 1
				continue
			endif
			call remove(bracket_pairs, x)
			let retry = 1
		endfor
		if retry != 0
			let search_pattern = s:create_search_pattern(a:separators, bracket_pairs)
			let [ chr_dict_index, counts ] = s:create_bracket_level_info(bracket_pairs)
			continue
		endif
		return [ chr, mpos ]
	endwhile
	call cursor(cur_pos)
	return [ '', cur_pos ]
endfunction

function! s:normalize(pos)
	let [lineNr,colNr] = a:pos
	if colNr > len(getline(lineNr))
		let lineNr = lineNr+1
		let colNr = 1
	elseif colNr <= 0
		let lineNr = lineNr-1
		let colNr = len(getline(lineNr))
	endif
	return [0, lineNr, colNr, 0]
endfunction

function! s:filter(start_chr, spos, end_chr, epos, skip_space)

	if (a:start_chr == '{' && a:end_chr == ';') || (a:start_chr == ';' && a:end_chr == '}')
		return 0
	endif

	if a:start_chr == ';' && a:end_chr == ';'
		call cursor(a:spos)
		let [ chr, pos ] = s:search_pos('b', [], s:bracket_pairs)
		if chr != '('
			return 0
		endif
	endif

	let [spos, epos] = [a:spos, a:epos]
	if a:skip_space != 0
		call cursor(spos)
		let spos = searchpos('\S', 'W')
		call cursor(epos)
		let epos = searchpos('\S', 'bW')
	else
		let spos[1] += 1
		let epos[1] -= 1
	endif

	return ['v', s:normalize(spos), s:normalize(epos)]
endfunction

function! s:get_current_cursor_text(pos)
	let chr = getline(a:pos[0])[a:pos[1]-1]
	if index(s:separators, chr) != -1
		return chr
	endif

	let text = expand('<cword>')
	if index(s:separators, text) != -1
		return text
	endif

	return chr
endfunction

function! s:select(skip_space)
	let chr = s:get_current_cursor_text(getpos('.')[1:2])

	if s:is_separator_chars(chr, s:separators) != 0 && !s:is_surrounded_or_escaped()
		return 0
	endif

	let [ bracket_pairs_b, bracket_pairs_f ] = [ [], [] ]
	for item in s:bracket_pairs
		let bracket_pairs_b += [ item + [chr == item[1]? 2: 1] ]
		let bracket_pairs_f += [ item + [chr == item[0]? 2: 1] ]
	endfor

	let [start_chr, spos] = s:search_pos('b', s:separators, bracket_pairs_b)
	let [end_chr, epos] = s:search_pos('', s:separators, bracket_pairs_f)
	if start_chr == '' || end_chr == ''
		return 0
	endif

	return s:filter(start_chr, spos, end_chr, epos, a:skip_space)
endfunction

let s:const_skip_space = 1

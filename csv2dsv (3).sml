exception emptyInputFile (*raises exception when input file is empty*)
exception UnevenFields of string (*throws exception that number of fields are not same in each row*)
exception EOLexception (*raises an exception that last line doesn't have EOL as last character*)

(*this function will take 1 and ~1 as input and make ~1 and 1 respectively*)
fun invert(x : int)= ~x

(*this will check wheather taken char is EOL or not*)
fun endline(x : int, c : char)=
	if (c= #"\n") then 1
	else 0

(*this function mainly takes a string in a list and makes delim1 as delim2 where ever needed, I know that we didn't need to take string in a list but I changed my code and earlier it was using list so I didn't changed it too much*)
(*this function working principle __ when it encounter delim1 or EOL or inverted comma then we it add inverted comma to list then that character which is to be returned and else simply add other charater character*)
fun delconv(xs : char list, delim1 : char, ys : char list, delim2 : char, no_fields : int, no_lines : int, xx : int, flag : int, start : int, last : int, eol : int)=
	if (null xs andalso eol=1) then ys
	else
		if (null xs andalso eol=0) then raise EOLexception
		else
			if (start = 1 andalso hd(xs) <> #"\"") then #"\""::delconv(xs, delim1, ys, delim2, no_fields, no_lines, xx, flag, invert(start), last, endline(eol,hd(xs)))
			else
				if (start = 1) then delconv(xs, delim1, ys, delim2, no_fields, no_lines, xx, flag, invert(start), invert(last), endline(eol,hd(xs)))
				else
					if (hd(xs) <> delim1 andalso hd(xs) <> #"\n" andalso hd(xs) <> #"\"") then hd(xs)::delconv(tl(xs), delim1, ys, delim2, no_fields, no_lines, xx, flag, start, last, endline(eol,hd(xs)))
					else
						if (hd(xs) = #"\"") then #"\"" :: delconv(tl(xs), delim1, ys, delim2, no_fields, no_lines, xx, invert(flag), start, last, endline(eol,hd(xs)))
						else
							if (hd(xs)=delim1 andalso flag = ~1) then hd(xs)::delconv(tl(xs), delim1, ys, delim2, no_fields, no_lines, xx, flag, start, last, endline(eol,hd(xs)))
							else
								if (hd(xs)=delim1 andalso flag = 1 andalso last = 1) then #"\"" :: delconv(xs, delim1, ys, delim2, no_fields, no_lines, xx, flag, start, invert(last), endline(eol,hd(xs)))
								else
									if (hd(xs)=delim1 andalso flag=1 andalso last = ~1) then delim2::delconv(tl(xs), delim1, ys, delim2, no_fields+1, no_lines, xx, flag, invert(start), invert(last), endline(eol,hd(xs)))
									else
										if (hd(xs)= #"\n" andalso flag = ~1) then hd(xs)::delconv(tl(xs), delim1, ys, delim2, no_fields, no_lines, xx, flag, start, last, endline(eol,hd(xs)))
										else
											if (hd(xs)= #"\n" andalso flag=1 andalso last = 1) then #"\"" :: delconv(xs, delim1, ys, delim2, no_fields, no_lines, xx, flag, start, invert(last), endline(eol,hd(xs)))
											else
												if (no_fields <> xx) then raise UnevenFields ("Expected: " ^ Int.toString(xx) ^ " fields, Present: " ^ Int.toString(no_fields) ^ " fields on Line " ^ Int.toString(no_lines) ^ "\n")
												else #"\n" :: delconv(tl(xs), delim1, ys, delim2, 1, no_lines+1, xx, flag, invert(start), invert(last), endline(eol,hd(xs)))


(*this function reads the given file and makes returns a list in which we have whole string*)
fun readlist1(infile)=
	let
		val ins = TextIO.openIn infile
		val result = TextIO.inputAll ins
		val result1 = [result]

	in
		result1
	end

fun readlist (infile) =
	let
		val ins = TextIO.openIn infile
		fun loop(indata)=
			case TextIO.inputLine indata of
				SOME line => line :: loop(indata)
				| NONE    => []
		val result = loop(ins)
	in
		TextIO.closeIn ins;
		if (length(result)=0) then raise emptyInputFile
		else result
	end

(*this function saves our worked file in working directory means if we give a string to this function then it will save it as OutFile name*)
fun quit1(outFile : string, s : string)=
	let
		val outStream = TextIO.openOut outFile
	in
		TextIO.output(outStream, s);
		TextIO.closeOut outStream
	end


(*to create an empty list of character which might be needed later*)
fun empty(xs : char list)=[] : char list


(*add all charater to form a single string which will be our output file, basically it takes *)
fun onebyone(xs : string list, D : string list, B : char list, delim1 : char, delim2 : char, no_fields : int, no_lines : int, xx : int)=
	if null xs then D
	else
		implode(delconv(explode(hd(xs)), delim1, empty(B), delim2, no_fields, no_lines, xx, 1, 1, 1, 1)) :: onebyone(tl(xs), D, empty(B), delim1, delim2, no_fields, no_lines+1, xx)	


(* this function just check the field count of first row*)
fun Field(xs : char list, delim1 : char, count : int, flag : int)=
	if null xs then count
	else
		if (hd(xs)= #"\"") then Field(tl(xs), delim1, count, invert(flag))
		else
			if (hd(xs)=delim1 andalso flag=1) then Field(tl(xs), delim1, count+1, flag)
			else
				Field(tl(xs),delim1,count, flag)

(*using all above function and exception we can convert our input file to desired format file*)
fun convertDelimiters(infilename : string, delim1 : char, outfilename : string, delim2 : char)=
	let
		val D = [] : string list
		val B = [] : char list	
		val L = readlist(infilename)
		val count = 1
		val xx = Field(explode(hd(L)), delim1, count,1)
	in
		quit1(outfilename, hd(onebyone(readlist1(infilename), D, B, delim1, delim2, 1, 1, xx)))

	end
	handle UnevenFields msg => print msg

fun csv2tsv(infilename, outfilename)=
	convertDelimiters(infilename, #",", outfilename, #"\t")
fun tsv2csv(infilename, outfilename)=
	convertDelimiters(infilename, #"\t", outfilename, #",")

(*Above some function or lines might be trivial but earlier I was doing TextIO.inputlines for which I needed that function but later I decided different approach of taking whole file as string so.... But we need all function so put all function*)


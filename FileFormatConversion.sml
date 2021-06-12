(* Exception emptyInputFile to be raised in case the Input File is empty  *)
exception emptyInputFile
(* Exception UnevenFields to be raised when the number of fields in a record are uneven *)
exception UnevenFields of string
(* Exception InvalidDoubleQuoteInput to be raised in case the Input File uses Invalid Double Quote Format *)
exception InvalidDoubleQuoteInput
(* Read the input file in a list of charcters *)
fun charList (infilename: string) =
    let
    val instream = TextIO.openIn infilename
    fun loop( c: char option) =
        case c of
            SOME c=> c :: loop(TextIO.input1 instream)
            | NONE => []
    in
        loop( TextIO.input1 instream)
    end;
(* Convert a list of characters to a list of strings each of which represents a new record in the file, taking in care the newline present in fields enclosed within double quotes *)
fun listofLines ( listOfChar) =
    let 
        fun group( [], s, dquoteCount) = [s]
        | group( x::xs, s, dquoteCount) =
            if(x= #"\"" andalso dquoteCount=1) then  group(xs,s^String.str(x), 0)
            else if(x= #"\"") then  group(xs,s^String.str(x), 1)
            else if ((x<> #"\n")orelse((x= #"\n") andalso dquoteCount=1)) then group(xs,s^String.str(x), dquoteCount)
            else s^String.str(x)::group( xs,"", 0)
    in
        group( listOfChar,"", 0)
    end;
(* Convert a list of records to a list of fields, making sure that fields containing delim2 are enclosed within double quotes if not so already. The listed function is used as a helper function to get a list of fields in a given line, making sure that fields containing delim2 are enclosed within double quotes if not so already. *)
fun listofFields ( [], idx, delim1, delim2, exFields) = []
| listofFields (x::xs, idx, delim1, delim2, exFields) =
	let
		fun repdoublequotes(s,i,s1,extraquote) = if (i>size(s)-1) then s1
		else if (String.sub(s,i)= #"\"") then repdoublequotes(s,i+1,s1^"\"\"",extraquote+1)
		else  repdoublequotes(s,i+1,s1^String.str(String.sub(s,i)),extraquote+1)	
		fun suredoublequotes(s) = 
			if (String.sub(s,0)= #"\"" andalso String.sub(s,String.size(s)-1)= #"\"") then s
			else "\"" ^repdoublequotes(s,0,"",0)^ "\""
		fun suredoublequotesl(s) = 
			if (String.size(s)=0) then s 
			else if (String.size(s)=1) then "\""^repdoublequotes(s,0,"",0)^"\"" 
			else if (String.sub(s,0)= #"\"" andalso String.sub(s,String.size(s)-2)= #"\"") then s
			else "\"" ^ String.substring(repdoublequotes(s,0,"",0),0,String.size(repdoublequotes(s,0,"",0))-1) ^ "\"\n"
		fun listed (x) = 
			let
				fun group([],s,fieldNo,charNo,dquoteCount,delim2exists) = 
					if (fieldNo=exFields andalso delim2exists=true) then [suredoublequotesl(s)] 
					else if (x= "\n" orelse xs=[] orelse fieldNo=exFields) then [s] 
					else (print("Expected: "^Int.toString(exFields)^" fields, Present: "^Int.toString(fieldNo)^" fields on Line "^Int.toString(idx)); raise UnevenFields("Expected: "^Int.toString(exFields)^" fields, Present: "^Int.toString(fieldNo)^" fields on Line "^Int.toString(idx)))
				| group(x::xs,s,fieldNo,charNo,dquoteCount,delim2exists) =
					if(x= #"\""andalso dquoteCount=1) then group(xs,s^String.str(x),fieldNo,charNo,0, delim2exists)
					else if(x= #"\"") then group(xs,s^String.str(x),fieldNo,charNo,1, delim2exists)
					else if (x=delim1 andalso dquoteCount=1) then group(xs,s^String.str(x),fieldNo,charNo+1,dquoteCount, delim2exists)
					else if (x=delim1 andalso delim2exists=false) then s::group(xs,"",fieldNo+1,0,0, delim2exists)
					else if (x=delim1) then suredoublequotes(s)::group(xs,"",fieldNo+1,0,0, false)
					else if (x=delim2) then group(xs,s^String.str(x),fieldNo,charNo+1,dquoteCount, true)
					else group(xs,s^String.str(x),fieldNo,charNo+1, dquoteCount, delim2exists)
			in
				group(String.explode(x),"",1,0,0, false)
			end;
	in
		listed(x)::listofFields(xs,idx+1, delim1, delim2, exFields)
	end;
(* Counts the Expected Number Fields, that is the number of fields present in line 1 *)
fun countexFields([], delim1) = 0
| countexFields(x::charListvar, delim1) = 
	let
		fun group([], dquoteCount) = 1
		| group(x::xs,dquoteCount) =
			if(x= #"\""andalso dquoteCount=1) then group(xs,0)
			else if(x= #"\"") then group(xs,1)
			else if (x=delim1 andalso dquoteCount=1) then group(xs,dquoteCount)
			else if (x=delim1) then 1+group(xs,0)
			else group(xs, dquoteCount)
		in
			group(String.explode(x),0)
		end;
(* Calls the function listofFields to obtain a list of fields from the list of lines, and raises emptyInputFile exception in case the input file is empty *)
fun make_words (infilename:string, delim1, delim2) = 
	let
		val charListvar = charList(infilename);
		val listofLinesvar = listofLines(charListvar);
		val exFields= countexFields(listofLinesvar, delim1)
	in
		if length(charListvar)=0 then raise emptyInputFile
		else listofFields(listofLinesvar,1,delim1 ,delim2, exFields)
	end;
(* the printLines function prints each line while using the printFields function to print with fields separated by delim2 for each line *)
fun printLines( [] , delim2, outfile) = ()
| printLines(x::xs, delim2, outfile) =
	let
		val outstream = TextIO.openAppend outfile;
		fun printFields([]) = ()
		| printFields([x]) = TextIO.output(outstream,x)
		| printFields(x::xs) = 
			let
				val _ = TextIO.output(outstream,x^String.str(delim2));
			in
				printFields(xs)
			end;
	in
		printFields(x);
		printLines(xs, delim2, outfile)
	end;
(* Calls the function printLines *)
fun convertDelimiters(infilename, delim1, outfilename, delim2) =
	let
		val outstream = TextIO.openOut outfilename;
	in
		printLines(make_words(infilename, delim1, delim2), delim2, outfilename) 
	end;
(* Calls the function convertDelimiters with #"," as delim1 and #"\t" as delim2 *)
fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",", outfilename, #"\t")
(* Calls the function convertDelimiters with #"\t" as delim1 and #"," as delim2 *)
fun tsv2csv(incilename, outfilename) = convertDelimiters(incilename, #"\t", outfilename, #",")

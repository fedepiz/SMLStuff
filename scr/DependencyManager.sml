structure CodeUnit =
struct
	datatype codeUnit = CodeUnit of string * string list * string
	fun unitIsIndependent(CodeUnit(_,ls,_)) = ls = []
	fun unitName(CodeUnit(x,_,_)) = x
	fun unitDependencies(CodeUnit(_,x,_)) = x
	fun unitBody (CodeUnit(_,_,x)) = x
	local
		fun allLines s = 
			let fun allLines' (s,accum) = 
			case TextIO.inputLine s of
				SOME(x) => allLines'(s,x::accum)
			|	NONE => rev accum
			in allLines'(s,[]) end
		val isUse = String.isPrefix "use"

		fun separateUseStm ls = 
			let fun separateUseStm'(ls,u,nu) = 
				case ls of
					[] => (rev u,rev nu)
				|	(x::xs) => if (isUse x)
							then separateUseStm'(xs,x::u,nu)
							else separateUseStm'(xs,u,x::nu)
			in separateUseStm' (ls,[],[]) end
		fun dropWhile (p,ls) = if p(hd ls)
							 then dropWhile (p,tl ls)
							 else ls
		fun takeWhile (p,[]) = []
		  | takeWhile (p,x::xs) = if p(x) then x::takeWhile(p,xs) else []
		fun useLineToUseName line =
			let val headOff = tl(dropWhile(fn x =>not(x = #"\""),String.explode line)) 
				val tailOff = takeWhile(fn x => not(x = #"."),headOff) in
					String.implode tailOff
			end
		fun filenameWithoutExtension s = #base (OS.Path.splitBaseExt(OS.Path.file s))
	in
		fun codeUnitFromStream (name,stream) = 
			let val (useLines,otherLines) = separateUseStm(allLines stream) in 
					 CodeUnit(name,
							  map useLineToUseName useLines,
							  String.concat otherLines)
			end
		fun codeUnitFromPath path = 
			let val stream = TextIO.openIn path in
				codeUnitFromStream(filenameWithoutExtension path,stream)
			end
		fun codeUnitFromSource (name,src) = 
			let val stream = TextIO.openString src in
				codeUnitFromStream(name,stream)
			end
	end
end :>
sig
	datatype codeUnit = CodeUnit of string * string list * string
	val unitIsIndependent : codeUnit -> bool
	val unitName : codeUnit -> string
	val unitDependencies : codeUnit -> string list
	val unitBody : codeUnit -> string
	val codeUnitFromStream : string * TextIO.instream ->codeUnit
	val codeUnitFromPath : string -> codeUnit
	val codeUnitFromSource : string * string -> codeUnit
end

structure DependencyArranger =
struct
	open CodeUnit
	datatype ('a,'b) arrangmentResult = SUCCESS of 'a | FAIL of 'b
	exception CannotLinearize of codeUnit list
	local
		fun findIndependent ls = 
			let fun inner ([],accum) = NONE
				  | inner (x::xs,accum) = 
					if unitIsIndependent x
					then SOME(x,(rev accum) @ xs)
					else inner(xs,x::accum) in
				inner(ls,[])
					end
		fun takeAwayDependency dep (CodeUnit(n,ls,s)) = 
			CodeUnit(n,List.filter (fn x => not (x = unitName dep)) ls,s)
		fun orderByImport ls = 	
			let fun importOrder'([],accum) = SUCCESS (rev accum)
			  | importOrder'(units,accum) =
					case findIndependent units of
						NONE => FAIL(units)
					|	SOME(indep,rest) => 
							importOrder'(map (takeAwayDependency indep) rest,indep::accum) in
			importOrder'(ls,[]) end
	in
		fun tryArrange units = 
			case orderByImport units of
				SUCCESS(x) => SUCCESS(String.concat (map unitBody x))
			|	FAIL(x) => FAIL(x)
		fun arrange units = 
			case tryArrange units of
				SUCCESS(x) => x
			|	FAIL(x) => raise CannotLinearize(x)
	end
end :>
sig
	type codeUnit = CodeUnit.codeUnit
	datatype ('a,'b)arrangmentResult = SUCCESS of 'a | FAIL of 'b
	exception CannotLinearize of codeUnit list 
	val tryArrange : codeUnit list -> (string, codeUnit list) arrangmentResult
	val arrange : codeUnit list -> string
end

structure DependencyManager =
struct
	type config = { searchDirs : string list, ext : string,
					extraFiles : string list, target : string ,
					useImmediately : bool}
	fun mkConfig(sd,ext,files,tgt,use) = { searchDirs = sd,
										   ext = ext,
										   extraFiles = files,
										   target = tgt,
										   useImmediately = use}
	fun oneDirConfig(dir,ext,tgt,use) = mkConfig([dir],ext,[],tgt,use)
	local
		fun openDirectory x = SOME(OS.FileSys.openDir x) handle _ => NONE
		fun collectThingsInDir(dir,accum) = 
			case OS.FileSys.readDir dir of
				SOME(x) => collectThingsInDir(dir,x::accum)
			|	NONE => accum
		fun getAllThingsInDir path = 
			case openDirectory path of
				NONE => []
			|	SOME(dir) => collectThingsInDir(dir,[])
		fun onlyWithExtension(ext,[]) = []
		  | onlyWithExtension (ext,(x::xs)) =
			let val info = OS.Path.splitBaseExt(x) in 
				if(#ext info = SOME(ext))
				then x::onlyWithExtension(ext,xs)
				else onlyWithExtension(ext,xs)
			end
		fun removeDoubles ls = 
			let fun inner([],accum) = rev accum 
				  | inner(x::xs,accum) = inner(List.filter (fn y => not (x = y)) xs,x::accum) 
			in
				inner(ls,[])
			end	  
	in
		fun pathsInDirectory ext path = 
			let val fileNames = onlyWithExtension(ext,getAllThingsInDir(path))
			in
				map (fn x => String.concat [path,"/",x]) fileNames
			end	
		fun arrangeAndOutput(inputs,target) = 
		let val units = map CodeUnit.codeUnitFromPath inputs 
			val outString = DependencyArranger.arrange units in
				TextIO.output ((TextIO.openOut target), outString)
		end
		fun run (config:config) = 
			let val f = pathsInDirectory (#ext config)
				val directoryPaths = List.concat (map f (#searchDirs config))
				val allPaths = directoryPaths @ (#extraFiles config) (*I should really check them beforehand*)
				val paths = removeDoubles allPaths 
			in 
					arrangeAndOutput(paths,#target config);
					if(#useImmediately config)
					then use (#target config)
					else ()
			end	
	end
end :>
sig
	type config = { searchDirs : string list, ext : string,
					extraFiles : string list, target : string ,
					useImmediately : bool}
	val mkConfig : string list * string * string list * string * bool -> config
	val oneDirConfig : string * string * string * bool -> config
	val run : config -> unit
end

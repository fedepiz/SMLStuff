structure State =
struct
	type ('a,'s) state = 's -> 'a * 's
	fun runState(x,s) = x (s)
	fun put x = fn _ => ((),x)
	val pull = fn x => (x,x)
	fun bind (first,second) = fn s1 => let val (x,s2) = runState(first,s1) in
									runState(second x,s1)
								end
	fun seq (first,second) = bind(first, fn _ => second)
	val >>= = bind
	val >> = seq
end :>
sig
	type ('a,'s) state
	val runState : ('a,'s) state * 's -> 'a * 's
	val put : 's -> (unit,'s) state
	val pull : ('s,'s) state
	val bind : ('a,'s) state * ('a -> ('b,'s) state) -> ('b,'s) state
	val seq : ('a,'s) state * ('b,'s) state -> ('b,'s) state
	val >>= : ('a,'s) state * ('a -> ('b,'s) state) -> ('b,'s) state
	val >> : ('a,'s) state * ('b,'s) state -> ('b,'s) state
end

import
  sequtils,
  macros,
  strutils,
  sugar,
  options

type
  Matcher*[T] = proc(p: ParseState): (Match[T], ParseState) 
  ParseState* = object
    input*: seq[char]
    index*: int
  Match*[T] = object
    case success*: bool
    of true:
      matchData*: T
    of false:
      index*: int
      reason*: string

# Returns the next character, or 0 if there is no more input
proc consume*(p: ParseState): (char, ParseState) =
  if p.index >= len(p.input): return ('\0', p)
  let c = p.input[p.index]
  var p2 = p
  p2.index += 1
  return (c, p2)

proc noMoreInput*[T](p: ParseState): Match[T] = Match[T](
  success: false,
  reason: "No more input to consume.",
  index: p.index
)

proc success*[T](t: T, p: ParseState): (Match[T], ParseState) = (Match[T](
  success: true,
  matchData: t
), p)

proc addCause*[T](m: Match[T], cause: Match[system.any]): Match[T] =
  if m.success or cause.success:
    raise newException(Exception, "Attempted to call addCause with successful match")
  else:
    return Match[T](
      success: false,
      index: m.index,
      reason: m.reason & "\n Caused by: " & cause.reason
    )

proc C*(c: char): Matcher[char] =
  return proc(p: ParseState): (Match[char], ParseState) =
    let (current, update) = p.consume()
    if current == '\0':
      return (noMoreInput[char](update), p)
    if current == c:
      return success[char](c, update) 

    return (Match[char](
      success: false,
      reason: "Unexpected character, found " & current & ", expected " & c,
      index: update.index,
    ), p)

proc repeat*[T](matcher: Matcher[T], minCount: int = 1, maxCount: int = -1): Matcher[seq[T]] =
  return proc(p: ParseState): (Match[seq[T]], ParseState) =
    var ret: seq[T] = @[]
    var state = p
    while true:
      let (match, s) = matcher(state)
      state = s
      if match.success:
        ret &= match.matchData
        if maxCount != -1 and len(ret) >= maxCount:
          return success[seq[T]](ret, state)
      else:
        if len(ret) < minCount:
          return (addCause[seq[T], T](Match[seq[T]](
            success: false,
            index: state.index,
            reason: "Could not match the required amount of times."
          ), match), p)
        else:
          return success[seq[T]](ret, state)

proc anyCount*[T](m: Matcher[T]): Matcher[seq[T]] = repeat(m, minCount = 0, maxCount = -1)
proc upTo*[T](m: Matcher[T], count: int): Matcher[seq[T]] = repeat(m, minCount = 0, maxCount = count)

proc map*[T,O](matcher: Matcher[T], mapper: proc(m: T): O): Matcher[O] =
  return proc(p: ParseState): (Match[O], ParseState) =
    let (match, state) = matcher(p)
    if match.success:
      return success[O](mapper(match.matchData), state)
    else:
      return (Match[O](
        success: false,
        index: match.index,
        reason: match.reason
      ), state)

proc inOrder*[T](ms: varargs[Matcher[T]]): Matcher[seq[T]] =
  let matchers = @ms
  return proc(p: ParseState): (Match[seq[T]], ParseState) =
    var ret: seq[T] = @[]
    var state = p
    for matcher in matchers:
      let (match, s) = matcher(state)
      if match.success:
        state = s
        ret &= match.matchData
      else:
        return (cast[Match[seq[T]]](match), p)
    return success[seq[T]](ret, state)

proc `&&`*[T](m1, m2: Matcher[seq[T]]): Matcher[seq[T]] =
  return proc(p: ParseState): (Match[seq[T]], ParseState) =
    let (match1, state1) = m1(p)
    if not match1.success: return (cast[Match[seq[T]]](match1), p)
    let (match2, state2) = m2(state1)
    if not match2.success: return (cast[Match[seq[T]]](match2), p)
    return success[seq[T]](match1.matchData & match2.matchData, state2)

proc `&:`*[T](m1: Matcher[seq[T]], m2: Matcher[T]): Matcher[seq[T]] =
  return proc(p: ParseState): (Match[seq[T]], ParseState) =
    let (match1, state1) = m1(p)
    if not match1.success: return (cast[Match[seq[T]]](match1), p)
    let (match2, state2) = m2(state1)
    if not match2.success: return (cast[Match[seq[T]]](match2), p)
    return success[seq[T]](match1.matchData & match2.matchData, state2)

proc `:&`*[T](m1: Matcher[T], m2: Matcher[seq[T]]): Matcher[seq[T]] =
  return proc(p: ParseState): (Match[seq[T]], ParseState) =
    let (match1, state1) = m1(p)
    if not match1.success: return (cast[Match[seq[T]]](match1), p)
    let (match2, state2) = m2(state1)
    if not match2.success: return (cast[Match[seq[T]]](match2), p)
    return success[seq[T]](match1.matchData & match2.matchData, state2)

proc `+`*[T](m1, m2: Matcher[T]): Matcher[seq[T]] = inOrder[T](m1, m2)

proc `++`*[T1, T2](m1: Matcher[T1], m2: Matcher[T2]): Matcher[(T1,T2)] =
  return proc(p: ParseState): (Match[(T1, T2)], ParseState) =
    let (match1, state1) = m1(p)
    if not match1.success: return (cast[Match[(T1, T2)]](match1), p)
    let (match2, state2) = m2(state1)
    if not match2.success: return (cast[Match[(T1, T2)]](match2), p)
    return success[(T1, T2)]((match1.matchData, match2.matchData), state2)

proc `|`*[T](m1, m2: Matcher[T]): Matcher[T] =
  return proc(p: ParseState): (Match[T], ParseState) =
    let (match1, state1) = m1(p)
    if match1.success: return (match1, state1)
    let (match2, state2) = m2(p)
    if match2.success: return (match2, state2)
    return (Match[T](
      success: false,
      index: match2.index,
      reason: "Could not match either condition: \n\t@" & $match1.index & ": " & match1.reason.replace("\n", "\n\t") & ",\n\t@" & $match2.index & ": " & match2.reason.replace("\n", "\n\t")
    ), p)

proc `?:`*[T](m1: Matcher[system.any], m2: Matcher[T]): Matcher[T] =
  return proc(p: ParseState): (Match[T], ParseState) =
    let (match1, state1) = m1(p)
    if not match1.success: return (cast[Match[(T)]](match1), p)
    return m2(state1)

proc `:?`*[T](m1: Matcher[T], m2: Matcher[system.any]): Matcher[T] =
  return proc(p: ParseState): (Match[T], ParseState) =
    let (match1, state1) = m1(p)
    if not match1.success: return (cast[Match[T]](match1), p)
    let (match2, state2) = m2(state1)
    if not match2.success: return (cast[Match[T]](match2), p)
    return (match1, state2)

proc untilMatch*[T](matcher: Matcher[T], terminalMatcher: Matcher[system.any], minCount = 1): Matcher[seq[T]] =
  return proc(p: ParseState): (Match[seq[T]], ParseState) =
    var ret: seq[T] = @[]
    var state = p
    while true:
      let (tMatch, tState) = terminalMatcher(state)
      if tMatch.success:
        if len(ret) < minCount:
          return (Match[seq[T]](
            success: false,
            index: state.index,
            reason: "Failed condition before reaching minCount"
          ), p)
        return success[seq[T]](ret, state)
      else:
        let (match, s) = matcher(state)
        if match.success:
          state = s
          ret &= match.matchData
        else:
          if len(ret) < minCount:
            return (cast[Match[seq[T]]](match), p)
          return success[seq[T]](ret, state)


proc until*[T](matcher: Matcher[T], fail: proc(t: T): bool, minCount = 1): Matcher[seq[T]] =
  return proc(p: ParseState): (Match[seq[T]], ParseState) =
    var ret: seq[T] = @[]
    var state = p
    while true:
      let (match, s) = matcher(state)
      if match.success:
        if fail(match.matchData):
          if len(ret) < minCount:
            return (Match[seq[T]](
              success: false,
              index: s.index,
              reason: "Failed before reaching minCount"
            ), p)
          return success[seq[T]](ret, state)
        else:
          state = s
          ret &= match.matchData
      else:
        if len(ret) < minCount:
          return (cast[Match[seq[T]]](match), p)
        return success[seq[T]](ret, state)
#

proc charIn*(chars: string): Matcher[char] =
  return proc(p: ParseState): (Match[char], ParseState) =
    let (c, state) = p.consume()
    if c == '\0':
      return (noMoreInput[char](state), p)
    if c in chars:
      return success[char](c, state)
    return (Match[char](
      success: false,
      index: state.index,
      reason: "Unexpected character " & c & ", expecting one of: " & join(chars, ", ")
    ),p)

proc anyChar*(p: ParseState): (Match[char], ParseState) =
  let (c, state) = p.consume()
  if c == '\0':
    return (noMoreInput[char](state), p)
  return success(c, state)

type Empty* = object

proc ending*(p: ParseState): (Match[Empty], ParseState) =
  let (c, _) = p.consume()
  if c == '\0':
    return success(Empty(),p)
  return (Match[Empty](
      success: false,
      index: p.index,
      reason: "Unexpected character " & c
  ),p)


type Printable = concept x
  $x is string

proc asString*[T: Printable](matcher: Matcher[seq[T]]): Matcher[string] = matcher.map(ts => join(ts, ""))

converter toCharMatcher*(c: char): Matcher[char] = C(c)
converter toStringMatcher*(s: string): Matcher[seq[char]] = inOrder(s.map(c => C(c)))

proc S*(s: string): Matcher[string] =
  return toStringMatcher(s).asString

proc match*[T](input: string, matcher: Matcher[T]): Match[T] =
  var state = ParseState(
    input: toSeq(input.items),
    index: 0
  )
  let (match, update) = matcher(state)
  if match.success and update.index != len(input):
    return Match[T](
      success: false,
      index: update.index,
      reason: "Trailing input ->" & input[update.index..^1]
    )
  return match


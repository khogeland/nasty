import
  sequtils,
  strutils,
  sugar,
  options

type
  Matcher[T] = func(p: ParseState): (Match[T], ParseState)
  ParseState = object
    input: seq[char]
    index: int
  Match[T] = object
    case success: bool
    of true:
      matchData: T
    of false:
      index: int
      reason: string

# Returns the next character, or 0 if there is no more input
func consume(p: ParseState): (char, ParseState) =
  if p.index >= len(p.input): return ('\0', p)
  let c = p.input[p.index]
  var p2 = p
  p2.index += 1
  return (c, p2)

func noMoreInput[T](p: ParseState): Match[T] = Match[T](
  success: false,
  reason: "No more input to consume.",
  index: p.index
)

func success[T](t: T, p: ParseState): (Match[T], ParseState) = (Match[T](
  success: true,
  matchData: t
), p)

func addCause[T](m: Match[T], cause: Match[system.any]): Match[T] =
  if m.success or cause.success:
    raise newException(Exception, "Attempted to call addCause with successful match")
  else:
    return Match[T](
      success: false,
      index: m.index,
      reason: m.reason & "\n Caused by: " & cause.reason
    )

func C(c: char): Matcher[char] =
  return func(p: ParseState): (Match[char], ParseState) =
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

func repeat[T](matcher: Matcher[T], minCount: int = 1, maxCount: int = -1): Matcher[seq[T]] =
  return func(p: ParseState): (Match[seq[T]], ParseState) =
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

func anyCount[T](m: Matcher[T]): Matcher[seq[T]] = repeat(m, minCount = 0, maxCount = -1)
func upTo[T](m: Matcher[T], count: int): Matcher[seq[T]] = repeat(m, minCount = 0, maxCount = count)

func map[T,O](matcher: Matcher[T], mapper: func(m: T): O): Matcher[O] =
  return func(p: ParseState): (Match[O], ParseState) =
    let (match, state) = matcher(p)
    if match.success:
      return success[O](mapper(match.matchData), state)
    else:
      return (Match[O](
        success: false,
        index: match.index,
        reason: match.reason
      ), state)

func inOrder[T](ms: varargs[Matcher[T]]): Matcher[seq[T]] =
  let matchers = @ms
  return func(p: ParseState): (Match[seq[T]], ParseState) =
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

func `&`[T](m1, m2: Matcher[seq[T]]): Matcher[seq[T]] =
  return func(p: ParseState): (Match[seq[T]], ParseState) =
    let (match1, state1) = m1(p)
    if not match1.success: return (cast[Match[seq[T]]](match1), p)
    let (match2, state2) = m2(state1)
    if not match2.success: return (cast[Match[seq[T]]](match2), p)
    return success[seq[T]](match1.matchData & match2.matchData, state2)

func `&`[T](m1: Matcher[seq[T]], m2: Matcher[T]): Matcher[seq[T]] =
  return func(p: ParseState): (Match[seq[T]], ParseState) =
    let (match1, state1) = m1(p)
    if not match1.success: return (cast[Match[seq[T]]](match1), p)
    let (match2, state2) = m2(state1)
    if not match2.success: return (cast[Match[seq[T]]](match2), p)
    return success[seq[T]](match1.matchData & match2.matchData, state2)

func `&`[T](m1, m2: Matcher[T]): Matcher[seq[T]] = inOrder[T](m1, m2)

func `|`[T](m1, m2: Matcher[T]): Matcher[T] =
  return func(p: ParseState): (Match[T], ParseState) =
    let (match1, state1) = m1(p)
    if match1.success: return (match1, state1)
    let (match2, state2) = m2(p)
    if match2.success: return (match2, state2)
    return (Match[T](
      success: false,
      index: match2.index,
      reason: "Could not match either condition: \n\t@" & $match1.index & ": " & match1.reason.replace("\n", "\n\t") & ",\n\t@" & $match2.index & ": " & match2.reason.replace("\n", "\n\t")
    ), p)

func until[T](matcher: Matcher[T], fail: func(t: T): bool, minCount = 1): Matcher[seq[T]] =
  return func(p: ParseState): (Match[seq[T]], ParseState) =
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

func charIn(chars: string): Matcher[char] =
  return func(p: ParseState): (Match[char], ParseState) =
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

func anyChar(p: ParseState): (Match[char], ParseState) =
  let (c, state) = p.consume()
  if c == '\0':
    return (noMoreInput[char](state), p)
  return success[char](c, state)

func asString[T](matcher: Matcher[seq[T]]): Matcher[string] = matcher.map(t => join(t, ""))

converter toMatcher(c: char): Matcher[char] = C(c)
converter toMatcher(s: string): Matcher[seq[char]] = inOrder(s.map(c => C(c)))

func S(s: string): Matcher[string] =
  return toMatcher(s).asString

func match[T](input: string, matcher: Matcher[T]): Match[T] =
  var state = ParseState(
    input: toSeq(input.items),
    index: 0
  )
  let (match, update) = matcher(state)
  if match.success and update.index != len(input):
    return Match[T](
      success: false,
      index: update.index,
      reason: "Trailing input: " & input[update.index..^1]
    )
  return match

let matchA = 'a'
let matchBs = 'b'.repeat

echo "a".match(matchA)
echo "b".match(matchBs)
echo "bbbbb".match(matchBs.asString)

echo "bbbaaaaaaabbbbbb".match('b'.repeat.asString & 'a'.repeat.asString & 'b'.repeat.asString)
echo "arst".match("ars" & 't')
echo "bbbaaaaaaabababababbababbbbb".match( ('a'.repeat.asString | 'b'.repeat.asString).repeat )

func describe(m: Matcher[string], desc: string): Matcher[string] = m.map(s => desc & "(" & s.unescape("", "") & ")")

let whitespace = charIn(" \n\r\t\b\f").anyCount.asString
let escape = ('\\' & anyChar).asString
let doubleQuotesNoEscape = anyChar.until(c => c in "\\\"").asString
let doubleQuotesString = (doubleQuotesNoEscape | escape).repeat.asString.map(s => unescape(s, "", ""))
let singleQuotesNoEscape = anyChar.until(c => c in "\\'").asString
let singleQuotesString = (singleQuotesNoEscape | escape).repeat.asString.map(s => unescape(s, "", ""))
let AND = S("and")
let OR = S("or")
let NOT = S("not")
let key = anyChar.until(c => c in "()!= \t\n\b\r\f").asString.describe("key")
let value = ((S("\"") & doubleQuotesString & S("\"")) | (S("'") & singleQuotesString & S("'"))).asString.describe("value")
let equals = (key & whitespace & S("=") & whitespace & value).asString.describe("equals")
let notEquals = (key & whitespace & S("!=") & whitespace & value).asString.describe("notEquals")
let statement = equals | notEquals

var parens, statementOrNestedExpression, notExpression, negatedExpression, andExpression, orExpression, getOrExpression: func(): Matcher[string]

parens = () => (S("(") & whitespace & orExpression() & whitespace & S(")")).asString.describe("parens")
statementOrNestedExpression = () => statement | parens()
notExpression = () => statementOrNestedExpression() | negatedExpression()
andExpression = () => (notExpression() & (whitespace & AND & whitespace & notExpression()).repeat(minCount = 0).asString).repeat.asString.describe("andExpression")

orExpression = proc(): Matcher[string] =
  return proc(p: ParseState): (Match[string], ParseState) =
    let (match, state) = ((andExpression() & (whitespace & OR & whitespace & orExpression()).repeat(minCount = 0).asString).repeat.asString.describe("orExpression"))(p)
    if match.success: return success[string](match.matchData, state)
    return (Match[string](
      success: false,
      index: match.index,
      reason: "Could not parse expression: " & match.reason
    ), p)

negatedExpression = proc(): Matcher[string] =
  return proc(p: ParseState): (Match[string], ParseState) =
    let (match, state) = ((NOT & whitespace & notExpression()).asString.describe("negatedExpression"))(p)
    if match.success: return success[string](match.matchData, state)
    return (Match[string](
      success: false,
      index: match.index,
      reason: "Could not parse expression: " & match.reason
    ), p)

let filter = (whitespace & orExpression() & whitespace).asString

proc test(input: string, matcher: Matcher[system.any]) =
  echo input
  let result = input.match(matcher)
  if not result.success:
    echo "failure @" & $result.index & ":"
    echo result.reason
  else: echo "success: " & $result.matchData

test("anyrep", anyChar.repeat)
test("doublerep", doubleQuotesNoEscape.repeat)
test("doubleesc", doubleQuotesNoEscape)
test("doublestr", doubleQuotesString)
test("key", key)
test("'val'", value)
test("\\'", escape)
test("sqs\\'s", singleQuotesString)
test("'QsqssQ'", S("'") & singleQuotesString & S("'"))
test("keywhitespace=", key & whitespace & S("="))
test("='whitespacevalue'", S("=") & whitespace & value)
test("='nowhitespacevalue'", S("=") & value)
test("'whitespacevalue'", whitespace & value)
test("key='val'", equals)
test("key=\"val\"", equals)

test("foo='bar' and foo='bar'", orExpression())
test("foo='bar' and (foo='bar')", orExpression())
test("foo='bar' and foo='bar'", andExpression())
test("(qux='no_match' or qux='ba\"ab' )", notExpression())
test("(baz='baz' and (qux='no_match'))", notExpression())
test("(A = 'b' and A = 'b' and (qux='no_match'))", notExpression())
test("(A = '1\\'2' and baz='baz' and (qux='no_match' or qux='ba\"ab' ))", notExpression())

let foo = "foo='bar' and (A = '1\\'2' and baz='baz' and (qux='no_match' or qux='ba\"ab' ) ) and q='\n\b\f\r\t'"
test(foo, filter)

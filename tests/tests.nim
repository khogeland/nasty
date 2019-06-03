import
  sugar,
  unittest,
  strutils,
  ../src/nasty

func describe(m: Matcher[string], desc: string): Matcher[string] = m.map(s => desc & "(" & s.unescape("", "") & ")")

let whitespace = charIn(" \n\r\t\b\f").anyCount.asString
let escape = ('\\' + anyChar).asString
let doubleQuotesNoEscape = anyChar.until(c => (c in "\\\"")).asString
let doubleQuotesString = (doubleQuotesNoEscape | escape).repeat.asString.map(s => unescape(s, "", ""))
let singleQuotesNoEscape = anyChar.until(c => c in "\\'").asString
let singleQuotesString = (singleQuotesNoEscape | escape).repeat.asString.map(s => unescape(s, "", ""))
let AND = S("and")
let OR = S("or")
let NOT = S("not")
let key = anyChar.until(c => c in "()!= \t\n\b\r\f").asString.describe("key")
let value = ((S("\"") + doubleQuotesString &: S("\"")) | (S("'") + singleQuotesString &: S("'"))).asString.describe("value")
let equals = (key + whitespace &: S("=") &: whitespace &: value).asString.describe("equals")
let notEquals = (key + whitespace &: S("!=") &: whitespace &: value).asString.describe("notEquals")
let statement = equals | notEquals

var parens, statementOrNestedExpression, notExpression, negatedExpression, andExpression, orExpression, getOrExpression: func(): Matcher[string]

parens = () => (S("(") + whitespace &: orExpression() &: whitespace &: S(")")).asString.describe("parens")
statementOrNestedExpression = () => statement | parens()
notExpression = () => statementOrNestedExpression() | negatedExpression()
andExpression = () => (notExpression() + (whitespace + AND &: whitespace &: notExpression()).repeat(minCount = 0).asString).repeat.asString.describe("andExpression")

orExpression = proc(): Matcher[string] =
  return proc(p: ParseState): (Match[string], ParseState) =
    let (match, state) = ((andExpression() + (whitespace + OR &: whitespace &: orExpression()).repeat(minCount = 0).asString).repeat.asString.describe("orExpression"))(p)
    if match.success: return success[string](match.matchData, state)
    return (Match[string](
      success: false,
      index: match.index,
      reason: "Could not parse expression: " & match.reason
    ), p)

negatedExpression = proc(): Matcher[string] =
  return proc(p: ParseState): (Match[string], ParseState) =
    let (match, state) = ((NOT + whitespace &: notExpression()).asString.describe("negatedExpression"))(p)
    if match.success: return success[string](match.matchData, state)
    return (Match[string](
      success: false,
      index: match.index,
      reason: "Could not parse expression: " & match.reason
    ), p)

let filter = (whitespace + orExpression() &: whitespace).asString

proc test(input: string, matcher: Matcher[system.any]) =
  let result = input.match(matcher)
  if not result.success:
    echo input
    echo "failure @" & $result.index & ": " & result.reason
  check(result.success)

suite "nasty test":
  test "all the things":
    test("aa", 'a' + anyChar)
    test("anyrep", anyChar.repeat)
    test("doublerep", doubleQuotesNoEscape.repeat)
    test("doubleesc", doubleQuotesNoEscape)
    test("doublestr", doubleQuotesString)
    test("key", key)
    test("'val'", value)
    test("\\", '\\')
    test("\\'", '\\' & '\'')
    test("\\'", escape)
    test("sqs\\'s", singleQuotesString)
    test("'QsqssQ'", S("'") + singleQuotesString &: S("'"))
    test("keywhitespace=", key + whitespace &: S("="))
    test("='whitespacevalue'", S("=") + whitespace &: value)
    test("='nowhitespacevalue'", S("=") + value)
    test("'whitespacevalue'", whitespace + value)
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

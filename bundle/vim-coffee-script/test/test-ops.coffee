# Various operators
abc instanceof def
typeof abc
delete abc
abc::def

abc + def
abc - def
abc * def
abc / def
abc % def
abc & def
abc | def
abc ^ def
abc >> def
abc << def
abc >>> def
abc ? def
abc && def
abc and def
abc || def
abc or def

abc += def
abc -= def
abc *= def
abc /= def
abc %= def
abc &= def
abc |= def
abc ^= def
abc >>= def
abc <<= def
abc >>>= def
abc ?= def
abc &&= def
abc ||= def

abc and= def
abc or= def

abc.def.ghi
abc?.def?.ghi

abc < def
abc > def
abc = def
abc == def
abc != def
abc <= def
abc >= def

abc++
abc--
++abc
--abc

# Nested operators
abc[def] = ghi
abc[def[ghi: jkl]] = 42
@abc[def] = ghi

abc["#{def = 42}"] = 42
abc["#{def.ghi = 42}"] = 42
abc["#{def[ghi] = 42}"] = 42
abc["#{def['ghi']}"] = 42

# Object assignments
abc =
  def: 123
  DEF: 123
  @def: 123
  Def: 123
  'def': 123
  42: 123

# Operators shouldn't be highlighted
vector=
wand=

abc+++
abc---
abc ** def
abc &&& def
abc ^^ def
abc ===== def
abc <==== def
abc >==== def
abc +== def
abc =^= def

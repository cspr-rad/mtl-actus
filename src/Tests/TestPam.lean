import Actus
import Tests.Runner
import Tests.Fixtures.Words.Pam

def pamAccepts : Bool := (PAM.automaton pamTerms).accepts _ word

def testPam : TestM Unit := assert pamAccepts "The given PAM word failed on the PAM automaton"

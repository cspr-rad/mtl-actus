import Actus
import Tests.Runner
import Tests.Fixtures.Words.Pam

def pamAccepts : Bool := (PAM.automaton somePam).accepts _ somePamWord

def testPam : TestM Unit := assert pamAccepts "The given PAM word failed on the PAM automaton"

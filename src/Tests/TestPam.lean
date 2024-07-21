import Actus
import Tests.Runner
import Tests.Fixtures.Words.Pam

namespace Pam
  def accepts : Bool := (PAM.automaton pamTerms).accepts _ word
  def test : TestM Unit := assert accepts "The given PAM word failed on the PAM automaton"
end Pam

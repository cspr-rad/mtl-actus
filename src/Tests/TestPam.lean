import Actus
import Tests.Runner
import Tests.Fixtures.Words.Pam

namespace Pam
  def accepts : Bool := (PAM.automaton PamAtMaturity4.terms).accepts _ PamAtMaturity4.word
  def test : TestM Unit := assert accepts "The given PAM word failed on the PAM automaton"
  namespace Debug
    def acceptsIo : IO Bool := (PAM.automaton PamAtMaturity4.terms).acceptsDebug _ PamAtMaturity4.word
  end Debug
end Pam

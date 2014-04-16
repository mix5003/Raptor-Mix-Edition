with MSSyst.String;
use MSSyst.String;
with Interpreter;
use Interpreter;
package Suggestions is

   function Suggestion(
      Text : in MSSyst.String.Ref;
      Kind : in Scenario) return Suggestion_Result;

end Suggestions;

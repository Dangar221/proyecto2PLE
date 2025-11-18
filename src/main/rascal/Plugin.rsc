module Plugin

import AST;
import Checker;
import IO;
import Implode;
import ParseTree;
import Relation;
import Syntax;
import util::LanguageServer;
import util::IDEServices;
import analysis::typepal::TypePal;
import util::Reflective;

PathConfig pcfg = getProjectPathConfig(|project://proyecto3|);
Language tdslLang = language(pcfg, "ALU", "alu", "Plugin", "contribs");

set[LanguageService] contribs() = {
  parser(start[Program](str program, loc src){
    return parse(#start[Program], program, src, allowAmbiguity = true);
  }),
  
  summarizer(Summary (loc l, start[Program] input) {
    AST::Program ast = implode(#AST::Program, input);
    tm = typeCheck(ast);
    
    return summary(l, 
      messages = {<m.at, m> | m <- getMessages(tm), !(m is info)}
    );
  })
};

void main() {
  registerLanguage(tdslLang);
}
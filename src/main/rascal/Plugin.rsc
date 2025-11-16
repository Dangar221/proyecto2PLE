module Plugin

import IO;
import ParseTree;
import util::Reflective;
import util::IDEServices;
import util::LanguageServer;
import Relation;
import Syntax;

PathConfig pcfg = getProjectPathConfig(|project://proyecto2|);
Language tdslLang = language(pcfg, "ALU", "alu", "Plugin,", "contribs");

set[LanguageService] contribs() = {
  parser(start[Program](str program, loc src){
    println("Run parser");
    return parse(#start[Program], program, src, allowAmbiguity = true);
  })
};

void demo() {
  println("ALU Language Plugin Loaded");
  println("Use Parser module to parse ALU programs");
}

void main() {
  registerLanguage(tdslLang);
}
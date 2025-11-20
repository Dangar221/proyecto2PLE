module Plugin

import IO;
import ParseTree;
import util::Reflective;
import util::IDEServices;
import util::LanguageServer;
import Syntax;
import Checker;

PathConfig pcfg = getProjectPathConfig(|project://proyecto3|);

Summary aluSummarizer(loc l, start[Program] input) {
    try {
        TModel tm = typeCheck(input.top);
        return summary(l, 
            messages = tm.messages,
            definitions = tm.useDef
        );
    } catch value e: {
        println("TypePal error: <e>");
        return summary(l);
    }
}

set[LanguageService] contribs() = {
    parser(start[Program](str program, loc src){
        return parse(#start[Program], program, src, allowAmbiguity = true);
    }),
    summarizer(aluSummarizer)
};

Language tdslLang = language(pcfg, "ALU", "alu", "Plugin", "contribs");

void main() {
    registerLanguage(tdslLang);
}
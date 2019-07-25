/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package sentenceanalyzer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import org.cogroo.analyzer.ComponentFactory;
import org.cogroo.analyzer.Analyzer;
import org.cogroo.text.Chunk;
import org.cogroo.text.Document;
import org.cogroo.text.Sentence;
import org.cogroo.text.SyntacticChunk;
import org.cogroo.text.Token;
import org.cogroo.text.impl.DocumentImpl;

/**
 *
 * @author Cleiton
 */
public class SentenceAnalyzer {

    // the CoGrOO pipe instance
    private Analyzer cogroo;

    // Cogroo Document Object
    private Document document;

    public SentenceAnalyzer() {
        /*
         * The following command creates a component factory given a locale. The
         * locale will be resolved as a configuration file in the classpath with the
         * following pattern: /models_lang_COUNTRY. Another option is to use the
         * method ComponentFactory.create(InputStream) directly.
         */
        ComponentFactory factory = ComponentFactory.create(new Locale("pt", "BR"));

        /*
         * Create the default pipe, which is complete, including from sentence
         * detection to featurization.
         */
        cogroo = factory.createPipe();

        // Create a document and set the text.
        document = new DocumentImpl();
    }

    public String getInfoSentence(String sentence) {
        document.setText(sentence);

        // analyze it
        cogroo.analyze(document);
        
        // Sentence word list 
        List<String> wordsList = new ArrayList<>();

        // Sentence postag list
        List<String> postagList = new ArrayList<>();

        // Sentence chunk list
        List<String> chunkList = new ArrayList<>();
        
        // Sentence chunk list
//        List<String> lemmaList = new ArrayList<>();
        
        for (Sentence sent: document.getSentences()){
            
            for (Token token : sent.getTokens()) {
                    wordsList.add(token.getLexeme());
                    postagList.add(token.getPOSTag());
                    chunkList.add(token.getChunkTag());
//                    System.out.println(token.getFeatures());
//                    System.out.println(token.getPOSTag());
//                    lemmaList.add(Arrays.toString(token.getLemmas()));
            }
            
//            for (SyntacticChunk structure : sent.getSyntacticChunks()) {
//                System.out.println(structure.getStart()); 
//                System.out.println(structure.getEnd());
//                System.out.println(structure.getTag());
//                System.out.println(structure.getTokens());
//            }
            
//            for (Chunk chunk : sent.getChunks()) {
//                    
//                for (Token token : chunk.getTokens()) {
////                    wordsList.add(token.getLexeme());
////                    postagList.add(token.getPOSTag());
////                    chunkList.add(token.getChunkTag());
//////                    lemmaList.add(Arrays.toString(token.getLemmas()));
//                }
//            }
        }

//        System.out.println(wordsList);
//        System.out.println(postagList);
//        System.out.println(chunkList);
//        System.out.print(lemmaList);
//        System.out.println(sentenceProcessing(wordsList, postagList, chunkList).toString());
            
        return sentenceProcessing(wordsList, postagList, chunkList).toString();
    }

    public ArrayList<List> sentenceProcessing(List words, List postag, List chunks) {
        // ArrayList with all information
        List<String> infoWords = new ArrayList<>();
        List<String> infoPostag = new ArrayList<>();
        List<String> infoChunk = new ArrayList<>();
        List<String> infoLemma = new ArrayList<>();

        ArrayList<List> infoList = new ArrayList<>();

        for (int iCount = 0; iCount < words.size(); iCount++) {

            if (words.size() - iCount != 1) {
                if (words.get(iCount).equals("em")) {
                    if (words.get(iCount + 1).equals("o")) {
                        
                        
                        words.set(iCount, "no");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("os")) {
                        
                        
                        words.set(iCount, "nos");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("a")) {
                        
                        
                        words.set(iCount, "na");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("as")) {
                        
                        
                        words.set(iCount, "nas");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("ele")) {
                        
                        
                        words.set(iCount, "nele");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("eles")) {
                        
                        
                        words.set(iCount, "neles");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("ela")) {
                        
                        
                        words.set(iCount, "nela");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("elas")) {
                        
                        
                        words.set(iCount, "nelas");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("este")) {
                        
                        
                        words.set(iCount, "neste");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("estes")) {
                        
                        
                        words.set(iCount, "nestes");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("esta")) {
                        
                        
                        words.set(iCount, "nesta");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("estas")) {
                        
                        
                        words.set(iCount, "nestas");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("isto")) {
                        
                        
                        words.set(iCount, "nisto");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("isso")) {
                        
                        
                        words.set(iCount, "nisso");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("esse")) {
                        
                        
                        words.set(iCount, "nesse");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("esses")) {
                        
                        
                        words.set(iCount, "nesses");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("essa")) {
                        
                        
                        words.set(iCount, "nessa");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("essas")) {
                        
                        
                        words.set(iCount, "nessas");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquele")) {
                        
                        
                        words.set(iCount, "naquele");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aqueles")) {
                        
                        
                        words.set(iCount, "naqueles");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquela")) {
                        
                        
                        words.set(iCount, "naquela");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquelas")) {
                        
                        
                        words.set(iCount, "naquelas");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquilo")) {
                        
                        
                        words.set(iCount, "naquilo");
                        words.set(iCount + 1, "");
                    }
                }

                if (words.get(iCount).equals("de")) {
                    if (words.get(iCount + 1).equals("o")) {
                        
                        
                        words.set(iCount, "do");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("os")) {
                        
                        
                        words.set(iCount, "dos");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("a")) {
                        
                        
                        words.set(iCount, "da");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("as")) {
                        
                        
                        words.set(iCount, "das");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("ele")) {
                        
                        
                        words.set(iCount, "dele");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("eles")) {
                        
                        
                        words.set(iCount, "deles");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("ela")) {
                        
                        
                        words.set(iCount, "dela");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("elas")) {
                        
                        
                        words.set(iCount, "delas");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("este")) {
                        
                        
                        words.set(iCount, "deste");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("estes")) {
                        
                        
                        words.set(iCount, "destes");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("esta")) {
                        
                        
                        words.set(iCount, "desta");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("estas")) {
                        
                        
                        words.set(iCount, "destas");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("isto")) {
                        
                        
                        words.set(iCount, "disto");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("esse")) {
                        
                        
                        words.set(iCount, "desse");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("esses")) {
                        
                        
                        words.set(iCount, "desses");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("essa")) {
                        
                        
                        words.set(iCount, "dessa");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("essas")) {
                        
                        
                        words.set(iCount, "dessas");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("isso")) {
                        
                        
                        words.set(iCount, "disso");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquele")) {
                        
                        
                        words.set(iCount, "daquele");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aqueles")) {
                        
                        
                        words.set(iCount, "daqueles");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquela")) {
                        
                        
                        words.set(iCount, "daquela");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquelas")) {
                        
                        
                        words.set(iCount, "daquelas");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquilo")) {
                        
                        
                        words.set(iCount, "daquilo");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aí")) {
                        
                        
                        words.set(iCount, "daí");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("ali")) {
                        
                        
                        words.set(iCount, "dali");
                        words.set(iCount + 1, "");
                    } 
                }

                if (words.get(iCount).equals("a")) {
                    if (words.get(iCount + 1).equals("o")) {
                        
                        
                        words.set(iCount, "ao");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("os")) {
                        
                        
                        words.set(iCount, "aos");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("a")) {
                        
                        
                        words.set(iCount, "à");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("as")) {
                        
                        
                        words.set(iCount, "às");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquele")) {
                        
                        
                        words.set(iCount, "àquele");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aqueles")) {
                        
                        
                        words.set(iCount, "àqueles");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquela")) {
                        
                        
                        words.set(iCount, "àquela");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquelas")) {
                        
                        
                        words.set(iCount, "àquelas");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("aquilo")) {
                        
                        
                        words.set(iCount, "àquilo");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("onde")) {
                        
                        
                        words.set(iCount, "aonde");
                        words.set(iCount + 1, "");
                    }
                }

                if (words.get(iCount).equals("por")) {
                    if (words.get(iCount + 1).equals("o")) {
                        
                        
                        words.set(iCount, "pelo");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("os")) {
                        
                        
                        words.set(iCount, "pelos");
                        words.set(iCount + 1, "");
                    } else if (words.get(iCount + 1).equals("a")) {
                        
                        
                        words.set(iCount, "pela");
                        words.set(iCount + 1, "");

                    } else if (words.get(iCount + 1).equals("as")) {
                        
                        
                        words.set(iCount, "pelas");
                        words.set(iCount + 1, "");
                    }
                }
                
                
            }
            
                infoWords.add(words.get(iCount).toString());
                infoPostag.add(postag.get(iCount).toString());
                infoChunk.add(chunks.get(iCount).toString());
            
//                  To see the code below uncomment
//                System.out.print("Valor iCount: " + iCount + " ");
//                System.out.print(infoWords.get(iCount) + " ");
//                System.out.print(infoPostag.get(iCount) + " ");
//                System.out.println(infoChunk.get(iCount) + "\n");
        }

        infoList.add(infoWords);
        infoList.add(infoPostag);
        infoList.add(infoChunk);
        infoList.add(infoLemma);

        return infoList;
    }
    
    public String getLemma (String sentence)
    {
        List<String> lemmaList = new ArrayList<>();
        
        document.setText(sentence);

        // analyze it
        cogroo.analyze(document);
        
        for (Sentence sent: document.getSentences()){
            
            for (Token token : sent.getTokens()) {
                    
                  lemmaList.add(Arrays.toString(token.getLemmas()));
            }
        }
        
        return lemmaList.toString();
    }
    
    public String getFeature (String sentence)
    {
        List<String> featureList = new ArrayList<>();
        
        document.setText(sentence);

        cogroo.analyze(document);
        
        for (Sentence sent: document.getSentences()){
            
            for (Token token : sent.getTokens()) {
                    
                  featureList.add(token.getFeatures());
            }
        }
        
        return featureList.toString();
    }
    
    public String getPostag (String sentence)
    {
        List<String> postagList = new ArrayList<>();
        
        document.setText(sentence);

        cogroo.analyze(document);
        
        for (Sentence sent: document.getSentences()){
            
            for (Token token : sent.getTokens()) {
                    
                  postagList.add(token.getPOSTag());
            }
        }
        
        return postagList.toString();
    }
    
    public static void main(String[] args) {
        // TODO code application logic here

//        Sentence example. To see the result uncomment the lines below
//        SentenceAnalyzer an = new SentenceAnalyzer();
//        an.getInfoSentence("O governo alega que uma lei proíbe \" símbolos religiosos ostensivos\" nas escolas mas o uso de crucifixos não tem sido reprimido.");
//        an.getInfoSentence("O professor não deveria ter postado a nota.");
//        an.getInfoSentence("Cleiton vai passear com os amigos, sem que faça as atividades.");
//        System.out.println(an.getPostag("poderia ter postado"));

    }
}

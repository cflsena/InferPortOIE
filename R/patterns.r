## Regular Expressions for relations and arguments

VERB  <- "(v-fin |v-inf |v-pcp |v-ger )"
ADV   <- "(adv )"
NOUN  <- "(n |prop )"
ADJ   <- "(adj |n-adj |num )"
PRON  <- "(pron-pers |pron-det |pron-indp )"

DET         <- "(det |art | spec )"
PARTICLE    <- "(conj-s |conj-c |intj )"
PREP        <- "(prp )"
INF_MARKER  <- "(EC |- )"

V <- paste0("(", VERB, PARTICLE, "?", ADV, "?)")

W <- paste0("(", NOUN, "|", ADJ, "|", ADV, "|", PRON, "|", DET, ")")

P <- paste0("(", PREP, "|", PARTICLE, "|", INF_MARKER, ")")

VP <- paste0("(", V, P, ")")

VWP <- paste0("(", V, W, "*", P, ")")

PATTERN <- paste0("(", VWP, "|", VP, "|", V, ")")

CHUNK <- "B-NP (I-NP )*(B-PP B-NP (I-NP )*)*"

## Regular expressions for transitivity and symmetry.

#Transitivity

####
# 
# Rule 1: |ARG1| |E-UM    | |ARG2| |QUALQUER RELACAO  | |ARG3| OU |ARG1| |QUALQUER RELACAO| |ARG2| |E-UM    | |ARG3|
# Rule 2: |ARG1| |PARTE-DE| |ARG2| |SIN               | |ARG3| OU |ARG1| |SIN             | |ARG2| |PARTE-DE| |ARG3|
# Rule 3: |ARG1| |LOC     | |ARG2| |PARTE-DE          | |ARG3| OU |ARG1| |LOC             | |ARG2| |PARTE-DE| |ARG3|
# Rule 4: |ARG1| |LOC     | |ARG2| |SIN               | |ARG3| OU |ARG1| |SIN             | |ARG2| |LOC     | |ARG3|
#
####

IS_A    <- "((ser )(um ))"
SIN     <- "((ser )((chamar )|(apelidar )|(conhecer ))((de ){1}|(como ){1}))"
PART_OF <- "(((ser )|(fazer ))(parte )(de ))"
LOC     <- "((ser )(o ))|(((ser ){0,1}|(estar ){0,1}))((localizar )|(situar )|(sediar ))(em )"

#Symmetry

####
#
# Rule 1: |ARG1| |SYM| |ARG2|
#
###

SYM_VERBS <-"((acordar )|(arguir )|(argumentar )|(brigar )|(brindar )|(cochichar )|(comerciar )|(conchavar )|(confabular )|(confraternizar )|(contracenar )|(conversar )|(copular )|(dialogar )|(duelar )|(ficar )|(flertar )|(fornicar )|(interagir )|(lutar )|(mancomunar )|(noivar )|(pactuar )|(reatar )|(tabelar )|(casar ))+"

SYM_1 <- "((ser )(um ))"
SYM_2 <- "((ser )(o ))"
SYM_3 <- "((ser )((o )|(um ))(de )(o ))"
SYM_4 <- paste0("(", "(ser ){0,1}", SYM_VERBS, "((com ){1}|(de ){1})","(o ){0,1}", ")")

SYM <- paste0("(", SYM_1, "|", SYM_2, "|", SYM_3, "|", SYM_4, ")")
  




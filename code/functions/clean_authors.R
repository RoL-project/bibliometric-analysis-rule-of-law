###############################################################################
## SCRIPT ID:
## Exploring the Rule of Law in Academic Discourse
## Bibliometric Description and Thematic Analysis with the Web of Science Data

## R version 4.5.0 (2022-10-11) -- "How About a Twenty-Six"
## Jaroslaw Kantorowicz and Bastián González-Bustamante
## Leiden University and Universidad Diego Portales
###############################################################################

## Fixation of names of authors function
clean_author_names <- function(df) {
  final_df$AU <- str_replace(final_df$AU, "ASONGU SA", "ASONGU S")
  
  final_df$AU <- str_replace(final_df$AU, "KOCHENOV DIMITRY\\.", "KOCHENOV D")
  
  final_df$AU <- str_replace(final_df$AU, "KOCHENOV DV", "KOCHENOV D")
  
  final_df$CR <- str_replace(final_df$CR, "ASONGU SA", "ASONGU S")
  
  final_df$CR <- str_replace(final_df$CR, "KOCHENOV DIMITRY\\.", "KOCHENOV D")
  
  final_df$CR <- str_replace(final_df$CR, "KOCHENOV D\\.", "KOCHENOV D")
  
  final_df$CR <- str_replace(final_df$CR, "KOCHENOV DIMITRY", "KOCHENOV D")
  
  final_df$CR <- str_replace(final_df$CR, "KOCHENOV DV\\.", "KOCHENOV D")
  
  final_df$CR <- str_replace(final_df$CR, "KOCHENOV DV", "KOCHENOV D")
  
  final_df$CR <- str_replace(final_df$CR, "KOCHENOV 8DIMITRY", "KOCHENOV D")
  
  final_df$CR <- str_replace(final_df$CR, "ANONYMOUS, 1990, I I CHANGE EC PERFOR, DOI DOI 10.1017/CBO9780511808678", "NORTH DC, 1990, INST INST CHANGE ECON PERFOR, DOI DOI 10.1017/CBO9780511808678")
  
  final_df$CR <- str_replace(final_df$CR, "NORTH DC, 1990, I I CHANGE EC PERFOR, DOI 10.1017/CBO9780511808678, DOI 10.1017/CBO9780511808678, DOI 10.1257/JEP.5.1.97, 10.1257/JEP.5.1.97", "NORTH DC, 1990, INST INST CHANGE ECON PERFOR, DOI DOI 10.1017/CBO9780511808678")
  
  final_df$CR <- str_replace(final_df$CR, "NORTH DOUGLASSC\\.", "NORTH DC")
  
  final_df$CR <- str_replace(final_df$CR, "NORTH D.C\\.", "NORTH DC")
  
  final_df$CR <- str_replace(final_df$CR, "NORTH DOUGLASS C\\.", "NORTH DC")
  
  final_df$CR <- str_replace(final_df$CR, "WALDRON JEREMY\\.", "WALDRON J")
  
  final_df$CR <- str_replace(final_df$CR, "WALDRON J\\.", "WALDRON J")
  
  final_df$CR <- str_replace(final_df$CR, "WALDRON JEREMY", "WALDRON J")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BZ., 2004, RULE LAW HIST POLITI", "TAMANAHA B, 2004, RULE LAW HIST POLITI")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BRIANZ., 2004, RULE LAW", "TAMANAHA B, 2004, RULE LAW HIST POLITI")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BRIAN, 2004, RULE LAW HIST POLITI, P3", "TAMANAHA B, 2004, RULE LAW HIST POLITI")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BRIAN Z, 2004, ON THE RULE OF LAW: HISTORY, POLITICS, THEORY, P1", "TAMANAHA B, 2004, RULE LAW HIST POLITI")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BRIAN Z, 2004, ON THE RULE OF LAW: HISTORY, POLITICS, THEORY, P92", "TAMANAHA B, 2004, RULE LAW HIST POLITI")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BRIAN., 2004, RULE LAW HIST POLITI, DOI 10.1017/CBO9780511812378, DOI 10.1017/CBO9780511812378", "TAMANAHA B, 2004, RULE LAW HIST POLITI")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BRIANZ\\.", "TAMANAHA B")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BRIAN\\.", "TAMANAHA B")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BZ\\.", "TAMANAHA B")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BRIAN", "TAMANAHA B")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BRIAN Z", "TAMANAHA B")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA BZ", "TAMANAHA B")
  
  final_df$CR <- str_replace(final_df$CR, "TAMANAHA B Z", "TAMANAHA B")
  
  final_df$CR <- str_replace(final_df$CR, "KAUFMANN DANIEL\\.", "KAUFMANN D")
  
  final_df$CR <- str_replace(final_df$CR, "KAUFMANN D\\.", "KAUFMANN D")
  
  final_df$CR <- str_replace(final_df$CR, "LAPORTA R", "LA PORTA R")
  
  final_df$CR <- str_replace(final_df$CR, "ACEMOGLU K. DARON\\.", "ACEMOGLU D")
  
  final_df$CR <- str_replace(final_df$CR, "ACEMOGLU DARON\\.", "ACEMOGLU D")
  
  final_df$CR <- str_replace(final_df$CR, "ACEMOGLU D\\.", "ACEMOGLU D")
  
  final_df$CR <- str_replace(final_df$CR, "ARELLANO M\\.", "ARELLANO M")
  
  final_df$CR <- str_replace(final_df$CR, "BLUNDELL R\\.", "BLUNDELL R")
  
  final_df$CR <- str_replace(final_df$CR, "FALLON RICHARDH\\.", "FALLON RH")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ J., 1979, AUTHORITY OF LAW", "RAZ J, 1979, AUTHORITY OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "ESSAYS ON LAW AND MORALITY, DOI DOI 10.1093/ACPROF:OSO/9780198253457.001.0001", "RAZ J, 1979, AUTHORITY OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ J., 1979, THE RULE OF LAW AND ITS VIRTUE. THE AUTHORITY OF LAW: ESSAYS ON LAW AND MORALITY, P210", "RAZ J, 1979, AUTHORITY OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ J., 2009, THE AUTHORITY OF LAW: ESSAYS ON LAW AND MORALITY, V2", "RAZ J, 1979, AUTHORITY OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ JOSEPH., 2009, AUTHORITY LAW, V2D", "RAZ J, 1979, AUTHORITY OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ JOSEPH, 1979, AUTHORITY LAW", "RAZ J, 1979, AUTHORITY OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ JOSEPH, 1979, AUTHORITY LAW, P212", "RAZ J, 1979, AUTHORITY OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ J., 1979, THE AUTHORITY OF LAW, P221", "RAZ J, 1979, AUTHORITY OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ J., 1979, AUTHORITY OF LAW", "RAZ J, 1979, AUTHORITY OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ J 1979, AUTHORITY OF LAW", "RAZ J, 1979, AUTHORITY OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ JOSEPH\\.", "RAZ J")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ J\\.", "RAZ J")
  
  final_df$CR <- str_replace(final_df$CR, "RAZ JOSEPH", "RAZ J")
  
  final_df$CR <- str_replace(final_df$CR, "ANONYMOUS, EC POLITICS, DOI 10.1111/J.1468-0343.1995.TB00111.X, DOI 10.1111/J.1468-0343.1995.TB00111.X", "KNACK S, EC POLITICS, DOI 10.1111/J.1468-0343.1995.TB00111.X, DOI 10.1111/J.1468-0343.1995.TB00111.X")
  
  final_df$CR <- str_replace(final_df$CR, "KNACK S\\.", "KNACK S")
  
  final_df$CR <- str_replace(final_df$CR, "ANONYMOUS, 1969, MORALITY LAW", "FULLER L, 1969, MORALITY LAW")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER LONL., 1969, MORALITY LAW", "FULLER L, 1969, MORALITY LAW")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER LONL., 1964, THE MORALITY OF LAW", "FULLER L, 1969, MORALITY LAW")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER LON L., 1969, THE MORALITY OF LAW: REVISED EDITION", "FULLER L, 1969, MORALITY LAW")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER LON L, 1969, THE MORALITY OF LAW, P39", "FULLER L, 1969, MORALITY LAW")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER LON L, 1969, THE MORALITY OF LAW, P46", "FULLER L, 1969, MORALITY LAW")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER LL, 1969, MORALITY LAW, PCH2", "FULLER L, 1969, MORALITY LAW")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER L, 1965, MORALITY LAW", "FULLER L, 1969, MORALITY LAW")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER LONL., 1977, MORALITY LAW", "FULLER L, 1969, MORALITY LAW")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER L, 1964, MORALITY LAW, PCH2", "FULLER L, 1969, MORALITY LAW")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER LL", "FULLER L")
  
  final_df$CR <- str_replace(final_df$CR, "FULLER LONL\\.", "FULLER L")
  
  final_df$CR <- str_replace(final_df$CR, "SCALIA ANTONIN\\.", "SCALIA A")
  
  final_df$CR <- str_replace(final_df$CR, "SCALIA ANTONIN", "SCALIA A")
  
  final_df$CR <- str_replace(final_df$CR, "RAWLS JOHN, 1971, A THEORY OF JUSTICE, P235", "RAWLS J, 1971, THEORY JUSTICE, DOI DOI 10.4159/9780674042605")
  
  final_df$CR <- str_replace(final_df$CR, "RAWLS J., 1999, A THEORY OF JUSTICE, DOI DOI 10.4159/9780674042582", "RAWLS J, 1971, THEORY JUSTICE, DOI DOI 10.4159/9780674042605")
  
  final_df$CR <- str_replace(final_df$CR, "RAWLS J., 2020, THEORY JUSTICE", "RAWLS J, 1971, THEORY JUSTICE, DOI DOI 10.4159/9780674042605")
  
  final_df$CR <- str_replace(final_df$CR, "RAWLS JOHN, 1999, THEORY JUSTICE, P207", "RAWLS J, 1971, THEORY JUSTICE, DOI DOI 10.4159/9780674042605")
  
  final_df$CR <- str_replace(final_df$CR, "RAWLS JOHN, 1993, POLITICAL LIBERALISM", "RAWLS J., 1994, POLITICAL LIBERALISM")
  
  final_df$CR <- str_replace(final_df$CR, "RAWLS J, 2005, POLITICAL LIBERALISM", "RAWLS J., 1994, POLITICAL LIBERALISM")
  
  final_df$CR <- str_replace(final_df$CR, "RAWLS JOHN., 2000, O LIBERALISMO POLITICO, V2A EDICAO,", "RAWLS J., 1994, POLITICAL LIBERALISM")
  
  final_df$CR <- str_replace(final_df$CR, "RAWLS JOHN\\.", "RAWLS J")
  
  final_df$CR <- str_replace(final_df$CR, "RAWLS J\\.", "RAWLS J")
  
  final_df$CR <- str_replace(final_df$CR, "RAWLS JOHN", "RAWLS J")
  
  final_df$CR <- str_replace(final_df$CR, "RODRIK DANIEL\\.", "RODRIK D")
  
  final_df$CR <- str_replace(final_df$CR, "RODRIK DANIEL", "RODRIK D")
  
  final_df$CR <- str_replace(final_df$CR, "RODRIK D\\.", "RODRIK D")
  
  final_df$CR <- str_replace(final_df$CR, "CRAIG PAUL., 1997, PUBLIC LAW, P467", "CRAIG P, 1997, PUBLIC LAW, P467")
  
  final_df$CR <- str_replace(final_df$CR, "PECH L\\.", "PECH L")
  
  final_df$CR <- str_replace(final_df$CR, "CAROTHERS THOMAS., 2006, PROMOTING THE RULE OF LAW ABROAD: IN SEARCH OF KNOWLEDGE", "CAROTHERS T, 2006, PROMOTING THE RULE OF LAW ABROAD: IN SEARCH OF KNOWLEDGE")
  
  final_df$CR <- str_replace(final_df$CR, "CAROTHERS THOMAS., 2006, PROMOTING THE RULE OF LAW ABROAD: IN SEARCH OF KNOWLEDGE, P31", "CAROTHERS T, 2006, PROMOTING THE RULE OF LAW ABROAD: IN SEARCH OF KNOWLEDGE")
  
  final_df$CR <- str_replace(final_df$CR, "CAROTHERS THOMAS., 2006, PROMOTING THE RULE OF LAW ABROAD: IN SEARCH OF KNOWLEDGE, P3", "CAROTHERS T, 2006, PROMOTING THE RULE OF LAW ABROAD: IN SEARCH OF KNOWLEDGE")
  
  final_df$CR <- str_replace(final_df$CR, "CAROTHERS THOMAS., 2003, PROMOTING THE RULE OF LAW ABROAD: THE PROBLEM OF KNOWLEDGE", "CAROTHERS T, 2006, PROMOTING THE RULE OF LAW ABROAD: IN SEARCH OF KNOWLEDGE")
  
  final_df$CR <- str_replace(final_df$CR, "CAROTHERS THOMAS., 2006, PROMOTING THE RULE-OF-LAW ABROAD, P15", "CAROTHERS T, 2006, PROMOTING THE RULE OF LAW ABROAD: IN SEARCH OF KNOWLEDGE")
  
  final_df$CR <- str_replace(final_df$CR, "CAROTHERS THOMAS., 2010, PROMOTING THE RULE OF LAW ABROAD", "CAROTHERS T, 2006, PROMOTING THE RULE OF LAW ABROAD: IN SEARCH OF KNOWLEDGE")
  
  final_df$CR <- str_replace(final_df$CR, "CAROTHERS THOMAS\\.", "CAROTHERS T")
  
  final_df$CR <- str_replace(final_df$CR, "CAROTHERS T\\.", "CAROTHERS T")
  
  final_df$CR <- str_replace(final_df$CR, "CAROTHERS THOMAS", "CAROTHERS T")
  
  final_df$CR <- str_replace(final_df$CR, "DWORKIN R., 1978, TAKING RIGHTS SERIOUSLY, V136", "DWORKIN RONALD, 1977, TAKING RIGHTS SERIOU")
  
  final_df$CR <- str_replace(final_df$CR, "DWORKIN RONALD, 1977, TAKING RIGHTS SERIOU, P31", "DWORKIN RONALD, 1977, TAKING RIGHTS SERIOU")
  
  final_df$CR <- str_replace(final_df$CR, "DWORKIN RONALD., 1986, LAW'S EMPIRE", "DWORKIN RONALD., 1988, LAWS EMPIRE")
  
  final_df$CR <- str_replace(final_df$CR, "DWORKIN RONALD\\.", "DWORKIN R")
  
  final_df$CR <- str_replace(final_df$CR, "DWORKIN R\\.", "DWORKIN R")
  
  final_df$CR <- str_replace(final_df$CR, "DWORKIN RONALD", "DWORKIN R")
  
  final_df$CR <- str_replace(final_df$CR, "HART H.L. A., 1997, THE CONCEPT OF LAW, V2ND", "HART H, 1997, THE CONCEPT OF LAW, V2ND")
  
  final_df$CR <- str_replace(final_df$CR, "HART H, 1994, THE CONCEPT OF LAW, V2", "HART H, 1997, THE CONCEPT OF LAW, V2ND")
  
  final_df$CR <- str_replace(final_df$CR, "HART H. L. A., 1961, CONCEPT LAW, P121", "HART H, 1997, THE CONCEPT OF LAW, V2ND")
  
  final_df$CR <- str_replace(final_df$CR, "HART H. L. A., 1994, CONCEPT LAW, V2", "HART H, 1997, THE CONCEPT OF LAW, V2ND")
  
  final_df$CR <- str_replace(final_df$CR, "HART H.L. A., 2012, THE CONCEPT OF LAW", "HART H, 1997, THE CONCEPT OF LAW, V2ND")
  
  final_df$CR <- str_replace(final_df$CR, "HART H.L. A\\.", "HART H")
  
  final_df$CR <- str_replace(final_df$CR, "HART H. L. A\\.", "HART H")
  
  final_df$CR <- str_replace(final_df$CR, "HART H.L.A\\.", "HART H")
  
  final_df$CR <- str_replace(final_df$CR, "HART H\\.", "HART H")
  
  final_df$CR <- str_replace(final_df$CR, "HART HLA", "HART H")
  
  final_df$CR <- str_replace(final_df$CR, "PEERENBOOM R.P\\.", "PEERENBOOM R")
  
  final_df$CR <- str_replace(final_df$CR, "PEERENBOOM RANDALL\\.", "PEERENBOOM R")
  
  final_df$CR <- str_replace(final_df$CR, "PEERENBOOM R\\.", "PEERENBOOM R")
  
  final_df$CR <- str_replace(final_df$CR, "O'DONNELL GUILLERMOA\\.", "O'DONNELL G")
  
  final_df$CR <- str_replace(final_df$CR, "O'DONNELL GUILLERMO\\.", "O'DONNELL G")
  
  final_df$CR <- str_replace(final_df$CR, "O'DONNELL G\\.", "O'DONNELL G")
  
  final_df$CR <- str_replace(final_df$CR, "O'DONNELL GA", "O'DONNELL G")
  
  final_df$CR <- str_replace(final_df$CR, "SCHEPPELE KIM LANE\\.", "SCHEPPELE KL")
  
  final_df$CR <- str_replace(final_df$CR, "SCHEPPELE KIMLANE\\.", "SCHEPPELE KL")
  
  final_df$CR <- str_replace(final_df$CR, "SCHEPPELE KIM LANE", "SCHEPPELE KL")
  
  final_df$CR <- str_replace(final_df$CR, "SCHEPPELE K.L\\.", "SCHEPPELE KL")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK F.A., 1999, THE CONSTITUTION OF LIBERTY: THE DEFINITIVE EDITION", "HAYEK FA, 1999, THE CONSTITUTION OF LIBERTY")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK FRIEDRICH, THE CONSTITUTION OF LIBERTY", "HAYEK FA, 1999, THE CONSTITUTION OF LIBERTY")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK FRIEDRICH, 1960, CONSTITUTION LIBERTY, P153", "HAYEK FA, 1999, THE CONSTITUTION OF LIBERTY")
  
  final_df$CR <- str_replace(final_df$CR, "VON HAYEK FRIEDRICHA., 1978, CONSTITUTION LIBERTY", "HAYEK FA, 1999, THE CONSTITUTION OF LIBERTY")
  
  final_df$CR <- str_replace(final_df$CR, "VON HAYEK FRIEDRICH., 1944, THE ROAD TO SERFDOM", "HAYEK FA, 1944, THE ROAD TO SERFDOM")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK F.A., 1976, ROAD SERFDOM", "HAYEK FA, 1944, THE ROAD TO SERFDOM")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK FRIEDRICH, 1944, ROAD SERFDOM, P72", "HAYEK FA, 1944, THE ROAD TO SERFDOM")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK FRIEDRICHA., 1945, THE ROAD TO SERFDOM", "HAYEK FA, 1944, THE ROAD TO SERFDOM")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK F. A., 1994, ROAD SERFDOM, P80", "HAYEK FA, 1944, THE ROAD TO SERFDOM")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK FRIEDRICH A. VON, 2007, THE ROAD TO SERFDOM, P112", "HAYEK FA, 1944, THE ROAD TO SERFDOM")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK FRIEDRICH A., 1944, ROAD SERFDOM, P80", "HAYEK FA, 1944, THE ROAD TO SERFDOM")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK FRIEDRICH, 1944, ROAD SERFDOM, P54", "HAYEK FA, 1944, THE ROAD TO SERFDOM")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK FA., 1973, LAW LEGISLATION LIBE", "HAYEK FA, 1973, LAW LEGISLATION LIBERTY")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK F.A., 1979, LAW, LEGISLATION AND LIBERTY: A NEW STATEMENT OF THE LIBERAL PRINCIPLES OF JUSTICE AND POLITICAL ECONOMY, V3", "HAYEK FA, 1973, LAW LEGISLATION LIBERTY")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK F., 1982, LAW LEGISLATION LIBE", "HAYEK FA, 1973, LAW LEGISLATION LIBERTY")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK F.A\\.", "HAYEK FA")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK FRIEDRICHA\\.", "HAYEK FA")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK FA\\.", "HAYEK FA")
  
  final_df$CR <- str_replace(final_df$CR, "HAYEK F. A\\.", "HAYEK FA")
  
  final_df$CR <- str_replace(final_df$CR, "VON HAYEK FA", "HAYEK FA")
  
  final_df$CR <- str_replace(final_df$CR, "ZAKARIA FAREED\\.", "ZAKARIA F")
  
  final_df$CR <- str_replace(final_df$CR, "ZAKARIA F\\.", "ZAKARIA F")
  
  final_df$CR <- str_replace(final_df$CR, "OLSON MANCUR\\.", "OLSON M")
  
  final_df$CR <- str_replace(final_df$CR, "OLSON M\\.", "OLSON M")
  
  final_df$CR <- str_replace(final_df$CR, "CHESTERMAN SIMON\\.", "CHESTERMAN S")
  
  final_df$CR <- str_replace(final_df$CR, "CHESTERMAN S\\.", "CHESTERMAN S")
  
  final_df$CR <- str_replace(final_df$CR, "KRYGIER MARTIN\\.", "KRYGIER M")
  
  final_df$CR <- str_replace(final_df$CR, "KRYGIER M\\.", "KRYGIER M")
  
  final_df$CR <- str_replace(final_df$CR, "KRYGIER MARTIN", "KRYGIER M")
  
  final_df$CR <- str_replace(final_df$CR, "BINGHAM T., 2011, THE RULE OF LAW", "BINGHAM T, 2011, THE RULE OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "BINGHAM TOM., 2010, THE RULE OF LAW", "BINGHAM T, 2011, THE RULE OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "BINGHAM TOM, 2010, THE RULE OF LAW, P8", "BINGHAM T, 2011, THE RULE OF LAW")
  
  final_df$CR <- str_replace(final_df$CR, "GINSBURG TOM\\.", "GINSBURG T")
  
  final_df$CR <- str_replace(final_df$CR, "GINSBURG T\\.", "GINSBURG T")
  
  final_df$CR <- str_replace(final_df$CR, "GINSBURG TOM", "GINSBURG T")
  
  final_df$CR <- str_replace(final_df$CR, "SHKLAR JUDITH\\.", "SHKLAR J")
  
  final_df$CR <- str_replace(final_df$CR, "SHKLAR J\\.", "SHKLAR J")
  
  final_df$CR <- str_replace(final_df$CR, "SHKLAR JUDITH", "SHKLAR J")
  
  final_df$CR <- str_replace(final_df$CR, "HUNTINGTON SAMUEL\\.", "HUNTINGTON SP")
  
  final_df$CR <- str_replace(final_df$CR, "HUNTINGTON S.P\\.", "HUNTINGTON SP")
  
  final_df$CR <- str_replace(final_df$CR, "HUNTINGTON SP\\.", "HUNTINGTON SP")
  
  final_df$CR <- str_replace(final_df$CR, "THOMPSON E. P., 1975, WHIGS AND HUNTERS: THE ORIGIN OF THE BLACK ACT", "THOMPSON EP, 1975, WHIGS AND HUNTERS: THE ORIGIN OF THE BLACK ACT")
  
  final_df$CR <- str_replace(final_df$CR, "THOMPSON E.P., 1975, WHIGS AND HUNTERS: THE ORIGIN OF THE BLACK ACT, P266", "THOMPSON EP, 1975, WHIGS AND HUNTERS: THE ORIGIN OF THE BLACK ACT")
  
  final_df$CR <- str_replace(final_df$CR, "BUSSE M., 2007, EUR. J. POLITICAL ECON., V23, P397, DOI 10.1016/J.EJPOLECO.2006.02.003, DOI 10.1016/J.EJPOLECO.2006.02.003", "BUSSE M, 2007, EUR. J. POLITICAL ECON., V23, P397, DOI 10.1016/J.EJPOLECO.2006.02.003, DOI 10.1016/J.EJPOLECO.2006.02.003")
  
  final_df$CR <- str_replace(final_df$CR, "LEVITSKY STEVENANDLUCAN A WAY., 2010, COMPETITIVE AUTHORITARIANISM: HYBRID REGIMES AFTER THE COLD WAR", "LEVITSKY S, 2010, COMPETITIVE AUTHORITARIANISM: HYBRID REGIMES AFTER THE COLD WAR")
  
  final_df$CR <- str_replace(final_df$CR, "LEVITSKY S., 2018, HOW DEMOCRACIES DIE", "LEVITSKY S, 2018, HOW DEMOCRACIES DIE")
  
  final_df$CR <- str_replace(final_df$CR, "ZIBLATT DANIELSTEVEN LEVITSKY., 2018, DEMOCRACIES", "LEVITSKY S, 2018, HOW DEMOCRACIES DIE")
  
  final_df$CR <- str_replace(final_df$CR, "DE SOTO HERNANDO\\.", "DE SOTO H")
  
  final_df$CR <- str_replace(final_df$CR, "DE SOTO H\\.", "DE SOTO H")
  
  final_df$CR <- str_replace(final_df$CR, "LANDAU DAVID\\.", "LANDAU D")
  
  final_df$CR <- str_replace(final_df$CR, "LANDAU D\\.", "LANDAU D")
  
  final_df$CR <- str_replace(final_df$CR, "HABERMAS JURGEN\\.", "HABERMAS J")
  
  final_df$CR <- str_replace(final_df$CR, "HABERMAS J\\.", "HABERMAS J")
  
  final_df$CR <- str_replace(final_df$CR, "HABERMAS JURGEN", "HABERMAS J")
  
  final_df$CR <- str_replace(final_df$CR, "FOUCAULT MICHEL\\.", "FOUCAULT M")
  
  final_df$CR <- str_replace(final_df$CR, "FOUCAULT M\\.", "FOUCAULT M")
  
  final_df$CR <- str_replace(final_df$CR, "FOUCAULT MICHEL", "FOUCAULT M")
  
  final_df$CR <- str_replace(final_df$CR, "SADURSKI WOJCIECH\\.", "SADURSKI W")
  
  final_df$CR <- str_replace(final_df$CR, "SADURSKI W\\.", "SADURSKI W")
  
  final_df$CR <- str_replace(final_df$CR, "SADURSKI WOJCIECH", "SADURSKI W")
  
  final_df$CR <- str_replace(final_df$CR, "TUSHNET MARK\\.", "TUSHNET M")
  
  final_df$CR <- str_replace(final_df$CR, "TUSHNET M\\.", "TUSHNET M")
  
  final_df$CR <- str_replace(final_df$CR, "TUSHNET MARK", "TUSHNET M")
  
  final_df$CR <- str_replace(final_df$CR, "SCHMITT CARL\\.", "SCHMITT C")
  
  final_df$CR <- str_replace(final_df$CR, "SCHMITT C\\.", "SCHMITT C")
  
  final_df$CR <- str_replace(final_df$CR, "SCHMITT CARL", "SCHMITT C")
  
  final_df$CR <- str_replace(final_df$CR, "DYZENHAUS DAVID\\.", "DYZENHAUS D")
  
  final_df$CR <- str_replace(final_df$CR, "DYZENHAUS D\\.", "DYZENHAUS D")
  
  final_df$CR <- str_replace(final_df$CR, "DYZENHAUS DAVID", "DYZENHAUS D")
  
  final_df$CR <- str_replace(final_df$CR, "KANT IMMANUEL\\.", "KANT I")
  
  final_df$CR <- str_replace(final_df$CR, "KANT I\\.", "KANT I")
  
  final_df$CR <- str_replace(final_df$CR, "KANT IMMANUEL", "KANT I")
  
  final_df$CR <- str_replace(final_df$CR, "POSNER RICHARD A\\.", "POSNER RA")
  
  final_df$CR <- str_replace(final_df$CR, "POSNER RICHARD A", "POSNER RA")
  
  final_df$CR <- str_replace(final_df$CR, "POSNER RICHARD\\.", "POSNER RA")
  
  final_df$CR <- str_replace(final_df$CR, "POSNER R\\.", "POSNER RA")
  
  final_df$CR <- str_replace(final_df$CR, "POSNER RICHARD", "POSNER RA")
  
  final_df$CR <- str_replace(final_df$CR, "PRZEWORSKI ADAM\\.", "PRZEWORSKI A")
  
  final_df$CR <- str_replace(final_df$CR, "PRZEWORSKI A\\.", "PRZEWORSKI A")
  
  final_df$CR <- str_replace(final_df$CR, "PRZEWORSKI ADAM", "PRZEWORSKI A")
  
  final_df$CR <- str_replace(final_df$CR, "ARENDT HANNAH\\.", "ARENDT H")
  
  final_df$CR <- str_replace(final_df$CR, "ARENDT H\\.", "ARENDT H")
  
  final_df$CR <- str_replace(final_df$CR, "ARENDT HANNAH", "ARENDT H")
  
  final_df$CR <- str_replace(final_df$CR, "AGAMBEN GIORGIO\\.", "AGAMBEN G")
  
  final_df$CR <- str_replace(final_df$CR, "AGAMBEN G\\.", "AGAMBEN G")
  
  final_df$CR <- str_replace(final_df$CR, "AGAMBEN GIORGIO", "AGAMBEN G")
  
  final_df$CR <- str_replace(final_df$CR, "ALEXY ROBERT\\.", "ALEXY R")
  
  final_df$CR <- str_replace(final_df$CR, "ALEXY R\\.", "ALEXY R")
  
  final_df$CR <- str_replace(final_df$CR, "ALEXY ROBERT", "ALEXY R")
  
  final_df$CR <- str_replace(final_df$CR, "WEBER MAX\\.", "WEBER M")
  
  final_df$CR <- str_replace(final_df$CR, "WEBER M\\.", "WEBER M")
  
  final_df$CR <- str_replace(final_df$CR, "WEBER MAX", "WEBER M")
  
  final_df$CR <- str_replace(final_df$CR, "KELEMEN DANIEL\\.", "KELEMEN RD")
  
  final_df$CR <- str_replace(final_df$CR, "KELEMEN RD\\.", "KELEMEN RD")
  
  final_df$CR <- str_replace(final_df$CR, "KELEMEN R\\.", "KELEMEN RD")
  
  final_df$CR <- str_replace(final_df$CR, "WEBER DANIEL", "KELEMEN RD")
  
  final_df$CR <- str_replace(final_df$CR, "FERRAJOLI LUIGI\\.", "FERRAJOLI L")
  
  final_df$CR <- str_replace(final_df$CR, "FERRAJOLI L\\.", "FERRAJOLI L")
  
  final_df$CR <- str_replace(final_df$CR, "FERRAJOLI LUIGI", "FERRAJOLI L")
  
  final_df$CR <- str_replace(final_df$CR, "HOBBES THOMAS\\.", "HOBBES T")
  
  final_df$CR <- str_replace(final_df$CR, "HOBBES T\\.", "HOBBES T")
  
  final_df$CR <- str_replace(final_df$CR, "HOBBES THOMAS", "HOBBES T")
  
  final_df$CR <- str_replace(final_df$CR, "LOCKE JOHN\\.", "LOCKE J")
  
  final_df$CR <- str_replace(final_df$CR, "LOCKE J\\.", "LOCKE J")
  
  final_df$CR <- str_replace(final_df$CR, "LOCKE JOHN", "LOCKE J")
  
  final_df$CR <- str_replace(final_df$CR, "FUKUYAMA FRANCIS\\.", "FUKUYAMA F")
  
  final_df$CR <- str_replace(final_df$CR, "FUKUYAMA F\\.", "FUKUYAMA F")
  
  final_df$CR <- str_replace(final_df$CR, "FUKUYAMA FRANCIS", "FUKUYAMA F")
  
  final_df$CR <- str_replace(final_df$CR, "MACCORMICK NEIL\\.", "MACCORMICK N")
  
  final_df$CR <- str_replace(final_df$CR, "MACCORMICK N\\.", "MACCORMICK N")
  
  final_df$CR <- str_replace(final_df$CR, "MACCORMICK NEIL", "MACCORMICK N")
  
  final_df$CR <- str_replace(final_df$CR, "DIAMOND LARRY\\.", "DIAMOND L")
  
  final_df$CR <- str_replace(final_df$CR, "DIAMOND L\\.", "DIAMOND L")
  
  final_df$CR <- str_replace(final_df$CR, "DIAMOND LARRY", "DIAMOND L")
  
  final_df$CR <- str_replace(final_df$CR, "SCHAUER FREDERICK\\.", "SCHAUER F")
  
  final_df$CR <- str_replace(final_df$CR, "SCHAUER F\\.", "SCHAUER F")
  
  final_df$CR <- str_replace(final_df$CR, "SCHAUER FREDERICK", "SCHAUER F")
  
  final_df$CR <- str_replace(final_df$CR, "BOBBIO NORBERTO\\.", "BOBBIO N")
  
  final_df$CR <- str_replace(final_df$CR, "BOBBIO N\\.", "BOBBIO N")
  
  final_df$CR <- str_replace(final_df$CR, "BOBBIO NORBERTO", "BOBBIO N")
  
  final_df$CR <- str_replace(final_df$CR, "FINNIS JOHN\\.", "FINNIS J")
  
  final_df$CR <- str_replace(final_df$CR, "FINNIS J\\.", "FINNIS J")
  
  final_df$CR <- str_replace(final_df$CR, "FINNIS JOHN", "FINNIS J")
  
  final_df$CR <- str_replace(final_df$CR, "KOSKENNIEMI MARTTI\\.", "KOSKENNIEMI M")
  
  final_df$CR <- str_replace(final_df$CR, "KOSKENNIEMI M\\.", "KOSKENNIEMI M")
  
  final_df$CR <- str_replace(final_df$CR, "KOSKENNIEMI MARTTI", "KOSKENNIEMI M")
  
  final_df$CR <- str_replace(final_df$CR, "BOURDIEU PIERRE\\.", "BOURDIEU P")
  
  final_df$CR <- str_replace(final_df$CR, "BOURDIEU P\\.", "BOURDIEU P")
  
  final_df$CR <- str_replace(final_df$CR, "BOURDIEU PIERRE", "BOURDIEU P")
  
  final_df$CR <- str_replace(final_df$CR, "ACKERMAN BRUCE\\.", "ACKERMAN B")
  
  final_df$CR <- str_replace(final_df$CR, "ACKERMAN B\\.", "ACKERMAN B")
  
  final_df$CR <- str_replace(final_df$CR, "ACKERMAN BRUCE", "ACKERMAN B")
  
  final_df$CR <- str_replace(final_df$CR, "SUNSTEIN CASS R\\.", "SUNSTEIN CR")
  
  final_df$CR <- str_replace(final_df$CR, "SUNSTEIN CASS R", "SUNSTEIN CR")
  
  final_df$CR <- str_replace(final_df$CR, "MUNGIU-PIPPIDI A\\.", "MUNGIU-PIPPIDI A")
  
  final_df$CR <- str_replace(final_df$CR, "DERRIDA JACQUES\\.", "DERRIDA J")
  
  final_df$CR <- str_replace(final_df$CR, "DERRIDA J\\.", "DERRIDA J")
  
  final_df$CR <- str_replace(final_df$CR, "DERRIDA JACQUES", "DERRIDA J")
  
  final_df$CR <- str_replace(final_df$CR, "TILLY CHARLES\\.", "TILLY C")
  
  final_df$CR <- str_replace(final_df$CR, "TILLY C\\.", "TILLY C")
  
  final_df$CR <- str_replace(final_df$CR, "TILLY CHARLES", "TILLY C")
  
  final_df$CR <- str_replace(final_df$CR, "LOUGHLIN MARTIN\\.", "LOUGHLIN M")
  
  final_df$CR <- str_replace(final_df$CR, "LOUGHLIN M\\.", "LOUGHLIN M")
  
  final_df$CR <- str_replace(final_df$CR, "LOUGHLIN MARTIN", "LOUGHLIN M")
  
  final_df$CR <- str_replace(final_df$CR, "SHAPIRO MARTIN\\.", "SHAPIRO M")
  
  final_df$CR <- str_replace(final_df$CR, "SHAPIRO M\\.", "SHAPIRO M")
  
  final_df$CR <- str_replace(final_df$CR, "SHAPIRO MARTIN", "SHAPIRO M")
  
  final_df$CR <- gsub("HABERMAS JURGEN.", "HABERMAS J", final_df$CR)
  
  final_df$CR <- gsub("KOCHENOV DMITRY.", "KOCHENOV D", final_df$CR)
  
  final_df$CR <- str_replace(final_df$CR, "GALANTER MARC\\.", "GALANTER M")
  
  final_df$CR <- str_replace(final_df$CR, "GALANTER M\\.", "GALANTER M")
  
  final_df$CR <- str_replace(final_df$CR, "GALANTER MARC", "GALANTER M")
  
  final_df$CR <- str_replace(final_df$CR, "LUHMANN NIKLAS\\.", "LUHMANN N")
  
  final_df$CR <- str_replace(final_df$CR, "LUHMANN N\\.", "LUHMANN N")
  
  final_df$CR <- str_replace(final_df$CR, "LUHMANN NIKLAS", "LUHMANN N")
  
  final_df$CR <- str_replace(final_df$CR, "HOLMES STEPHEN\\.", "HOLMES S")
  
  final_df$CR <- str_replace(final_df$CR, "HOLMES S\\.", "HOLMES S")
  
  final_df$CR <- str_replace(final_df$CR, "HOLMES STEPHEN", "HOLMES S")
  
  final_df$CR <- str_replace(final_df$CR, "HAMILTON ALEXANDER\\.", "HAMILTON A")
  
  final_df$CR <- str_replace(final_df$CR, "HAMILTON A\\.", "HAMILTON A")
  
  final_df$CR <- str_replace(final_df$CR, "HAMILTON ALEXANDER", "HAMILTON A")
  
  final_df$CR <- str_replace(final_df$CR, "MADISON JAMES\\.", "MADISON J")
  
  final_df$CR <- str_replace(final_df$CR, "MADISON J\\.", "MADISON J")
  
  final_df$CR <- str_replace(final_df$CR, "MADISON JAMES", "MADISON J")
  
  final_df$CR <- gsub("THE WORLD BANK", "WORLD BANK", final_df$CR)
  
  final_df$CR <- gsub("PEERENBOOM RANDALL", "PEERENBOOM R", final_df$CR)
  
  final_df$CR <- gsub("PEERENBOOM RP.", "PEERENBOOM R", final_df$CR)
  
  final_df$CR <- gsub("RANDALL PEERENBOOM", "PEERENBOOM R", final_df$CR)
  
  final_df$CR <- gsub("WALDRON JEREMY", "WALDRON J", final_df$CR)
  
  final_df$CR <- gsub("ROSE ACKERMAN SUSAN", "ROSE ACKERMAN S", final_df$CR)
  
  final_df$CR <- str_replace(final_df$CR, "MAMDANI MAHMOOD\\.", "MAMDANI M")
  
  final_df$CR <- str_replace(final_df$CR, "MAMDANI M\\.", "MAMDANI M")
  
  final_df$CR <- str_replace(final_df$CR, "MMAMDANI MAHMOOD", "MAMDANI M")
  
  final_df$CR <- str_replace(final_df$CR, "DE TOCQUEVILLE ALEXIS\\.", "DE TOCQUEVILLE A")
  
  final_df$CR <- str_replace(final_df$CR, "DE TOCQUEVILLE A\\.", "DE TOCQUEVILLE A")
  
  final_df$CR <- str_replace(final_df$CR, "DE TOCQUEVILLE ALEXIS", "DE TOCQUEVILLE A")
  
  final_df$CR <- str_replace(final_df$CR, "BENTHAM JEREMY\\.", "BENTHAM J")
  
  final_df$CR <- str_replace(final_df$CR, "BENTHAM J\\.", "BENTHAM J")
  
  final_df$CR <- str_replace(final_df$CR, "BENTHAM JEREMY", "BENTHAM J")
  
  final_df$CR <- gsub("ASONGU S A", "ASONGU S", final_df$CR)
  
  final_df$CR <- gsub("ASONGU SA", "ASONGU S", final_df$CR)
  
  final_df$CR <- gsub("POSNER RAA", "POSNER RA", final_df$CR)
  
  final_df$CR <- gsub("BARRO R J", "BARRO RJ", final_df$CR)
  
  final_df$CR <- gsub("TAMANAHA B.Z.", "TAMANAHA B", final_df$CR)
  
  final_df$CR <- gsub("TAMANAHA B\\.", "TAMANAHA B", final_df$CR)
  
  final_df$CR <- gsub("TAMANAHA BZ", "TAMANAHA B", final_df$CR)
  
  final_df$CR <- gsub("BRIAN TAMANAHA", "TAMANAHA B", final_df$CR)
  
  final_df$CR <- gsub("CHEESMAN NICK.", "CHEESMAN N", final_df$CR)
  
  final_df$CR <- gsub("CHEESMAN N\\.", "CHEESMAN N", final_df$CR)
  
  final_df$CR <- gsub("CHEESMAN NICK", "CHEESMAN N", final_df$CR)
  
  final_df$CR <- gsub("WALDRON J\\.", "WALDRON J", final_df$CR)
  
  final_df$CR <- gsub("ACEMOGLU DARONROBINSON.", "ACEMOGLU D", final_df$CR)
  
  final_df$CR <- gsub("NORTH D. C.", "NORTH DC", final_df$CR)
  
  final_df$CR <- gsub("NORTH D\\.", "NORTH DC", final_df$CR)
  
  final_df$CR <- gsub("NORTH DOUGLASS.", "NORTH DC", final_df$CR)
  
  final_df$CR <- gsub("KAUFMANN DANIEL.", "KAUFMANN D", final_df$CR)
  
  final_df$CR <- gsub("KAUFMANN D\\.", "KAUFMANN D", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN RONALDM\\.", "DWORKIN R", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN RONALD\\.", "DWORKIN R", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN RONALD", "DWORKIN R", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN RM\\.", "DWORKIN R", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN R\\.", "DWORKIN R", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN R A., 1995, MATTER OF PRINCIPLE", "DWORKIN R, 1995, MATTER OF PRINCIPLE", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN, 1967, U CHICAGO LAW REV, V35, P14", "DWORKIN R, 1967, U CHICAGO LAW REV, V35, P14", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN, 1972, YALE LJ, V81, P855", "DWORKIN R, 1972, YALE LJ, V81, P855", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN, 1975, HARVARD LAW REV, V88, P1060", "DWORKIN R, 1975, HARVARD LAW REV, V88, P1060", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN RM, 1967, U CHICAGO LAW REV, V35, P14, DOI 10.2307/1598947", "DWORKIN R, 1967, U CHICAGO LAW REV, V35, P14, DOI 10.2307/1598947", final_df$CR)
  
  final_df$CR <- gsub("DWORKIN D, 1985, MATTER PRINCIPLE", "DWORKIN R, 1985, MATTER PRINCIPLE", final_df$CR)
  
  final_df$CR <- gsub("HART HM, 1953, HARVARD LAW REV, V66, P1362, DOI 10.2307/1336866", "HART H, 1953, HARVARD LAW REV, V66, P1362, DOI 10.2307/1336866", final_df$CR)
  
  final_df$CR <- gsub("HART HL.A, 2012, LEGAL REASONING LEGA, V3RD", "HART H, 2012, LEGAL REASONING LEGA, V3RD", final_df$CR)
  
  final_df$CR <- gsub("HART HENRYM., 1995, THE LEGAL PROCESS: BASIC PROBLEMS IN THE MAKING AND APPLICATION OF LAW", "HART H, 1995, THE LEGAL PROCESS: BASIC PROBLEMS IN THE MAKING AND APPLICATION OF LAW", final_df$CR)
  
  final_df$CR <- gsub("HART HM, 1958, LAW CONTEMP PROBL, V23, P401, DOI 10.2307/1190221", "HART H, 1958, LAW CONTEMP PROBL, V23, P401, DOI 10.2307/1190221", final_df$CR)
  
  final_df$CR <- gsub("HART JR HENRY M, 1994, THE LEGAL PROCESS: BASIC PROBLEMS IN THE MAKING AND APPLICATION OF LAW, P4", "HART H, 1994, THE LEGAL PROCESS: BASIC PROBLEMS IN THE MAKING AND APPLICATION OF LAW, P4", final_df$CR)
  
  final_df$CR <- gsub("HABERMAS JRGEN.", "HABERMAS J", final_df$CR)
  
  final_df$CR <- gsub("HABERMAS J\\.", "HABERMAS J", final_df$CR)
  
  final_df$CR <- gsub("FULLER LON L.", "FULLER L", final_df$CR)
  
  final_df$CR <- gsub("FULLER LON L", "FULLER L", final_df$CR)
  
  final_df$CR <- gsub("FULLER LON.", "FULLER L", final_df$CR)
  
  final_df$CR <- gsub("FULLER LON", "FULLER L", final_df$CR)
  
  final_df$CR <- gsub("FULLER LL", "FULLER L", final_df$CR)
  
  final_df$CR <- gsub("FULLER, 1969, THE MORALITY OF LAW, V1", "FULLER L, 1969, MORALITY LAW", final_df$CR)
  
  final_df$CR <- gsub("RAZ, 1979, AUTHORITY LAW ESSAYS, P37, DOI DOI 10.1093/ACPROF:OSO/9780198253457.003.0003", "RAZ J, 1979, AUTHORITY OF LAW", final_df$CR)
  
  final_df$CR <- gsub("TAMANAHA, 2004, ON THE RULE OF LAW H", "TAMANAHA B, 2004, RULE LAW HIST POLITI", final_df$CR)
  
  final_df$CR <- gsub("BRIAN TAMANAHA, 2004, RULE LAW HIST POLITI", "TAMANAHA B, 2004, RULE LAW HIST POLITI", final_df$CR)
  
  final_df$CR <- gsub("ASONGU S.A.", "ASONGU S", final_df$CR)
  
  final_df$CR <- gsub("KRYGIER MARTYN", "KRYGIER M", final_df$CR)
  
  final_df$CR <- gsub("KRYGIER MARTIN", "KRYGIER M", final_df$CR)
  
  final_df$CR <- gsub("SCHMITT C -\\.", "SCHMITT C", final_df$CR)
  
  final_df$CR <- gsub("SCHMITT C\\.", "SCHMITT C", final_df$CR)
  
  final_df$CR <- gsub("SCHMITT CARL.", "SCHMITT C", final_df$CR)
  
  final_df$CR <- gsub("HAYEK F\\.", "HAYEK FA", final_df$CR)
  
  final_df$CR <- gsub("HAYEK F.A.", "HAYEK FA", final_df$CR)
  
  final_df$CR <- gsub("HAYEK FRIEDRICH A\\.", "HAYEK FA", final_df$CR)
  
  final_df$CR <- gsub("HAYEK FRIEDRICHAUGUST.", "HAYEK FA", final_df$CR)
  
  final_df$CR <- gsub("HAYEK FRIEDRICH A", "HAYEK FA", final_df$CR)
  
  final_df$CR <- gsub("HAYEK FRIEDRICH", "HAYEK FA", final_df$CR)
  
  final_df$CR <- gsub("HART HLA", "HART H", final_df$CR)
  
  final_df$CR <- gsub("SUNSTEIN CASSR.", "SUNSTEIN CR", final_df$CR)
  
  final_df$CR <- gsub("SUNSTEIN C.R.", "SUNSTEIN CR", final_df$CR)
  
  final_df$CR <- gsub("SUNSTEIN CASS.", "SUNSTEIN CR", final_df$CR)
  
  final_df$CR <- gsub("SUNSTEIN C. R\\.", "SUNSTEIN CR", final_df$CR)
  
  final_df$CR <- gsub("SUNSTEIN C. R\\.", "SUNSTEIN CR", final_df$CR)
  
  final_df$CR <- gsub("PECH LAURENT -\\.", "PECH L", final_df$CR)
  
  final_df$CR <- gsub("PECH L\\.", "PECH L", final_df$CR)
  
  final_df$CR <- gsub("PECH LAURENT", "PECH L", final_df$CR)
  
  final_df$CR <- gsub("SCHEPPELE KIM LANE", "SCHEPPELE KL", final_df$CR)
  
  final_df$CR <- gsub("SCHEPPELE K. L", "SCHEPPELE KL", final_df$CR)
  
  final_df$CR <- gsub("SCHEPPELE K L", "SCHEPPELE KL", final_df$CR)
  
  final_df$CR <- gsub("SCHEPPELE KIMLANE", "SCHEPPELE KL", final_df$CR)
  
  final_df$CR <- gsub("RODRIK DANI.", "RODRIK D", final_df$CR)
  
  final_df$CR <- gsub("FALLON R. H", "FALLON RH", final_df$CR)
  
  final_df$CR <- gsub("RICHARD H.FALLON.", "FALLON RH", final_df$CR)
  
  final_df$CR <- gsub("FALLON RICHARD H\\.", "FALLON RH", final_df$CR)
  
  final_df$CR <- gsub("FALLON JR RICHARD H\\.", "FALLON RH", final_df$CR)
  
  final_df$CR <- gsub("FALLON JR RICHARD H\\.", "FALLON RH", final_df$CR)
  
  final_df$CR <- gsub("KELSEN HANS -\\.", "KELSEN H", final_df$CR)
  
  final_df$CR <- gsub("KELSEN HANS\\.", "KELSEN H", final_df$CR)
  
  final_df$CR <- gsub("KELSEN H\\.", "KELSEN H", final_df$CR)
  
  final_df$CR <- gsub("KELSEN HANS", "KELSEN H", final_df$CR)
  
  final_df$CR <- gsub("TUSHNET MARKV\\.", "TUSHNET M", final_df$CR)
  
  final_df$CR <- gsub("TUSHNET MV\\.", "TUSHNET M", final_df$CR)
  
  final_df$CR <- gsub("TUSHNET M\\.", "TUSHNET M", final_df$CR)
  
  final_df$CR <- gsub("TUSHNET MV", "TUSHNET M", final_df$CR)
  
  final_df$CR <- gsub("CRAIG PAUL\\.", "CRAIG P", final_df$CR)
  
  final_df$CR <- gsub("CRAIG P\\.", "CRAIG P", final_df$CR)
  
  final_df$CR <- gsub("CRAIG PAUL", "CRAIG P", final_df$CR)
  
  final_df$CR <- gsub("DYZENHAUS DAVID\\.", "DYZENHAUS D", final_df$CR)
  
  final_df$CR <- gsub("DYZENHAUS D\\.", "DYZENHAUS D", final_df$CR)
  
  final_df$CR <- gsub("DYZENHAUS DAVID", "DYZENHAUS D", final_df$CR)
  
  final_df$CR <- gsub("VON BOGDANDY ARMIN\\.", "VON BOGDANDY A", final_df$CR)
  
  final_df$CR <- gsub("VON BOGDANDY A\\.", "VON BOGDANDY A", final_df$CR)
  
  final_df$CR <- gsub("VONBOGDANDY A", "VON BOGDANDY A", final_df$CR)
  
  final_df$CR <- gsub("KAUFMAN D\\.", "KAUFMANN D", final_df$CR)
  
  final_df$CR <- gsub("KAUFMAN DANIEL\\.", "KAUFMANN D", final_df$CR)
  
  final_df$CR <- gsub("KAUFMAN D.K. A\\.", "KAUFMANN D", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1982, INTRO STUDY LAW CONS", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY ALBERTV., 1959, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, V10TH", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1885, LECT INTRO STUDY LAW, V1ST EDN", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV., 1885, INTRO STUDY LAW CONS, V10TH", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY ALBERTVENN., 1889, INTRO STUDY LAW CONS", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY ALBERTVENN., 1889, INTRO STUDY LAW CONS", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1915, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, V8THS", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1979, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, V10TH EDN", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY ALBERTVENN., 1908, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, VSEVENTH", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1982, INTRO STUDY LAW CONS, V8TH, P110", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A. V., 1915, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, V8, P183", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1897, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1902, INTRO STUDY LAW CONS, P183", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1982, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, P114", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A. V., 1948, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, V9TH", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1902, INTRO STUDY LAW CONS", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1959, INTRO LAW CONSTITUTI", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1959, INTRO STUDY LAW CONS, P181", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY ALBERT VENN, 1915, INTRO STUDY LAW CONS, P110", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY ALBERT VENN, 1982, INTRO STUDY LAW CONS", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY ALBERTVENN., 1908, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, V7TH, P179", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1915, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, V8TH, P198", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1959, INTRO STUDY LAW CONS, P193", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1982, INTRO STUDY LAW CONS, P107", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("ALBERT VENN DICEY, 1982, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, V8TH", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A. V., 1982, INTRO STUDY LAW CONS, P110", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A. V., 1982, INTRO STUDY LAW CONS, P21", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A., 1939, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, V9TH", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1878, NATION, V27, P352", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1883, NATION, V37, P95", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1894, NATL REV, V23, P65", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1910, Q REV, V212, P538", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1914, LECT RELATION LAW PU", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1959, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, V10TH, P188", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1960, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, TENTH EDITION, VTENTH", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.V., 1982, LAW CONSTITUTION, V8TH", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY A.W., 1959, LAW CONSTITUTION", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY ALBERT VENN, 1915, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, P179", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1885, LECTURES INTRODUCTOR, P215", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1915, INTRO STUDY LAW CONS, P107", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1915, LAW QUART REV, V31, P148", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1959, INTRO STUDY LAW CONS, P202", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1959, INTRO STUDY LAW CONS, P72", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1961, LAW CONSTITUTION", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV, 1982, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION, P268", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV., 1883, CAN ENGLISH LAW BE TAUGHT AT THE UNIVERSITIES?", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV., 1915, INTRODUCTION TO THE LAW OF THE CONSTITUTION, V8", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY AV., 1964, INTRO STUDY LAW CONS, V10TH", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("DICEY, 1880, LAW MAGAZINE REV, P386", "DICEY AV, 1885, INTRODUCTION TO THE STUDY OF THE LAW OF THE CONSTITUTION", final_df$CR)
  
  final_df$CR <- gsub("LEVITSKY S.E\\.", "LEVITSKY S", final_df$CR)
  
  final_df$CR <- gsub("OSTROM E\\.", "OSTROM E", final_df$CR)
  
  final_df$CR <- gsub("MARX KARL\\.", "MARX K", final_df$CR)
  
  final_df$CR <- gsub("MARX K\\.", "MARX K", final_df$CR)
  
  final_df$CR <- gsub("MARX KARL", "MARX K", final_df$CR)
  
  final_df$CR <- gsub("PETERSMANN ERNST-ULRICH\\.", "PETERSMANN EU", final_df$CR)
  
  final_df$CR <- gsub("PETERSMANN ERNST-ULRICH", "PETERSMANN EU", final_df$CR)
  
  final_df$CR <- gsub("PETERSMANN E.-U\\.", "PETERSMANN EU", final_df$CR)
  
  final_df$CR <- gsub("PETERSMANN E-U\\.", "PETERSMANN EU", final_df$CR)
  
  final_df$CR <- gsub("ALLAN TREVORR.S\\.", "ALLAN TRS", final_df$CR)
  
  final_df$CR <- gsub("ALLAN T. R. S\\.", "ALLAN TRS", final_df$CR)
  
  final_df$CR <- gsub("ALLAN T.R. S\\.", "ALLAN TRS", final_df$CR)
  
  final_df$CR <- gsub("ALLAN T.R.S\\.", "ALLAN TRS", final_df$CR)
  
  final_df$CR <- gsub("ALLAN TR S\\.", "ALLAN TRS", final_df$CR)
  
  final_df$CR <- gsub("ALLAN TRS\\.", "ALLAN TRS", final_df$CR)
  
  final_df$CR <- gsub("HILDEBRANDT M\\.", "HILDEBRANDT M", final_df$CR)
  
  final_df$CR <- gsub("HILDEBRANDT MIREILLE", "HILDEBRANDT M", final_df$CR)
  
  final_df$CR <- gsub("SCHEUERMAN WILLIAME\\.", "SCHEUERMAN WE", final_df$CR)
  
  final_df$CR <- gsub("SCHEUERMAN WILLIAM\\.", "SCHEUERMAN WE", final_df$CR)
  
  final_df$CR <- gsub("SCHEUERMAN WE\\.", "SCHEUERMAN WE", final_df$CR)
  
  final_df$CR <- gsub("SCHEUERMAN W\\.", "SCHEUERMAN WE", final_df$CR)
  
  final_df$CR <- gsub("IKENBERRY JOHNG\\.", "IKENBERRY GJ", final_df$CR)
  
  final_df$CR <- gsub("IKENBERRY G.JOHN\\.", "IKENBERRY GJ", final_df$CR)
  
  final_df$CR <- gsub("IKENBERRY G\\.", "IKENBERRY GJ", final_df$CR)
  
  final_df$CR <- gsub("AUSTIN LISA M\\.", "AUSTIN LM", final_df$CR)
  
  final_df$CR <- gsub("AUSTIN LISAM\\.", "AUSTIN LM", final_df$CR)
  
  final_df$CR <- gsub("AUSTIN LISA", "AUSTIN LM", final_df$CR)
  
  final_df$CR <- gsub("AUSTIN LISA", "AUSTIN LM", final_df$CR)
  
  final_df$CR <- gsub("TYLER T\\.", "TYLER TR", final_df$CR)
  
  final_df$CR <- gsub("DAHL ROBERT A\\.", "DAHL RA", final_df$CR)
  
  final_df$CR <- gsub("DAHL ROBERTA\\.", "DAHL RA", final_df$CR)
  
  final_df$CR <- gsub("DAHL R.A\\.", "DAHL RA", final_df$CR)
  
  final_df$CR <- gsub("DAHL R\\.", "DAHL RA", final_df$CR)
  
  final_df$CR <- gsub("SCHEDLER ANDREAS\\.", "SCHEDLER A", final_df$CR)
  
  final_df$CR <- gsub("KENNEDY DUNCAN\\.", "KENNEDY D", final_df$CR)
  return(df)
}




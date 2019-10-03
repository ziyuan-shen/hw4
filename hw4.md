---
title: "Homework 4"
author: '[Group names]'
date: "9/26/2019"
output: 
  html_document:
    keep_md: yes
---



## Packages


```r
require(tidyverse)
```

## Data


```r
nasa <- readRDS(file = "nasa.rds")
```

<!--

```r
#easy_df <- data.frame(lapply(easy_df, unlist), stringsAsFactoers=FALSE)
var_names <- names(nasa[[1]])
easy_part <- map(nasa, `[`, var_names[1:9])
easy_df <- do.call(rbind, easy_part) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  lapply(as.character) %>%
  data.frame(stringsAsFactors = FALSE)

geo_coord <- map(nasa, list("geolocation", "coordinates"))
features <- map(nasa, `[[`, "features") %>%
  map(unlist)
nasa_df <- easy_df %>%
  mutate("features" = features,
         "geo_coord" = geo_coord)
nasa_df[nasa_df=='NULL'] <- NA
```
-->

## Task1

```r
nasa_df <- nasa %>%
  lapply(unlist) %>%
  lapply(data.frame, stringsAsFactors = FALSE) %>%
  lapply(t) %>%
  lapply(data.frame, stringsAsFactors = FALSE) %>%
  bind_rows()
```


## Task2

### select useful columns

```r
nasa_df <- nasa_df %>%
  select(-(22:23)) %>%
  select(-geolocation.type)
```

### remove duplicates

```r
nasa_df <- nasa_df %>%
  filter(!duplicated(nasa_df))
```


### clean id

```r
nasa_df$id <- as.integer(nasa_df$id)
```

### clean date time

```r
nasa_df$year <- str_replace(nasa_df$year, "T+", " ")
nasa_df$year <- str_replace(nasa_df$year, "\\..*", "")
nasa_df <- nasa_df %>%
  separate(col=year, into=c('year', 'month', 'day'), sep='-') %>%
  separate(col=day, into=c('day', 'hours'), sep=" ") %>%
  separate(col=hours, into=(c('hours', 'minutes', 'seconds')), sep=':')
time_names <- c('year', 'month', 'day', 'hours', 'minutes', 'seconds')
nasa_df[time_names] <- nasa_df[time_names] %>%
  map(as.integer)
```

### clean coordinates

```r
# rename column
names(nasa_df)[names(nasa_df) == 'geolocation.coordinates1'] <- 'geo_coord1'
names(nasa_df)[names(nasa_df) == 'geolocation.coordinates2'] <- 'geo_coord2'
nasa_df$geo_coord1 <- nasa_df$geo_coord1 %>%
  str_extract("-?\\d*\\.?\\d*$") %>%
  as.numeric()
nasa_df$geo_coord2 <- nasa_df$geo_coord2 %>%
  str_extract("-?\\d*\\.?\\d*$") %>%
  as.numeric()
nasa_df$geo_coords <- map2(nasa_df$geo_coord1, nasa_df$geo_coord2, ~ c(.x, .y))
nasa_df <- nasa_df %>%
  select(-'geo_coord1', -'geo_coord2')
```

### clean features

```r
nasa_df$features[nasa_df$features == 'n/a'] <- NA
nasa_df$features[nasa_df$features == 'NA'] <- NA
nasa_df$features[nasa_df$features == 'unknown'] <- NA
for (feature in names(nasa_df)[15:22])
{
  nasa_df$features <- map2(nasa_df$features, nasa_df[[feature]], ~c(.x, .y))
}
nasa_df <- nasa_df %>%
  select(-('features1':'features8'))
nasa_df$features <- nasa_df$features %>%
  map(function (x) {x[!is.na(x)]})
```

<!--

```r
nasa_df
```

```
                             name    id nametype              recclass
1                        Djermaia  7656    Valid                     H
2                         Elbogen  7823    Valid             Iron, IID
3                          Pacula 18068    Valid                    L6
4                            Bali  4928    Valid                   CV3
5                           Benld  5021    Valid                    H6
6                         Lumpkin 14753    Valid                    L6
7                            Nejo 16941    Valid                    L6
8                        Kushiike 12381    Valid                    OC
9                        Pavlovka 18177    Valid             Howardite
10                  Lanzenkirchen 12465    Valid                    L4
11                       Komagome 12343    Valid                  Iron
12                        Idutywa 12000    Valid                    H5
13                     Diep River  7642    Valid                    L6
14                        Blanket  5071    Valid                    L6
15                      Conquista  5418    Valid                    H4
16                    Benares (a)  5011    Valid                   LL4
17               Oldenburg (1930) 18009    Valid                    L6
18                        Hainaut 11472    Valid                  H3-6
19                      Semarkona 23487    Valid                LL3.00
20                       Karoonda 12264    Valid                   CK4
21                  Alexandrovsky   465    Valid                    H4
22                       Slobodka 23645    Valid                    L4
23          Northwest Africa 5815 50693    Valid                    L5
24                      St-Robert 23733    Valid                    H5
25                        Hvittis 11989    Valid                   EL6
26                          Awere  4910    Valid                    L4
27                        Kainsaz 12229    Valid                 CO3.2
28                      Hachi-oji 11468    Valid                    H?
29                       Phu Hong 18809    Valid                    H4
30                          Lanxi 12464    Valid                    L6
31                      Ekh Khera  7777    Valid                    H6
32                          Naoki 16908    Valid                    H6
33                       Kernouve 12284    Valid                    H6
34                       Crescent  5470    Valid                   CM2
35                     Ställdalen 23712    Valid                    H5
36               Monte das Fortes 16725    Valid                    L5
37                     Chervettaz  5341    Valid                    L5
38                        Cilimus  5364    Valid                    L5
39                Battle Mountain 56133    Valid                    L6
40                      Parnallee 18108    Valid                 LL3.6
41                          Atoka  4888    Valid                    L6
42                          Bocas  5093    Valid                    L6
43                           Mhow 16629    Valid                    L6
44                    San Michele 31315    Valid                    L6
45                      St. Louis 23089    Valid                    H4
46                        Al Rais   446    Valid                CR2-an
47                     Ibbenbüren 11992    Valid             Diogenite
48                  Aguila Blanca   417    Valid                     L
49                        Castine  5293    Valid                    L6
50                         Khohar 12298    Valid                  L3.6
51                         Nyaung 17969    Valid           Iron, IIIAB
52                          Patti 18118    Valid                  Iron
53              Noventa Vicentina 17930    Valid                    H4
54               Bielokrynitschie  5043    Valid                    H4
55                        L'Aigle 12434    Valid                    L6
56                  Birni N'konni  5056    Valid                    H4
57                        Mornans 16747    Valid                    H5
58                     Nyirábrany 17970    Valid                   LL5
59                   Monte Milone 16726    Valid                    L5
60                      Menziswyl 15486    Valid                    L5
61         St. Caprais-de-Quinsac 23081    Valid                    L6
62                         Oviedo 18058    Valid                    H5
63                       Kilbourn 12308    Valid                    H5
64                       Leighton 12760    Valid                    H5
65                        Sauguis 23188    Valid                    L6
66                         Ovambo 18055    Valid                    L6
67                       Mirzapur 16701    Valid                    L5
68                    Palahatchie 18073    Valid                    OC
69                      Çanakkale  5250    Valid                    L6
70                         Sharps 23525    Valid                  H3.4
71                          Renca 22587    Valid                    L5
72                         Lorton 52843    Valid                    L6
73                      New Halfa 16954    Valid                    L4
74                          Akaba   426    Valid                    L6
75           Olmedilla de Alarcón 18015    Valid                    H5
76                      Khmelevka 12297    Valid                    L5
77            Adzhi-Bogdo (stone)   390    Valid                 LL3-6
78                   Gualeguaychú 11432    Valid                    H6
79                        Pleşcoi 51706    Valid                  L5-6
80                       Guangnan 11436    Valid                    L6
81                        Raoyang 22394    Valid                    L6
82                      Bulls Run  5163    Valid                 Iron?
83                         Okniny 18002    Valid                   LL6
84                          Mtola 16820    Valid            Stone-uncl
85                        Dergaon  6664    Valid                    H5
86                    Kaptal-Aryk 12253    Valid                    L6
87                      Galim (a) 10848    Valid                   LL6
88                        Bukhara 30448    Valid                   CV3
89              Jajh deh Kot Lalu 12067    Valid                   EL6
90                  Millbillillie 16643    Valid         Eucrite-mmict
91                        Karewar 12260    Valid                    L6
92                        Laborel 12408    Valid                    H5
93                   Min-Fan-Zhun 16697    Valid                   LL6
94                       Barbotan  4942    Valid                    H5
95                   Fayetteville 10079    Valid                    H4
96                     Cranganore  5465    Valid                    L6
97                         Nakhla 16898    Valid    Martian (nakhlite)
98                    Alessandria   463    Valid                    H5
99                          Fünen 10838    Valid            Stone-uncl
100                 Gasseltepaoua 10866    Valid                    H5
101                          Kaee 12222    Valid                    H5
102                 Serra de Magé 23502    Valid            Eucrite-cm
103                         Silao 23594    Valid                    H5
104                        Kikino 12306    Valid                    H6
105                        Krymka 12364    Valid                 LL3.2
106                       Ijopega 12004    Valid                    H6
107           San Juan Capistrano 23128    Valid                    H6
108                        Demina  6649    Valid                    L6
109                       Pribram 18887    Valid                    H5
110               Boumdeid (2011) 57167    Valid                    L6
111                    Ras Tanura 22395    Valid                    H6
112                       Akyumak   433    Valid             Iron, IVA
113                   Noblesville 16985    Valid                  H4-6
114                         Felix 10081    Valid                 CO3.3
115                       Kadonah 12221    Valid                    H6
116                     Al Zarnkh   447    Valid                   LL5
117                    Sabetmahet 22792    Valid                    H5
118                      Erxleben 10049    Valid                    H6
119                        Jartai 12074    Valid                    L6
120             Frankfort (stone) 10177    Valid             Howardite
121                     Shergotty 23530    Valid Martian (shergottite)
122                       Ankober  2304    Valid                    H4
123                        Beuste  5034    Valid                    L5
124                     Stratford 23728    Valid                    L6
125              Aioun el Atrouss   423    Valid          Diogenite-pm
126                         Enshi 10038    Valid                    H5
127                      Qingzhen 18908    Valid                   EH3
128                       Glatton 10930    Valid                    L6
129                     Jalandhar 12069    Valid                  Iron
130                         Kulak 12369    Valid                    L5
131                        Benton  5026    Valid                   LL6
132                Neuschwanstein 16950    Valid                   EL6
133                     Bholghati  5041    Valid             Howardite
134                    Kisvarsány 12325    Valid                    L6
135                        Sabrum 22793    Valid                   LL6
136                     Gütersloh 11466    Valid                  H3/4
137                      Caratash  5265    Valid                   LL6
138                      Segowlie 23476    Valid                   LL6
139                        Kilabo 12307    Valid                   LL6
140                     Sylacauga 23773    Valid                    H4
141                      Kayakent 12268    Valid           Iron, IIIAB
142                        Maribo 48973    Valid                   CM2
143                         Junan 12210    Valid                    L6
144                          Sone 23667    Valid                    H5
145                   Cross Roads  5476    Valid                    H5
146                  Oliva-Gandia 18012    Valid            Stone-uncl
147                    Magombedze 15387    Valid                  H3-5
148                       Kapoeta 12251    Valid             Howardite
149                        Bovedy  5121    Valid                    L3
150               Portales Valley 18874    Valid                    H6
151                   Pervomaisky 18798    Valid                    L6
152                    Minnichhof 16700    Valid                    OC
153                         Okabe 17998    Valid                    H5
154                      Slavetic 23626    Valid                    H5
155                          Deal  6634    Valid                    L6
156                       Haraiya 11824    Valid         Eucrite-mmict
157                        Łowicz 14718    Valid       Mesosiderite-A3
158                       Orgueil 18026    Valid                   CI1
159                      Chandpur  5321    Valid                    L6
160                        Jiange 12086    Valid                    H5
161                       Sologne 23663    Valid                    H5
162                       Pultusk 18901    Valid                    H5
163                        Picote 18816    Valid            Stone-uncl
164                Château-Renard  5332    Valid                    L6
165                       Allende  2278    Valid                   CV3
166                        Fuyang 10840    Valid            Stone-uncl
167                     Paragould 18101    Valid                   LL5
168                         Seoni 23500    Valid                    H6
169                Inner Mongolia 12037    Valid                    L6
170                          Guêa 11440    Valid            Stone-uncl
171                      Forsbach 10163    Valid                    H6
172                     N'Kandhla 16983    Valid             Iron, IID
173                 Plantersville 18846    Valid                    H6
174                       Kamiomi 12240    Valid                    H5
175                     Pétèlkolé 18800    Valid                    H5
176                     Darmstadt  6603    Valid                    H5
177                 Santa Barbara 23161    Valid                    L4
178                        Ningbo 16980    Valid             Iron, IVA
179                         Duwun  7754    Valid                    L6
180                       Hatford 11855    Valid            Stone-uncl
181                 Daniel's Kuil  5513    Valid                   EL6
182                     Kokubunji 12342    Valid                    L6
183                        Pantar 18098    Valid                    H5
184                       Thuathe 23976    Valid                  H4/5
185                        Lodran 14675    Valid             Lodranite
186        Angra dos Reis (stone)  2302    Valid               Angrite
187                    Pettiswood 18804    Valid            Stone-uncl
188                     Grzempach 11429    Valid                    H5
189                        Desuri  6693    Valid                    H6
190                    Kendrapara 12276    Valid                  H4-5
191                      Luponnas 14757    Valid                  H3-5
192                   Laochenzhen 12466    Valid                    H5
193             Dresden (Ontario)  7731    Valid                    H6
194                         Menow 15485    Valid                    H4
195                      Castalia  5291    Valid                    H5
196                        Konovo 12344    Valid                   LL5
197                        Aguada   398    Valid                    L6
198                       Uzcudun 24140    Valid                     L
199                   Mason Gully 53653    Valid                    H5
200                       Saratov 23176    Valid                    L4
201                 Raghunathpura 22371    Valid            Iron, IIAB
202                  Gurram Konda 11464    Valid                    L6
203                    Mariaville 15419    Valid                  Iron
204                         Ngawi 16966    Valid                 LL3.6
205                    Glanerbrug 10923    Valid                 L/LL5
206                        Madiun 15380    Valid                    L6
207                       Chitado  5356    Valid                    L6
208                          Dowa  7725    Valid            Stone-uncl
209                      Esnandes 10051    Valid                    L6
210                         Hoima 44714    Valid                    H6
211                       Katagum 35465    Valid                    L6
212                Bradford Woods  5128    Valid                     L
213                       Crumlin  5477    Valid                    L5
214                       Chadong  5313    Valid                    L6
215                     Tennasilm 23898    Valid                    L4
216                         Salem 23107    Valid                    L6
217                     Stavropol 23717    Valid                    L6
218                 Rahimyar Khan 31302    Valid                    L5
219                       Barnaul  4947    Valid                    H5
220                          Kiel 12301    Valid                    L6
221                         Ekeby  7776    Valid                    H4
222                     Talampaya 23791    Valid            Eucrite-cm
223                        Kutais 12383    Valid                    H5
224               Itapicuru-Mirim 12056    Valid                    H5
225                         Quija 22361    Valid                     H
226                      Kagarlyk 12227    Valid                    L6
227                   Hassi-Jekna 11852    Valid         Iron, IAB-sHL
228                          Raco 22368    Valid                    H5
229                      Ipiranga 12043    Valid                    H6
230                         Jhung 12085    Valid                    L5
231                     Homestead 11901    Valid                    L5
232                       Dyalpur  7757    Valid              Ureilite
233                Motta di Conti 16762    Valid                    H4
234                     Juancheng 12203    Valid                    H5
235                   Sterlitamak 23724    Valid           Iron, IIIAB
236                     Cynthiana  5500    Valid                 L/LL4
237                     Rampurhat 22385    Valid                    LL
238                        Calivo  5200    Valid            Stone-uncl
239                       Putinga 18905    Valid                    L6
240                  Moore County 16736    Valid            Eucrite-cm
241                     Ceniceros  5306    Valid                  L3.7
242                    Karloowala 12263    Valid                    L6
243            Santa Lucia (2008) 50909    Valid                    L6
244                     Grüneberg 11426    Valid                    H4
245                      Machinga 15371    Valid                    L6
246                     Ash Creek 48954    Valid                    L6
247                        Soroti 23671    Valid       Iron, ungrouped
248                     Rose City 22766    Valid                    H5
249                       Claxton  5374    Valid                    L6
250                      Richmond 22603    Valid                   LL5
251                      Belville  5009    Valid                    OC
252                     Medanitos 15467    Valid            Eucrite-cm
253                        Shytal 23584    Valid                    L6
254                 Appley Bridge  2318    Valid                   LL6
255                        Hessle 11878    Valid                    H5
256                    Kirbyville 12321    Valid         Eucrite-mmict
257                    La Criolla 12396    Valid                    L6
258                     Sitathali 23616    Valid                    H5
259                       Dharwar  6699    Valid                    OC
260                         Mafra 15383    Valid                  L3-4
261                    Louisville 14716    Valid                    L6
262                        Shalka 23521    Valid             Diogenite
263         São Jose do Rio Preto 23171    Valid                    H4
264                   Peace River 18180    Valid                    L6
265                     Searsmont 23472    Valid                    H5
266                       Jianshi 12087    Valid           Iron, IIIAB
267                      Songyuan 23668    Valid                    L6
268                     Mauritius 15447    Valid                    L6
269                          Bath  4974    Valid                    H4
270                 Puerto Lápice 45984    Valid            Eucrite-br
271                    Pillistfer 18822    Valid                   EL6
272                       Muddoor 16841    Valid                    L5
273                    Phum Sambo 18811    Valid                    H4
274                       Butsura  5183    Valid                    H6
275                      Luotolax 14756    Valid             Howardite
276                        Mokoia 16713    Valid                   CV3
277                       Suizhou 23738    Valid                    L6
278                       Karakol 12256    Valid                   LL6
279                         Bells  5005    Valid                C2-ung
280                         Monze 16733    Valid                    L6
281                      Sasagase 23187    Valid                     H
282                        Maridi 15421    Valid                    H6
283                   New Orleans 16960    Valid                    H5
284                     Shikarpur 23534    Valid                    L6
285                     Chergach  47347    Valid                    H5
286                Bjelaja Zerkov  5063    Valid                    H6
287                         Okano 18000    Valid            Iron, IIAB
288                        Sayama 23192    Valid                   CM2
289                        Archie  2329    Valid                    H6
290                       Ourique 18052    Valid                    H4
291                  Padvarninkai 18069    Valid         Eucrite-mmict
292                     Isthilart 12053    Valid                    H5
293                 Sutter's Mill 55529    Valid                     C
294                          Mocs 16709    Valid                  L5-6
295                        Leedey 12755    Valid                    L6
296                  Fenghsien-Ku 10086    Valid                    H5
297               Boumdeid (2003) 57168    Valid                    L6
298                    Farmington 10074    Valid                    L5
299                      Pnompehn 18851    Valid                    L6
300                  Hallingeberg 11479    Valid                  L3.4
301                 Berlanguillas  5029    Valid                    L6
302                        Rewari 22593    Valid                    L6
303                Leighlinbridge 12759    Valid                    L6
304                        Madrid 15382    Valid                    L6
305                        Oesede 17988    Valid                    H5
306                  Donga Kohrod  7707    Valid                    H6
307                  Leeuwfontein 12756    Valid                    L6
308                       Bensour  5024    Valid                   LL6
309                        Gursum 11465    Valid                  H4/5
310                       Lohawat 14678    Valid             Howardite
311                Djati-Pengilon  7652    Valid                    H6
312                      Shupiyan 23583    Valid                    H6
313                    Långhalsen 12461    Valid                    L6
314                     Binningup  5051    Valid                    H5
315                        Patora 18112    Valid                    H6
316                     Rio Negro 22611    Valid                    L4
317                     Dolgovoli  7659    Valid                    L6
318                   Schenectady 23458    Valid                    H5
319                         Daule 51559    Valid                    L5
320                       Malakal 15394    Valid                    L5
321                    Revelstoke 22592    Valid                   CI1
322                 Rich Mountain 22597    Valid                    L6
323                  Mabwe-Khoywa 14764    Valid                    L5
324                Cabezo de Mayo  5185    Valid                 L/LL6
325                       Kaprada 47357    Valid                  L5/6
326                   St.-Chinian 23097    Valid                    L6
327                Cangas de Onis  5252    Valid                    H5
328                      Ruhobobo 22780    Valid                    L6
329                        Malaga 15393    Valid                    OC
330                     Pontlyfni 18865    Valid             Winonaite
331                       Beyrout  5035    Valid                 LL3.8
332                      Kidairat 12300    Valid                    H6
333                      Pirgunje 18834    Valid                    L6
334                        Renqiu 22589    Valid                    L6
335                        Assisi  2353    Valid                    H5
336                       Samelia 23115    Valid           Iron, IIIAB
337                      Iguaracu 12003    Valid                    H5
338           San Pedro de Quiles 23130    Valid                    L6
339                      Holbrook 11894    Valid                 L/LL6
340                     Blackwell  5068    Valid                    L5
341                         Kiffa 12303    Valid                    H5
342                   Breitscheid  5134    Valid                    H5
343                         Didim 47350    Valid                  H3-5
344                     Juromenha 12213    Valid           Iron, IIIAB
345                        Nulles 17959    Valid                    H6
346                       Nagaria 16892    Valid            Eucrite-cm
347                          Tauk 23887    Valid                    L6
348                        Aumale  4899    Valid                    L6
349                      Tjerebon 24012    Valid                    L5
350                        Elbert  7822    Valid                   LL6
351                        Atarra  4883    Valid                    L4
352                      Albareto   453    Valid                 L/LL4
353                Cold Bokkeveld  5397    Valid                   CM2
354                       Mifflin 52090    Valid                    L5
355                 Norton County 17922    Valid               Aubrite
356                         Natal 16923    Valid            Stone-uncl
357                Krasnyi Klyuch 12357    Valid                    H5
358                         Hotse 11913    Valid                    L6
359                         Bogou  5097    Valid          Iron, IAB-MG
360                      Jesenice 51589    Valid                    L6
361                     Stolzenau 23726    Valid            Stone-uncl
362                 Fort Flatters 10166    Valid            Stone-uncl
363                   Rockhampton 22640    Valid            Stone-uncl
364                        Milena 16640    Valid                    L6
365                    Petersburg 18801    Valid         Eucrite-pmict
366                    Canon City  5253    Valid                    H6
367                Jackalsfontein 12065    Valid                    L6
368                      Cañellas  5251    Valid                    H4
369                       Dwaleni  7755    Valid                  H4-6
370                           Aïr   424    Valid                    L6
371                   Forest Vale 10120    Valid                    H4
372                     La Charca 12394    Valid                    OC
373                        Nogoya 16989    Valid                   CM2
374                     Novo-Urei 17933    Valid              Ureilite
375                        Bununu  5165    Valid             Howardite
376                       Senboku 23496    Valid                    H6
377                  Douar Mghila  7723    Valid                   LL6
378                      Kamalpur 12238    Valid                    L6
379                      Pampanga 18093    Valid                    L5
380                         Mineo 16696    Valid             Pallasite
381                    N'Goureyma 16968    Valid       Iron, ungrouped
382                       Kharkov 12291    Valid                    L6
383                     La Colina 12395    Valid                    H5
384                         Linum 14655    Valid                    L6
385                      Schellin 23457    Valid                     L
386                       Numakai 17960    Valid                    H4
387                      Sołtmany 53829    Valid                    L6
388               Harrison County 11842    Valid                    L6
389                        Meerut 15469    Valid                    H5
390                  Higashi-koen 11883    Valid                    H5
391                   Park Forest 18106    Valid                    L5
392                       Rakovka 22376    Valid                    L6
393                        Ornans 18030    Valid                 CO3.4
394                       Chajari  5316    Valid                    L5
395                       Parambu 18102    Valid                   LL5
396                  Independence 12028    Valid                    L6
397                       Guidder 11443    Valid                   LL5
398                  Beaver Creek  4986    Valid                    H5
399                  Lavrentievka 12743    Valid                    L6
400                     Futtehpur 10839    Valid                    L6
401                       Malvern 15400    Valid         Eucrite-pmict
402                     Bishunpur  5060    Valid                LL3.15
403                 Kunya-Urgench 12379    Valid                    H5
404                         Narni 16914    Valid            Stone-uncl
405                      Buschhof  5178    Valid                    L6
406                     Tomakovka 24019    Valid                   LL6
407                        Kyushu 12390    Valid                    L6
408                    Mayo Belwa 15451    Valid               Aubrite
409                        Ottawa 18046    Valid                   LL6
410                      Mjelleim 16707    Valid                     H
411                     Emmaville 10033    Valid         Eucrite-mmict
412                  Santa Isabel 23165    Valid                    L6
413                      Ramsdorf 22386    Valid                    L6
414                   Devri-Khera  6696    Valid                    L6
415                         Hoxie 11915    Valid                    OC
416                        Borkut  5113    Valid                    L5
417                     Galim (b) 10849    Valid              EH3/4-an
418                        Karatu 12258    Valid                   LL6
419                     Peekskill 18782    Valid                    H6
420                          Meru 15491    Valid                   LL6
421                       Blansko  5072    Valid                    H6
422                         Quesa 22360    Valid         Iron, IAB-ung
423                     Chassigny  5331    Valid Martian (chassignite)
424                   Bur-Gheluai  5169    Valid                    H5
425                      Aumieres  4900    Valid                    L6
426                          Kabo 12220    Valid                    H4
427                   Forest City 10119    Valid                    H5
428                       Bachmut  4917    Valid                    L6
429                        Ergheo 10044    Valid                    L5
430                     Eichstädt  7775    Valid                    H5
431                    Tambakwatu 23795    Valid                    L6
432                   Beni M'hira  5018    Valid                    L6
433                     Boriskino  5112    Valid                   CM2
434                 Ojuelos Altos 17997    Valid                    L6
435                      Kamsagar 12241    Valid                    L6
436                       Indarch 12027    Valid                   EH4
437       Phillips County (stone) 18808    Valid                    L6
438                       Success 23736    Valid                    L6
439                        Anlong  2305    Valid                    H5
440                    Niger (L6) 16974    Valid                    L6
441                    Nikolaevka 16976    Valid                    H4
442                      Pesyanoe 18799    Valid               Aubrite
443                        Maziba 15454    Valid                    L6
444                  Stretchleigh 23732    Valid            Stone-uncl
445                       Norfork 16994    Valid           Iron, IIIAB
446                Nuevo Mercurio 17938    Valid                    H5
447                       Rasgrad 22396    Valid            Stone-uncl
448                        Baroti  4949    Valid                    L6
449                         Kemer 53654    Valid                    L4
450                        Owrucz 18062    Valid                    OC
451                        Kakowa 12231    Valid                    L6
452                         Bawku  4976    Valid                   LL5
453                  Krasnoi-Ugol 12355    Valid                    L6
454                         Kesen 12286    Valid                    H4
455                        Košice 53810    Valid                    H5
456                        Jodiya 47362    Valid                    L5
457                     Grefsheim 11196    Valid                    L5
458                     Baszkówka  4957    Valid                    L5
459                      Cronstad  5474    Valid                    H5
460                         Lissa 14661    Valid                    L6
461                 Kijima (1906) 12305    Valid            Stone-uncl
462                        Kediri 12270    Valid                    L4
463                      Reliegos 22584    Valid                    L5
464                        Brient  5140    Valid         Eucrite-pmict
465                 Repeev Khutor 22590    Valid             Iron, IIF
466                        Hamlet 11485    Valid                   LL4
467                         Chela  5338    Valid                    H4
468                  Po-wang Chen 18879    Valid                    LL
469                        Athens  4885    Valid                   LL6
470                         Bhola  5040    Valid                 LL3-6
471                          Agen   392    Valid                    H5
472                        Benoni  5023    Valid                    H6
473                   Khor Temiki 12299    Valid               Aubrite
474                      Moorleah 16738    Valid                    L6
475                      Tamdakht 48691    Valid                    H5
476                   Nagy-Borové 16893    Valid                    L5
477                        Jelica 12078    Valid                   LL6
478                        Gashua 44882    Valid                    L6
479                       Git-Git 10919    Valid                    L6
480                      Ferguson 10088    Valid                    OC
481                   Bremervörde  5135    Valid                H/L3.9
482                        Tenham 23897    Valid                    L6
483                        Hungen 11986    Valid                    H6
484                         Parsa 18109    Valid                   EH3
485                     Dhurmsala  7640    Valid                   LL6
486                    Charwallas  5330    Valid                    H6
487                       Dhajala  6698    Valid                  H3.8
488                 Kangra Valley 12246    Valid                    H5
489               Palca de Aparzo 18074    Valid                    L5
490                 Botschetschki  5117    Valid                    L4
491                       Kasauli 30740    Valid                    H4
492               Red Canyon Lake 53502    Valid                    H5
493                        Aarhus     2    Valid                    H6
494                       Guareña 11439    Valid                    H6
495                      Florence 10111    Valid                    H3
496                         Merua 15492    Valid                    H5
497                     Johnstown 12198    Valid             Diogenite
498                          Cali 45976    Valid                  H/L4
499                       Kerilis 12282    Valid                    H5
500                        Qidong 18907    Valid                 L/LL5
501                     Lost City 14711    Valid                    H5
502                    Shuangyang 23582    Valid                    H5
503                  Bath Furnace  4975    Valid                    L6
504                     Kasamatsu 12266    Valid                     H
505                       Dundrum  7745    Valid                    H5
506                      El Tigre  7819    Valid                    L6
507                         Sinai 23606    Valid                    L6
508                      Carancas 45817    Valid                  H4-5
509                      Atemajac  4884    Valid                    L6
510                     Palinshih 18077    Valid                  Iron
511                      Sulagiri 48951    Valid                   LL6
512                        Duruma  7752    Valid                    L6
513                     Nedagolla 16935    Valid       Iron, ungrouped
514                    St. Michel 23093    Valid                    L6
515                        Banten  4938    Valid                   CM2
516                     Pricetown 18888    Valid                    L6
517                      Ramnagar 22384    Valid                    L6
518                       Orvinio 18034    Valid                    H6
519                      Stannern 23713    Valid         Eucrite-mmict
520                       Dongtai  7708    Valid                   LL6
521                     Maryville 15436    Valid                    L6
522                        Manych 15409    Valid                 LL3.4
523                        Bahjoi  4922    Valid         Iron, IAB-sLL
524                          Gifu 10914    Valid                    L6
525                    Alta'ameem  2284    Valid                   LL5
526                        Muraid 16874    Valid                    L6
527                          Lucé 14724    Valid                    L6
528                      Silistra 55584    Valid        Achondrite-ung
529             Capilla del Monte  5264    Valid                    H6
530                      Nanjemoy 16904    Valid                    H6
531                    Forksville 10123    Valid                    L6
532                      Barntrup  4948    Valid                   LL4
533          Dominion Range 03240 32592    Valid                   LL5
534                         Lunan 14754    Valid                    H6
535                        Baxter  4977    Valid                    L6
536               Alby sur Chéran   458    Valid         Eucrite-mmict
537                   Schönenberg 23460    Valid                    L6
538                      Bjurböle  5064    Valid                 L/LL4
539                  Myhee Caunta 16887    Valid                    OC
540                        Bansur  4936    Valid                    L6
541                        Granes 10956    Valid                    L6
542                       Braunau  5133    Valid            Iron, IIAB
543                      Manegaon 15403    Valid             Diogenite
544                     Hedeskoga 11869    Valid                    H5
545                        Nogata 16988    Valid                    L6
546                       Holetta 11895    Valid            Stone-uncl
547                          Kobe 12336    Valid                   CK4
548                       Achiras   370    Valid                    L6
549                        Oterøy 18042    Valid                    L6
550                         Ohaba 17995    Valid                    H5
551                       Launton 12740    Valid                    L6
552                          Fuhe 52412    Valid                    L5
553                       Ochansk 17979    Valid                    H4
554                 Guangmingshan 11435    Valid                    H5
555                     Tieschitz 23989    Valid                H/L3.6
556                      Dashoguz  6604    Valid                    H5
557                          Kaba 12218    Valid                   CV3
558                      Jumapalo 12209    Valid                    L6
559                        Rupota 22783    Valid                  L4-6
560                Savtschenskoje 23190    Valid                   LL4
561                      Galapian 10846    Valid                    H6
562                        Berduc 48975    Valid                    L6
563                       Sediköy 23473    Valid                    L6
564                        Jodzie 12173    Valid             Howardite
565                          Abee     6    Valid                   EH4
566                       Garland 10861    Valid          Diogenite-pm
567                         Gujba 11449    Valid                   CBa
568                     Bialystok  5042    Valid         Eucrite-pmict
569                        Favars 10078    Valid                    H5
570             Colby (Wisconsin)  5395    Valid                    L6
571                      Manbhoom 15402    Valid                   LL6
572                        Jonzac 12202    Valid         Eucrite-mmict
573                   Sete Lagoas 23504    Valid                    H4
574                         Mbale 15455    Valid                  L5/6
575                       Sindhri 23611    Valid                    H5
576                      Akbarpur   427    Valid                    H4
577                     Charlotte  5328    Valid             Iron, IVA
578                  Maria Linden 15418    Valid                    L4
579                         Macau 15370    Valid                    H5
580                      Magnesia 15386    Valid         Iron, IAB-sHL
581                 Saint-Séverin 23102    Valid                   LL6
582                      Killeter 12309    Valid                    H6
583             Maigatari-Danduma 30751    Valid                  H5/6
584                       Rembang 22585    Valid             Iron, IVA
585                    Gnadenfrei 10936    Valid                    H5
586                           Lua 14721    Valid                    L5
587             Castel Berardenga  5292    Valid            Stone-uncl
588                       Khanpur 12289    Valid                   LL5
589                       Morávka 16742    Valid                    H5
590                        Tahara 23784    Valid                  H4/5
591                      Dandapur  5511    Valid                    L6
592                     Hraschina 11916    Valid             Iron, IID
593                         Tabor 23776    Valid                    H5
594                      Burnwell  5175    Valid                 H4-an
595                       Ishinga 12049    Valid                     H
596                        Bustee  5181    Valid               Aubrite
597                       Mazapil 15453    Valid         Iron, IAB-sLL
598                     Sultanpur 23741    Valid                 L/LL6
599                        Rowton 22773    Valid           Iron, IIIAB
600                          Asco  2345    Valid                    H6
601                   High Possil 11884    Valid                    L6
602                     Ouadangou 56729    Valid                    L5
603                        Khetri 12296    Valid                    H6
604                    Sixiangkou 23619    Valid                    L5
605                       Devgaon  6694    Valid                  H3.8
606                        Taonan 23873    Valid                    L5
607                Almahata Sitta 48915    Valid           Ureilite-an
608                        Aachen     1    Valid                    L5
609                    Arbol Solo  2325    Valid                    H5
610                        Geidam 10870    Valid                    H5
611                        Pokhra 18858    Valid                    H5
612        Kandahar (Afghanistan) 12243    Valid                    L6
613                 Middlesbrough 16632    Valid                    L6
614               Dong Ujimqin Qi  7706    Valid          Mesosiderite
615                       Bilanga  5045    Valid             Diogenite
616                         Cacak  5187    Valid                    OC
617                     Glasatovo 10926    Valid                    H4
618                           Ski 23621    Valid                    L6
619                   Chandakapur  5320    Valid                    L5
620                       Coleman  5401    Valid                    L6
621                        Aleppo   462    Valid                    L6
622                  Campos Sales  5249    Valid                    L5
623                         Ohuma 17996    Valid                    L5
624                     Moradabad 16740    Valid                    L6
625                   Kuttippuram 12384    Valid                    L6
626                Caswell County  5296    Valid                    OC
627                      Guangrao 11437    Valid                    L6
628               Monahans (1998) 16719    Valid                    H5
629                       Bandong  4935    Valid                   LL6
630                     Malampaka 15395    Valid                     H
631              Richland Springs 22602    Valid                    OC
632                 Marion (Iowa) 15424    Valid                    L6
633                     Andreevka  2296    Valid                    L3
634                     Innisfree 12039    Valid                    L5
635                       Malotas 15397    Valid                    H5
636                       Sheyang 23531    Valid                    L6
637             Miller (Arkansas) 16645    Valid                    H5
638                     Pavlograd 18176    Valid                    L6
639                       Pulsora 18899    Valid                    H5
640                         Dosso  7722    Valid                    L6
641                   Le Pressoir 12748    Valid                    H5
642                         Barea  4946    Valid       Mesosiderite-A1
643                     Les Ormes 12769    Valid                    L6
644                        Haverö 11859    Valid              Ureilite
645                     Glanggang 10924    Valid                  H5-6
646                    Phuoc-Binh 18812    Valid                    L5
647                          Moss 36592    Valid                 CO3.6
648                     Madhipura 15379    Valid                     L
649                       Juvinas 12214    Valid         Eucrite-mmict
650                    Krähenberg 12353    Valid                   LL5
651                    Mihonoseki 16635    Valid                    L6
652                         Alais   448    Valid                   CI1
653  St. Christophe-la-Chartreuse 23082    Valid                    L6
654                         Pavel 18173    Valid                    H5
655                      Djoumine  7657    Valid                  H5-6
656                      Ellemeet 10019    Valid             Diogenite
657                      Ofehértó 17990    Valid                    L6
658                    Mahadevpur 47361    Valid                  H4/5
659                      Adhi Kot   379    Valid                   EH4
660                        Aubres  4893    Valid               Aubrite
661                       Allegan  2276    Valid                    H5
662                      Kukschin 12368    Valid                    L6
663                      Tirupati 24009    Valid                    H6
664                      Jemlapur 12079    Valid                    L6
665                     Salzwedel 23114    Valid                   LL5
666                   Chiang Khan  5345    Valid                    H6
667                      Leonovka 12765    Valid                    L6
668                 Saint-Sauveur 23101    Valid                   EH5
669                       Messina 15495    Valid                    L5
670                      Peramiho 18792    Valid         Eucrite-mmict
671                   Lichtenberg 14646    Valid                    H6
672                     Lakangaon 12435    Valid         Eucrite-mmict
673                  Modoc (1905) 16711    Valid                    L6
674                       Pohlitz 18853    Valid                    L5
675                      Krutikha 12363    Valid                    OC
676                        Guibga 11442    Valid                    L5
677                  Los Martinez 14708    Valid                    L6
678                   Niger (LL6) 16975    Valid                   LL6
679                         Seres 23501    Valid                    H4
680                  Mount Browne 16766    Valid                    H6
681                        Mighei 16634    Valid                   CM2
682                        Tilden 23998    Valid                    L6
683                   El Idrissia  7807    Valid                    L6
684                    Gao-Guenie 10854    Valid                    H5
685                    Piquetberg 18832    Valid                     H
686                         Sopot 23670    Valid                    OC
687                    Bruderheim  5156    Valid                    L6
688                        Lishui 14659    Valid                    L5
689                         Eagle  7760    Valid                   EL6
690                  Klein-Wenden 12332    Valid                    H6
691                     Suchy Dul 23737    Valid                    L6
692                        Omolon 18019    Valid        Pallasite, PMG
693              Meester-Cornelis 15470    Valid                    H5
694               Ste. Marguerite 23099    Valid                    H4
695                      Haripura 11829    Valid                   CM2
696                         Karkh 12262    Valid                    L6
697                       Kangean 12245    Valid                    H5
698                   New Concord 16953    Valid                    L6
699                       Tissint 54823    Valid Martian (shergottite)
700                   Noyan-Bogdo 17936    Valid                    L6
701                      Acapulco    10    Valid           Acapulcoite
702                    Soko-Banja 23661    Valid                   LL4
703                    Feid Chair 10080    Valid                    H4
704                          Bori  5111    Valid                    L6
705                         Sagan 22796    Valid            Stone-uncl
706                   Drake Creek  7728    Valid                    L6
707                     Quenggouk 22358    Valid                    H4
708                          Mike 16636    Valid                    L6
709                  Nan Yang Pao 16903    Valid                    L6
710                       Changde  5322    Valid                    H5
711             San Pedro Jacuaro 34063    Valid                   LL6
712                    Chantonnay  5325    Valid                    L6
713                    Richardton 22599    Valid                    H5
714                    Oum Dreyga 31282    Valid                  H3-5
715                    Mooresfort 16737    Valid                    H5
716                          Sfax 23512    Valid                    L6
717                  Little Piney 14664    Valid                    L5
718                 Castrovillari  5295    Valid            Stone-uncl
719                       Sevilla 23508    Valid                   LL4
720                         Tauti 23888    Valid                    L6
721                      Gorlovka 10949    Valid                  H3.7
722                       Supuhee 23760    Valid                    H6
723                       Banswal  4937    Valid                    L5
724                         Otomi 18045    Valid                     H
725                         Oesel 17989    Valid                    L6
726                       Alberta   454    Valid                     L
727                     Nikolskoe 16977    Valid                    L4
728                 Dyarrl Island  7758    Valid       Mesosiderite-A1
729                         Mezel 16627    Valid                    L6
730                 Arroyo Aguiar  2340    Valid                    H5
731                      Kheragur 12294    Valid                    L6
732                     Mangwendi 15405    Valid                   LL6
733                  Nobleborough 16984    Valid         Eucrite-pmict
734                   Kuleschovka 12370    Valid                    L6
735                Oued el Hadjar 18050    Valid                   LL6
736                         Fermo 10091    Valid                  H3-5
737                   Estherville 10059    Valid     Mesosiderite-A3/4
738                       Hashima 11848    Valid                    H4
739                     Mulletiwu 16851    Valid                     L
740                      Sazovice 23455    Valid                    L5
741              Cherokee Springs  5340    Valid                   LL6
742                   Chelyabinsk 57165    Valid                   LL5
743                  Mauerkirchen 15446    Valid                    L6
744                     Sevrukovo 23509    Valid                    L5
745                   Muzaffarpur 16885    Valid         Iron, IAB-sHL
746                    Maromandia 15430    Valid                    L6
747                  Chervony Kut  5342    Valid         Eucrite-mmict
748                       Andhara  2294    Valid            Stone-uncl
749                        Bhagur  5037    Valid                    L6
750                    Beddgelert  4993    Valid                    H5
751                     Novy-Ergi 17934    Valid            Stone-uncl
752                       Perpeti 18793    Valid                    L6
753                     Kakangari 12230    Valid                    K3
754            Distrito Quebracho  7649    Valid                    H4
755                       Orlando 34489    Valid               Eucrite
756                     Ningqiang 16981    Valid                C3-ung
757               Mount Tazerzait 16804    Valid                    L5
758          Black Moshannan Park  5065    Valid                    L5
759                        Rodach 22642    Valid            Stone-uncl
760                         Itqiy 12058    Valid                EH7-an
761                        Pollen 18860    Valid                   CM2
762                        Dunhua  7749    Valid            Stone-uncl
763                      Harleton 11830    Valid                    L6
764                       Quincay 22363    Valid                    L6
765          Dominion Range 03239 32591    Valid                    L6
766                      Danville  5514    Valid                    L6
767                     Montferré 16727    Valid                    H5
768                      Marmande 15429    Valid                    L5
769                     Farmville 10075    Valid                    H4
770                   Montlivault 16729    Valid                    L6
771                      Fukutomi 10836    Valid                    L5
772                      Olivenza 18013    Valid                   LL5
773                Buzzard Coulee 48654    Valid                    H4
774                      Sakauchi 23103    Valid                  Iron
775                       Misshof 16703    Valid                    H5
776                   Ban Rong Du  4934    Valid       Iron, ungrouped
777                      Kunashak 12377    Valid                    L6
778                       Kalumbi 12236    Valid                    L6
779             Bunburra Rockhole 48653    Valid               Eucrite
780                        Naragh 16909    Valid                    H6
781             El Paso de Aguila 45977    Valid                    H5
782                      Gopalpur 10948    Valid                    H6
783                     Gujargaon 11448    Valid                    H5
784                        Lancon 12456    Valid                    H6
785                      Khairpur 12288    Valid                   EL6
786                     Murchison 16875    Valid                   CM2
787                    Judesegeri 12207    Valid                    H6
788                    Lillaverke 14650    Valid                    H5
789                        Cosina  5451    Valid                    H5
790                    Strathmore 23729    Valid                    L6
791                      Limerick 14652    Valid                    H5
792                      Grosnaja 11206    Valid                   CV3
793                         Huaxi 54719    Valid                    H5
794                         Chail  5314    Valid                    H6
795               Prambachkirchen 18883    Valid                    L6
796                 Palolo Valley 18082    Valid                    H5
797                      Tathlith 23885    Valid                    L6
798                     Dubrovnik  7736    Valid                  L3-6
799                         Lancé 12455    Valid                 CO3.5
800                       Soheria 23660    Valid                    OC
801                        Salles 23111    Valid                    L5
802                        Lesves 12772    Valid                    L6
803                    Meuselbach 16626    Valid                    L6
804                     Tillaberi 23999    Valid                    L6
805                      Rumuruti 22782    Valid                R3.8-6
806                   Chernyi Bor  5339    Valid                    H4
807                        Avilez  4907    Valid                     H
808                   Tagish Lake 23782    Valid                C2-ung
809                        Épinal 10041    Valid                    H5
810               Aire-sur-la-Lys   425    Valid               Unknown
811                  Sikhote-Alin 23593    Valid            Iron, IIAB
812                   Mount Vaisi 16805    Valid            Stone-uncl
813                         Delhi  6642    Valid                    L5
814                        Durala  7750    Valid                    L6
815                       Dokachi  7658    Valid                    H5
816                 Chetrinahatti  5344    Valid            Stone-uncl
817                     Punganaru 18902    Valid            Stone-uncl
818                       Ibrisim 11994    Valid                    OC
819          St. Germain-du-Pinel 23087    Valid                    H6
820                         Bursa  5177    Valid                    L6
821                        Chaves  5334    Valid             Howardite
822                       Ryechki 22791    Valid                    L5
823                     Pasamonte 18110    Valid         Eucrite-pmict
824              Borgo San Donino  5110    Valid                   LL6
825                       Palmyra 18079    Valid                    L3
826                        Paitan 18072    Valid                    H6
827                     Ranchapur 22387    Valid                    H4
828                    La Bécasse 12392    Valid                    L6
829                    Santa Cruz 23164    Valid                   CM2
830            Peña Blanca Spring 18786    Valid               Aubrite
831                Cape Girardeau  5260    Valid                    H6
832                     Knyahinya 12335    Valid                 L/LL5
833                       Clohars  5383    Valid                    L4
834                     Ensisheim 10039    Valid                   LL6
835                     Doroninsk  7718    Valid                  H5-7
836                          Mern 15489    Valid                    L6
837                          Sena 23495    Valid                    H4
838                     Pirthalla 18835    Valid                    H6
839                      Nassirah 16922    Valid                    H4
840                     Kendleton 12275    Valid                    L4
841                  Charsonville  5329    Valid                    H6
842                      Honolulu 11904    Valid                    L5
843                        Bherai  5039    Valid                    L6
844                        Mardan 15414    Valid                    H5
845                   Seldebourak 23483    Valid                    H5
846                       Mässing 15443    Valid             Howardite
847                 Moti-ka-nagla 16759    Valid                    H6
848                           Nio 16982    Valid                  H3-4
849                        Monroe 16720    Valid                    H4
850                       Hökmark 11893    Valid                    L4
851                  Gross-Divina 11207    Valid                    H5
852                          Tané 23801    Valid                    L5
853                       Bo Xian  5090    Valid                 LL3.9
854                        Andura  2298    Valid                    H6
855                       Nantong 16907    Valid                    H6
856                   Bloomington  5076    Valid                   LL6
857                    Boguslavka  5098    Valid            Iron, IIAB
858                  Novy-Projekt 17935    Valid                    OC
859                     Beardsley  4984    Valid                    H5
860                         Nerft 16945    Valid                    L6
861                       Molteno 16717    Valid             Howardite
862                 Mamra Springs 15401    Valid                    L6
863                    St. Mark's 23090    Valid                   EH5
864                   Piancaldoli 18813    Valid                 LL3.4
865                   Garhi Yasin 10860    Valid             Iron, IIE
866             Barcelona (stone)  4944    Valid                    OC
867                     Aldsworth   461    Valid                   LL5
868                       Renazzo 22586    Valid                   CR2
869                         Nagai 16890    Valid                    L6
870                       Nicorps 16970    Valid            Stone-uncl
871             St. Denis Westrem 23083    Valid                    L6
872                        Lusaka 14759    Valid               Unknown
873                        Kalaba 12232    Valid                    H4
874                       Kusiali 12382    Valid                    L6
875                        Fisher 10107    Valid                    L6
876                       Simmern 23603    Valid                    H5
877                       Barwell  4954    Valid                    L5
878                       Neagari 16934    Valid                    L6
879                       Andover  2295    Valid                    L6
880                      Minamino 16692    Valid                     L
881                      Jalanash 12068    Valid              Ureilite
882                       Marilia 15422    Valid                    H4
883                       Heredia 11875    Valid                    H5
884                    Alfianello   466    Valid                    L6
885              Rivolta de Bassi 22614    Valid            Stone-uncl
886                  Ploschkovitz 18849    Valid                    L5
887                        Erevan 10043    Valid             Howardite
888                      Berthoud 47355    Valid         Eucrite-mmict
889                   Cabin Creek  5186    Valid           Iron, IIIAB
890                    Patrimonio 18116    Valid                    L6
891                       Ibitira 11993    Valid         Eucrite-mmict
892                       Dahmani  5504    Valid                   LL6
893                  De Cewsville  6621    Valid                    H6
894                        Murray 16882    Valid                   CM2
895                         Tjabe 24011    Valid                    H6
896                    St. Mesmin 23092    Valid                   LL6
897                          Kulp 12373    Valid                    H6
898                  Collescipoli  5403    Valid                    H5
899                 Nakhon Pathom 16899    Valid                    L6
900            Rancho de la Presa 22390    Valid                    H5
901                            Pê 18179    Valid                    L6
902                      Selakopi 23481    Valid                    H5
903                      Portugal 18876    Valid            Stone-uncl
904                       Sungach 23745    Valid                    H5
905                      Cereseto  5308    Valid                    H5
906                        Molina 16715    Valid                    H5
907                      Jamkheir 12072    Valid                    H6
908                         Pitts 18837    Valid         Iron, IAB-ung
909                         Sivas 23617    Valid                    H6
910                       Grimsby 50911    Valid                    H5
911                    Takenouchi 23789    Valid                    H5
912                       Ortenau 18033    Valid            Stone-uncl
913                         Aztec  4913    Valid                    L6
914                       Baldwyn  4926    Valid                    L6
915                         Siena 23586    Valid                   LL5
916                           Apt  2320    Valid                    L6
917                      Elenovka  7824    Valid                    L5
918                 Bald Mountain  4925    Valid                    L4
919                      Lahrauli 12433    Valid              Ureilite
920                     Rochester 22637    Valid                    H6
921                      Gyokukei 11467    Valid                    OC
922                   Le Teilleul 12749    Valid             Howardite
923                      Chainpur  5315    Valid                 LL3.4
924                        Galkiv 10850    Valid                    H4
925                      Macibini 15372    Valid         Eucrite-pmict
926                       Ichkala 11995    Valid                    H6
927                    Kuznetzovo 12385    Valid                    L6
928                        Patwar 18171    Valid       Mesosiderite-A1
929                      Lalitpur 12451    Valid                    L6
930                        Aomori  2313    Valid                    L6
931                      Nawapali 16927    Valid                   CM2
932                         Thika 54493    Valid                    L6
933                   Avanhandava  4905    Valid                    H4
934                        Gambat 10851    Valid                    L6
935                     Kavarpura 47351    Valid          Iron, IIE-an
936                       Jolomba 12199    Valid                   LL6
937                     Paranaiba 18103    Valid                    L6
938                       Forsyth 10164    Valid                    L6
939           Cumulus Hills 04075 32531    Valid             Pallasite
940                        Kingai 12316    Valid                    H6
941                        Hedjaz 11870    Valid                L3.7-6
942                         Ehole  7774    Valid                    H5
943                       Akwanga   432    Valid                     H
944                          Roda 22641    Valid             Diogenite
945                       Mianchi 16631    Valid                    H5
946                  Sioux County 23614    Valid         Eucrite-mmict
947                      Borodino  5114    Valid                    H5
948                  Piplia Kalan 18831    Valid         Eucrite-mmict
949                     Kitchener 12326    Valid                    L6
950                          Avce  4906    Valid            Iron, IIAB
951                   Centerville  5307    Valid                    H5
952                     Tianzhang 23984    Valid                    H5
953             St. Mary's County 23091    Valid                 LL3.3
954                    Marjalahti 15426    Valid        Pallasite, PMG
955              Pavlodar (stone) 18175    Valid                    H5
956                        Béréba  5028    Valid         Eucrite-mmict
957                        Erakot 10042    Valid                   CM2
958                     Benguerir 30443    Valid                   LL6
959                      Chitenay  5357    Valid                    L6
960                      Chisenga  5355    Valid           Iron, IIIAB
961                     Lundsgård 14755    Valid                    L6
962                        Essebi 10055    Valid                C2-ung
963                      Timochin 24004    Valid                    H5
964                         Lixna 14670    Valid                    H4
965                        Denver  6660    Valid                    L6
966                          Thal 23908    Valid                    H6
967                   Bassikounou 44876    Valid                    H5
968               Grossliebenthal 11208    Valid                    L6
969                      Girgenti 10917    Valid                    L6
970                        Angers  2301    Valid                    L6
971                    Gumoschnik 11450    Valid                    H5
972                      Narellan 16912    Valid                    L6
973                       Tadjera 23778    Valid                    L5
974                         Ratyn 22398    Valid            Stone-uncl
975                         Jilin 12171    Valid                    H5
976                    Tatahouine 23884    Valid             Diogenite
977                  Mezö-Madaras 16628    Valid                  L3.7
978                        Kaidun 12228    Valid                   CR2
979                        Sinnai 23613    Valid                    H6
980                      Dunbogan  7743    Valid                    L6
981                       Chicora  5349    Valid                   LL6
982                     Bethlehem  5032    Valid                     H
983                   Nammianthal 16902    Valid                    H5
984                     Hiroshima 11889    Valid                    H5
985                         Perth 18797    Valid                   LL5
986                 Queen's Mercy 22357    Valid                    H6
987                     Domanitch  7661    Valid                    L5
988                         Ivuna 12063    Valid                   CI1
989                           Ogi 17994    Valid                    H6
990                        Ausson  4903    Valid                    L5
991                       Rangala 22392    Valid                    L6
992                     Shelburne 23529    Valid                    L5
993                     Mascombes 15438    Valid                    L6
994                        Ashdon  2346    Valid                    L6
995                    Nadiabondi 16889    Valid                    H5
996              Cumberland Falls  5496    Valid               Aubrite
997                   Peckelsheim 18181    Valid          Diogenite-pm
998                        Bhawad 36591    Valid                   LL6
999                 Ambapur Nagla  2290    Valid                    H5
1000                  Bishopville  5059    Valid               Aubrite
                   mass  fall year month day hours minutes seconds
1                  3950  Fell 1925     2   3     6      59      59
2                107000  Fell 1400     1   1     0       0       0
3                  3400  Fell 1925     2   3     7      35       3
4                  1000  Fell 1907     1   1     0       0       0
5                1770.5  Fell 1925     2   3     7      15      37
6                  -340  Fell 1869     1   1     0       0       0
7                  2450  Fell 1970     1   1     0       0       0
8                  4460  Fell 1920     1   1     0       0       0
9                  2000  Fell 1882     1   1     0       0       0
10                 7000  Fell 1925     1   1     0       0       0
11                  238  Fell 1926     1   1     0       0       0
12                 3457  Fell 1925     2   3     8      14      29
13                 1000  Fell 1906     1   1     0       0       0
14                 5100  Fell 1909     1   1     0       0       0
15                20350  Fell 1965     1   1     0       0       0
16                 3700  Fell 1798     1   1     0       0       0
17                16570  Fell 1930     1   1     0       0       0
18                 9000  Fell 1925     2   3     6       7      54
19                  691  Fell 1940     1   1     0       0       0
20                41730  Fell 1930     1   1     0       0       0
21                 9251  Fell 1900     1   1     0       0       0
22                 2750  Fell 1925     2   3     8      43      31
23                256.8 Found   NA    NA  NA    NA      NA      NA
24                25400  Fell 1994     1   1     0       0       0
25                14000  Fell 1925     2   3     8      35      27
26                  134  Fell 1925     2   3     8      26      34
27               200000  Fell 1925     2   3     7      11      43
28                  0.2  Fell 1925     2   3     8      30      46
29                  500  Fell 1887     1   1     0       0       0
30                 1282  Fell 1986     1   1     0       0       0
31                  840  Fell 1916     1   1     0       0       0
32                17000  Fell 1925     2   3     8      13      35
33                80000  Fell 1869     1   1     0       0       0
34   78.400000000000006  Fell 1925     2   3     8      38      43
35                34000  Fell 1876     1   1     0       0       0
36                 4885  Fell 1925     2   3     8      42      54
37                  705  Fell 1901     1   1     0       0       0
38                 1600  Fell 1925     2   3     8      46       8
39                 2900  Fell 2012     1   1     0       0       0
40                77600  Fell 1925     2   3     7      45       4
41               1384.2  Fell 1945     1   1     0       0       0
42                   56  Fell 1925     2   3     5      50      14
43                  350  Fell 1827     1   1     0       0       0
44                  237  Fell 1925     2   3     8      59       1
45                 1000  Fell 1950     1   1     0       0       0
46                  160  Fell 1957     1   1     0       0       0
47                 2000  Fell 1925     2   3     7      40      57
48                 1440  Fell 1920     1   1     0       0       0
49                   94  Fell 1925     2   3     7      15      41
50                 9700  Fell 1925     2   3     7      22      36
51                737.6  Fell 1939     1   1     0       0       0
52                   12  Fell 1925     2   3     8      22      53
53                  177  Fell 1971     1   1     0       0       0
54                 1900  Fell 1887     1   1     0       0       0
55                37000  Fell 1803     1   1     0       0       0
56                  560  Fell 1925     2   3     8      39      23
57                 1300  Fell 1875     1   1     0       0       0
58                 1100  Fell 1914     1   1     0       0       0
59                 3130  Fell 1846     1   1     0       0       0
60                 28.9  Fell 1903     1   1     0       0       0
61                  360  Fell 1883     1   1     0       0       0
62                  205  Fell 1856     1   1     0       0       0
63                  772  Fell 1911     1   1     0       0       0
64                  877  Fell 1907     1   1     0       0       0
65                 4000  Fell 1868     1   1     0       0       0
66                121.5  Fell 1900     1   1     0       0       0
67                 8510  Fell 1910     1   1     0       0       0
68                 <NA>  Fell 1925     2   3     7       5      26
69                 4000  Fell 1964     1   1     0       0       0
70                 1265  Fell 1921     1   1     0       0       0
71                  300  Fell 1925     2   3     6      56       4
72                329.7  Fell 2010     1   1     0       0       0
73                12000  Fell 1925     2   3     6      44      25
74                  779  Fell 1949     1   1     0       0       0
75                40000  Fell 1925     2   3     7      39      29
76                 6109  Fell 1929     1   1     0       0       0
77                  910  Fell 1949     1   1     0       0       0
78                22000  Fell 1932     1   1     0       0       0
79                 6913  Fell 2008     1   1     0       0       0
80                 <NA>  Fell 1983     1   1     0       0       0
81                 4910  Fell 1925     2   3     7      56      53
82                 2250  Fell 1964     1   1     0       0       0
83                12000  Fell 1834     1   1     0       0       0
84                 1100  Fell 1944     1   1     0       0       0
85                12500  Fell 1925     2   3     7      46      49
86                 3500  Fell 1937     1   1     0       0       0
87                 36.1  Fell 1925     2   3     7      21      15
88                 5300  Fell 1925     2   3     7      48      45
89                  973  Fell 1925     2   3     7      16      57
90              -330000  Fell 1925     2   3     7      32      54
91                  180  Fell 1949     1   1     0       0       0
92                 3833  Fell 1871     1   1     0       0       0
93                 5500  Fell 1925     2   3     7      39      58
94                 6400  Fell 1790     1   1     0       0       0
95                 2360  Fell 1934     1   1     0       0       0
96                 1460  Fell 1917     1   1     0       0       0
97                10000  Fell 1911     1   1     0       0       0
98                  908  Fell 1860     1   1     0       0       0
99                 <NA>  Fell 1925     2   3     6      53      11
100                <NA>  Fell 2000     1   1     0       0       0
101                 230  Fell 1925     2   3     7      12      42
102                1800  Fell 1923     1   1     0       0       0
103                1710  Fell 1995     1   1     0       0       0
104                 195  Fell 1809     1   1     0       0       0
105               50000  Fell 1946     1   1     0       0       0
106                7330  Fell 1975     1   1     0       0       0
107                  56  Fell 1973     1   1     0       0       0
108               16400  Fell 1925     2   3     8      14      48
109                5555  Fell 1925     2   3     8       6       6
110                3599  Fell 2011     1   1     0       0       0
111                 6.1  Fell 1961     1   1     0       0       0
112               50000  Fell 1981     1   1     0       0       0
113               483.7  Fell 1991     1   1     0       0       0
114                3200  Fell 1900     1   1     0       0       0
115                  89  Fell 1822     1   1     0       0       0
116                 700  Fell 2001     1   1     0       0       0
117                1250  Fell 1855     1   1     0       0       0
118                2250  Fell 1925     2   3     8      50      20
119               20500  Fell 1979     1   1     0       0       0
120                 650  Fell 1925     2   3     7      14      36
121                5000  Fell 1865     1   1     0       0       0
122                6500  Fell 1942     1   1     0       0       0
123                2000  Fell 1925     2   3     7      36      52
124                  50  Fell 1974     1   1     0       0       0
125                1000  Fell 1974     1   1     0       0       0
126                8000  Fell 1925     2   3     7      45      32
127                2600  Fell 1976     1   1     0       0       0
128                 767  Fell 1991     1   1     0       0       0
129                1967  Fell 1621     1   1     0       0       0
130               453.6  Fell 1925     2   3     7       5      43
131                2840  Fell 1949     1   1     0       0       0
132                6189  Fell 2002     1   1     0       0       0
133                2500  Fell 1905     1   1     0       0       0
134                1550  Fell 1914     1   1     0       0       0
135                 478  Fell 1999     1   1     0       0       0
136                1000  Fell 1851     1   1     0       0       0
137                   8  Fell 1902     1   1     0       0       0
138                6930  Fell 1853     1   1     0       0       0
139               19000  Fell 2002     1   1     0       0       0
140                5560  Fell 1954     1   1     0       0       0
141               85000  Fell 1961     1   1     0       0       0
142               25.81  Fell 2009     1   1     0       0       0
143                 950  Fell 1925     2   3     7      40      35
144               17100  Fell 1925     2   3     7      55      33
145                 167  Fell 1892     1   1     0       0       0
146                <NA>  Fell 1520     1   1     0       0       0
147               666.6  Fell 1990     1   1     0       0       0
148               11355  Fell 1942     1   1     0       0       0
149                5460  Fell 1925     2   3     8      53       8
150               71400  Fell 1998     1   1     0       0       0
151               66000  Fell 1933     1   1     0       0       0
152                 550  Fell 1905     1   1     0       0       0
153                 194  Fell 1958     1   1     0       0       0
154                1708  Fell 1868     1   1     0       0       0
155                  28  Fell 1829     1   1     0       0       0
156                1000  Fell 1878     1   1     0       0       0
157               59000  Fell 1925     2   3     7      54       3
158               14000  Fell 1925     2   3     8      49       2
159                1100  Fell 1885     1   1     0       0       0
160                 222  Fell 1964     1   1     0       0       0
161                  54  Fell 1925     2   3     7      46       4
162              250000  Fell 1868     1   1     0       0       0
163                <NA>  Fell 1843     1   1     0       0       0
164               30000  Fell 1841     1   1     0       0       0
165             2000000  Fell 1969     1   1     0       0       0
166                2500  Fell 1925     2   3     8      39      34
167              408000  Fell 1925     2   3     6      27      43
168               20000  Fell 1966     1   1     0       0       0
169                3000  Fell 1963     1   1     0       0       0
170                1915  Fell 1891     1   1     0       0       0
171                 240  Fell 1900     1   1     0       0       0
172               17200  Fell 1912     1   1     0       0       0
173                2085  Fell 1925     2   3     7      24      39
174                 448  Fell 1925     2   3     8       6       6
175                 189  Fell 1995     1   1     0       0       0
176                 100  Fell 1804     1   1     0       0       0
177                 400  Fell 1873     1   1     0       0       0
178               14250  Fell 1975     1   1     0       0       0
179                2117  Fell 1943     1   1     0       0       0
180               29000  Fell 1628     1   1     0       0       0
181                1064  Fell 1868     1   1     0       0       0
182               11510  Fell 1925     2   3     7      26      45
183                2130  Fell 1938     1   1     0       0       0
184               45300  Fell 2002     1   1     0       0       0
185                1000  Fell 1925     2   3     4      44      42
186                1500  Fell 1869     1   1     0       0       0
187                <NA>  Fell 1779     1   1     0       0       0
188                 690  Fell 1910     1   1     0       0       0
189               25400  Fell 1962     1   1     0       0       0
190              6669.2  Fell 1925     2   3     9      52      29
191               14000  Fell 1753     1   1     0       0       0
192               14250  Fell 1987     1   1     0       0       0
193               47700  Fell 1939     1   1     0       0       0
194               10500  Fell 1925     2   3     9      27      18
195                7300  Fell 1874     1   1     0       0       0
196                  90  Fell 1925     2   3     8      18      16
197                1620  Fell 1930     1   1     0       0       0
198               20000  Fell 1948     1   1     0       0       0
199               24.54  Fell 2010     1   1     0       0       0
200              200000  Fell 1918     1   1     0       0       0
201               10200  Fell 1986     1   1     0       0       0
202                  28  Fell 1814     1   1     0       0       0
203                 340  Fell 1898     1   1     0       0       0
204                1393  Fell 1925     2   3     9      12      59
205                 670  Fell 1990     1   1     0       0       0
206                 400  Fell 1935     1   1     0       0       0
207                <NA>  Fell 1966     1   1     0       0       0
208                -642  Fell 1976     1   1     0       0       0
209                1500  Fell 1837     1   1     0       0       0
210               167.7  Fell 2003     1   1     0       0       0
211                1500  Fell 1999     1   1     0       0       0
212                 762  Fell 1886     1   1     0       0       0
213                4255  Fell 1902     1   1     0       0       0
214                3700  Fell 1998     1   1     0       0       0
215               28500  Fell 1872     1   1     0       0       0
216                61.4  Fell 1981     1   1     0       0       0
217                1500  Fell 1857     1   1     0       0       0
218               67225  Fell 1983     1   1     0       0       0
219                23.2  Fell 1904     1   1     0       0       0
220               737.6  Fell 1962     1   1     0       0       0
221                3336  Fell 1939     1   1     0       0       0
222                1421  Fell 1995     1   1     0       0       0
223                  23  Fell 1925     2   3     7      53      49
224                2024  Fell 1879     1   1     0       0       0
225               17450  Fell 1925     2   3     7      19       8
226                1900  Fell 1908     1   1     0       0       0
227                1250  Fell 1925     2   3     7      43      34
228                5000  Fell 1925     2   3     6      10      10
229                7000  Fell 1972     1   1     0       0       0
230                5900  Fell 1873     1   1     0       0       0
231              230000  Fell 1875     1   1     0       0       0
232                 300  Fell 1872     1   1     0       0       0
233                9150  Fell 1868     1   1     0       0       0
234              100000  Fell 1997     1   1     0       0       0
235              325000  Fell 1990     1   1     0       0       0
236                6000  Fell 1877     1   1     0       0       0
237                 100  Fell 1916     1   1     0       0       0
238                2400  Fell 1916     1   1     0       0       0
239              300000  Fell 1937     1   1     0       0       0
240                1880  Fell 1913     1   1     0       0       0
241                1025  Fell 1988     1   1     0       0       0
242                2950  Fell 1955     1   1     0       0       0
243                4000  Fell 2008     1   1     0       0       0
244                1000  Fell 1841     1   1     0       0       0
245               93200  Fell 1981     1   1     0       0       0
246                9500  Fell 2009     1   1     0       0       0
247                2050  Fell 1925     2   3     8      14      21
248               10600  Fell 1921     1   1     0       0       0
249                1455  Fell 1984     1   1     0       0       0
250                1800  Fell 1828     1   1     0       0       0
251                <NA>  Fell 1937     1   1     0       0       0
252                  31  Fell 1953     1   1     0       0       0
253                3200  Fell 1863     1   1     0       0       0
254               15000  Fell 1914     1   1     0       0       0
255               20000  Fell 1869     1   1     0       0       0
256                97.7  Fell 1906     1   1     0       0       0
257               45000  Fell 1985     1   1     0       0       0
258                1600  Fell 1925     2   3     8       0       1
259                1800  Fell 1848     1   1     0       0       0
260                 600  Fell 1925     2   3     7      12      22
261                1300  Fell 1925     2   3     7      54      24
262                4000  Fell 1850     1   1     0       0       0
263                 927  Fell 1925     2   3     9       0      22
264               45760  Fell 1963     1   1     0       0       0
265                5400  Fell 1925     2   3     6      38      32
266             -600000  Fell 1890     1   1     0       0       0
267               36900  Fell 1993     1   1     0       0       0
268                 220  Fell 1801     1   1     0       0       0
269               21000  Fell 1925     2   3     6      47      53
270                 500  Fell 2007     1   1     0       0       0
271               23250  Fell 1925     2   3     9       0      54
272                4400  Fell 1865     1   1     0       0       0
273                7800  Fell 1933     1   1     0       0       0
274               29000  Fell 1861     1   1     0       0       0
275                 885  Fell 1813     1   1     0       0       0
276                4500  Fell 1925     2   3     7       4      49
277              260000  Fell 1986     1   1     0       0       0
278                3000  Fell 1840     1   1     0       0       0
279                 375  Fell 1961     1   1     0       0       0
280                <NA>  Fell 1950     1   1     0       0       0
281                 695  Fell 1925     2   3     8       2      45
282                3200  Fell 1941     1   1     0       0       0
283               19256  Fell 2003     1   1     0       0       0
284              3679.7  Fell 1921     1   1     0       0       0
285              100000  Fell 2007     1   1     0       0       0
286                1850  Fell 1796     1   1     0       0       0
287                4742  Fell 1904     1   1     0       0       0
288                 430  Fell 1986     1   1     0       0       0
289                5070  Fell 1932     1   1     0       0       0
290               20000  Fell 1998     1   1     0       0       0
291                3858  Fell 1929     1   1     0       0       0
292                3050  Fell 1928     1   1     0       0       0
293               992.5  Fell 2012     1   1     0       0       0
294              300000  Fell 1882     1   1     0       0       0
295               51500  Fell 1943     1   1     0       0       0
296                  82  Fell 1925     2   3     8      46      22
297                 190  Fell 2003     1   1     0       0       0
298               89400  Fell 1925     2   3     8      30       8
299                  96  Fell 1868     1   1     0       0       0
300                1456  Fell 1944     1   1     0       0       0
301                1440  Fell 1811     1   1     0       0       0
302                3332  Fell 1929     1   1     0       0       0
303  271.39999999999998  Fell 1999     1   1     0       0       0
304                 400  Fell 1896     1   1     0       0       0
305                1400  Fell 1925     2   3     9      27       2
306                 230  Fell 1899     1   1     0       0       0
307                 460  Fell 1925     2   3     6      54      52
308               45000  Fell 2002     1   1     0       0       0
309               34650  Fell 1925     2   3     7      49      32
310               40000  Fell 1994     1   1     0       0       0
311              166000  Fell 1884     1   1     0       0       0
312                5000  Fell 1912     1   1     0       0       0
313                2300  Fell 1925     2   3     9       3      40
314               488.1  Fell 1984     1   1     0       0       0
315                4375  Fell 1969     1   1     0       0       0
316                1310  Fell 1934     1   1     0       0       0
317                1600  Fell 1864     1   1     0       0       0
318               283.3  Fell 1968     1   1     0       0       0
319                6580  Fell 2008     1   1     0       0       0
320                2000  Fell 1970     1   1     0       0       0
321                   1  Fell 1965     1   1     0       0       0
322                 668  Fell 1903     1   1     0       0       0
323                 540  Fell 1937     1   1     0       0       0
324               25000  Fell 1870     1   1     0       0       0
325                1600  Fell 2004     1   1     0       0       0
326  134.30000000000001  Fell 1959     1   1     0       0       0
327               34000  Fell 1866     1   1     0       0       0
328               465.5  Fell 1976     1   1     0       0       0
329                 150  Fell 1933     1   1     0       0       0
330                 157  Fell 1931     1   1     0       0       0
331                1100  Fell 1925     2   3     7      57      51
332              100000  Fell 1925     2   3     6      27      32
333                 842  Fell 1882     1   1     0       0       0
334                 355  Fell 1916     1   1     0       0       0
335                2000  Fell 1886     1   1     0       0       0
336                2462  Fell 1921     1   1     0       0       0
337                1200  Fell 1977     1   1     0       0       0
338                 282  Fell 1956     1   1     0       0       0
339              220000  Fell 1925     2   3     9      17      58
340                2381  Fell 1925     2   3     7      25       5
341                1500  Fell 1970     1   1     0       0       0
342                1500  Fell 1956     1   1     0       0       0
343                3396  Fell 2007     1   1     0       0       0
344               25250  Fell 1968     1   1     0       0       0
345                5000  Fell 1851     1   1     0       0       0
346                  20  Fell 1875     1   1     0       0       0
347                6000  Fell 1929     1   1     0       0       0
348               50000  Fell 1865     1   1     0       0       0
349               16500  Fell 1922     1   1     0       0       0
350               680.5  Fell 1998     1   1     0       0       0
351                1280  Fell 1920     1   1     0       0       0
352                2000  Fell 1766     1   1     0       0       0
353                5200  Fell 1838     1   1     0       0       0
354                3584  Fell 2010     1   1     0       0       0
355             1100000  Fell 1948     1   1     0       0       0
356                 1.4  Fell 1925     2   3     7      48      39
357                4000  Fell 1946     1   1     0       0       0
358                 180  Fell 1925     2   3     9      45      29
359                8800  Fell 1962     1   1     0       0       0
360                3667  Fell 2009     1   1     0       0       0
361                <NA>  Fell 1647     1   1     0       0       0
362                <NA>  Fell 1944     1   1     0       0       0
363                1641  Fell 1895     1   1     0       0       0
364               10000  Fell 1925     2   3     8      34      49
365                1800  Fell 1855     1   1     0       0       0
366                1400  Fell 1973     1   1     0       0       0
367               48000  Fell 1925     2   3     8      20      51
368                 945  Fell 1925     2   3     7      32       4
369                3230  Fell 1970     1   1     0       0       0
370               24000  Fell 1925     1   1     0       0       0
371               26000  Fell 1942     1   1     0       0       0
372                 399  Fell 1925     2   3     8      49       2
373                4000  Fell 1925     2   3     6      57      36
374                1900  Fell 1886     1   1     0       0       0
375                 357  Fell 1925     2   3     7      59      27
376                 866  Fell 1993     1   1     0       0       0
377                1161  Fell 1925     2   3     7      49       8
378                2770  Fell 1942     1   1     0       0       0
379               10500  Fell 1925     2   3     6      34       9
380                  42  Fell 1826     1   1     0       0       0
381               37500  Fell 1900     1   1     0       0       0
382                1500  Fell 1787     1   1     0       0       0
383                2000  Fell 1924     1   1     0       0       0
384                1862  Fell 1854     1   1     0       0       0
385                7000  Fell 1925     2   3     8      20      44
386                 363  Fell 1925     2   3     7      30      41
387                1066  Fell 2011     1   1     0       0       0
388                 680  Fell 1925     2   3     8      33      13
389                  22  Fell 1861     1   1     0       0       0
390                 750  Fell 1897     1   1     0       0       0
391               18000  Fell 2003     1   1     0       0       0
392                9000  Fell 1878     1   1     0       0       0
393                6000  Fell 1868     1   1     0       0       0
394               18300  Fell 1933     1   1     0       0       0
395                2000  Fell 1967     1   1     0       0       0
396                 880  Fell 1917     1   1     0       0       0
397                 968  Fell 1949     1   1     0       0       0
398               14000  Fell 1893     1   1     0       0       0
399                 800  Fell 1938     1   1     0       0       0
400                4000  Fell 1822     1   1     0       0       0
401                 807  Fell 1933     1   1     0       0       0
402                1039  Fell 1895     1   1     0       0       0
403             1100000  Fell 1998     1   1     0       0       0
404                <NA>  Fell  921     1   1     0       0       0
405                5000  Fell 1925     2   3     8      19      54
406                 600  Fell 1905     1   1     0       0       0
407               45000  Fell 1886     1   1     0       0       0
408                4850  Fell 1974     1   1     0       0       0
409                 840  Fell 1896     1   1     0       0       0
410               100.7  Fell 1898     1   1     0       0       0
411                 127  Fell 1900     1   1     0       0       0
412                5500  Fell 1925     2   3     8      13      31
413                4682  Fell 1958     1   1     0       0       0
414                1140  Fell 1994     1   1     0       0       0
415  266.10000000000002  Fell 1963     1   1     0       0       0
416                7000  Fell 1852     1   1     0       0       0
417                  28  Fell 1952     1   1     0       0       0
418                2220  Fell 1963     1   1     0       0       0
419               12570  Fell 1925     2   3     6      42      11
420                6000  Fell 1945     1   1     0       0       0
421                 470  Fell 1833     1   1     0       0       0
422               10750  Fell 1898     1   1     0       0       0
423                4000  Fell 1815     1   1     0       0       0
424              120000  Fell 1919     1   1     0       0       0
425                2000  Fell 1842     1   1     0       0       0
426               13400  Fell 1971     1   1     0       0       0
427              152000  Fell 1890     1   1     0       0       0
428               18000  Fell 1814     1   1     0       0       0
429               20000  Fell 1889     1   1     0       0       0
430                3000  Fell 1785     1   1     0       0       0
431               10500  Fell 1975     1   1     0       0       0
432               19000  Fell 2001     1   1     0       0       0
433                1342  Fell 1930     1   1     0       0       0
434                5850  Fell 1926     1   1     0       0       0
435                1293  Fell 1902     1   1     0       0       0
436               27000  Fell 1891     1   1     0       0       0
437               57900  Fell 1901     1   1     0       0       0
438                3500  Fell 1924     1   1     0       0       0
439                2500  Fell 1971     1   1     0       0       0
440                 3.3  Fell 1967     1   1     0       0       0
441                3996  Fell 1935     1   1     0       0       0
442                3393  Fell 1933     1   1     0       0       0
443                4975  Fell 1942     1   1     0       0       0
444               10400  Fell 1623     1   1     0       0       0
445                1050  Fell 1925     2   3     8      35      58
446               50000  Fell 1978     1   1     0       0       0
447               24700  Fell 1740     1   1     0       0       0
448                4500  Fell 1925     2   3     6      53      49
449                5760  Fell 2008     1   1     0       0       0
450                <NA>  Fell 1775     1   1     0       0       0
451                 577  Fell 1925     2   3     8      50      56
452                1557  Fell 1989     1   1     0       0       0
453                2440  Fell 1829     1   1     0       0       0
454              135000  Fell 1850     1   1     0       0       0
455                4300  Fell 1925     2   3     8       6       3
456                 100  Fell 2006     1   1     0       0       0
457                45.5  Fell 1976     1   1     0       0       0
458               15500  Fell 1994     1   1     0       0       0
459                3650  Fell 1877     1   1     0       0       0
460               12800  Fell 1808     1   1     0       0       0
461                 331  Fell 1906     1   1     0       0       0
462                3300  Fell 1940     1   1     0       0       0
463               17300  Fell 1947     1   1     0       0       0
464                 219  Fell 1925     2   3     9      10       2
465                7000  Fell 1925     2   3     8       4      16
466                3710  Fell 1959     1   1     0       0       0
467                2936  Fell 1988     1   1     0       0       0
468                 665  Fell 1933     1   1     0       0       0
469                 265  Fell 1933     1   1     0       0       0
470                1047  Fell 1940     1   1     0       0       0
471               30000  Fell 1814     1   1     0       0       0
472                3880  Fell 1943     1   1     0       0       0
473                3200  Fell 1932     1   1     0       0       0
474              8887.5  Fell 1930     1   1     0       0       0
475              100000  Fell 2008     1   1     0       0       0
476                6100  Fell 1895     1   1     0       0       0
477               34000  Fell 1889     1   1     0       0       0
478                4162  Fell 1984     1   1     0       0       0
479                 480  Fell 1947     1   1     0       0       0
480                 220  Fell 1889     1   1     0       0       0
481                7250  Fell 1855     1   1     0       0       0
482              160000  Fell 1879     1   1     0       0       0
483                 112  Fell 1877     1   1     0       0       0
484                 800  Fell 1942     1   1     0       0       0
485               32000  Fell 1860     1   1     0       0       0
486               12000  Fell 1925     2   3    11       6      20
487               45000  Fell 1925     2   3     8      34       5
488                 400  Fell 1897     1   1     0       0       0
489                1430  Fell 1988     1   1     0       0       0
490                 614  Fell 1823     1   1     0       0       0
491               16820  Fell 2003     1   1     0       0       0
492               18.41  Fell 2007     1   1     0       0       0
493                 720  Fell 1951     1   1     0       0       0
494               39000  Fell 1892     1   1     0       0       0
495                3640  Fell 1922     1   1     0       0       0
496               71400  Fell 1925     2   3    10       4      44
497               40300  Fell 1924     1   1     0       0       0
498                 478  Fell 2007     1   1     0       0       0
499                5000  Fell 1925     2   3     7      49      59
500                1275  Fell 1925     2   3     8      15       8
501               17000  Fell 1970     1   1     0       0       0
502                3900  Fell 1971     1   1     0       0       0
503               86000  Fell 1925     2   3     5      38      19
504                 710  Fell 1938     1   1     0       0       0
505                2270  Fell 1865     1   1     0       0       0
506                5000  Fell 1993     1   1     0       0       0
507                1455  Fell 1925     2   3     7      35      50
508                 342  Fell 2007     1   1     0       0       0
509                94.2  Fell 1896     1   1     0       0       0
510               18000  Fell 1925     2   3     7      52      18
511              110000  Fell 2008     1   1     0       0       0
512                 577  Fell 1853     1   1     0       0       0
513                4500  Fell 1925     2   3     9      14      22
514               17000  Fell 1910     1   1     0       0       0
515                 629  Fell 1933     1   1     0       0       0
516                 900  Fell 1893     1   1     0       0       0
517                3766  Fell 1940     1   1     0       0       0
518                3400  Fell 1872     1   1     0       0       0
519               52000  Fell 1808     1   1     0       0       0
520                5500  Fell 1970     1   1     0       0       0
521                1443  Fell 1983     1   1     0       0       0
522                3555  Fell 1951     1   1     0       0       0
523               10322  Fell 1934     1   1     0       0       0
524               14290  Fell 1909     1   1     0       0       0
525                6000  Fell 1977     1   1     0       0       0
526                4703  Fell 1925     2   3     8       4      46
527                3500  Fell 1768     1   1     0       0       0
528                0.15  Fell 1917     1   1     0       0       0
529                 750  Fell 1934     1   1     0       0       0
530                7500  Fell 1825     1   1     0       0       0
531                6067  Fell 1924     1   1     0       0       0
532                  17  Fell 1886     1   1     0       0       0
533  290.89999999999998 Found 2002     1   1     0       0       0
534                2520  Fell 1980     1   1     0       0       0
535                 611  Fell 1916     1   1     0       0       0
536                 252  Fell 2002     1   1     0       0       0
537                8000  Fell 1846     1   1     0       0       0
538              330000  Fell 1899     1   1     0       0       0
539                <NA>  Fell 1842     1   1     0       0       0
540               15000  Fell 1892     1   1     0       0       0
541                9000  Fell 1964     1   1     0       0       0
542               39000  Fell 1847     1   1     0       0       0
543                  50  Fell 1843     1   1     0       0       0
544                3500  Fell 1922     1   1     0       0       0
545                 472  Fell  861     1   1     0       0       0
546                1415  Fell 1923     1   1     0       0       0
547                 136  Fell 1999     1   1     0       0       0
548                 780  Fell 1902     1   1     0       0       0
549                 246  Fell 1928     1   1     0       0       0
550               16250  Fell 1925     2   3     7      36      35
551                1060  Fell 1830     1   1     0       0       0
552               23000  Fell 1945     1   1     0       0       0
553              500000  Fell 1887     1   1     0       0       0
554                2910  Fell 1925     2   3     8      59      44
555               28000  Fell 1878     1   1     0       0       0
556                7000  Fell 1998     1   1     0       0       0
557                3000  Fell 1857     1   1     0       0       0
558               32490  Fell 1984     1   1     0       0       0
559                6000  Fell 1949     1   1     0       0       0
560                2500  Fell 1925     2   3     8      31      50
561  132.69999999999999  Fell 1925     2   3     9      28       6
562                 270  Fell 2008     1   1     0       0       0
563                 240  Fell 1917     1   1     0       0       0
564                  30  Fell 1877     1   1     0       0       0
565              107000  Fell 1952     1   1     0       0       0
566                 102  Fell 1950     1   1     0       0       0
567              100000  Fell 1984     1   1     0       0       0
568                4000  Fell 1925     2   3     7      14       0
569                1500  Fell 1925     2   3     8      49      20
570              104000  Fell 1917     1   1     0       0       0
571                1700  Fell 1863     1   1     0       0       0
572                5000  Fell 1819     1   1     0       0       0
573                 350  Fell 1908     1   1     0       0       0
574              150000  Fell 1992     1   1     0       0       0
575                8400  Fell 1901     1   1     0       0       0
576                1800  Fell 1838     1   1     0       0       0
577                4300  Fell 1835     1   1     0       0       0
578                 114  Fell 1925     1   1     0       0       0
579                1500  Fell 1836     1   1     0       0       0
580                5000  Fell 1899     1   1     0       0       0
581              271000  Fell 1966     1   1     0       0       0
582                 140  Fell 1844     1   1     0       0       0
583                4629  Fell 2004     1   1     0       0       0
584               10000  Fell 1919     1   1     0       0       0
585                1750  Fell 1879     1   1     0       0       0
586                9241  Fell 1926     1   1     0       0       0
587                <NA>  Fell 1791     1   1     0       0       0
588                3698  Fell 1932     1   1     0       0       0
589                 633  Fell 2000     1   1     0       0       0
590                1000  Fell 1991     1   1     0       0       0
591                5650  Fell 1878     1   1     0       0       0
592               49000  Fell 1751     1   1     0       0       0
593                7540  Fell 1753     1   1     0       0       0
594                1504  Fell 1990     1   1     0       0       0
595                1300  Fell 1954     1   1     0       0       0
596                1500  Fell 1852     1   1     0       0       0
597                4000  Fell 1925     2   3     8       6      58
598              1710.5  Fell 1916     1   1     0       0       0
599                3500  Fell 1876     1   1     0       0       0
600                  41  Fell 1805     1   1     0       0       0
601                4500  Fell 1804     1   1     0       0       0
602                4440  Fell 2003     1   1     0       0       0
603                 100  Fell 1867     1   1     0       0       0
604                 630  Fell 1989     1   1     0       0       0
605               12000  Fell 1925     2   3     7      50       0
606                3850  Fell 1965     1   1     0       0       0
607                3950  Fell 2008     1   1     0       0       0
608                  21  Fell 1880     1   1     0       0       0
609                 810  Fell 1954     1   1     0       0       0
610                 725  Fell 1950     1   1     0       0       0
611                 350  Fell 1866     1   1     0       0       0
612                 299  Fell 1959     1   1     0       0       0
613                1600  Fell 1881     1   1     0       0       0
614              128800  Fell 1925     2   3     8       2      39
615               25000  Fell 1999     1   1     0       0       0
616                 212  Fell 1919     1   1     0       0       0
617              152000  Fell 1918     1   1     0       0       0
618                 850  Fell 1848     1   1     0       0       0
619                8800  Fell 1838     1   1     0       0       0
620                 469  Fell 1994     1   1     0       0       0
621                3200  Fell 1873     1   1     0       0       0
622               23680  Fell 1991     1   1     0       0       0
623                7700  Fell 1963     1   1     0       0       0
624                  70  Fell 1808     1   1     0       0       0
625               45000  Fell 1914     1   1     0       0       0
626                1360  Fell 1810     1   1     0       0       0
627                1900  Fell 1925     2   3     7      34      11
628                2587  Fell 1925     2   3     8      34       0
629               11500  Fell 1871     1   1     0       0       0
630                 470  Fell 1930     1   1     0       0       0
631                1900  Fell 1980     1   1     0       0       0
632               28400  Fell 1847     1   1     0       0       0
633                 600  Fell 1969     1   1     0       0       0
634                4576  Fell 1925     2   3     7      56      34
635                <NA>  Fell 1931     1   1     0       0       0
636                 605  Fell 1976     1   1     0       0       0
637               16700  Fell 1930     1   1     0       0       0
638               40000  Fell 1826     1   1     0       0       0
639                 560  Fell 1863     1   1     0       0       0
640                1250  Fell 1962     1   1     0       0       0
641                3000  Fell 1925     2   3     7       5      49
642                3200  Fell 1842     1   1     0       0       0
643                 125  Fell 1857     1   1     0       0       0
644                1544  Fell 1925     2   3     8      31      38
645                1303  Fell 1939     1   1     0       0       0
646               11000  Fell 1925     2   3     7      21      17
647                3763  Fell 2006     1   1     0       0       0
648                1000  Fell 1950     1   1     0       0       0
649               91000  Fell 1821     1   1     0       0       0
650               16500  Fell 1869     1   1     0       0       0
651                6380  Fell 1992     1   1     0       0       0
652                6000  Fell 1806     1   1     0       0       0
653                5500  Fell 1925     2   3     7      18      28
654                2968  Fell 1966     1   1     0       0       0
655               10000  Fell 1925     2   3     8      44      42
656                1470  Fell 1925     1   1     0       0       0
657                3750  Fell 1900     1   1     0       0       0
658               70500  Fell 2007     1   1     0       0       0
659                4239  Fell 1919     1   1     0       0       0
660                 800  Fell 1836     1   1     0       0       0
661               32000  Fell 1899     1   1     0       0       0
662                2250  Fell 1938     1   1     0       0       0
663                 230  Fell 1934     1   1     0       0       0
664                 450  Fell 1901     1   1     0       0       0
665                  43  Fell 1985     1   1     0       0       0
666                 367  Fell 1981     1   1     0       0       0
667                 700  Fell 1900     1   1     0       0       0
668               14000  Fell 1914     1   1     0       0       0
669                2405  Fell 1955     1   1     0       0       0
670                 165  Fell 1899     1   1     0       0       0
671                4000  Fell 1973     1   1     0       0       0
672               212.5  Fell 1910     1   1     0       0       0
673               35000  Fell 1925     2   3     9       8      38
674                3000  Fell 1819     1   1     0       0       0
675               845.2  Fell 1906     1   1     0       0       0
676                 288  Fell 1972     1   1     0       0       0
677                  25  Fell 1925     2   3     8      19      12
678                 3.3  Fell 1967     1   1     0       0       0
679                8500  Fell 1818     1   1     0       0       0
680               11300  Fell 1925     2   3     8      22       4
681                8000  Fell 1925     2   3     8      30      30
682               74800  Fell 1927     1   1     0       0       0
683               10000  Fell 1989     1   1     0       0       0
684                <NA>  Fell 1960     1   1     0       0       0
685                  37  Fell 1925     2   3     7      41      46
686                 958  Fell 1927     1   1     0       0       0
687              303000  Fell 1960     1   1     0       0       0
688                 498  Fell 1978     1   1     0       0       0
689               10000  Fell 1947     1   1     0       0       0
690                3250  Fell 1925     2   3     6      18       2
691               815.3  Fell 1969     1   1     0       0       0
692              250000  Fell 1981     1   1     0       0       0
693               24750  Fell 1925     2   3     7      40      12
694                4960  Fell 1962     1   1     0       0       0
695                 315  Fell 1921     1   1     0       0       0
696               22000  Fell 1905     1   1     0       0       0
697                1630  Fell 1908     1   1     0       0       0
698              230000  Fell 1860     1   1     0       0       0
699                7000  Fell 2011     1   1     0       0       0
700                 250  Fell 1925     2   3     8       1       3
701                1914  Fell 1976     1   1     0       0       0
702               80000  Fell 1925     2   3     6      35      47
703                 380  Fell 1875     1   1     0       0       0
704                8600  Fell 1894     1   1     0       0       0
705                <NA>  Fell 1925     2   3     7      24      48
706                5000  Fell 1827     1   1     0       0       0
707                6045  Fell 1925     2   3     8      31      57
708               224.2  Fell 1944     1   1     0       0       0
709               52900  Fell 1925     2   3     8      51      20
710                1810  Fell 1977     1   1     0       0       0
711                 460  Fell 1925     2   3     5      40       9
712               31500  Fell 1812     1   1     0       0       0
713               90000  Fell 1918     1   1     0       0       0
714               17000  Fell 2003     1   1     0       0       0
715                3520  Fell 1925     2   3     6      55      59
716                7000  Fell 1989     1   1     0       0       0
717                 491  Fell 1839     1   1     0       0       0
718               15000  Fell 1583     1   1     0       0       0
719                 180  Fell 1862     1   1     0       0       0
720               21000  Fell 1937     1   1     0       0       0
721                3618  Fell 1974     1   1     0       0       0
722                7235  Fell 1865     1   1     0       0       0
723                  14  Fell 1913     1   1     0       0       0
724                6510  Fell 1867     1   1     0       0       0
725                6000  Fell 1855     1   1     0       0       0
726                 625  Fell 1949     1   1     0       0       0
727                6000  Fell 1925     2   3     9      34      35
728                 188  Fell 1933     1   1     0       0       0
729                1300  Fell 1949     1   1     0       0       0
730                7450  Fell 1950     1   1     0       0       0
731                 450  Fell 1860     1   1     0       0       0
732               22300  Fell 1934     1   1     0       0       0
733                2300  Fell 1925     2   3     7      52      15
734                6000  Fell 1811     1   1     0       0       0
735              1215.5  Fell 1986     1   1     0       0       0
736               10200  Fell 1996     1   1     0       0       0
737              320000  Fell 1879     1   1     0       0       0
738  1110.5999999999999  Fell 1910     1   1     0       0       0
739                25.5  Fell 1795     1   1     0       0       0
740                 412  Fell 1934     1   1     0       0       0
741                8400  Fell 1933     1   1     0       0       0
742              100000  Fell 2013     1   1     0       0       0
743               19000  Fell 1768     1   1     0       0       0
744              101000  Fell 1925     2   3     7      56       1
745                1245  Fell 1964     1   1     0       0       0
746                6000  Fell 2002     1   1     0       0       0
747                1700  Fell 1939     1   1     0       0       0
748                2700  Fell 1880     1   1     0       0       0
749                  18  Fell 1877     1   1     0       0       0
750                 794  Fell 1949     1   1     0       0       0
751                <NA>  Fell 1662     1   1     0       0       0
752               23474  Fell 1925     2   3     8      53      21
753                 350  Fell 1890     1   1     0       0       0
754                 400  Fell 1957     1   1     0       0       0
755                 180  Fell 1925     2   3     9      18      22
756                4610  Fell 1983     1   1     0       0       0
757              110000  Fell 1925     2   3     8       4      33
758                 705  Fell 1941     1   1     0       0       0
759                2900  Fell 1775     1   1     0       0       0
760                4720  Fell 1990     1   1     0       0       0
761               253.6  Fell 1925     2   3     8      47      10
762                <NA>  Fell 1925     2   3     7      12       5
763                8360  Fell 1961     1   1     0       0       0
764                  65  Fell 1851     1   1     0       0       0
765                69.5 Found 2002     1   1     0       0       0
766                2000  Fell 1868     1   1     0       0       0
767              149000  Fell 1923     1   1     0       0       0
768                3000  Fell 1848     1   1     0       0       0
769             ++56000  Fell 1934     1   1     0       0       0
770                 500  Fell 1925     2   3     8      24      47
771               11620  Fell 1882     1   1     0       0       0
772              150000  Fell 1924     1   1     0       0       0
773               41000  Fell 2008     1   1     0       0       0
774                4180  Fell 1913     1   1     0       0       0
775                5800  Fell 1890     1   1     0       0       0
776               16700  Fell 1925     2   3     8      19      15
777              200000  Fell 1925     2   3     8      24      29
778                4500  Fell 1879     1   1     0       0       0
779                 324  Fell 2007     1   1     0       0       0
780                2700  Fell 1925     2   3     6      31       5
781               17226  Fell 1977     1   1     0       0       0
782                1600  Fell 1865     1   1     0       0       0
783                2449  Fell 1982     1   1     0       0       0
784                7000  Fell 1925     2   3     7      17      15
785               13600  Fell 1873     1   1     0       0       0
786              100000  Fell 1969     1   1     0       0       0
787                 680  Fell 1876     1   1     0       0       0
788                6862  Fell 1930     1   1     0       0       0
789                1200  Fell 1844     1   1     0       0       0
790               13400  Fell 1917     1   1     0       0       0
791               50000  Fell 1813     1   1     0       0       0
792                3500  Fell 1861     1   1     0       0       0
793                1600  Fell 2010     1   1     0       0       0
794                 0.5  Fell 1814     1   1     0       0       0
795                2125  Fell 1932     1   1     0       0       0
796                 682  Fell 1949     1   1     0       0       0
797                2500  Fell 1967     1   1     0       0       0
798                1900  Fell 1925     2   3     7      51      46
799               51700  Fell 1872     1   1     0       0       0
800  72.900000000000006  Fell 1960     1   1     0       0       0
801                9000  Fell 1798     1   1     0       0       0
802                2000  Fell 1896     1   1     0       0       0
803                 870  Fell 1897     1   1     0       0       0
804                3000  Fell 1970     1   1     0       0       0
805                  67  Fell 1934     1   1     0       0       0
806                6000  Fell 1925     2   3     7      19      49
807                 146  Fell 1855     1   1     0       0       0
808               10000  Fell 2000     1   1     0       0       0
809                 277  Fell 1925     2   3     7      17      53
810                <NA>  Fell 1769     1   1     0       0       0
811            23000000  Fell 1947     1   1     0       0       0
812               17000  Fell 1637     1   1     0       0       0
813                 0.8  Fell 1897     1   1     0       0       0
814               13200  Fell 1815     1   1     0       0       0
815                3840  Fell 1903     1   1     0       0       0
816                  72  Fell 1880     1   1     0       0       0
817                 100  Fell 1811     1   1     0       0       0
818                <NA>  Fell 1925     2   3     8       3      55
819                4000  Fell 1890     1   1     0       0       0
820               25000  Fell 1946     1   1     0       0       0
821                2945  Fell 1925     1   1     0       0       0
822               13000  Fell 1914     1   1     0       0       0
823                5100  Fell 1933     1   1     0       0       0
824                1676  Fell 1808     1   1     0       0       0
825                 135  Fell 1926     1   1     0       0       0
826                 515  Fell 1925     2   3     9       3      32
827  290.39999999999998  Fell 1925     2   3     9      22      20
828                2800  Fell 1925     2   3     7       4      16
829                  60  Fell 1939     1   1     0       0       0
830               70000  Fell 1925     2   3     9       4      40
831                2300  Fell 1846     1   1     0       0       0
832              500000  Fell 1866     1   1     0       0       0
833                48.6  Fell 1822     1   1     0       0       0
834              127000  Fell 1492     1   1     0       0       0
835                3891  Fell 1925     2   3     7      41      24
836                4000  Fell 1878     1   1     0       0       0
837                4000  Fell 1773     1   1     0       0       0
838                1161  Fell 1884     1   1     0       0       0
839                 347  Fell 1925     2   3     8       7      28
840                6937  Fell 1925     2   3     7      27      33
841               27000  Fell 1810     1   1     0       0       0
842                2420  Fell 1825     1   1     0       0       0
843                 100  Fell 1893     1   1     0       0       0
844                4500  Fell 1948     1   1     0       0       0
845                 150  Fell 1947     1   1     0       0       0
846                1600  Fell 1925     2   3     7      26      48
847                1500  Fell 1868     1   1     0       0       0
848                 467  Fell 1897     1   1     0       0       0
849                8600  Fell 1849     1   1     0       0       0
850               305.5  Fell 1925     2   3     7      30      37
851               10500  Fell 1837     1   1     0       0       0
852                 905  Fell 1918     1   1     0       0       0
853                7500  Fell 1977     1   1     0       0       0
854               17900  Fell 1939     1   1     0       0       0
855                 529  Fell 1925     2   3     6      55      17
856                67.8  Fell 1938     1   1     0       0       0
857              256000  Fell 1925     2   3     8       5      28
858                1001  Fell 1908     1   1     0       0       0
859               16000  Fell 1929     1   1     0       0       0
860               10250  Fell 1864     1   1     0       0       0
861                 150  Fell 1953     1   1     0       0       0
862                1000  Fell 1927     1   1     0       0       0
863               13780  Fell 1903     1   1     0       0       0
864                13.1  Fell 1968     1   1     0       0       0
865                 380  Fell 1917     1   1     0       0       0
866                <NA>  Fell 1704     1   1     0       0       0
867                 700  Fell 1835     1   1     0       0       0
868                1000  Fell 1824     1   1     0       0       0
869                1810  Fell 1922     1   1     0       0       0
870                <NA>  Fell 1750     1   1     0       0       0
871                 700  Fell 1925     2   3     7      31      55
872                <NA>  Fell 1951     1   1     0       0       0
873                 950  Fell 1925     2   3     9      11      58
874                   5  Fell 1925     2   3     7      17       9
875               17600  Fell 1894     1   1     0       0       0
876                1222  Fell 1925     2   3     7      12      54
877               44000  Fell 1965     1   1     0       0       0
878                 420  Fell 1995     1   1     0       0       0
879                3200  Fell 1898     1   1     0       0       0
880                1040  Fell 1632     1   1     0       0       0
881                 700  Fell 1990     1   1     0       0       0
882                2500  Fell 1971     1   1     0       0       0
883                1000  Fell 1857     1   1     0       0       0
884              228000  Fell 1883     1   1     0       0       0
885               103.3  Fell 1491     1   1     0       0       0
886                  39  Fell 1723     1   1     0       0       0
887               107.2  Fell 1911     1   1     0       0       0
888                 960  Fell 2004     1   1     0       0       0
889               48500  Fell 1886     1   1     0       0       0
890                2121  Fell 1950     1   1     0       0       0
891                2500  Fell 1925     2   3     7      13      20
892               18000  Fell 1981     1   1     0       0       0
893                 340  Fell 1887     1   1     0       0       0
894               12600  Fell 1950     1   1     0       0       0
895               20000  Fell 1869     1   1     0       0       0
896                8300  Fell 1866     1   1     0       0       0
897                3719  Fell 1906     1   1     0       0       0
898                5000  Fell 1890     1   1     0       0       0
899               23200  Fell 1923     1   1     0       0       0
900                 300  Fell 1899     1   1     0       0       0
901                <NA>  Fell 1989     1   1     0       0       0
902                1590  Fell 1939     1   1     0       0       0
903                4500  Fell 1796     1   1     0       0       0
904                 637  Fell 1935     1   1     0       0       0
905                6460  Fell 1925     2   3     7      17      35
906              144000  Fell 1858     1   1     0       0       0
907                  22  Fell 1866     1   1     0       0       0
908                3760  Fell 1921     1   1     0       0       0
909               40000  Fell 1925     2   3     9      10      27
910                 215  Fell 2009     1   1     0       0       0
911                 720  Fell 1880     1   1     0       0       0
912                4500  Fell 1671     1   1     0       0       0
913                2830  Fell 1938     1   1     0       0       0
914                 345  Fell 1922     1   1     0       0       0
915                3700  Fell 1925     2   3     9      24      50
916                3200  Fell 1803     1   1     0       0       0
917               54640  Fell 1951     1   1     0       0       0
918                3700  Fell 1929     1   1     0       0       0
919                 900  Fell 1955     1   1     0       0       0
920                 340  Fell 1876     1   1     0       0       0
921                1320  Fell 1930     1   1     0       0       0
922               ++780  Fell 1845     1   1     0       0       0
923                8200  Fell 1907     1   1     0       0       0
924                5000  Fell 1995     1   1     0       0       0
925                1995  Fell 1925     2   3     6      38      25
926                3973  Fell 1936     1   1     0       0       0
927                4047  Fell 1932     1   1     0       0       0
928               37350  Fell 1925     2   3     7      28      55
929                 372  Fell 1887     1   1     0       0       0
930                 320  Fell 1984     1   1     0       0       0
931                 105  Fell 1925     2   3     7       6      15
932               14200  Fell 2011     1   1     0       0       0
933                9330  Fell 1925     2   3     7      18      52
934                6400  Fell 1897     1   1     0       0       0
935                6800  Fell 2006     1   1     0       0       0
936                 483  Fell 1974     1   1     0       0       0
937              100000  Fell 1956     1   1     0       0       0
938               16300  Fell 1829     1   1     0       0       0
939                 9.6 Found 2003     1   1     0       0       0
940  67.400000000000006  Fell 1967     1   1     0       0       0
941                6100  Fell 1910     1   1     0       0       0
942                2400  Fell 1961     1   1     0       0       0
943                3000  Fell 1959     1   1     0       0       0
944                 400  Fell 1871     1   1     0       0       0
945                1100  Fell 1980     1   1     0       0       0
946                4100  Fell 1933     1   1     0       0       0
947                 500  Fell 1925     2   3     8      54      56
948               42000  Fell 1996     1   1     0       0       0
949               202.6  Fell 1998     1   1     0       0       0
950                1230  Fell 1925     2   3     8      19       4
951                45.6  Fell 1925     2   3     9      35      37
952                2232  Fell 1986     1   1     0       0       0
953                24.3  Fell 1919     1   1     0       0       0
954               45000  Fell 1902     1   1     0       0       0
955               142.5  Fell 1938     1   1     0       0       0
956               18000  Fell 1924     1   1     0       0       0
957                 113  Fell 1940     1   1     0       0       0
958               25000  Fell 2004     1   1     0       0       0
959                4000  Fell 1978     1   1     0       0       0
960                3920  Fell 1988     1   1     0       0       0
961               11000  Fell 1889     1   1     0       0       0
962                 500  Fell 1957     1   1     0       0       0
963               65500  Fell 1807     1   1     0       0       0
964                5213  Fell 1925     2   3     8      46      45
965                 230  Fell 1967     1   1     0       0       0
966                 342  Fell 1950     1   1     0       0       0
967               29560  Fell 2006     1   1     0       0       0
968                8000  Fell 1881     1   1     0       0       0
969               18000  Fell 1853     1   1     0       0       0
970                <NA>  Fell 1822     1   1     0       0       0
971                5700  Fell 1925     2   3     6      53      29
972               367.5  Fell 1925     2   3     6      54      18
973                9000  Fell 1867     1   1     0       0       0
974                 910  Fell 1880     1   1     0       0       0
975             4000000  Fell 1925     2   3     9      25      46
976               12000  Fell 1931     1   1     0       0       0
977               22700  Fell 1925     2   3     7      16      24
978                2000  Fell 1925     2   3     7      48      53
979                2000  Fell 1956     1   1     0       0       0
980                  30  Fell 1999     1   1     0       0       0
981                 303  Fell 1938     1   1     0       0       0
982                13.9  Fell 1859     1   1     0       0       0
983                4500  Fell 1886     1   1     0       0       0
984                 414  Fell 1925     2   3     6      50       0
985                   2  Fell 1830     1   1     0       0       0
986                7000  Fell 1925     1   1     0       0       0
987                 438  Fell 1907     1   1     0       0       0
988               704.5  Fell 1925     2   3     8      15      30
989               14360  Fell 1741     1   1     0       0       0
990               50000  Fell 1858     1   1     0       0       0
991              3224.5  Fell 1925     2   3     8       5      14
992               18600  Fell 1904     1   1     0       0       0
993                1000  Fell 1836     1   1     0       0       0
994                1300  Fell 1923     1   1     0       0       0
995                8165  Fell 1925     2   3     9       9      25
996               17000  Fell 1919     1   1     0       0       0
997               117.8  Fell 1953     1   1     0       0       0
998                 678  Fell 2002     1   1     0       0       0
999                6400  Fell 1895     1   1     0       0       0
1000               6000  Fell 1925     2   3     9      18      52
         reclat     reclong
1     12.733330   15.050000
2     50.183330   12.733330
3     21.050000  -99.300000
4      5.383330   16.383330
5     39.083330  -89.150000
6     32.033330  -84.766670
7      9.500000   35.333330
8     37.050000  138.383330
9     52.033330   43.000000
10    47.750000   16.233330
11    35.733330  139.750000
12   -32.100000   28.333330
13   -33.750000   18.566670
14    31.833330  -98.833330
15   -19.850000  -47.550000
16    25.366670   82.916670
17    52.950000    8.166670
18    50.316670    3.733330
19    22.250000   79.000000
20   -35.083330  139.916670
21    50.950000   31.816670
22    55.000000   35.000000
23     0.000000    0.000000
24    45.968610  -72.978060
25    61.183330   22.683330
26     2.716670   32.833330
27    55.433330   53.250000
28    35.650000  139.333330
29    11.250000  108.583330
30    46.241670  126.196110
31    28.266670   78.783330
32    19.250000   77.000000
33    48.116670   -3.083330
34    35.950000  -97.583330
35    59.933330   14.950000
36    38.016670   -8.250000
37    46.550000    6.816670
38    -6.950000  108.100000
39    40.668130 -117.189130
40     9.233330   78.350000
41    34.316670  -96.150000
42    23.000000 -102.000000
43    25.900000   83.616670
44    43.666670   13.000000
45    38.700000  -90.233330
46    24.416670   39.516670
47    52.283330    7.700000
48   -30.866670  -64.550000
49    44.383330  -68.750000
50    25.100000   81.533330
51    21.208330   94.916670
52    38.133330   14.966670
53    45.291670   11.527220
54    50.133330   27.166670
55    48.766670    0.633330
56    13.766670    5.300000
57    44.600000    5.133330
58    47.550000   22.025000
59    43.266670   13.350000
60    46.818670    7.218170
61    44.750000    0.050000
62    43.400000   -5.866670
63    43.583330  -89.600000
64    34.583330  -87.500000
65    43.150000   -0.850000
66   -18.000000   16.000000
67    25.683330   83.250000
68    32.316670  -89.716670
69    39.800000   26.600000
70    37.833330  -76.700000
71   -32.750000  -65.283330
72    38.700660  -77.211630
73    15.366670   35.683330
74    29.516670   35.050000
75    39.566670   -2.100000
76    56.750000   75.333330
77    44.833330   95.166670
78   -33.000000  -58.616670
79    45.275000   26.709720
80    24.100000  105.000000
81    38.200000  115.700000
82         <NA>        <NA>
83    50.833330   25.500000
84   -11.500000   33.500000
85    26.683330   93.866670
86    42.450000   73.366670
87     7.050000   12.433330
88    39.779780   64.600350
89    26.750000   68.416670
90   -26.450000  120.366670
91    12.900000    7.150000
92    44.283330    5.583330
93    32.333330  120.666670
94    43.950000   -0.050000
95    36.050000  -94.166670
96    10.200000   76.266670
97    31.316670   30.350000
98    44.883330    8.750000
99    55.333330   10.333330
100   14.150830   -2.041670
101   27.250000   79.966670
102   -8.383330  -36.766670
103   20.933330 -101.383330
104   55.000000   34.000000
105   47.833330   30.766670
106   -6.033330  145.366670
107   33.484720 -117.662500
108   51.466670   84.766670
109   49.666670   14.033330
110   17.174930  -11.341330
111   26.666670   50.150000
112   39.916670   42.816670
113   40.085280  -86.055000
114   32.533330  -87.166670
115   27.083330   78.333330
116   13.660330   28.960000
117   27.433330   82.083330
118   52.216670   11.250000
119   39.700000  105.800000
120   34.483330  -87.833330
121   24.550000   84.833330
122    9.533330   39.716670
123   43.216670   -0.233330
124   41.200000  -73.133330
125   16.398060   -9.570280
126   30.300000  109.500000
127   26.533330  106.466670
128   52.459720   -0.300000
129   31.000000   75.000000
130   30.731110   66.802220
131   45.950000  -67.550000
132   47.525000   10.808330
133   22.083330   86.900000
134   48.166670   22.308330
135   23.083330   91.666670
136   51.916670    8.383330
137   38.500000   27.000000
138   26.750000   84.783330
139   12.766670    9.800000
140   33.188360  -86.294500
141   39.263330   31.780000
142   54.761830   11.467450
143   35.200000  118.800000
144   35.166670  135.333330
145   35.633330  -78.133330
146   39.000000   -0.033330
147  -19.483330   31.650000
148    4.700000   33.633330
149   54.566670   -6.333330
150   34.175000 -103.295000
151   56.633330   39.433330
152   47.700000   16.600000
153   36.183330  139.216670
154   45.683330   15.600000
155   40.250000  -74.000000
156   26.800000   82.533330
157   52.000000   19.916670
158   43.883330    1.383330
159   27.283330   79.050000
160   31.916670  104.916670
161   47.366670    1.733330
162   52.766670   21.266670
163   41.366670   -6.233330
164   47.933330    2.916670
165   26.966670 -105.316670
166   32.900000  115.900000
167   36.066670  -90.500000
168   21.683890   79.500830
169   41.000000  112.000000
170   43.766670   20.233330
171   50.950000    7.316670
172  -28.566670   30.700000
173   30.700000  -96.116670
174   36.041670  139.956670
175   14.052000    0.420000
176   49.866670    8.650000
177  -29.200000  -51.866670
178   29.866670  121.483330
179   33.433330  127.266670
180   51.650000   -1.516670
181  -28.200000   24.566670
182   34.300000  133.950000
183    8.066670  124.283330
184  -29.333330   27.583330
185   29.533330   71.800000
186  -22.966670  -44.316670
187   53.533330   -7.333330
188   52.866670   16.633330
189   25.733330   73.616670
190   20.462500   86.702780
191   46.216670    5.000000
192   33.133330  115.166670
193   42.520000  -82.260000
194   53.183330   13.150000
195   36.083330  -78.066670
196   42.516670   26.166670
197  -31.600000  -65.233330
198  -44.116670  -66.150000
199    0.000000    0.000000
200   52.550000   46.550000
201   27.725280   76.465000
202   13.783330   78.566670
203   42.716670  -99.383330
204   -7.450000  111.416670
205   52.200000    6.866670
206   -7.750000  111.533330
207  -17.350000   13.966670
208  -13.666670   33.916670
209   46.250000   -1.100000
210    1.345000   31.472780
211   11.333330   10.083330
212   40.500000  -80.083330
213   54.616670   -6.216670
214   28.533330  109.316670
215   58.033330   26.950000
216   44.979170 -122.969440
217   45.050000   41.983330
218   28.225000   70.200000
219   52.733330   84.083330
220   54.400000   10.150000
221   56.033330   13.000000
222        <NA>        <NA>
223   44.516670   39.300000
224   -3.400000  -44.333330
225   44.616670  126.133330
226   49.866670   30.833330
227   28.950000    0.816670
228  -26.666670  -65.450000
229  -25.500000  -54.500000
230   31.300000   72.383330
231   41.800000  -91.866670
232   26.250000   82.000000
233   45.200000    8.500000
234   35.500000  115.416670
235   53.666670   55.983330
236   38.400000  -84.250000
237   24.166670   87.766670
238   11.750000  122.333330
239  -29.033330  -53.050000
240   35.416670  -79.383330
241   26.466670 -105.233330
242   31.583330   71.600000
243  -31.535556  -68.489444
244   51.933330   15.500000
245  -15.212220   35.242220
246   31.805000  -97.010000
247    1.700000   33.633330
248   44.516670  -83.950000
249   32.102500  -81.872780
250   37.466670  -77.500000
251  -32.333330  -64.866670
252  -27.250000  -67.500000
253   24.333330   90.166670
254   53.583330   -2.716670
255   59.850000   17.666670
256   30.750000  -95.950000
257  -31.233330  -58.166670
258   20.916670   82.583330
259   14.883330   75.600000
260  -26.166670  -49.933330
261   38.250000  -85.750000
262   23.100000   87.300000
263  -20.810000  -49.380560
264   56.133330 -117.933330
265   44.366670  -69.200000
266   30.808330  109.500000
267   45.250000  125.000000
268  -20.000000   57.000000
269   45.416670  -98.316670
270   39.350000   -3.516670
271   58.666670   25.733330
272   12.633330   77.016670
273   12.000000  105.483330
274   27.083330   84.083330
275   61.200000   27.700000
276  -39.633330  174.400000
277   31.616670  113.466670
278   47.216670   81.016670
279   33.600000  -96.466670
280  -15.966670   27.350000
281   34.716670  137.783330
282    4.666670   29.250000
283   29.947180  -90.109760
284   25.850000   87.577500
285   23.696390   -5.014720
286   49.783330   30.166670
287   35.083330  135.200000
288   35.866670  139.400000
289   38.500000  -94.300000
290   37.608330   -8.280000
291   55.666670   25.000000
292  -31.183330  -57.950000
293   38.803890 -120.908060
294   46.800000   24.033330
295   35.883330  -99.333330
296   34.600000  116.750000
297   17.710670  -11.371500
298   39.750000  -97.033330
299   11.583330  104.916670
300   57.816670   16.233330
301   41.683330   -3.800000
302   28.200000   76.666670
303   52.666670   -6.966670
304   40.416670   -3.716670
305   52.283330    8.050000
306   21.866670   82.450000
307  -25.666670   28.366670
308   30.000000   -7.000000
309    9.366670   42.416670
310   26.965560   72.626670
311   -7.500000  111.500000
312   33.716670   74.833330
313   58.850000   16.733330
314  -33.156390  115.676390
315   20.936940   82.050000
316  -26.100000  -49.800000
317   50.750000   25.300000
318   42.860830  -73.950280
319   -1.870890  -79.957560
320    9.500000   31.750000
321   51.333330 -118.950000
322   35.033330  -83.033330
323   19.000000   97.000000
324   37.983330   -1.166670
325   20.339160   73.223290
326   43.433330    2.950000
327   43.383330   -5.150000
328   -1.450000   29.833330
329   32.216670 -104.000000
330   53.036390   -4.319440
331   33.883330   35.500000
332   14.000000   28.000000
333   25.800000   88.450000
334   38.666670  116.133330
335   43.033330   12.550000
336   25.666670   74.866670
337  -23.200000  -51.833330
338  -31.016670  -71.400000
339   34.900000 -110.183330
340   36.833330  -97.333330
341   16.583330  -11.333330
342   50.666940    8.183610
343   37.351720   27.329970
344   38.740280   -7.270000
345   41.633330    0.750000
346   26.983330   78.216670
347   35.133330   44.450000
348   36.166670    3.666670
349   -6.666670  106.583330
350   39.246670 -104.588170
351   25.254170   80.625000
352   44.650000   11.016670
353  -33.133330   19.383330
354   42.907500  -90.365560
355   39.683330  -99.866670
356        <NA>        <NA>
357   54.333330   56.083330
358   35.666670  115.500000
359   12.500000    0.700000
360   46.421370   14.052170
361   52.533330    9.050000
362   28.250000    7.000000
363  -23.383330  150.516670
364   46.183330   16.100000
365   35.300000  -86.633330
366   38.470280 -105.241390
367  -32.500000   21.900000
368   41.250000    1.666670
369  -27.200000   31.316670
370   19.083330    8.383330
371  -33.350000  146.858330
372   20.666670 -101.283330
373  -32.366670  -59.833330
374   54.816670   46.000000
375   10.016670    9.583330
376   39.438330  140.511670
377   32.333330   -6.300000
378   26.033330   81.466670
379   15.083330  120.700000
380   37.283330   14.700000
381   13.850000   -4.383330
382   50.625000   35.075000
383  -37.333330  -61.533330
384   52.750000   12.900000
385   53.350000   15.050000
386   43.333330  141.866670
387   54.008830   22.005000
388   38.250000  -86.166670
389   29.016670   77.800000
390   33.600000  130.433330
391   41.484720  -87.679170
392   52.983330   37.033330
393   47.116670    6.150000
394  -30.783330  -58.050000
395   -6.233330  -40.700000
396   39.083330  -94.400000
397    9.916670   13.983330
398   51.166670 -117.333330
399   52.450000   51.566670
400   25.950000   80.816670
401  -29.450000   26.766670
402   25.383330   82.600000
403   42.250000   59.200000
404   42.516670   12.516670
405   46.450000   25.783330
406   47.850000   34.766670
407   32.033330  130.633330
408    8.966670   12.083330
409   38.600000  -95.216670
410   61.733330    5.933330
411  -29.466670  151.616670
412  -33.900000  -61.700000
413   51.883330    6.933330
414   24.225000   76.525000
415   39.350000 -100.450000
416   48.150000   24.283330
417    7.050000   12.433330
418   -3.500000   35.583330
419   41.283330  -73.916670
420    0.000000   37.666670
421   49.366670   16.633330
422   39.000000   -0.666670
423   47.716670    5.366670
424    5.000000   48.000000
425   44.333330    3.233330
426   11.850000    8.216670
427   43.250000  -93.666670
428   48.600000   38.000000
429    1.166670   44.166670
430   48.900000   11.216670
431   -7.750000  112.766670
432   32.866670   10.800000
433   54.233330   52.483330
434   38.183330   -5.400000
435   14.183330   75.800000
436   39.750000   46.666670
437   40.000000  -99.250000
438   36.483330  -90.666670
439   25.150000  105.183330
440        <NA>        <NA>
441   52.450000   78.633330
442   55.500000   66.083330
443   -1.216670   30.000000
444   50.383330   -3.950000
445   36.216670  -92.266670
446   24.300000 -102.133330
447   43.500000   26.533330
448   31.616670   76.800000
449   36.541940   29.418220
450   51.333330   28.833330
451   45.133330   21.666670
452   11.083330   -0.183330
453   54.033330   40.900000
454   38.983330  141.616670
455   48.763670   21.176330
456   22.680000   70.313330
457   60.666670   11.000000
458   52.033330   20.935830
459  -27.700000   27.300000
460   50.200000   14.850000
461   36.850000  138.383330
462   -7.750000  112.016670
463   42.475000   -5.333330
464   52.133330   59.316670
465   48.600000   45.666670
466   41.383330  -86.600000
467   -3.666670   32.500000
468   31.416670  118.500000
469   34.750000  -87.000000
470   22.683330   90.650000
471   44.216670    0.616670
472  -26.166670   28.416670
473   16.000000   36.000000
474  -40.975000  145.600000
475   31.163330   -7.015000
476   49.166670   19.500000
477   43.833330   20.441670
478   12.850000   11.033330
479    9.600000    9.916670
480   36.100000  -81.416670
481   53.400000    9.100000
482  -25.733330  142.950000
483   50.300000    8.916670
484   26.200000   85.400000
485   32.233330   76.466670
486   29.483330   75.500000
487   22.377780   71.427220
488   32.083330   76.300000
489  -23.116670  -65.100000
490   51.333330   33.883330
491   29.583330   77.583330
492   38.137420 -119.758120
493   56.183330   10.233330
494   38.733330   -6.016670
495   30.833330  -97.766670
496   25.483330   81.983330
497   40.350000 -104.900000
498    3.405000  -76.510000
499   48.400000   -3.300000
500   32.083330  121.500000
501   36.008330  -95.150000
502   43.500000  125.666670
503   38.250000  -83.750000
504   35.366670  136.766670
505   52.550000   -8.033330
506   19.967220 -103.051670
507   30.900000   32.483330
508  -16.664440  -69.043890
509   20.066670 -103.666670
510   43.483330  118.616670
511   12.666670   78.033330
512   -4.000000   39.500000
513   18.683330   83.483330
514   61.650000   27.200000
515   -6.333330  106.000000
516   39.116670  -83.850000
517   26.450000   82.900000
518   42.133330   12.933330
519   49.283330   15.566670
520   32.916670  120.783330
521   35.800000  -84.100000
522   45.816670   44.633330
523   28.483330   78.500000
524   35.533330  136.883330
525   35.273330   44.215560
526   24.500000   90.216670
527   47.850000    0.483330
528   44.116670   27.266670
529  -30.883330  -64.550000
530   38.416670  -77.166670
531   36.783330  -78.083330
532   52.000000    9.100000
533        <NA>        <NA>
534   24.800000  103.300000
535   36.750000  -93.500000
536   45.821330    6.015330
537   48.116670   10.466670
538   60.400000   25.800000
539   23.050000   72.633330
540   27.700000   76.333330
541   42.900000    2.250000
542   50.600000   16.300000
543   20.966670   76.100000
544   55.466670   13.783330
545   33.725000  130.750000
546    9.066670   38.416670
547   34.733330  135.166670
548  -33.166670  -64.950000
549   58.883330    9.400000
550   46.066670   23.583330
551   51.900000   -1.116670
552   31.475560  113.566940
553   57.783330   55.266670
554   39.804170  122.763890
555   49.600000   17.116670
556   41.984440   59.685000
557   47.350000   21.300000
558   -7.716670  111.200000
559  -10.266670   38.766670
560   47.216670   29.866670
561   44.300000    0.400000
562  -31.910000  -58.328330
563   38.300000   27.133330
564   55.700000   24.400000
565   54.216670 -113.000000
566   41.683330 -112.133330
567   11.491670   11.658330
568   53.100000   23.200000
569   44.383330    2.816670
570   44.900000  -90.283330
571   23.050000   86.700000
572   45.433330   -0.450000
573  -19.466670  -44.216670
574    1.066670   34.166670
575   26.216670   69.550000
576   29.716670   77.950000
577   36.166670  -87.333330
578        <NA>        <NA>
579   -5.200000  -36.666670
580   37.866670   27.516670
581   45.300000    0.233330
582   54.666670   -7.666670
583   12.833330    9.383330
584   -6.733330  111.366670
585   50.666670   16.766670
586   24.950000   75.150000
587   43.350000   11.500000
588   25.550000   83.116670
589   49.600000   18.533330
590   34.720000  137.305000
591   26.916670   83.966670
592   46.100000   16.333330
593   49.400000   14.650000
594   37.621940  -82.237220
595   -8.933330   33.800000
596   26.783330   82.833330
597   24.683330 -101.683330
598   25.933330   84.283330
599   52.766670   -2.516670
600   42.450000    9.033330
601   55.900000   -4.233330
602   12.900000    0.080000
603   28.016670   75.816670
604   32.433330  119.866670
605   19.000000   81.000000
606   45.400000  122.900000
607   20.745750   32.412750
608   50.775000    6.083330
609  -33.000000  -66.000000
610   12.916670   11.916670
611   26.716670   82.666670
612   31.600000   65.783330
613   54.566670   -1.166670
614   45.500000  119.033330
615   12.450000   -0.083330
616   43.838890   20.333330
617   57.350000   37.616670
618   59.733330   10.866670
619   20.266670   76.016670
620   43.761110  -84.507780
621   36.233330   37.133330
622   -7.033330  -40.166670
623    6.750000    8.500000
624   28.783330   78.833330
625   10.833330   76.033330
626   36.500000  -79.250000
627   37.100000  118.400000
628   31.608330 -102.858330
629   -6.916670  107.600000
630   -3.133330   33.516670
631   31.250000  -99.033330
632   41.900000  -91.600000
633   48.700000   37.500000
634   53.415000 -111.337500
635  -28.933330  -63.233330
636   33.650000  120.066670
637   35.400000  -92.050000
638   48.533330   35.983330
639   23.366670   75.183330
640   13.050000    3.166670
641   47.166670    0.433330
642   42.383330   -2.500000
643   48.350000    3.250000
644   60.245560   22.061940
645   -7.250000  107.700000
646   15.716670  108.100000
647   59.433330   10.700000
648   25.916670   86.366670
649   44.716670    4.300000
650   49.326940    7.464720
651   35.568330  133.220000
652   44.116670    4.083330
653   46.950000   -1.500000
654   43.466670   25.516670
655   36.950000    9.550000
656   51.750000    4.000000
657   47.883330   22.033330
658   27.666670   95.783330
659   32.100000   71.800000
660   44.383330    5.166670
661   42.533330  -85.883330
662   51.150000   31.700000
663   13.633330   79.416670
664        <NA>        <NA>
665   52.750000   11.050000
666   17.900000  101.633330
667   52.266670   32.850000
668   43.733330    1.383330
669   38.183330   15.566670
670  -10.666670   35.500000
671  -26.150000   26.183330
672   21.866670   76.033330
673   38.500000 -101.100000
674   50.933330   12.133330
675   56.800000   77.000000
676   13.500000   -0.683330
677   38.000000   -0.833330
678        <NA>        <NA>
679   41.050000   23.566670
680  -29.800000  141.700000
681   48.066670   30.966670
682   38.200000  -89.683330
683   34.416670    3.250000
684   11.650000   -2.183330
685  -32.866670   18.716670
686   44.416670   23.500000
687   53.900000 -112.883330
688   31.633330  118.983330
689   40.781670  -96.471670
690   51.600000   10.800000
691   50.538060   16.263330
692   64.020000  161.808330
693   -6.233330  106.883330
694   50.766670    3.000000
695   28.383330   75.783330
696   27.800000   67.166670
697   -7.000000  115.500000
698   40.000000  -81.766670
699   29.481950   -7.611230
700   42.916670  102.466670
701   16.883330  -99.900000
702   43.666670   21.866670
703   36.883330    8.450000
704   21.950000   78.033330
705   51.533330   14.883330
706   36.400000  -86.500000
707   17.766670   95.183330
708   46.233330   17.533330
709   35.666670  103.500000
710   29.083330  111.750000
711   19.766670 -100.650000
712   46.683330    1.050000
713   46.883330 -102.316670
714   24.300000  -13.100000
715   52.450000   -8.333330
716   34.750000   10.716670
717   37.916670  -92.083330
718   39.800000   16.200000
719   37.416670   -6.000000
720   46.716670   23.500000
721   48.283330   38.083330
722   26.716670   84.216670
723   30.400000   78.200000
724   38.400000  140.350000
725   58.500000   23.000000
726    2.000000   22.666670
727   56.116670   37.333330
728   -3.000000  151.000000
729   45.766670    3.250000
730  -31.416670  -60.666670
731   26.950000   77.883330
732  -17.650000   31.600000
733   44.083330  -69.483330
734   50.750000   33.500000
735   30.180000   -6.577170
736   43.181110   13.753330
737   43.416670  -94.833330
738   35.294500  136.700330
739    9.333330   80.833330
740   49.233330   17.566670
741   35.033330  -81.883330
742   54.816670   61.116670
743   48.183330   13.133330
744   50.616670   36.600000
745   26.133330   85.533330
746  -14.200000   48.100000
747   50.833330   34.000000
748   26.583330   85.566670
749   20.883330   74.833330
750   53.016670   -4.100000
751   58.550000   31.333330
752   23.325000   91.000000
753   12.383330   78.516670
754  -31.883330  -60.466670
755   28.547500  -81.362220
756   32.925000  105.906670
757   18.700000    4.800000
758   40.916670  -78.083330
759   50.350000   10.800000
760   26.590830  -12.952170
761   66.348330   14.015000
762   43.333330  128.250000
763   32.675000  -94.511670
764   46.600000    0.250000
765        <NA>        <NA>
766   34.400000  -87.066670
767   43.390560    1.962500
768   44.500000    0.150000
769   35.550000  -77.533330
770   47.633330    1.583330
771   33.183330  130.200000
772   38.716670   -7.066670
773   52.996000 -109.848170
774   35.666670  136.300000
775   56.666670   23.000000
776   16.666670  101.183330
777   55.783330   61.366670
778   17.833330   73.983330
779  -31.350000  129.190000
780   33.750000   51.500000
781   25.370000  -97.370000
782   24.233330   89.050000
783   22.983330   76.050000
784   43.750000    5.116670
785   29.533330   72.300000
786  -36.616670  145.200000
787   12.850000   76.800000
788   56.650000   15.866670
789   21.166670 -100.866670
790   56.583330   -3.250000
791   52.566670   -8.783330
792   43.666670   45.383330
793   26.464690  106.632410
794   25.366670   81.666670
795   48.302500   13.940830
796   21.300000 -157.783330
797   19.383330   43.733330
798   42.458330   18.441670
799   47.700000    1.066670
800   27.133330   84.066670
801   46.050000    4.633330
802   50.366670    4.733330
803   50.583330   11.100000
804   14.250000    1.533330
805    0.266670   36.533330
806   53.700000   30.100000
807   25.000000 -103.500000
808   59.704440 -134.201390
809   48.183330    6.466670
810   50.666670    2.333330
811   46.160000  134.653330
812   44.083330    6.866670
813   28.566670   77.250000
814   30.300000   76.633330
815   23.500000   90.333330
816   14.500000   76.500000
817   13.333330   78.950000
818   38.000000   35.000000
819   48.016670   -1.150000
820   40.200000   29.233330
821   41.933330   -7.466670
822   51.133330   34.500000
823   36.216670 -103.400000
824   44.866670   10.050000
825   39.800000  -91.500000
826   17.743330  120.455830
827   23.983330   87.083330
828   47.083330    1.750000
829   24.166670  -99.333330
830   30.125000 -103.116670
831   37.266670  -89.583330
832   48.900000   22.400000
833        <NA>        <NA>
834   47.866670    7.350000
835   51.200000  112.300000
836   55.050000   12.066670
837   41.716670   -0.050000
838   29.583330   76.000000
839  -21.733330  165.900000
840   29.450000  -96.000000
841   47.933330    1.566670
842   21.300000 -157.866670
843   20.833330   71.466670
844   34.233330   72.083330
845   22.833330    4.983330
846   48.133330   12.616670
847   26.833330   77.333330
848   34.200000  131.566670
849   35.250000  -80.500000
850   64.433330   21.200000
851   49.266670   18.716670
852   35.433330  136.233330
853   33.833330  115.833330
854   20.883330   76.866670
855   32.116670  121.800000
856   40.480000  -89.004170
857   44.550000  131.633330
858   56.000000   22.000000
859   39.800000 -101.200000
860   56.500000   21.500000
861  -31.250000   26.466670
862   45.216670   62.083330
863  -32.016670   27.416670
864   44.244170   11.502220
865   27.883330   68.533330
866   41.366670    2.166670
867   51.783330   -1.783330
868   44.766670   11.283330
869   38.121670  140.061670
870   49.033330   -1.433330
871   51.050000    3.750000
872   -7.216670   29.433330
873   -6.833330   29.500000
874   29.683330   78.383330
875   47.816670  -96.850000
876   49.983330    7.533330
877   52.565280   -1.339720
878   36.449170  136.465280
879   44.616670  -70.750000
880   35.078330  136.933330
881        <NA>        <NA>
882  -22.250000  -49.933330
883   10.000000  -84.100000
884   45.266670   10.150000
885   45.483330    9.516670
886   50.533330   14.116670
887   40.300000   44.500000
888   40.305830 -105.023250
889   35.500000  -93.500000
890  -19.533330  -48.566670
891  -20.000000  -45.000000
892   35.616670    8.833330
893   43.000000  -80.000000
894   36.600000  -88.100000
895   -7.083330  111.533330
896   48.450000    3.933330
897   41.116670   45.000000
898   42.533330   12.616670
899   13.733330  100.083330
900   19.866670 -100.816670
901   11.333670   -3.542170
902   -7.233330  107.333330
903   38.500000   -8.000000
904   44.866670  133.166670
905   45.083330    8.300000
906   38.116670   -1.166670
907   18.750000   75.333330
908   31.950000  -83.516670
909   39.824670   36.135830
910   43.200000  -79.616670
911   35.383330  134.900000
912   48.500000    8.000000
913   36.800000 -108.000000
914   34.500000  -88.666670
915   43.116670   11.600000
916   43.866670    5.383330
917   47.833330   37.666670
918   35.966670  -82.483330
919   26.783330   82.716670
920   41.083330  -86.283330
921   35.000000  127.500000
922   48.533330   -0.866670
923   25.850000   83.483330
924   51.683330   30.783330
925  -28.833330   31.950000
926   58.200000   82.933330
927   55.200000   75.333330
928   23.150000   91.183330
929   24.450000   78.566670
930   40.810560  140.785560
931   21.250000   83.666670
932   -1.002780   37.150280
933  -21.460280  -49.950830
934   27.350000   68.533330
935   25.143330   75.813330
936  -11.850000   15.833330
937  -19.133330  -51.666670
938   33.016670  -83.966670
939        <NA>        <NA>
940   11.633330   24.683330
941   27.333330   35.666670
942  -17.300000   15.833330
943    8.916670    8.433330
944   42.300000    0.550000
945   34.800000  111.700000
946   42.583330 -103.666670
947   55.466670   35.866670
948   26.034720   73.941670
949   43.383330  -80.383330
950   46.000000   13.500000
951   43.200000  -96.916670
952   32.946670  118.990000
953   38.166670  -76.383330
954   61.500000   30.500000
955   52.300000   77.033330
956   11.650000   -3.650000
957   19.033330   81.891670
958   32.250000   -8.150000
959   47.470830    0.976670
960  -10.059440   33.395000
961   56.216670   13.033330
962    2.883330   30.833330
963   54.500000   35.200000
964   56.000000   26.433330
965   39.782500 -104.930560
966   33.400000   70.600000
967   15.783330   -5.900000
968   46.350000   30.583330
969   37.316670   13.566670
970   47.466670   -0.550000
971   42.900000   24.700000
972  -34.050000  150.688890
973   36.183330    5.416670
974   52.200000   17.983330
975   44.050000  126.166670
976   32.950000   10.416670
977   46.500000   25.733330
978   15.000000   48.300000
979   39.300000    9.200000
980  -31.666670  152.833330
981   40.933330  -79.733330
982   42.533330  -73.833330
983   12.283330   79.200000
984   34.450000  132.383330
985   56.400000   -3.433330
986  -30.116670   28.700000
987   40.000000   29.000000
988   -8.416670   32.433330
989   33.283330  130.200000
990   43.083330    0.583330
991   25.383330   72.016670
992   44.050000  -80.166670
993   45.366670    1.866670
994   52.050000    0.300000
995   12.000000    1.000000
996   36.833330  -84.350000
997   51.666670    9.250000
998   26.508330   73.115280
999   27.666670   78.250000
1000  34.166670  -80.283330
                                                                                   features
1    grey, some holes, smooth, irregular, thin lines, black, high in iron and nickel, shiny
2                                   black, smooth, thin lines, shiny, irregular, some holes
3                                                                   high in iron and nickel
4                                                                                          
5                                                                  grey, smooth, thin lines
6    black, high in iron and nickel, smooth, some holes, grey, thin lines, shiny, irregular
7                                     some holes, shiny, irregular, high in iron and nickel
8           irregular, thin lines, some holes, high in iron and nickel, grey, black, smooth
9                                            thin lines, high in iron and nickel, irregular
10               thin lines, black, smooth, high in iron and nickel, irregular, grey, shiny
11                                                                                         
12                                             black, grey, high in iron and nickel, smooth
13                                                          high in iron and nickel, smooth
14          irregular, grey, some holes, high in iron and nickel, shiny, smooth, thin lines
15          thin lines, irregular, grey, some holes, black, high in iron and nickel, smooth
16                                         thin lines, shiny, grey, high in iron and nickel
17               some holes, black, high in iron and nickel, irregular, shiny, grey, smooth
18                                                      thin lines, irregular, black, shiny
19   shiny, irregular, thin lines, some holes, smooth, grey, black, high in iron and nickel
20                                                                                         
21                                  irregular, grey, shiny, smooth, high in iron and nickel
22                                                                                         
23                                                                                         
24   black, some holes, grey, high in iron and nickel, irregular, thin lines, smooth, shiny
25                                                                                         
26                           high in iron and nickel, black, some holes, smooth, thin lines
27                              grey, irregular, black, high in iron and nickel, thin lines
28                                                                             smooth, grey
29   thin lines, grey, shiny, black, some holes, high in iron and nickel, smooth, irregular
30                                                                                         
31                            thin lines, smooth, high in iron and nickel, black, irregular
32   thin lines, some holes, smooth, black, grey, shiny, irregular, high in iron and nickel
33                                                                                         
34                                    some holes, shiny, irregular, high in iron and nickel
35                                                                               thin lines
36                                               black, high in iron and nickel, thin lines
37                                                                                         
38                                                                                         
39                                                                                         
40                                                                                         
41                                           thin lines, irregular, high in iron and nickel
42                                                        irregular, thin lines, some holes
43               high in iron and nickel, thin lines, black, grey, smooth, irregular, shiny
44   irregular, thin lines, some holes, black, shiny, high in iron and nickel, smooth, grey
45                                                                                         
46                                                                                         
47                                                  irregular, thin lines, grey, some holes
48                                       high in iron and nickel, smooth, shiny, some holes
49              black, smooth, thin lines, grey, shiny, high in iron and nickel, some holes
50                black, thin lines, some holes, smooth, irregular, high in iron and nickel
51              shiny, thin lines, high in iron and nickel, some holes, black, grey, smooth
52                                                                                         
53                                                                                         
54                                                                                         
55                                                                  high in iron and nickel
56                                                                                irregular
57                  grey, black, high in iron and nickel, some holes, irregular, thin lines
58                                  some holes, thin lines, smooth, black, shiny, irregular
59                                                                                         
60                                          black, high in iron and nickel, grey, irregular
61                                                            black, thin lines, some holes
62                                                                                         
63                                                          smooth, high in iron and nickel
64                black, smooth, high in iron and nickel, thin lines, some holes, irregular
65                                                                                         
66                                                                  high in iron and nickel
67          high in iron and nickel, some holes, smooth, irregular, thin lines, black, grey
68                                          high in iron and nickel, grey, irregular, shiny
69                                                                                         
70                 shiny, black, irregular, some holes, thin lines, high in iron and nickel
71                                                 thin lines, black, irregular, some holes
72                                shiny, thin lines, high in iron and nickel, black, smooth
73                                         smooth, some holes, thin lines, black, irregular
74                                                                             shiny, black
75                                                                        black, thin lines
76                                                                                         
77                                                                                         
78              shiny, black, smooth, thin lines, grey, some holes, high in iron and nickel
79                                                                             shiny, black
80                                                      thin lines, high in iron and nickel
81                                                                                     grey
82                      high in iron and nickel, shiny, some holes, grey, smooth, irregular
83                                                                               thin lines
84                                              high in iron and nickel, smooth, some holes
85         thin lines, irregular, black, shiny, some holes, smooth, high in iron and nickel
86   irregular, smooth, black, high in iron and nickel, thin lines, some holes, shiny, grey
87                                                                               some holes
88                                                                                         
89                                                                                         
90         shiny, irregular, high in iron and nickel, smooth, thin lines, some holes, black
91                                                   shiny, high in iron and nickel, smooth
92                                             grey, black, high in iron and nickel, smooth
93                             smooth, high in iron and nickel, grey, irregular, some holes
94                                                           thin lines, some holes, smooth
95                                                                                    shiny
96                                    black, thin lines, high in iron and nickel, irregular
97   high in iron and nickel, some holes, thin lines, shiny, irregular, smooth, grey, black
98                                                                      smooth, shiny, grey
99                                                                                         
100              some holes, smooth, high in iron and nickel, irregular, grey, black, shiny
101                                                          some holes, smooth, thin lines
102                                                                   irregular, some holes
103          irregular, shiny, some holes, grey, high in iron and nickel, thin lines, black
104         grey, high in iron and nickel, thin lines, smooth, black, some holes, irregular
105  shiny, smooth, some holes, black, high in iron and nickel, grey, thin lines, irregular
106                      black, shiny, high in iron and nickel, some holes, grey, irregular
107                                                                        grey, some holes
108                                                                 grey, black, thin lines
109                                                                                        
110                                                                                        
111                                                                   some holes, irregular
112                                            smooth, some holes, shiny, thin lines, black
113                                                                                        
114                                                                                        
115                                                   shiny, thin lines, smooth, some holes
116                                                                 high in iron and nickel
117  shiny, thin lines, irregular, grey, smooth, black, high in iron and nickel, some holes
118         shiny, thin lines, smooth, high in iron and nickel, grey, some holes, irregular
119                                                                              thin lines
120                grey, smooth, thin lines, high in iron and nickel, irregular, some holes
121                                                              smooth, shiny, grey, black
122                                      smooth, thin lines, high in iron and nickel, shiny
123  shiny, high in iron and nickel, smooth, thin lines, black, grey, some holes, irregular
124                                                                           shiny, smooth
125                                  shiny, irregular, grey, smooth, some holes, thin lines
126                                                                                        
127                                                               black, some holes, smooth
128          black, shiny, grey, some holes, high in iron and nickel, irregular, thin lines
129                                                     grey, irregular, smooth, some holes
130                                 irregular, shiny, smooth, thin lines, black, some holes
131                                                                                   black
132  shiny, irregular, smooth, thin lines, black, some holes, grey, high in iron and nickel
133                                               black, high in iron and nickel, irregular
134  high in iron and nickel, grey, thin lines, some holes, shiny, irregular, black, smooth
135                                                   shiny, black, high in iron and nickel
136                                                                               irregular
137                                                    irregular, some holes, black, smooth
138                                                                                        
139  grey, irregular, black, shiny, thin lines, smooth, some holes, high in iron and nickel
140                                               black, high in iron and nickel, irregular
141                                                                                        
142                                                                                        
143                                                                                        
144  high in iron and nickel, black, smooth, irregular, grey, some holes, shiny, thin lines
145         shiny, irregular, some holes, high in iron and nickel, grey, thin lines, smooth
146             thin lines, black, shiny, smooth, grey, high in iron and nickel, some holes
147                                                                                        
148                                                                                   shiny
149                                                        some holes, black, shiny, smooth
150                                                                               irregular
151                               high in iron and nickel, black, some holes, smooth, shiny
152                                                                             grey, shiny
153                                                                           smooth, shiny
154                                                                                        
155                                                                                        
156                                                                                        
157                                                                                   black
158                                                                                        
159  grey, smooth, some holes, thin lines, black, irregular, shiny, high in iron and nickel
160              high in iron and nickel, thin lines, irregular, grey, black, shiny, smooth
161                     high in iron and nickel, smooth, black, some holes, grey, irregular
162               high in iron and nickel, black, thin lines, smooth, irregular, some holes
163                                                                                        
164                                                                                        
165                                              irregular, smooth, high in iron and nickel
166                                   black, high in iron and nickel, some holes, irregular
167                                                                       thin lines, shiny
168                                          thin lines, irregular, black, some holes, grey
169                                                   black, shiny, high in iron and nickel
170                                                                            shiny, black
171                                      grey, thin lines, some holes, black, shiny, smooth
172                                                                                        
173                                                                        irregular, black
174                                 smooth, irregular, some holes, thin lines, shiny, black
175                                          irregular, some holes, grey, thin lines, shiny
176  grey, irregular, high in iron and nickel, black, shiny, smooth, some holes, thin lines
177                                                     shiny, some holes, thin lines, grey
178                                            grey, shiny, high in iron and nickel, smooth
179                                                                 high in iron and nickel
180                                                           high in iron and nickel, grey
181  grey, irregular, some holes, shiny, thin lines, smooth, high in iron and nickel, black
182                                                                       thin lines, black
183                                                                                        
184                                   thin lines, high in iron and nickel, shiny, irregular
185                                                                        irregular, shiny
186                                              thin lines, some holes, grey, black, shiny
187                                  thin lines, grey, some holes, black, smooth, irregular
188                                                                   thin lines, irregular
189              smooth, grey, irregular, some holes, black, shiny, high in iron and nickel
190                                                                  grey, irregular, black
191                                  thin lines, grey, some holes, shiny, smooth, irregular
192  shiny, grey, black, some holes, irregular, thin lines, high in iron and nickel, smooth
193  shiny, smooth, thin lines, black, some holes, high in iron and nickel, grey, irregular
194                                         smooth, grey, irregular, thin lines, some holes
195                                                                       smooth, irregular
196                    thin lines, smooth, grey, shiny, high in iron and nickel, some holes
197                         high in iron and nickel, grey, black, thin lines, shiny, smooth
198                                                           some holes, smooth, irregular
199                                                                 high in iron and nickel
200  thin lines, black, shiny, grey, some holes, smooth, high in iron and nickel, irregular
201                                                         high in iron and nickel, smooth
202                                              black, irregular, smooth, grey, thin lines
203                 shiny, some holes, grey, thin lines, high in iron and nickel, irregular
204                    shiny, thin lines, high in iron and nickel, black, irregular, smooth
205                                             shiny, some holes, smooth, irregular, black
206                                                            irregular, shiny, thin lines
207                                                                                        
208                                                                            shiny, black
209                                  shiny, thin lines, some holes, high in iron and nickel
210                                                                              thin lines
211                                             shiny, black, smooth, irregular, some holes
212                                                               black, smooth, some holes
213                                                      some holes, shiny, irregular, grey
214              grey, shiny, some holes, irregular, smooth, high in iron and nickel, black
215  some holes, thin lines, high in iron and nickel, black, grey, shiny, smooth, irregular
216                                                                                        
217  high in iron and nickel, some holes, thin lines, smooth, black, grey, shiny, irregular
218                           thin lines, shiny, irregular, smooth, black, some holes, grey
219                                                                   some holes, irregular
220                                                     irregular, some holes, shiny, black
221                                                                       shiny, thin lines
222         grey, shiny, some holes, high in iron and nickel, thin lines, irregular, smooth
223                                                                                        
224  some holes, grey, smooth, irregular, shiny, thin lines, high in iron and nickel, black
225              high in iron and nickel, grey, thin lines, black, shiny, smooth, irregular
226                                  thin lines, black, smooth, irregular, grey, some holes
227                                high in iron and nickel, smooth, shiny, some holes, grey
228  black, shiny, some holes, smooth, high in iron and nickel, grey, thin lines, irregular
229                    smooth, thin lines, grey, black, some holes, high in iron and nickel
230                                                                                        
231                                                                                        
232        thin lines, smooth, black, shiny, high in iron and nickel, some holes, irregular
233  black, smooth, irregular, grey, high in iron and nickel, some holes, shiny, thin lines
234                                                   black, shiny, high in iron and nickel
235                                                                                        
236                                       some holes, grey, shiny, black, smooth, irregular
237                                                irregular, thin lines, some holes, black
238          irregular, thin lines, high in iron and nickel, some holes, black, shiny, grey
239                       high in iron and nickel, some holes, shiny, irregular, thin lines
240                                                                                   shiny
241                    smooth, grey, high in iron and nickel, some holes, black, thin lines
242                                                                                        
243                                                                         irregular, grey
244                                                                                        
245                                                                              thin lines
246                                                                                        
247                     irregular, high in iron and nickel, grey, black, some holes, smooth
248                                         thin lines, some holes, high in iron and nickel
249                                                                                        
250                     irregular, smooth, black, some holes, grey, high in iron and nickel
251                                                     shiny, some holes, black, irregular
252                                                                                        
253                                                                              some holes
254                                        shiny, high in iron and nickel, black, irregular
255                       thin lines, irregular, high in iron and nickel, black, some holes
256                                                                                  smooth
257              shiny, black, smooth, irregular, some holes, grey, high in iron and nickel
258                           some holes, shiny, thin lines, grey, black, smooth, irregular
259                                                                       black, thin lines
260                                                                               irregular
261                                                                       some holes, black
262                                                                                        
263                                                                            smooth, grey
264  high in iron and nickel, shiny, grey, thin lines, irregular, smooth, black, some holes
265                                                           grey, high in iron and nickel
266                           irregular, black, smooth, thin lines, high in iron and nickel
267                                                                           shiny, smooth
268                                                                              some holes
269             high in iron and nickel, thin lines, some holes, black, smooth, shiny, grey
270                                              some holes, grey, smooth, black, irregular
271                                                                              thin lines
272                                                                    shiny, black, smooth
273                                                                                        
274                    black, high in iron and nickel, smooth, irregular, some holes, shiny
275                                                                                   black
276                                   irregular, thin lines, black, high in iron and nickel
277                                                                                        
278                                               thin lines, high in iron and nickel, grey
279                                                                        grey, some holes
280                      irregular, grey, shiny, black, thin lines, high in iron and nickel
281                                                           grey, high in iron and nickel
282                                                                                        
283  black, high in iron and nickel, irregular, thin lines, grey, smooth, shiny, some holes
284                                                                    shiny, black, smooth
285  irregular, black, thin lines, shiny, high in iron and nickel, grey, some holes, smooth
286         high in iron and nickel, irregular, smooth, thin lines, shiny, grey, some holes
287                                                                                        
288                                              shiny, high in iron and nickel, thin lines
289                                                                               irregular
290          black, some holes, shiny, irregular, thin lines, high in iron and nickel, grey
291                     smooth, some holes, irregular, high in iron and nickel, grey, black
292                     high in iron and nickel, grey, smooth, irregular, some holes, shiny
293                                               thin lines, grey, high in iron and nickel
294                                                            grey, thin lines, some holes
295                                                         smooth, grey, thin lines, black
296  high in iron and nickel, some holes, grey, smooth, thin lines, irregular, shiny, black
297                             irregular, shiny, thin lines, high in iron and nickel, grey
298                                                     irregular, black, shiny, thin lines
299                                                                        thin lines, grey
300                                                                              thin lines
301  thin lines, black, grey, irregular, smooth, high in iron and nickel, some holes, shiny
302                                                                               irregular
303                                                                  some holes, thin lines
304                                                                             shiny, grey
305                                                         black, smooth, some holes, grey
306                                                                                    grey
307                                          thin lines, irregular, high in iron and nickel
308  some holes, grey, thin lines, irregular, shiny, smooth, black, high in iron and nickel
309                                                                                        
310              some holes, irregular, grey, smooth, black, high in iron and nickel, shiny
311                                              high in iron and nickel, thin lines, shiny
312                                                                                        
313                           shiny, smooth, irregular, some holes, thin lines, grey, black
314                 irregular, thin lines, some holes, high in iron and nickel, grey, black
315  black, smooth, grey, irregular, thin lines, high in iron and nickel, some holes, shiny
316                                                      irregular, high in iron and nickel
317                                                                                        
318                                                  high in iron and nickel, smooth, black
319                                                                     shiny, grey, smooth
320                                                                                   shiny
321  smooth, some holes, irregular, shiny, high in iron and nickel, thin lines, black, grey
322                                                                      thin lines, smooth
323                                                  shiny, high in iron and nickel, smooth
324             some holes, thin lines, smooth, high in iron and nickel, grey, shiny, black
325                                                                                        
326                     black, grey, thin lines, high in iron and nickel, shiny, some holes
327                           thin lines, black, smooth, some holes, irregular, shiny, grey
328                    smooth, high in iron and nickel, thin lines, shiny, irregular, black
329                                                                black, irregular, smooth
330  smooth, thin lines, high in iron and nickel, some holes, grey, shiny, black, irregular
331                                                                       smooth, irregular
332                                      thin lines, some holes, black, shiny, grey, smooth
333                grey, irregular, some holes, thin lines, smooth, high in iron and nickel
334                                                               thin lines, smooth, black
335                                         grey, high in iron and nickel, irregular, shiny
336                                                                     grey, smooth, black
337                                   some holes, shiny, high in iron and nickel, irregular
338                                                            some holes, irregular, black
339                                                         shiny, grey, smooth, some holes
340         shiny, grey, smooth, high in iron and nickel, thin lines, irregular, some holes
341                                                                                        
342                                                                                    grey
343                                                                 black, some holes, grey
344                                                                                        
345  shiny, irregular, thin lines, some holes, grey, black, smooth, high in iron and nickel
346                                                    smooth, some holes, grey, thin lines
347  black, high in iron and nickel, irregular, shiny, smooth, grey, some holes, thin lines
348                                                   grey, smooth, high in iron and nickel
349                                                           grey, high in iron and nickel
350                                                                black, shiny, some holes
351                                                  high in iron and nickel, smooth, black
352                                                                        thin lines, grey
353                                                                                        
354  black, grey, high in iron and nickel, shiny, thin lines, smooth, irregular, some holes
355  some holes, smooth, irregular, thin lines, black, shiny, grey, high in iron and nickel
356                                                               thin lines, smooth, black
357                                                                 some holes, grey, shiny
358  high in iron and nickel, some holes, black, smooth, shiny, irregular, thin lines, grey
359                          smooth, black, thin lines, high in iron and nickel, some holes
360                          black, irregular, smooth, shiny, grey, high in iron and nickel
361                                                                  some holes, thin lines
362                                                                                        
363         smooth, black, grey, high in iron and nickel, some holes, thin lines, irregular
364                                                                 shiny, grey, some holes
365                                                                                        
366                          grey, high in iron and nickel, shiny, irregular, black, smooth
367                                             some holes, shiny, smooth, grey, thin lines
368                                                  high in iron and nickel, smooth, shiny
369                                  black, some holes, irregular, thin lines, grey, smooth
370                                                                                        
371                     high in iron and nickel, irregular, smooth, black, grey, some holes
372                                       high in iron and nickel, smooth, some holes, grey
373                                                                                  smooth
374                         grey, thin lines, smooth, high in iron and nickel, shiny, black
375                                                                                        
376                                                                                    grey
377                                          some holes, irregular, shiny, grey, thin lines
378                                                         irregular, shiny, black, smooth
379                                                                                        
380                                                                     grey, smooth, shiny
381                                                     some holes, high in iron and nickel
382  some holes, thin lines, shiny, black, irregular, grey, smooth, high in iron and nickel
383                                                                                        
384                                                 some holes, grey, irregular, thin lines
385         thin lines, high in iron and nickel, black, irregular, some holes, smooth, grey
386  irregular, high in iron and nickel, shiny, some holes, thin lines, smooth, grey, black
387                                                                                        
388                                              some holes, smooth, grey, irregular, shiny
389        shiny, some holes, high in iron and nickel, thin lines, black, smooth, irregular
390  black, high in iron and nickel, thin lines, smooth, some holes, shiny, irregular, grey
391                                                     high in iron and nickel, thin lines
392                     smooth, grey, some holes, high in iron and nickel, irregular, shiny
393  irregular, black, smooth, some holes, high in iron and nickel, shiny, grey, thin lines
394  shiny, high in iron and nickel, irregular, black, some holes, grey, thin lines, smooth
395                                                                 high in iron and nickel
396                                                                                        
397                                 thin lines, shiny, grey, high in iron and nickel, black
398                                                                                        
399  black, shiny, thin lines, smooth, high in iron and nickel, irregular, grey, some holes
400                                      smooth, shiny, thin lines, high in iron and nickel
401                                                           irregular, smooth, some holes
402                                                                                        
403                                       irregular, smooth, high in iron and nickel, shiny
404                                                        black, thin lines, smooth, shiny
405                                                                            grey, smooth
406  thin lines, irregular, some holes, grey, smooth, high in iron and nickel, shiny, black
407                                   thin lines, shiny, irregular, high in iron and nickel
408  smooth, black, shiny, irregular, some holes, high in iron and nickel, grey, thin lines
409                                                                       smooth, irregular
410                                                                         irregular, grey
411                                                                                        
412  grey, some holes, black, smooth, high in iron and nickel, shiny, irregular, thin lines
413                                                                 black, grey, some holes
414                                                      grey, irregular, shiny, some holes
415                                         some holes, thin lines, irregular, shiny, black
416                                                                               irregular
417                                     high in iron and nickel, black, grey, smooth, shiny
418  smooth, some holes, thin lines, irregular, grey, shiny, black, high in iron and nickel
419                                                                                        
420                                         black, high in iron and nickel, irregular, grey
421  grey, black, shiny, smooth, irregular, high in iron and nickel, some holes, thin lines
422                                         some holes, high in iron and nickel, thin lines
423                                                                       black, some holes
424                                   high in iron and nickel, shiny, some holes, irregular
425                          smooth, high in iron and nickel, some holes, thin lines, shiny
426                                                                      shiny, grey, black
427                                                                                        
428                                                          shiny, high in iron and nickel
429                                                    some holes, shiny, thin lines, black
430                                                                                        
431  smooth, thin lines, high in iron and nickel, grey, shiny, irregular, some holes, black
432         grey, some holes, smooth, high in iron and nickel, shiny, thin lines, irregular
433                                                                                        
434                                                                                        
435                                                                                        
436                                                                                    grey
437                                                                                        
438                                                                        black, irregular
439              black, irregular, high in iron and nickel, some holes, grey, smooth, shiny
440                                                           shiny, some holes, thin lines
441                                  some holes, black, thin lines, high in iron and nickel
442              high in iron and nickel, irregular, grey, smooth, thin lines, shiny, black
443                                                  black, smooth, high in iron and nickel
444              some holes, black, irregular, smooth, grey, shiny, high in iron and nickel
445                                                           high in iron and nickel, grey
446                                                                        irregular, black
447                                      smooth, shiny, high in iron and nickel, thin lines
448                                                                                    grey
449  irregular, black, high in iron and nickel, some holes, grey, shiny, smooth, thin lines
450                     some holes, high in iron and nickel, black, grey, smooth, irregular
451                                                                        irregular, shiny
452              black, irregular, smooth, high in iron and nickel, some holes, shiny, grey
453                                                                      smooth, thin lines
454                                          high in iron and nickel, irregular, some holes
455                                                                                        
456  black, some holes, high in iron and nickel, shiny, irregular, grey, smooth, thin lines
457                                                                                        
458  shiny, smooth, thin lines, grey, some holes, irregular, high in iron and nickel, black
459                                                                                        
460        black, some holes, high in iron and nickel, irregular, shiny, smooth, thin lines
461                                                                                        
462                                              smooth, high in iron and nickel, irregular
463                                                            black, irregular, some holes
464                                                                                        
465                                                                                        
466                                                                      smooth, some holes
467  some holes, shiny, thin lines, irregular, smooth, grey, black, high in iron and nickel
468                                                                                        
469                                                         grey, smooth, thin lines, black
470  thin lines, grey, high in iron and nickel, black, smooth, some holes, irregular, shiny
471                                                                                        
472                               thin lines, high in iron and nickel, shiny, smooth, black
473                                                                                        
474                            irregular, black, shiny, some holes, high in iron and nickel
475         grey, high in iron and nickel, thin lines, irregular, some holes, shiny, smooth
476                                                                                    grey
477              high in iron and nickel, thin lines, grey, smooth, shiny, irregular, black
478                                                                                        
479                                                                                        
480                                                                                        
481              smooth, irregular, shiny, thin lines, black, high in iron and nickel, grey
482                                                                        grey, thin lines
483                                        shiny, grey, high in iron and nickel, thin lines
484                           shiny, irregular, thin lines, smooth, high in iron and nickel
485                    irregular, high in iron and nickel, thin lines, black, smooth, shiny
486                                                                smooth, shiny, irregular
487                                                                                        
488                                                                                   black
489  shiny, some holes, irregular, grey, thin lines, high in iron and nickel, smooth, black
490  grey, thin lines, some holes, high in iron and nickel, black, smooth, shiny, irregular
491                           shiny, some holes, high in iron and nickel, black, thin lines
492                                              grey, smooth, irregular, black, thin lines
493                                                                  thin lines, some holes
494  high in iron and nickel, thin lines, some holes, shiny, smooth, irregular, black, grey
495                                                                                        
496                                                                        thin lines, grey
497                                                                                        
498  some holes, thin lines, grey, shiny, black, smooth, high in iron and nickel, irregular
499                                                                                        
500                                                                                        
501                                                                                        
502                                                          irregular, smooth, black, grey
503                           high in iron and nickel, shiny, thin lines, some holes, black
504                                       shiny, grey, some holes, irregular, black, smooth
505                                                                           smooth, black
506                                          irregular, high in iron and nickel, some holes
507                                                                           shiny, smooth
508        irregular, smooth, black, high in iron and nickel, some holes, shiny, thin lines
509                                                                                        
510                                                                                        
511  shiny, some holes, grey, smooth, black, irregular, thin lines, high in iron and nickel
512                                                                        some holes, grey
513                                shiny, grey, smooth, some holes, high in iron and nickel
514                                                                                   black
515             smooth, thin lines, grey, high in iron and nickel, black, shiny, some holes
516                                                                                        
517                                                          grey, shiny, thin lines, black
518                                                                                        
519                                 grey, shiny, smooth, irregular, high in iron and nickel
520                                                                                        
521                                                   shiny, black, high in iron and nickel
522                                                black, irregular, thin lines, some holes
523                                                  black, grey, thin lines, smooth, shiny
524                                                                                        
525                                                                                  smooth
526                                                                                   shiny
527                                                                  some holes, thin lines
528  some holes, thin lines, shiny, grey, irregular, black, smooth, high in iron and nickel
529                                                          thin lines, some holes, smooth
530                                                           irregular, thin lines, smooth
531          some holes, grey, high in iron and nickel, shiny, irregular, thin lines, black
532                                                                        thin lines, grey
533                                              irregular, shiny, grey, smooth, some holes
534                                                                           black, smooth
535                                                                                        
536                                                                        some holes, grey
537          black, irregular, high in iron and nickel, grey, some holes, shiny, thin lines
538                                                               smooth, some holes, black
539                                      smooth, grey, shiny, black, some holes, thin lines
540                                                                                        
541         thin lines, irregular, grey, high in iron and nickel, some holes, smooth, black
542                                                                              some holes
543             high in iron and nickel, grey, shiny, smooth, black, thin lines, some holes
544                                                           some holes, smooth, irregular
545                                                grey, high in iron and nickel, irregular
546                                  smooth, grey, shiny, irregular, some holes, thin lines
547                                                                     smooth, grey, black
548                                                      high in iron and nickel, irregular
549                                                          grey, smooth, shiny, irregular
550                                                                                        
551                     thin lines, grey, shiny, high in iron and nickel, irregular, smooth
552  high in iron and nickel, thin lines, grey, black, shiny, some holes, smooth, irregular
553         some holes, grey, high in iron and nickel, thin lines, smooth, black, irregular
554                                               thin lines, high in iron and nickel, grey
555                                                                                        
556                                                                              some holes
557                                                          high in iron and nickel, black
558                                                                                        
559                                                                              some holes
560                grey, some holes, smooth, high in iron and nickel, irregular, thin lines
561                     shiny, grey, thin lines, smooth, high in iron and nickel, irregular
562                    irregular, shiny, thin lines, high in iron and nickel, smooth, black
563             high in iron and nickel, shiny, some holes, smooth, grey, thin lines, black
564                                                                                        
565                                        smooth, thin lines, irregular, some holes, shiny
566                                                        thin lines, smooth, shiny, black
567                                                                           shiny, smooth
568                                                                 shiny, thin lines, grey
569                grey, high in iron and nickel, thin lines, some holes, irregular, smooth
570                                 irregular, black, thin lines, smooth, some holes, shiny
571              high in iron and nickel, smooth, grey, black, thin lines, irregular, shiny
572                                                                                        
573                                                                         grey, irregular
574                                                                 high in iron and nickel
575                                                                   some holes, irregular
576                                                                                        
577  shiny, grey, smooth, black, some holes, thin lines, high in iron and nickel, irregular
578                                                                 black, some holes, grey
579                                                           smooth, thin lines, irregular
580                                      smooth, black, high in iron and nickel, thin lines
581               some holes, black, high in iron and nickel, thin lines, irregular, smooth
582                                  black, shiny, grey, irregular, high in iron and nickel
583                                                                 black, some holes, grey
584                                                                               irregular
585                                                                              some holes
586                    black, irregular, smooth, some holes, high in iron and nickel, shiny
587                                                                    black, smooth, shiny
588                                                                 high in iron and nickel
589                                                                            smooth, grey
590                      shiny, high in iron and nickel, thin lines, irregular, black, grey
591                                                                 high in iron and nickel
592                                                                                        
593                                        high in iron and nickel, grey, black, thin lines
594                                                                                        
595  grey, smooth, some holes, thin lines, black, irregular, shiny, high in iron and nickel
596                                              some holes, high in iron and nickel, shiny
597                                                         smooth, high in iron and nickel
598                                                                                        
599                           some holes, shiny, smooth, irregular, black, thin lines, grey
600                                                                                        
601                      thin lines, irregular, grey, high in iron and nickel, shiny, black
602                           smooth, black, some holes, shiny, grey, thin lines, irregular
603                       high in iron and nickel, some holes, thin lines, black, irregular
604  irregular, grey, shiny, some holes, thin lines, black, high in iron and nickel, smooth
605                                                         smooth, high in iron and nickel
606                                                                                        
607  black, smooth, grey, some holes, shiny, high in iron and nickel, thin lines, irregular
608                                                                                        
609                                                                                        
610                                grey, shiny, smooth, high in iron and nickel, some holes
611              smooth, some holes, shiny, black, high in iron and nickel, irregular, grey
612                                                                 high in iron and nickel
613                                                            black, some holes, irregular
614                                                          grey, thin lines, shiny, black
615                    irregular, high in iron and nickel, shiny, black, thin lines, smooth
616  shiny, some holes, thin lines, smooth, black, grey, irregular, high in iron and nickel
617                                                          shiny, high in iron and nickel
618                                                     some holes, high in iron and nickel
619  thin lines, smooth, shiny, grey, irregular, some holes, black, high in iron and nickel
620             some holes, black, grey, smooth, thin lines, high in iron and nickel, shiny
621                                                                smooth, grey, some holes
622  some holes, irregular, smooth, black, high in iron and nickel, grey, shiny, thin lines
623                                                                                        
624                                          irregular, high in iron and nickel, some holes
625         grey, irregular, smooth, some holes, thin lines, black, high in iron and nickel
626                                                                                        
627                                          high in iron and nickel, irregular, thin lines
628                                             shiny, smooth, grey, some holes, thin lines
629                                                                              thin lines
630                                                                                        
631                                                                                        
632                                                                       black, some holes
633                                   some holes, high in iron and nickel, irregular, shiny
634                                                                                        
635                                                                                        
636                                                                        irregular, black
637              high in iron and nickel, grey, black, shiny, smooth, irregular, some holes
638                                                                                        
639                 thin lines, grey, shiny, some holes, high in iron and nickel, irregular
640                                               high in iron and nickel, shiny, irregular
641                                              high in iron and nickel, thin lines, black
642        black, some holes, irregular, shiny, thin lines, high in iron and nickel, smooth
643                                                                              thin lines
644                 black, grey, thin lines, high in iron and nickel, some holes, irregular
645                                                                grey, thin lines, smooth
646                                                                  shiny, grey, irregular
647                                                                                        
648                                      smooth, shiny, grey, thin lines, some holes, black
649                                                                 high in iron and nickel
650  grey, thin lines, irregular, smooth, black, high in iron and nickel, shiny, some holes
651                                                                               irregular
652                                                                         grey, irregular
653         grey, irregular, smooth, thin lines, high in iron and nickel, some holes, black
654  grey, black, irregular, smooth, shiny, high in iron and nickel, some holes, thin lines
655                                                                                        
656                                                             some holes, irregular, grey
657                                                                                        
658                                          irregular, high in iron and nickel, thin lines
659                                                                                        
660                                               thin lines, high in iron and nickel, grey
661                    high in iron and nickel, some holes, thin lines, grey, black, smooth
662                                                                               irregular
663                                              high in iron and nickel, smooth, irregular
664                                 shiny, black, irregular, thin lines, smooth, some holes
665                           irregular, some holes, high in iron and nickel, black, smooth
666                                  smooth, irregular, some holes, high in iron and nickel
667                         grey, thin lines, black, high in iron and nickel, shiny, smooth
668                                                                 grey, irregular, smooth
669  thin lines, irregular, black, grey, some holes, shiny, smooth, high in iron and nickel
670  high in iron and nickel, smooth, black, grey, shiny, thin lines, some holes, irregular
671                                         high in iron and nickel, irregular, grey, shiny
672                                  grey, smooth, some holes, shiny, irregular, thin lines
673                                               thin lines, grey, black, shiny, irregular
674                            black, thin lines, shiny, high in iron and nickel, irregular
675                                               some holes, smooth, irregular, thin lines
676                                                                                        
677                                                                                        
678                                                                              some holes
679                                                                                        
680  thin lines, smooth, shiny, grey, some holes, irregular, black, high in iron and nickel
681  high in iron and nickel, irregular, grey, shiny, smooth, some holes, thin lines, black
682                           thin lines, black, high in iron and nickel, irregular, smooth
683                                                                       thin lines, black
684  black, high in iron and nickel, thin lines, irregular, shiny, some holes, smooth, grey
685                                                                                        
686                                 irregular, grey, shiny, high in iron and nickel, smooth
687                                                                            black, shiny
688                           irregular, shiny, some holes, smooth, grey, thin lines, black
689                                                                     smooth, shiny, grey
690                                   some holes, shiny, high in iron and nickel, irregular
691                    high in iron and nickel, black, irregular, smooth, shiny, thin lines
692                                             thin lines, shiny, smooth, black, irregular
693                    smooth, high in iron and nickel, some holes, shiny, irregular, black
694  smooth, irregular, some holes, grey, shiny, black, thin lines, high in iron and nickel
695  thin lines, irregular, smooth, black, high in iron and nickel, grey, shiny, some holes
696                                                               black, smooth, thin lines
697                 shiny, high in iron and nickel, grey, irregular, thin lines, some holes
698                                                                              thin lines
699                                                                              some holes
700                                                                                        
701                      grey, shiny, irregular, high in iron and nickel, black, thin lines
702                                                                      some holes, smooth
703                           irregular, black, thin lines, some holes, grey, smooth, shiny
704  shiny, irregular, grey, high in iron and nickel, black, thin lines, some holes, smooth
705  shiny, grey, high in iron and nickel, smooth, irregular, thin lines, some holes, black
706                                                                 shiny, irregular, black
707                                                           smooth, some holes, irregular
708  smooth, black, shiny, grey, some holes, high in iron and nickel, thin lines, irregular
709                           black, grey, some holes, irregular, thin lines, smooth, shiny
710                                                                 grey, some holes, black
711  some holes, thin lines, high in iron and nickel, shiny, black, smooth, grey, irregular
712                                                            thin lines, irregular, shiny
713                                                                                        
714                                                                   irregular, some holes
715                                                           grey, high in iron and nickel
716  smooth, black, high in iron and nickel, some holes, grey, irregular, shiny, thin lines
717                                                                                    grey
718  smooth, black, shiny, thin lines, grey, irregular, some holes, high in iron and nickel
719  some holes, shiny, high in iron and nickel, black, smooth, thin lines, irregular, grey
720                                                                                    grey
721                                                                              some holes
722                                                                                    grey
723                                                                   irregular, some holes
724                                        high in iron and nickel, grey, shiny, thin lines
725                                                                                        
726                           shiny, some holes, black, thin lines, smooth, grey, irregular
727                                                                                        
728                                         irregular, high in iron and nickel, grey, black
729                                                                                        
730                                                                                        
731                                                                                   shiny
732                            high in iron and nickel, irregular, some holes, black, shiny
733                          thin lines, high in iron and nickel, smooth, shiny, some holes
734                                                                                  smooth
735                                                                                        
736                                  thin lines, some holes, shiny, grey, irregular, smooth
737                                                                                        
738                                  irregular, thin lines, high in iron and nickel, smooth
739                                                                                    grey
740                                                                                        
741  grey, high in iron and nickel, shiny, smooth, black, irregular, some holes, thin lines
742                                            smooth, black, shiny, thin lines, some holes
743                                smooth, some holes, high in iron and nickel, grey, black
744                     black, thin lines, shiny, high in iron and nickel, grey, some holes
745                                                                   some holes, irregular
746                                              some holes, high in iron and nickel, shiny
747                                                                       thin lines, black
748                                            smooth, black, high in iron and nickel, grey
749                                                                                   black
750                                                                            shiny, black
751                                       grey, thin lines, black, smooth, shiny, irregular
752                                                                              some holes
753                                                          shiny, high in iron and nickel
754                                                                                        
755                some holes, black, shiny, irregular, thin lines, high in iron and nickel
756                                                            irregular, thin lines, shiny
757                                                                              thin lines
758                                              black, grey, some holes, smooth, irregular
759                                                                              thin lines
760         thin lines, smooth, grey, some holes, high in iron and nickel, black, irregular
761                                                                irregular, smooth, black
762                                                                                        
763  irregular, smooth, some holes, black, high in iron and nickel, thin lines, shiny, grey
764                                                                                        
765                                                                                        
766                                                                                   black
767                                                                                        
768                                                               black, smooth, some holes
769                                              high in iron and nickel, black, some holes
770                                                                      grey, black, shiny
771                                                                irregular, smooth, shiny
772                                             shiny, high in iron and nickel, grey, black
773                                                                                        
774                                                                                        
775         black, irregular, grey, some holes, thin lines, high in iron and nickel, smooth
776                     smooth, some holes, irregular, high in iron and nickel, grey, black
777                                                       thin lines, some holes, irregular
778                                                                                        
779                      thin lines, high in iron and nickel, smooth, irregular, some holes
780                     black, thin lines, high in iron and nickel, shiny, grey, some holes
781             thin lines, grey, shiny, smooth, black, some holes, high in iron and nickel
782                                                         shiny, smooth, irregular, black
783  shiny, high in iron and nickel, black, smooth, grey, some holes, irregular, thin lines
784                                                     irregular, thin lines, grey, smooth
785                    shiny, high in iron and nickel, thin lines, grey, some holes, smooth
786                                                  smooth, black, high in iron and nickel
787                                                                                    grey
788  shiny, some holes, smooth, thin lines, grey, black, high in iron and nickel, irregular
789  high in iron and nickel, black, smooth, thin lines, shiny, grey, some holes, irregular
790                                                                                  smooth
791             thin lines, high in iron and nickel, shiny, some holes, black, grey, smooth
792                                                                                   shiny
793  some holes, shiny, smooth, grey, black, irregular, thin lines, high in iron and nickel
794                                                                                        
795                                                                                        
796                                             irregular, smooth, thin lines, shiny, black
797  high in iron and nickel, smooth, shiny, thin lines, irregular, some holes, grey, black
798                                                                                        
799                                                            shiny, some holes, irregular
800  irregular, smooth, high in iron and nickel, grey, thin lines, shiny, black, some holes
801                                                                                        
802                                                     high in iron and nickel, thin lines
803  irregular, black, high in iron and nickel, some holes, grey, shiny, smooth, thin lines
804                                                                            black, shiny
805  smooth, some holes, thin lines, grey, high in iron and nickel, shiny, black, irregular
806             some holes, thin lines, black, smooth, grey, shiny, high in iron and nickel
807                    black, high in iron and nickel, grey, smooth, some holes, thin lines
808                      shiny, high in iron and nickel, thin lines, black, irregular, grey
809                                            some holes, smooth, shiny, thin lines, black
810                                                                              thin lines
811                                                                    smooth, shiny, black
812  black, some holes, thin lines, grey, high in iron and nickel, shiny, irregular, smooth
813                                   high in iron and nickel, thin lines, black, irregular
814                     grey, high in iron and nickel, thin lines, smooth, irregular, shiny
815                                                                                        
816         some holes, smooth, shiny, grey, thin lines, irregular, high in iron and nickel
817              black, some holes, irregular, shiny, grey, high in iron and nickel, smooth
818                                                            irregular, black, some holes
819                                  smooth, thin lines, irregular, shiny, some holes, grey
820         thin lines, irregular, smooth, high in iron and nickel, some holes, grey, shiny
821                                                                                        
822  smooth, shiny, thin lines, grey, black, high in iron and nickel, some holes, irregular
823                                                                        some holes, grey
824                                                                                        
825                                                     high in iron and nickel, thin lines
826        shiny, thin lines, some holes, high in iron and nickel, irregular, black, smooth
827                                                                                        
828                                                                                        
829                                                                                  smooth
830                                                                                        
831                                                   smooth, thin lines, shiny, some holes
832                                                            thin lines, shiny, irregular
833        black, some holes, thin lines, irregular, high in iron and nickel, shiny, smooth
834             shiny, some holes, high in iron and nickel, thin lines, grey, smooth, black
835  shiny, grey, some holes, thin lines, irregular, smooth, black, high in iron and nickel
836                                                                                        
837                                                     grey, thin lines, some holes, shiny
838                                  black, thin lines, some holes, grey, irregular, smooth
839                                                                                        
840                      black, shiny, thin lines, irregular, high in iron and nickel, grey
841                            high in iron and nickel, smooth, irregular, thin lines, grey
842                                              thin lines, grey, shiny, black, some holes
843                                                                        black, irregular
844                                 smooth, high in iron and nickel, irregular, black, grey
845  high in iron and nickel, grey, shiny, irregular, some holes, smooth, black, thin lines
846                    some holes, smooth, black, thin lines, grey, high in iron and nickel
847                                                                                        
848                                          some holes, high in iron and nickel, irregular
849                                                                 grey, thin lines, shiny
850              smooth, black, grey, shiny, high in iron and nickel, thin lines, irregular
851                                       high in iron and nickel, smooth, some holes, grey
852             black, grey, thin lines, some holes, high in iron and nickel, smooth, shiny
853                     thin lines, high in iron and nickel, irregular, shiny, grey, smooth
854                                              smooth, irregular, high in iron and nickel
855              irregular, black, high in iron and nickel, grey, smooth, thin lines, shiny
856                                                                                        
857                                                                 some holes, grey, shiny
858                                                                thin lines, grey, smooth
859                                                                              thin lines
860          some holes, black, grey, thin lines, shiny, irregular, high in iron and nickel
861                                   irregular, shiny, some holes, high in iron and nickel
862                                  smooth, grey, thin lines, irregular, shiny, some holes
863  some holes, smooth, grey, irregular, black, thin lines, high in iron and nickel, shiny
864  black, thin lines, high in iron and nickel, irregular, some holes, shiny, grey, smooth
865                                                         black, shiny, irregular, smooth
866                                                                  shiny, irregular, grey
867  thin lines, black, grey, high in iron and nickel, smooth, shiny, irregular, some holes
868                          black, some holes, high in iron and nickel, thin lines, smooth
869                                              some holes, smooth, irregular, grey, black
870                    black, thin lines, shiny, irregular, high in iron and nickel, smooth
871                                                                 high in iron and nickel
872                                                                        grey, some holes
873  black, shiny, irregular, some holes, thin lines, high in iron and nickel, smooth, grey
874                                                                                        
875                           grey, some holes, high in iron and nickel, smooth, thin lines
876                                                                              some holes
877                                                                       some holes, shiny
878                                                                 grey, irregular, smooth
879                          thin lines, shiny, smooth, high in iron and nickel, some holes
880                                                                                        
881                                         grey, high in iron and nickel, black, irregular
882                                                                       shiny, thin lines
883  some holes, high in iron and nickel, shiny, smooth, black, thin lines, grey, irregular
884  black, smooth, shiny, some holes, grey, high in iron and nickel, thin lines, irregular
885                                                                                        
886                                                   thin lines, smooth, black, some holes
887                                                                                        
888                                                                              thin lines
889                                                                        irregular, black
890                                                                              some holes
891                                        black, grey, some holes, high in iron and nickel
892  shiny, grey, some holes, thin lines, smooth, black, irregular, high in iron and nickel
893                                                         smooth, thin lines, grey, shiny
894                                                                                    grey
895                                                                                    grey
896          some holes, black, grey, irregular, shiny, high in iron and nickel, thin lines
897                                                         high in iron and nickel, smooth
898                                                                                        
899                               black, high in iron and nickel, smooth, some holes, shiny
900                           irregular, shiny, some holes, smooth, high in iron and nickel
901                             grey, shiny, irregular, high in iron and nickel, thin lines
902                                                                                        
903                                                                        black, irregular
904         some holes, smooth, black, irregular, grey, high in iron and nickel, thin lines
905        black, smooth, thin lines, irregular, some holes, high in iron and nickel, shiny
906                                                                                        
907                           black, irregular, shiny, some holes, thin lines, grey, smooth
908                           shiny, black, some holes, high in iron and nickel, thin lines
909                                                                                        
910                    smooth, thin lines, some holes, grey, shiny, high in iron and nickel
911                                                                                        
912                                              some holes, black, high in iron and nickel
913                                                      irregular, high in iron and nickel
914                                                                                        
915                                smooth, black, shiny, high in iron and nickel, irregular
916                                                                                   black
917  thin lines, some holes, black, irregular, shiny, smooth, high in iron and nickel, grey
918                                                                                   black
919                                        thin lines, some holes, irregular, black, smooth
920                     some holes, high in iron and nickel, irregular, grey, smooth, shiny
921                                                         smooth, high in iron and nickel
922                    smooth, black, high in iron and nickel, thin lines, some holes, grey
923                                                         grey, black, smooth, thin lines
924                                                                                        
925                                          grey, some holes, thin lines, irregular, black
926             thin lines, high in iron and nickel, shiny, black, some holes, grey, smooth
927                                                                       irregular, smooth
928              shiny, irregular, black, smooth, grey, thin lines, high in iron and nickel
929                                                                                        
930                                  some holes, irregular, black, grey, thin lines, smooth
931                            shiny, some holes, irregular, high in iron and nickel, black
932                                                                                        
933                                                                                        
934                                                                                        
935                                                                                        
936                                                                                        
937               shiny, high in iron and nickel, smooth, thin lines, some holes, irregular
938                                                                                        
939                shiny, thin lines, high in iron and nickel, black, irregular, some holes
940                                                                thin lines, grey, smooth
941                                                                                        
942  irregular, thin lines, some holes, smooth, black, shiny, high in iron and nickel, grey
943                 grey, some holes, shiny, irregular, high in iron and nickel, thin lines
944                                                     high in iron and nickel, thin lines
945             shiny, high in iron and nickel, thin lines, smooth, grey, black, some holes
946                                                                     smooth, grey, shiny
947  shiny, high in iron and nickel, black, thin lines, some holes, irregular, smooth, grey
948                                                                                        
949                                                                               irregular
950              black, smooth, some holes, grey, high in iron and nickel, shiny, irregular
951  thin lines, smooth, some holes, high in iron and nickel, shiny, grey, black, irregular
952                                                                                        
953                                                          grey, shiny, thin lines, black
954                          irregular, smooth, grey, high in iron and nickel, shiny, black
955  black, thin lines, shiny, high in iron and nickel, grey, some holes, smooth, irregular
956  shiny, high in iron and nickel, thin lines, some holes, black, irregular, smooth, grey
957                                                                                    grey
958                                              some holes, irregular, black, grey, smooth
959                                                                                        
960                                                                                        
961                                                                        grey, thin lines
962                                                                                  smooth
963                                                          shiny, high in iron and nickel
964                                                                                        
965                                                               smooth, thin lines, shiny
966         irregular, grey, some holes, thin lines, smooth, high in iron and nickel, shiny
967                                 black, some holes, high in iron and nickel, grey, shiny
968                                                                                        
969                                                                                    grey
970                                                    black, some holes, irregular, smooth
971  thin lines, shiny, smooth, high in iron and nickel, irregular, some holes, grey, black
972                                      grey, smooth, black, thin lines, shiny, some holes
973                    black, shiny, smooth, some holes, high in iron and nickel, irregular
974                           some holes, irregular, grey, thin lines, black, smooth, shiny
975              shiny, irregular, black, grey, smooth, high in iron and nickel, thin lines
976                                high in iron and nickel, grey, thin lines, shiny, smooth
977                                                                                        
978                                             some holes, smooth, thin lines, shiny, grey
979  shiny, black, smooth, some holes, high in iron and nickel, thin lines, irregular, grey
980                                                high in iron and nickel, irregular, grey
981                                              black, some holes, high in iron and nickel
982  high in iron and nickel, grey, black, some holes, thin lines, shiny, smooth, irregular
983                                             smooth, high in iron and nickel, thin lines
984        smooth, shiny, high in iron and nickel, irregular, thin lines, black, some holes
985                    thin lines, black, grey, some holes, smooth, high in iron and nickel
986                                                                                        
987                                                                                        
988              black, grey, irregular, thin lines, high in iron and nickel, smooth, shiny
989                 some holes, irregular, grey, thin lines, high in iron and nickel, black
990                                                                       black, thin lines
991  high in iron and nickel, thin lines, smooth, irregular, grey, some holes, black, shiny
992                                                                                        
993                           high in iron and nickel, grey, some holes, smooth, thin lines
994                                                                                        
995  irregular, some holes, black, grey, thin lines, high in iron and nickel, shiny, smooth
996                                                                                        
997              smooth, irregular, some holes, grey, high in iron and nickel, black, shiny
998  shiny, thin lines, smooth, grey, irregular, black, high in iron and nickel, some holes
999                                                                                        
1000                                                                     smooth, thin lines
               geo_coords
1      15.05000, 12.73333
2      12.73333, 50.18333
3           -99.30, 21.05
4       16.38333, 5.38333
5     -89.15000, 39.08333
6     -84.76667, 32.03333
7       35.33333, 9.50000
8       138.3833, 37.0500
9      43.00000, 52.03333
10     16.23333, 47.75000
11    139.75000, 35.73333
12    28.33333, -32.10000
13    18.56667, -33.75000
14    -98.83333, 31.83333
15         -47.55, -19.85
16     82.91667, 25.36667
17      8.16667, 52.95000
18      3.73333, 50.31667
19           79.00, 22.25
20   139.91667, -35.08333
21     31.81667, 50.95000
22                 35, 55
23                   0, 0
24    -72.97806, 45.96861
25     22.68333, 61.18333
26      32.83333, 2.71667
27     53.25000, 55.43333
28      139.3333, 35.6500
29      108.5833, 11.2500
30    126.19611, 46.24167
31     78.78333, 28.26667
32           77.00, 19.25
33     -3.08333, 48.11667
34    -97.58333, 35.95000
35     14.95000, 59.93333
36     -8.25000, 38.01667
37      6.81667, 46.55000
38          108.10, -6.95
39   -117.18913, 40.66813
40      78.35000, 9.23333
41    -96.15000, 34.31667
42               -102, 23
43     83.61667, 25.90000
44     13.00000, 43.66667
45    -90.23333, 38.70000
46     39.51667, 24.41667
47      7.70000, 52.28333
48   -64.55000, -30.86667
49    -68.75000, 44.38333
50     81.53333, 25.10000
51     94.91667, 21.20833
52     14.96667, 38.13333
53     11.52722, 45.29167
54     27.16667, 50.13333
55      0.63333, 48.76667
56      5.30000, 13.76667
57      5.13333, 44.60000
58         22.025, 47.550
59     13.35000, 43.26667
60      7.21817, 46.81867
61            0.05, 44.75
62     -5.86667, 43.40000
63    -89.60000, 43.58333
64    -87.50000, 34.58333
65           -0.85, 43.15
66                16, -18
67     83.25000, 25.68333
68    -89.71667, 32.31667
69             26.6, 39.8
70    -76.70000, 37.83333
71   -65.28333, -32.75000
72    -77.21163, 38.70066
73     35.68333, 15.36667
74     35.05000, 29.51667
75     -2.10000, 39.56667
76     75.33333, 56.75000
77     95.16667, 44.83333
78   -58.61667, -33.00000
79     26.70972, 45.27500
80            105.0, 24.1
81            115.7, 38.2
82                 NA, NA
83     25.50000, 50.83333
84            33.5, -11.5
85     93.86667, 26.68333
86     73.36667, 42.45000
87      12.43333, 7.05000
88     64.60035, 39.77978
89     68.41667, 26.75000
90     120.3667, -26.4500
91            7.15, 12.90
92      5.58333, 44.28333
93    120.66667, 32.33333
94           -0.05, 43.95
95    -94.16667, 36.05000
96     76.26667, 10.20000
97     30.35000, 31.31667
98      8.75000, 44.88333
99     10.33333, 55.33333
100    -2.04167, 14.15083
101    79.96667, 27.25000
102   -36.76667, -8.38333
103  -101.38333, 20.93333
104                34, 55
105    30.76667, 47.83333
106   145.36667, -6.03333
107  -117.66250, 33.48472
108    84.76667, 51.46667
109    14.03333, 49.66667
110   -11.34133, 17.17493
111    50.15000, 26.66667
112    42.81667, 39.91667
113   -86.05500, 40.08528
114   -87.16667, 32.53333
115    78.33333, 27.08333
116    28.96000, 13.66033
117    82.08333, 27.43333
118    11.25000, 52.21667
119           105.8, 39.7
120   -87.83333, 34.48333
121    84.83333, 24.55000
122     39.71667, 9.53333
123    -0.23333, 43.21667
124   -73.13333, 41.20000
125    -9.57028, 16.39806
126           109.5, 30.3
127   106.46667, 26.53333
128    -0.30000, 52.45972
129                75, 31
130    66.80222, 30.73111
131         -67.55, 45.95
132    10.80833, 47.52500
133    86.90000, 22.08333
134    22.30833, 48.16667
135    91.66667, 23.08333
136     8.38333, 51.91667
137            27.0, 38.5
138    84.78333, 26.75000
139     9.80000, 12.76667
140   -86.29450, 33.18836
141    31.78000, 39.26333
142    11.46745, 54.76183
143           118.8, 35.2
144   135.33333, 35.16667
145   -78.13333, 35.63333
146    -0.03333, 39.00000
147   31.65000, -19.48333
148     33.63333, 4.70000
149    -6.33333, 54.56667
150      -103.295, 34.175
151    39.43333, 56.63333
152            16.6, 47.7
153   139.21667, 36.18333
154    15.60000, 45.68333
155         -74.00, 40.25
156    82.53333, 26.80000
157    19.91667, 52.00000
158     1.38333, 43.88333
159    79.05000, 27.28333
160   104.91667, 31.91667
161     1.73333, 47.36667
162    21.26667, 52.76667
163    -6.23333, 41.36667
164     2.91667, 47.93333
165  -105.31667, 26.96667
166           115.9, 32.9
167   -90.50000, 36.06667
168    79.50083, 21.68389
169               112, 41
170    20.23333, 43.76667
171     7.31667, 50.95000
172   30.70000, -28.56667
173   -96.11667, 30.70000
174   139.95667, 36.04167
175         0.420, 14.052
176     8.65000, 49.86667
177  -51.86667, -29.20000
178   121.48333, 29.86667
179   127.26667, 33.43333
180    -1.51667, 51.65000
181   24.56667, -28.20000
182         133.95, 34.30
183    124.28333, 8.06667
184   27.58333, -29.33333
185    71.80000, 29.53333
186  -44.31667, -22.96667
187    -7.33333, 53.53333
188    16.63333, 52.86667
189    73.61667, 25.73333
190    86.70278, 20.46250
191     5.00000, 46.21667
192   115.16667, 33.13333
193         -82.26, 42.52
194    13.15000, 53.18333
195   -78.06667, 36.08333
196    26.16667, 42.51667
197  -65.23333, -31.60000
198  -66.15000, -44.11667
199                  0, 0
200          46.55, 52.55
201    76.46500, 27.72528
202    78.56667, 13.78333
203   -99.38333, 42.71667
204     111.4167, -7.4500
205     6.86667, 52.20000
206     111.5333, -7.7500
207   13.96667, -17.35000
208   33.91667, -13.66667
209          -1.10, 46.25
210     31.47278, 1.34500
211    10.08333, 11.33333
212   -80.08333, 40.50000
213    -6.21667, 54.61667
214   109.31667, 28.53333
215    26.95000, 58.03333
216  -122.96944, 44.97917
217    41.98333, 45.05000
218        70.200, 28.225
219    84.08333, 52.73333
220          10.15, 54.40
221    13.00000, 56.03333
222                NA, NA
223    39.30000, 44.51667
224   -44.33333, -3.40000
225   126.13333, 44.61667
226    30.83333, 49.86667
227     0.81667, 28.95000
228  -65.45000, -26.66667
229          -54.5, -25.5
230    72.38333, 31.30000
231   -91.86667, 41.80000
232          82.00, 26.25
233             8.5, 45.2
234     115.4167, 35.5000
235    55.98333, 53.66667
236         -84.25, 38.40
237    87.76667, 24.16667
238     122.3333, 11.7500
239  -53.05000, -29.03333
240   -79.38333, 35.41667
241  -105.23333, 26.46667
242    71.60000, 31.58333
243  -68.48944, -31.53556
244    15.50000, 51.93333
245   35.24222, -15.21222
246       -97.010, 31.805
247     33.63333, 1.70000
248   -83.95000, 44.51667
249   -81.87278, 32.10250
250   -77.50000, 37.46667
251  -64.86667, -32.33333
252        -67.50, -27.25
253    90.16667, 24.33333
254    -2.71667, 53.58333
255    17.66667, 59.85000
256         -95.95, 30.75
257  -58.16667, -31.23333
258    82.58333, 20.91667
259    75.60000, 14.88333
260  -49.93333, -26.16667
261         -85.75, 38.25
262            87.3, 23.1
263  -49.38056, -20.81000
264  -117.93333, 56.13333
265   -69.20000, 44.36667
266   109.50000, 30.80833
267         125.00, 45.25
268               57, -20
269   -98.31667, 45.41667
270    -3.51667, 39.35000
271    25.73333, 58.66667
272    77.01667, 12.63333
273     105.4833, 12.0000
274    84.08333, 27.08333
275            27.7, 61.2
276  174.40000, -39.63333
277   113.46667, 31.61667
278    81.01667, 47.21667
279   -96.46667, 33.60000
280   27.35000, -15.96667
281   137.78333, 34.71667
282     29.25000, 4.66667
283   -90.10976, 29.94718
284      87.5775, 25.8500
285    -5.01472, 23.69639
286    30.16667, 49.78333
287   135.20000, 35.08333
288   139.40000, 35.86667
289           -94.3, 38.5
290    -8.28000, 37.60833
291    25.00000, 55.66667
292  -57.95000, -31.18333
293  -120.90806, 38.80389
294    24.03333, 46.80000
295   -99.33333, 35.88333
296         116.75, 34.60
297   -11.37150, 17.71067
298   -97.03333, 39.75000
299   104.91667, 11.58333
300    16.23333, 57.81667
301    -3.80000, 41.68333
302    76.66667, 28.20000
303    -6.96667, 52.66667
304    -3.71667, 40.41667
305     8.05000, 52.28333
306    82.45000, 21.86667
307   28.36667, -25.66667
308                -7, 30
309     42.41667, 9.36667
310    72.62667, 26.96556
311           111.5, -7.5
312    74.83333, 33.71667
313    16.73333, 58.85000
314  115.67639, -33.15639
315    82.05000, 20.93694
316          -49.8, -26.1
317          25.30, 50.75
318   -73.95028, 42.86083
319   -79.95756, -1.87089
320           31.75, 9.50
321  -118.95000, 51.33333
322   -83.03333, 35.03333
323                97, 19
324    -1.16667, 37.98333
325    73.22329, 20.33916
326     2.95000, 43.43333
327    -5.15000, 43.38333
328    29.83333, -1.45000
329  -104.00000, 32.21667
330    -4.31944, 53.03639
331    35.50000, 33.88333
332                28, 14
333          88.45, 25.80
334   116.13333, 38.66667
335    12.55000, 43.03333
336    74.86667, 25.66667
337  -51.83333, -23.20000
338  -71.40000, -31.01667
339    -110.1833, 34.9000
340   -97.33333, 36.83333
341   -11.33333, 16.58333
342     8.18361, 50.66694
343    27.32997, 37.35172
344    -7.27000, 38.74028
345     0.75000, 41.63333
346    78.21667, 26.98333
347    44.45000, 35.13333
348     3.66667, 36.16667
349   106.58333, -6.66667
350  -104.58817, 39.24667
351    80.62500, 25.25417
352    11.01667, 44.65000
353   19.38333, -33.13333
354   -90.36556, 42.90750
355   -99.86667, 39.68333
356                NA, NA
357    56.08333, 54.33333
358   115.50000, 35.66667
359             0.7, 12.5
360    14.05217, 46.42137
361     9.05000, 52.53333
362           7.00, 28.25
363  150.51667, -23.38333
364    16.10000, 46.18333
365   -86.63333, 35.30000
366  -105.24139, 38.47028
367           21.9, -32.5
368     1.66667, 41.25000
369   31.31667, -27.20000
370     8.38333, 19.08333
371    146.8583, -33.3500
372  -101.28333, 20.66667
373  -59.83333, -32.36667
374    46.00000, 54.81667
375     9.58333, 10.01667
376   140.51167, 39.43833
377    -6.30000, 32.33333
378    81.46667, 26.03333
379   120.70000, 15.08333
380    14.70000, 37.28333
381    -4.38333, 13.85000
382        35.075, 50.625
383  -61.53333, -37.33333
384          12.90, 52.75
385          15.05, 53.35
386   141.86667, 43.33333
387    22.00500, 54.00883
388   -86.16667, 38.25000
389    77.80000, 29.01667
390     130.4333, 33.6000
391   -87.67917, 41.48472
392    37.03333, 52.98333
393     6.15000, 47.11667
394  -58.05000, -30.78333
395   -40.70000, -6.23333
396   -94.40000, 39.08333
397     13.98333, 9.91667
398  -117.33333, 51.16667
399    51.56667, 52.45000
400    80.81667, 25.95000
401   26.76667, -29.45000
402    82.60000, 25.38333
403          59.20, 42.25
404    12.51667, 42.51667
405    25.78333, 46.45000
406    34.76667, 47.85000
407   130.63333, 32.03333
408     12.08333, 8.96667
409   -95.21667, 38.60000
410     5.93333, 61.73333
411  151.61667, -29.46667
412          -61.7, -33.9
413     6.93333, 51.88333
414        76.525, 24.225
415        -100.45, 39.35
416    24.28333, 48.15000
417     12.43333, 7.05000
418    35.58333, -3.50000
419   -73.91667, 41.28333
420     37.66667, 0.00000
421    16.63333, 49.36667
422    -0.66667, 39.00000
423     5.36667, 47.71667
424                 48, 5
425     3.23333, 44.33333
426     8.21667, 11.85000
427   -93.66667, 43.25000
428            38.0, 48.6
429     44.16667, 1.16667
430    11.21667, 48.90000
431     112.7667, -7.7500
432    10.80000, 32.86667
433    52.48333, 54.23333
434    -5.40000, 38.18333
435    75.80000, 14.18333
436    46.66667, 39.75000
437         -99.25, 40.00
438   -90.66667, 36.48333
439     105.1833, 25.1500
440                NA, NA
441    78.63333, 52.45000
442    66.08333, 55.50000
443    30.00000, -1.21667
444    -3.95000, 50.38333
445   -92.26667, 36.21667
446    -102.1333, 24.3000
447    26.53333, 43.50000
448    76.80000, 31.61667
449    29.41822, 36.54194
450    28.83333, 51.33333
451    21.66667, 45.13333
452    -0.18333, 11.08333
453    40.90000, 54.03333
454   141.61667, 38.98333
455    21.17633, 48.76367
456    70.31333, 22.68000
457    11.00000, 60.66667
458    20.93583, 52.03333
459           27.3, -27.7
460          14.85, 50.20
461     138.3833, 36.8500
462     112.0167, -7.7500
463    -5.33333, 42.47500
464    59.31667, 52.13333
465    45.66667, 48.60000
466   -86.60000, 41.38333
467    32.50000, -3.66667
468   118.50000, 31.41667
469         -87.00, 34.75
470    90.65000, 22.68333
471     0.61667, 44.21667
472   28.41667, -26.16667
473                36, 16
474      145.600, -40.975
475    -7.01500, 31.16333
476    19.50000, 49.16667
477    20.44167, 43.83333
478    11.03333, 12.85000
479      9.91667, 9.60000
480   -81.41667, 36.10000
481             9.1, 53.4
482  142.95000, -25.73333
483     8.91667, 50.30000
484            85.4, 26.2
485    76.46667, 32.23333
486    75.50000, 29.48333
487    71.42722, 22.37778
488    76.30000, 32.08333
489  -65.10000, -23.11667
490    33.88333, 51.33333
491    77.58333, 29.58333
492  -119.75812, 38.13742
493    10.23333, 56.18333
494    -6.01667, 38.73333
495   -97.76667, 30.83333
496    81.98333, 25.48333
497        -104.90, 40.35
498        -76.510, 3.405
499            -3.3, 48.4
500   121.50000, 32.08333
501   -95.15000, 36.00833
502     125.6667, 43.5000
503         -83.75, 38.25
504   136.76667, 35.36667
505    -8.03333, 52.55000
506  -103.05167, 19.96722
507    32.48333, 30.90000
508  -69.04389, -16.66444
509  -103.66667, 20.06667
510   118.61667, 43.48333
511    78.03333, 12.66667
512            39.5, -4.0
513    83.48333, 18.68333
514          27.20, 61.65
515   106.00000, -6.33333
516   -83.85000, 39.11667
517          82.90, 26.45
518    12.93333, 42.13333
519    15.56667, 49.28333
520   120.78333, 32.91667
521           -84.1, 35.8
522    44.63333, 45.81667
523    78.50000, 28.48333
524   136.88333, 35.53333
525    44.21556, 35.27333
526    90.21667, 24.50000
527     0.48333, 47.85000
528    27.26667, 44.11667
529  -64.55000, -30.88333
530   -77.16667, 38.41667
531   -78.08333, 36.78333
532             9.1, 52.0
533                NA, NA
534           103.3, 24.8
535         -93.50, 36.75
536     6.01533, 45.82133
537    10.46667, 48.11667
538            25.8, 60.4
539    72.63333, 23.05000
540    76.33333, 27.70000
541           2.25, 42.90
542            16.3, 50.6
543    76.10000, 20.96667
544    13.78333, 55.46667
545       130.750, 33.725
546     38.41667, 9.06667
547   135.16667, 34.73333
548  -64.95000, -33.16667
549     9.40000, 58.88333
550    23.58333, 46.06667
551    -1.11667, 51.90000
552   113.56694, 31.47556
553    55.26667, 57.78333
554   122.76389, 39.80417
555    17.11667, 49.60000
556    59.68500, 41.98444
557          21.30, 47.35
558   111.20000, -7.71667
559   38.76667, -10.26667
560    29.86667, 47.21667
561             0.4, 44.3
562  -58.32833, -31.91000
563    27.13333, 38.30000
564            24.4, 55.7
565  -113.00000, 54.21667
566  -112.13333, 41.68333
567    11.65833, 11.49167
568            23.2, 53.1
569     2.81667, 44.38333
570   -90.28333, 44.90000
571          86.70, 23.05
572    -0.45000, 45.43333
573  -44.21667, -19.46667
574     34.16667, 1.06667
575    69.55000, 26.21667
576    77.95000, 29.71667
577   -87.33333, 36.16667
578                NA, NA
579   -36.66667, -5.20000
580    27.51667, 37.86667
581     0.23333, 45.30000
582    -7.66667, 54.66667
583     9.38333, 12.83333
584   111.36667, -6.73333
585    16.76667, 50.66667
586          75.15, 24.95
587          11.50, 43.35
588    83.11667, 25.55000
589    18.53333, 49.60000
590       137.305, 34.720
591    83.96667, 26.91667
592    16.33333, 46.10000
593          14.65, 49.40
594   -82.23722, 37.62194
595    33.80000, -8.93333
596    82.83333, 26.78333
597  -101.68333, 24.68333
598    84.28333, 25.93333
599    -2.51667, 52.76667
600     9.03333, 42.45000
601    -4.23333, 55.90000
602           0.08, 12.90
603    75.81667, 28.01667
604   119.86667, 32.43333
605                81, 19
606           122.9, 45.4
607    32.41275, 20.74575
608     6.08333, 50.77500
609              -66, -33
610    11.91667, 12.91667
611    82.66667, 26.71667
612    65.78333, 31.60000
613    -1.16667, 54.56667
614     119.0333, 45.5000
615    -0.08333, 12.45000
616    20.33333, 43.83889
617    37.61667, 57.35000
618    10.86667, 59.73333
619    76.01667, 20.26667
620   -84.50778, 43.76111
621    37.13333, 36.23333
622   -40.16667, -7.03333
623            8.50, 6.75
624    78.83333, 28.78333
625    76.03333, 10.83333
626         -79.25, 36.50
627           118.4, 37.1
628  -102.85833, 31.60833
629   107.60000, -6.91667
630    33.51667, -3.13333
631   -99.03333, 31.25000
632           -91.6, 41.9
633            37.5, 48.7
634    -111.3375, 53.4150
635  -63.23333, -28.93333
636     120.0667, 33.6500
637         -92.05, 35.40
638    35.98333, 48.53333
639    75.18333, 23.36667
640     3.16667, 13.05000
641     0.43333, 47.16667
642    -2.50000, 42.38333
643           3.25, 48.35
644    22.06194, 60.24556
645         107.70, -7.25
646   108.10000, 15.71667
647    10.70000, 59.43333
648    86.36667, 25.91667
649     4.30000, 44.71667
650     7.46472, 49.32694
651   133.22000, 35.56833
652     4.08333, 44.11667
653          -1.50, 46.95
654    25.51667, 43.46667
655           9.55, 36.95
656           4.00, 51.75
657    22.03333, 47.88333
658    95.78333, 27.66667
659            71.8, 32.1
660     5.16667, 44.38333
661   -85.88333, 42.53333
662          31.70, 51.15
663    79.41667, 13.63333
664                NA, NA
665          11.05, 52.75
666     101.6333, 17.9000
667    32.85000, 52.26667
668     1.38333, 43.73333
669    15.56667, 38.18333
670   35.50000, -10.66667
671   26.18333, -26.15000
672    76.03333, 21.86667
673          -101.1, 38.5
674    12.13333, 50.93333
675            77.0, 56.8
676    -0.68333, 13.50000
677    -0.83333, 38.00000
678                NA, NA
679    23.56667, 41.05000
680          141.7, -29.8
681    30.96667, 48.06667
682   -89.68333, 38.20000
683     3.25000, 34.41667
684    -2.18333, 11.65000
685   18.71667, -32.86667
686    23.50000, 44.41667
687    -112.8833, 53.9000
688   118.98333, 31.63333
689   -96.47167, 40.78167
690            10.8, 51.6
691    16.26333, 50.53806
692     161.8083, 64.0200
693   106.88333, -6.23333
694     3.00000, 50.76667
695    75.78333, 28.38333
696    67.16667, 27.80000
697           115.5, -7.0
698   -81.76667, 40.00000
699    -7.61123, 29.48195
700   102.46667, 42.91667
701   -99.90000, 16.88333
702    21.86667, 43.66667
703     8.45000, 36.88333
704    78.03333, 21.95000
705    14.88333, 51.53333
706           -86.5, 36.4
707    95.18333, 17.76667
708    17.53333, 46.23333
709   103.50000, 35.66667
710   111.75000, 29.08333
711  -100.65000, 19.76667
712     1.05000, 46.68333
713  -102.31667, 46.88333
714           -13.1, 24.3
715    -8.33333, 52.45000
716    10.71667, 34.75000
717   -92.08333, 37.91667
718            16.2, 39.8
719    -6.00000, 37.41667
720    23.50000, 46.71667
721    38.08333, 48.28333
722    84.21667, 26.71667
723            78.2, 30.4
724         140.35, 38.40
725            23.0, 58.5
726     22.66667, 2.00000
727    37.33333, 56.11667
728               151, -3
729     3.25000, 45.76667
730  -60.66667, -31.41667
731    77.88333, 26.95000
732         31.60, -17.65
733   -69.48333, 44.08333
734          33.50, 50.75
735    -6.57717, 30.18000
736    13.75333, 43.18111
737   -94.83333, 43.41667
738     136.7003, 35.2945
739     80.83333, 9.33333
740    17.56667, 49.23333
741   -81.88333, 35.03333
742    61.11667, 54.81667
743    13.13333, 48.18333
744    36.60000, 50.61667
745    85.53333, 26.13333
746           48.1, -14.2
747    34.00000, 50.83333
748    85.56667, 26.58333
749    74.83333, 20.88333
750    -4.10000, 53.01667
751    31.33333, 58.55000
752        91.000, 23.325
753    78.51667, 12.38333
754  -60.46667, -31.88333
755   -81.36222, 28.54750
756     105.9067, 32.9250
757             4.8, 18.7
758   -78.08333, 40.91667
759          10.80, 50.35
760   -12.95217, 26.59083
761    14.01500, 66.34833
762   128.25000, 43.33333
763   -94.51167, 32.67500
764           0.25, 46.60
765                NA, NA
766   -87.06667, 34.40000
767     1.96250, 43.39056
768           0.15, 44.50
769   -77.53333, 35.55000
770     1.58333, 47.63333
771   130.20000, 33.18333
772    -7.06667, 38.71667
773    -109.8482, 52.9960
774   136.30000, 35.66667
775    23.00000, 56.66667
776   101.18333, 16.66667
777    61.36667, 55.78333
778    73.98333, 17.83333
779        129.19, -31.35
780          51.50, 33.75
781         -97.37, 25.37
782    89.05000, 24.23333
783    76.05000, 22.98333
784     5.11667, 43.75000
785    72.30000, 29.53333
786  145.20000, -36.61667
787          76.80, 12.85
788    15.86667, 56.65000
789  -100.86667, 21.16667
790    -3.25000, 56.58333
791    -8.78333, 52.56667
792    45.38333, 43.66667
793   106.63241, 26.46469
794    81.66667, 25.36667
795    13.94083, 48.30250
796    -157.7833, 21.3000
797    43.73333, 19.38333
798    18.44167, 42.45833
799     1.06667, 47.70000
800    84.06667, 27.13333
801     4.63333, 46.05000
802     4.73333, 50.36667
803    11.10000, 50.58333
804     1.53333, 14.25000
805     36.53333, 0.26667
806            30.1, 53.7
807          -103.5, 25.0
808  -134.20139, 59.70444
809     6.46667, 48.18333
810     2.33333, 50.66667
811     134.6533, 46.1600
812     6.86667, 44.08333
813    77.25000, 28.56667
814    76.63333, 30.30000
815    90.33333, 23.50000
816            76.5, 14.5
817    78.95000, 13.33333
818                35, 38
819    -1.15000, 48.01667
820    29.23333, 40.20000
821    -7.46667, 41.93333
822    34.50000, 51.13333
823  -103.40000, 36.21667
824    10.05000, 44.86667
825           -91.5, 39.8
826   120.45583, 17.74333
827    87.08333, 23.98333
828     1.75000, 47.08333
829   -99.33333, 24.16667
830    -103.1167, 30.1250
831   -89.58333, 37.26667
832            22.4, 48.9
833                NA, NA
834     7.35000, 47.86667
835           112.3, 51.2
836    12.06667, 55.05000
837    -0.05000, 41.71667
838    76.00000, 29.58333
839  165.90000, -21.73333
840         -96.00, 29.45
841     1.56667, 47.93333
842    -157.8667, 21.3000
843    71.46667, 20.83333
844    72.08333, 34.23333
845     4.98333, 22.83333
846    12.61667, 48.13333
847    77.33333, 26.83333
848     131.5667, 34.2000
849         -80.50, 35.25
850    21.20000, 64.43333
851    18.71667, 49.26667
852   136.23333, 35.43333
853   115.83333, 33.83333
854    76.86667, 20.88333
855   121.80000, 32.11667
856   -89.00417, 40.48000
857     131.6333, 44.5500
858                22, 56
859          -101.2, 39.8
860            21.5, 56.5
861   26.46667, -31.25000
862    62.08333, 45.21667
863   27.41667, -32.01667
864    11.50222, 44.24417
865    68.53333, 27.88333
866     2.16667, 41.36667
867    -1.78333, 51.78333
868    11.28333, 44.76667
869   140.06167, 38.12167
870    -1.43333, 49.03333
871           3.75, 51.05
872    29.43333, -7.21667
873    29.50000, -6.83333
874    78.38333, 29.68333
875   -96.85000, 47.81667
876     7.53333, 49.98333
877    -1.33972, 52.56528
878   136.46528, 36.44917
879   -70.75000, 44.61667
880   136.93333, 35.07833
881                NA, NA
882  -49.93333, -22.25000
883           -84.1, 10.0
884    10.15000, 45.26667
885     9.51667, 45.48333
886    14.11667, 50.53333
887            44.5, 40.3
888  -105.02325, 40.30583
889           -93.5, 35.5
890  -48.56667, -19.53333
891              -45, -20
892     8.83333, 35.61667
893               -80, 43
894           -88.1, 36.6
895   111.53333, -7.08333
896     3.93333, 48.45000
897    45.00000, 41.11667
898    12.61667, 42.53333
899   100.08333, 13.73333
900  -100.81667, 19.86667
901    -3.54217, 11.33367
902   107.33333, -7.23333
903            -8.0, 38.5
904   133.16667, 44.86667
905     8.30000, 45.08333
906    -1.16667, 38.11667
907    75.33333, 18.75000
908   -83.51667, 31.95000
909    36.13583, 39.82467
910   -79.61667, 43.20000
911   134.90000, 35.38333
912             8.0, 48.5
913          -108.0, 36.8
914   -88.66667, 34.50000
915    11.60000, 43.11667
916     5.38333, 43.86667
917    37.66667, 47.83333
918   -82.48333, 35.96667
919    82.71667, 26.78333
920   -86.28333, 41.08333
921           127.5, 35.0
922    -0.86667, 48.53333
923    83.48333, 25.85000
924    30.78333, 51.68333
925   31.95000, -28.83333
926    82.93333, 58.20000
927    75.33333, 55.20000
928    91.18333, 23.15000
929    78.56667, 24.45000
930   140.78556, 40.81056
931    83.66667, 21.25000
932    37.15028, -1.00278
933  -49.95083, -21.46028
934    68.53333, 27.35000
935    75.81333, 25.14333
936   15.83333, -11.85000
937  -51.66667, -19.13333
938   -83.96667, 33.01667
939                NA, NA
940    24.68333, 11.63333
941    35.66667, 27.33333
942   15.83333, -17.30000
943      8.43333, 8.91667
944           0.55, 42.30
945           111.7, 34.8
946  -103.66667, 42.58333
947    35.86667, 55.46667
948    73.94167, 26.03472
949   -80.38333, 43.38333
950            13.5, 46.0
951   -96.91667, 43.20000
952   118.99000, 32.94667
953   -76.38333, 38.16667
954            30.5, 61.5
955    77.03333, 52.30000
956          -3.65, 11.65
957    81.89167, 19.03333
958          -8.15, 32.25
959     0.97667, 47.47083
960   33.39500, -10.05944
961    13.03333, 56.21667
962     30.83333, 2.88333
963            35.2, 54.5
964    26.43333, 56.00000
965    -104.9306, 39.7825
966            70.6, 33.4
967    -5.90000, 15.78333
968    30.58333, 46.35000
969    13.56667, 37.31667
970    -0.55000, 47.46667
971            24.7, 42.9
972    150.6889, -34.0500
973     5.41667, 36.18333
974    17.98333, 52.20000
975     126.1667, 44.0500
976    10.41667, 32.95000
977    25.73333, 46.50000
978            48.3, 15.0
979             9.2, 39.3
980  152.83333, -31.66667
981   -79.73333, 40.93333
982   -73.83333, 42.53333
983    79.20000, 12.28333
984     132.3833, 34.4500
985    -3.43333, 56.40000
986   28.70000, -30.11667
987                29, 40
988    32.43333, -8.41667
989   130.20000, 33.28333
990     0.58333, 43.08333
991    72.01667, 25.38333
992   -80.16667, 44.05000
993     1.86667, 45.36667
994           0.30, 52.05
995                 1, 12
996   -84.35000, 36.83333
997     9.25000, 51.66667
998    73.11528, 26.50833
999    78.25000, 27.66667
1000  -80.28333, 34.16667
```
-->

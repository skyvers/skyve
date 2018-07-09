package org.skyve.impl.util;

import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.skyve.impl.util.ArticleNode.Data;
import org.skyve.util.Util;

/**
 * Pluarlise function converted and enhanced Java implementation of
 * https://github.com/rhroyston/pluralizer-js/blob/master/pluralizer.js.
 * License: MIT
 * 
 * Article function converted Java implementation of https://github.com/EamonNerbonne/a-vs-an.
 * Licence: Apache 2.0
 */
public class PluralUtil {

	final static String LOWERCASE_PATTERN = "\\b[a-z'\\-]+\\b";
	final static String TITLECASE_PATTERN = "\\b[A-Z][a-z'\\-]+\\b";
	final static String UPPERCASE_PATTERN = "\\b[A-Z'\\-]+\\b";

	private static final String ARTICLE_DICTIONARY = "p3ezz;4wrlg;2h;#2rg;22;2;a;7;;if;z;;&4h;1c;1;N;6;;*yp;6a;4;a2;q;;e1;q;;i1;h;;o;7;;/op;5n;9;a3;i;;e5;h;;h;;1;o5;;;i;r;;l;;1;/;6;;n;;1;o6;;;o1;a;;r;;1;e7;;;s;;1;/2;j;;09pa;y3;1;8e;10;;17qoq;qmm;2;12hp;7nw;a;0o4;45;1;0n;2w;;15r;1n;2;8;7;;9;5;;28s;x;;34q;z;1;7;5;;45n;n;;598;w;;65k;u;;74j;y;;850;y;;93x;g;;81ux;hgk;a;0zb;el;a;0p;4h;;11;16;;21;10;;32;15;;4;18;;54;v;;6;12;;7;s;;8;v;;9;17;;11k;bq;1; v;2;;229;f3;2; 1a;3;;–5;;;31x;jc;1; 12;2;;41w;kq;2; z;4;;–5;;;559;sp;5; 2k;1;;,h;;;h7;;;kd;;;m6;;;62p;rm;2; 1b;5;;k5;;;72n;y6;2; 14;;;–5;;;82r;192;2; 19;4;;,5;;;93i;1ig;2; 10;4;;–5;;;8ys;nsu;;<e9;1x;2;m;;1;d;6;;o;6;;=kq;1x;1;=3m;g;1;E;6;;@16;2a;;A3wn;bu7d;6;Abr;y1;1;A8b;5o;1;S3;e;;i2g;4rh;1;r26;4dg;1;ob;2;;mg9;5490;1;a1k;140;1;rv;1e;1; t;1;;n6n;gfl;1;d25;1ae;1;a1c;5b;1;l1b;4c;1;u1b;3r;1;c18;c;1;i;;1;a;;1;n;6;;s3f;feu;1;tn;sc;1;uh;1t;1;rh;1s;1;ih;1q;1;ah;1q;1;sg;1;;t71;41h;1;h53;112;1;l4x;7i;1;e4w;6m;1;t4w;6m;1;e4w;2j;1;s;f;;B7cel;sz;2;hlb;42;2;aaw;28;1;ij;1y;1;rf;1s;1;ae;;;á;11;;olhj;28;1;r1cy;j;1;des;f;1; 3;f;;C8l05;10j;3;a29ng;5t;1;i9c;b;1;s;;1;s5;;;h1ska;9p;4;aa1a;1x;2;i115;o;1;s5;g;1;e5;;;o5p;7;1;i;6;;l17;l;1;á;f;;rkxk;19;1;o9t;a;1;í;5;;u2re;y;2;i1;a;;r2fq;e;1;re;5;1;a;5;;o1vrk;88;1;njpy;1m;1;g380;f;1;j;5;;D40dx;hh;7;aj56;1y;1;o35;6;1;q;5;;ijhc;2t;2;nbw;j;1;a17;f;1;s4;f;;ogz;g;1;t1;9;;uivv;1r;1;ad2;7;1;lci;7;1;c;7;;á30;7;1; ;5;;ò;5;;ù1;e;;ư;7;;Efoe;42zy;7;U3t;zk;1;R1r;4;;be;42;1;oc;1t;1;lb;p;1;ib;;;m2k;73m;1;pw;13w;1;eh;91;1;zd;;;n7x;27zz;1;af;14;1;md;3;;syp;35x;1;pws;9c;5;awn;p;1;c;7;;e4;7d;;h;5;;o;b;;r;h;;udjl;hc;1;lj;6o;1;o6;;;wr;1e;1;id;2;;F4eta;hhx;11; cf;1rz;;,;17;;.2j;6h;;/i;29;;05;2y;;128;e1;;216;9g;;311;7d;;418;8i;;5i;5u;;69;z;;7;;1;0a;;;86;j;;97;l;;A404;5ij;e;Be;3;;Ct9;my;;Dx;9;;Fc;;;I;;2;L8;;;Rq;;;K11;;;Ll;4;;M1x;1;;Ny;2;;P13;2;;Q9h;5j;;Rad;3p;;S1n;9;;T2t;2;;B57;1ji;;C4s;jp;1;Mb;1;;D2g;ak;;E7r;39;2;C6;k;;I5;f;;F33;64;1;r5;;;Hz;1u;;I1vx;bk;3;A24;45;1;Tm;1;;R;;1; w;30;;U2;a;;J5;f;;Lkk;g1;3;C2p;3v;;N4;e;;P1;f;;M6w;1ds;;Og3;2e;1; 5;i;;Ps4;w7;1;.7i;53;;R8v;3z;2;A1e;g;1; 3;e;;Se;y;;S3g;91;;T8a;1ah;1;S1x;6;;Uel;3m;2;,2;b;;.;k;;Xm;1o;;Yz;5w;1;V6;;;c3;l;;f;;1;o7;;;h1;31;;σ1;i;;G55z0;ig;7;e1sct;3h;1;r1fvh;1n;1;n;;1;e;a;;h1sd;31;1;a17k;2e;3;e;q;;i;n;;o;6;;i4b9;s;1;alt;c;1;nl0;b;1;g1;a;;ovi2;2d;2;f;;1; ;b;;t238;f;1;t2z;7;1; ;6;;r17jv;32;1;iql;e;1;a;9;;w43;6;1;a1p;5;1;rd;5;1;r;5;;ú;5;;H24sn;cwd;s; 5i;mk;;&b;17;;,1;f;;.37;42;2;Ai;;;Ie;4;;1i;1j;;2r;22;;39;n;;5a;19;;72;a;;B2z;cx;1;P7;;;C1q;35;;Dd1;tb;;F1l;2x;;Gl;14;;Hy;30;1;I5;;;I80;i9;6;Cc;1;;G10;2;;Nb;1;;Pq;2;;S;;1;D1;j;;Tf;1;;K4k;1w;1;9;7;;L1e;35;;M36;6u;1;.c;;;Ny;2v;;O95;3g;2; 7;t;;Vc;16;;P4b;bf;;R2v;4v;1;Tn;7;;S;;1;F5;;;T9c;115;;V1k;3u;;eaox;7p;2;im1;11;1;r4;o;;r29t;2m;1;r1d;7;1;n;5;;ohh1;6d0;2;n3jr;65v;6;d121;v;;e4c;1m;1;s7;1m;;g21f;e;;kp;;;oad;61b;2;l26;1;;r7c;5jz;1;i;;1;f;5;;v6;;;u2q4;32;1;r2;2x;;I266;4mgb;3;I4o;8f;2;*6;;;I1v;j;;nij;1hm4;1;d9p;wtp;1;e1c;3wy;1;p12;3r6;1;e12;3r6;1;n12;3r4;1;d12;3r4;2;et;3qw;1;nt;3qw;1;c7;41;1;i6;;;ê8;;;s4h;cif;1;l2i;42z;1;a2h;3yr;1;ne;gy;1;de;gy;1;sd;3;;J24b6;7h;1;iw9;t;1;a;;1;o2;o;;L2u98;abe;y; 5s;17t;;,2;s;;.25;9q;;1l;1t;;2j;2h;;3a;10;;48;l;;8;c;;Anc;ce;3; 1e;6q;;Po;2t;;X1;a;;B1j;2r;;C59;r4;;D42;lc;;Ejg;hg;2; 2;i;;D4x;e9;;F20;3s;1;Ti;2;;G8s;ko;;H;;1;Dk;6;;Idy;1m;1;Rd;u;;Lc0;25k;;M3k;94;;N26;51;;P63;1g0;;R23;6z;;S2z;f6;1;m7;;;T36;86;;U1s;n;1; 5;h;;V14;35;1;C7;;;W;;1;T;5;;Z9;t;;^;5;;am5d;3g;1;o91;e;1;i;;1;g;e;;oln2;3r;1;c2ik;19;1;h1s;11;1;a7;z;1;b6;;;p1;h;;u51i;1q;1;s4p;x;1;hg;w;1;ob;;;ộ;5;;M5bhu;skd;15; 6n;n2;;&b;1h;;,;g;;.1kr;5er;1;Alm;2kf;1;.kf;2ht;1;Sa;2;;/4;1f;;14f;f4;;22a;6k;;3y;31;;41j;5h;;5n;1a;;6r;3q;;7t;1f;;87;11;;A1fx;2mt;i;B5;;;C9q;1;;D2d;3;;F12;4;;G17;4;;J38;;;L11;2;;Mv;1;;N2i;a;;P2f;6;;R2o;;;S2t;6;;T2l;f;;Vc;2;;W7;;;X1h;;;Ya;;;Z7;;;Bbv;39s;1;Sb;r;1;T9;;;C5e;ni;;D5e;na;;Efj;d4;3;P1j;9f;1;S7;;;W;7;;n6;i;;F64;14y;;G31;az;;H2i;7d;;Izq;ij;4; f;w;;5;2o;;6f;35;;T25;7y;1;Mi;1;;K1o;3c;;Lcl;17r;;Mcc;rp;;N1i;40;;Osl;av;1;U17;2c;;Pel;3xz;;Qa;1g;;R4l;nw;;Sku;1ld;;Tai;yj;1;R1y;w;;Uas;1t;2; 2;i;;V3;d;;V3s;a5;;Xq;2p;;b1a;i;1;ur;d;1;m1;a;;f2q;ek;;hf;1g;;om4z;7h;2;D;;1;e7;;;U2b;34;1;.5;;;p;;1;31;b;;s23;h;1;c3;c;;N3qyx;ipi;y; 7p;qe;;+;9;;.;;1;Eg;5;;46;k;;7;7;;8;7;;:;5;;A4ga;in;3;A3g;6j;1;Fo;2;;I1m;52;;S2l3;1x;1;Lb;1b;;B8k;2gd;;Ckh;2ib;;D2t;aq;;Ejp;9r;3;A10;2r;;H5;16;;S;;1; 1y;2v;;Fas;2sm;1;Uc;2;;G5x;zb;;H8o;1oq;;I;;7;C1w;j;;D;;1; ;9;;Ka;;;Ld;2;;Nj;;;O8;;;S20;n;;K1c;4g;;L2t;94;;M1s;9a;1;M5;;;Ndl;36;2;R1;d;;T4;h;;P;;1;O25k;1j4;1;V24y;1i8;1;/;;1;B;5;;R9n;f6;2;Je;3;;T3y;e;;Sby;lt;1;W5m;g;;T3m;9h;1;L;;1;D5;;;U60;1a;1;S4;i;;V1a;4h;;W64;4n;2;Au;30;;O;;1;H;8;;Xp;4p;;Ys5;af;3;C64;1i;1;L;6;;P1k;3t;;Uj;1p;;a10pz;8f;1;sq1;d;1;a;;1;n1;8;;t;;1;h1;a;;vk;1c;;²;7;;×;5;;O1n9;1g7f;5;N17;3l;1;E12;2;;l29;8yb;1;vl;2;;nuh;1ti;2;c;;1;ec;2;;etc;21;1;i2;1k;;oi;3e;1;pf;1;;u3x;1ey;1;i3c;b;;P5dsg;kg;1;hmb6;3r;2;i39j;v;1;a;9;;o128;1s;2;bh;19;2;i8;;;o9;;;i1;j;;Qb3c;1l;1;i8c;t;1;n61;l;1;g4x;l;1;x1;f;;R3te6;u0n;z; 6h;119;;&8t;ze;;,;k;;.2r;4t;1;Jj;3;;/a;q;;11s;3k;;4a;r;;67;j;;A12c;ym;3; d;1o;;.1;a;;F7v;ra;1;Tf;;;B4b;v4;1;Rk;3;;C52;l7;;D34;9b;;Enp;3a;3; b;q;;S3z;q;1;Pa;p;;U;5;;Fxl;5cg;;G11;5w;;H1x;1a;1;S4;f;;If2;6o;2;A17;3s;;C;;1; a;1c;;Jn;2e;;Kz;5g;;L1h;2k;1;P6;;;M5u;cg;5;1e;4;;26;;;36;;;59;;;65;;;N4y;q3;;Ogs;86;2;Hk;16;;T1n;3b;2;Av;;;O7;;;P4u;q7;;Q8;q;;R21;37;1;F6;;;S1pl;1up;4;,85;6b;;.ba;7l;;?2l;y;;Tv;6;;T5t;iz;;U23;4u;4;B5;;;M5;;;Nb;;;S9;1;;V1h;8u;;Xa;o;;c;8;;f3qh;db8;1;M;;1;.h;4;;h1xk;1a;2;A;5;;B;5;;ò;5;;S7ldf;ea7;11; 9n;18a;;&1o;3s;;,2;f;;.e8;91;4;E7;m;;Mc;10;;Oa;18;;T;;1;.;;3;B2;b;;D3;c;;M2;c;;1m;29;;35;o;;4b;s;;55;i;;68;s;;84;i;;92;b;;A1b1;l3;3; 2a;7d;;8;5;;S2v;3t;2;Ep;4;;I6;;;B30;74;;Clf;a7;3;A2z;15;1; 3;y;;R;;2;Ap;;;U5;;;T6;s;;D66;ns;;Eri;jq;6; s;1g;;5;7;;C3g;ba;3;Ac;;;Oh;;;Rh;3;;I;;1;F9;;;O1a;28;;U;5;;F58;7w;3;&5;;;H6;;;O;;1;S8;;;G24;5v;1;Df;5;;H6m;3t;3;25;h;;32;g;;L;7;;It0;83;2; f;50;;S;;1;O6;;;K;;2;I9;1;;Yh;;;L6z;8m;3;A2k;o;2; 1;g;;.;5;;Iu;e;1; 3;c;;Om;5;;M9j;lh;3;A28;o;1; 4;m;;Ii;5;;U;;1;R6;;;N;;5;A1e;b;;C;;3;A8;;;B5;;;C8;1;;E18;o;;Fa;;;O4g;1;;Oi7;dc;5; c;u;;A4c;1h;1;I5;g;;Eg;1m;;Sv;5m;1;U5;;;V9;p;;P1r8;320;7;A102;1dn;7;Aa;;;Ch;;;D22;;;Mr;;;Ne;;;R2f;;;S8;1;;E3k;j;1; 4;g;;Ib9;155;4;Co;;;D6;;;N5;;;R5;;;L13;26;1;O6;;;O18;e;;U1x;5;;Y5;;;Q;;1; 1;e;;R3i;9z;;Sf7;1kv;1;We;;;Tjg;e7;9;A46;w;1; 8;s;;C7;r;;Dn;2z;;F4;n;;Iq;1c;;L6;k;;Mm;15;;Sb;12;;V5;12;;U;;7;Bn;1;;L42;28;;Mj;;;N18;;;P15;2;;Rf;;;Sf;;;V6j;y7;;Whe;2v;1;R5;m;;X;;1;M2;c;;ha9m;14;1;i2m7;i;1;g;;1;a;;1;o;7;;pxbl;3u;1;i1lg;s;1;o6;j;1;n6;;;tpmv;33;2;B;5;;ikz;b;1;c5b;a;1;i;8;;T3dav;if;4;aczy;4f;2;i1wj;15;2;g;;1;a6;;;s;;1;c;8;;v1q;q;1;e17;h;1;s;h;;ega6;29;1;a1o2;o;1;mg2;6;1;p;6;;uc4t;22;1;i;;2;l1;8;;r;8;;à1;n;;U2oh8;a1j;m;.jqk;80;1;N78;h;1;.70;h;1;C;f;;/2r;f;1;1;9;;1;;1; c;;;24b;x;1;17;k;;L3q;j;1;T;6;;M7h;1d;1;N8;q;;N4bp;am;4;C2u;k;1;L;5;;D;;2; 6;;;P14;2;;K;5;;R;;1; 5;;;a3;40;;b2r;43;2;am;4;;is;9;;de;16;;glp;3a;1;l;1n;;i;;1;gq;a;;l2u;14r;2;ig;4;;yc;;;my;8n;;ntrc;2uh;o; 5;k;;a;;1;n1e;b;;b3;1q;;ce;9w;;d1u;x2;;e19;1t;1;s16;2;;f3;2d;;g;l;;h6;l;;itj3;kb;2;d3;e;;n2;65;;j;7;;k3;55;;l3;2c;;m5;1e;;n2;t;;o4;13;;p2;1p;;q;5;;r6;71;;s5;2u;;t11;4a;;u6;v;;v1;a;;w1;10;;p12;ze;;r1ap;1rs;3;a4h;k;;ea;;;uz5;e2;1;k1;d;;s1at;41;3; ;7;;h9;t;;tw;1r;1;rp;1;;tla;49;4;h2;a;;n1;n;;ra;u;;t7;j;;x2;j;;z2d;dy;;í2;e;;W3fzh;1xp;2;Pgh8;1kw;1;:dtp;1jr;4;A;;1;Fp;1b;;I;;2;B1;9;;N;;1;H8;1;;Oe2;7b;1;V;a;;R1n7;cv;1;F;;1;C;;2;N5;;;U1b;e;;i150z;6l;1;kucb;5n;1;iu0s;5l;1;El;8;1;l;;1;i1;8;;X18k;5ru;7;a2o;6;;e7o;5;;h22;;;i8v;c;;om;4;;ux;4;;yt;4;;Ydr1;5s;3;e1xq;v;1;t35;a;1;h2;a;;p3;d;;s1;8;;[1f3;b3;5;A6;m;;E;b;;I1;a;;ai;13;;ed;30;;_1pde;5f3;2;_1pa2;5em;1;i;6;;e;5;;`4j;19;3;a1;g;;e;5;;i;7;;aa4e;1k1cr;7; 2e6;1r4;;m6h;1bmc;1;a2k;gdk;1;r;;1; 9;;;n1yq;4tcz;1;dsz;136;2; sm;69;;ě5;;;p8x;2tn1;1;rj;e8;1;t6;;;r1ql;dm6d;1;t1bb;7rrp;1;í6;;;uat;271y;1;sc;wm;1;s6;l;1;i;;1; 5;;;v31;2qsv;1;u5;21;1;t5;;;d14132;4sp;3;a4bh7;3i;1;s1q3;1j;1; ;1h;;edez2;3tl;2;mosa;1n;1; 2;10;;r9oc;3b1;1; 1;3an;;idsd0;ky;2;cfg1;k;1;hdj;d;1; ;c;;e57k;6w;1;s1rl;l;1;e1pp;k;2;m;5;;r;5;;e7wn;zlrz;8;c2u;mo2;1;o1z;egy;1;n1g;bbv;1;o1e;bbg;1;m1e;bbc;1;i1b;9c8;1;a6;;;ee;15d;1;w8;;;lbr;2yiq;1;it;5bi;1;tj;3qh;1;eg;3hs;1;i5;;;m8g;1t5c;1;p2z;oie;1;ed;v6;1;zd;;;nyp;6b4j;1;t7j;1g2s;1;e16;5d4;1;n5;r;1;d5;;;s96;1ypr;2;a;;1; 5;;;t45;1716;4;a2g;c3l;1; d;;;e8;i0;1; 6;;;il;tl8;1;ml;tl5;1;al;tl4;1;r5;;;rd;bo;1;u;;1;t6;;;u2qo;74;1;p11a;2j;1; ;i;;w2e;o;;f1n9ht;1ct;3; 1b;44;;M7;2j;;y1l;7;1;i;7;;hpp6o;1t04;9; 37;kq;;,1;9;;.;5;;C2;1d;;a4tpe;la;1;u1je;1r;1;tn;1j;1;b5;;;e3q3g;3wn;2;iajd;24b;1;r1z;232;1;a15;1;;rjaj;1ep;1;m1uj;14;1;a83;e;1;n;5;;o63t5;1eb3;4;mymg;1rd;2;a16t;1mn;1; 6;;;mm;1g;;n1aw;owo;8; a;1;;b8;;;df;;;ez3;32r;3; 6;;;de;;;yvn;1;;ga;;;if;1;;k4e;;;vd;;;ro27;7b;1;sc8c;1v;1; 8;15;;u1lyy;mvr;1;r26;mus;;ry;7;1; ;5;;t2t;51;1;t1e;24;1;p1e;24;1;:u;f;;i550;t5c8;6;bc;3a;1;n7;;;e9;l;1;r7;;;n2x3;jmka;2;fbo;1jnw;2;l1s;fl3;1;u17;cuh;1;ê9;;;o6p;gaj;1;ad;1;;s84;1pv1;1;t3y;whk;1;a17;9gq;1;ld;21c;1;a5;;;r2j;ekg;1; 8;1;;sqz;1agk;1; jd;4q;;u16;8;;k3v63;5f;1;urs;n;1; 3;n;;l1ewzo;zp;3;cj;7;1;d;6;;sl;5;1; ;5;;vm;8;1;a;8;;m1ql98;243;a; 1b;93;;,;7;;Rs;3e;;b;;1;a5;;;egur2;9r;1;i2c;d;1;n3;d;;f5;g;;pz;6b;;t;;1;aa;;;u5ql8;5t;1;l193t;2t;1;t17us;2t;1;i17ty;2t;1;c1d4;8;1;a4z;6;1;mo;6;1;p;;1;i;6;;×;a;;n14seq;3b7;6; 9r;1ei;;V7;w;;W4;q;;dx;1n;1; e;;;te;u;;×1;b;;o1po1;gmse;8;c2l;ewy;2;hb;4w;1;oa;1;;u7;6u;1;p7;;;d21;6ii;1;d1o;59e;1;ya;;;fkz;2cg9;1; br;51;;gp;fd;1;ge;11;1;ia;;;n1muy;151v;o; 4a;6y6;;';5;;,;8;;/1;3t;;a1;e;;b;db;;c1qc;se;1;o3;rn;;d1;n;;e1kqx;bu;2;i;b;;r6;3a;;f;9;;g1t;adm;;i3;ca;;l6q;jw7;;m4;14;;n;c;;o2;57;;r;1a;;s6;10f;;t6;hb;;u1;1l;;w1;1k;;y;15;;z;7;;–;6;;rht;2sas;1;i30;gwn;1;g2p;fl4;1;e6;;;t37;50i;1;r;;1;s;8;;u7j;1e6t;1;i16;6;;r1fxi6;1u1;3; 14;6g;;&1;8;;f12;52;;s3aro5;3io;9;,;e;;?;5;;R2;h;;f;;2;e8;;;o8;;;iggn8;b9;1;c1vp;18;1;hb;17;;l20u4;1m;1; ;5;;pbado;de;2;33;j;;ipic;12;1; ;5;;r1t;k;1;g;c;;v;;3;ae;;;e11;;;i6;;;t1c1p7;1mt;2;A1;9;;S3;2u;;u4dhq;8rnc;c; 7x;n;;.9;;;beh;3f;2;e7;1m;;l;5;;fl;6;;k6h;2e;2;ae;11;;i8;n;;m1j;4kj;1;a;;1; f;5;;n2eo5;6zny;3;a3ln;7j0;3; 1r;4;;n3dz;in;5;a2;c;;c;8;;n5;a7;;s;37;;t;3h;;r4a;jh;1;y46;4;;e28;a8m;2; m;;;i7;;;i29p3;wfu;8;c14a;17;2;oeg;i;1;rcl;h;1;p;a;;u;;1;m;6;;d4x;2b9;1;i4d;f;1;o;7;;g;;1;n;5;;lsl;17;1;l;i;;m2i;r9;1;o2a;2;;n3y;ssp;;s9j;m;1;s;f;;vhg5;5x;1;oj;17;1;cd;3;;r1cb;bd4;5;aco;r;;e3v;h;;ih7;i;;l7m;1j;;o47;k;;s1szw;1ig;3; h;3f;;h1;9f;;u1bh;iv;1;rda;gs;2;eq;a;;pcd;gg;3;a3b;1y;;e7q;36;;ig;;;t33j;1ai;2;m;r;;ta;16b;;v3c;c;;w3e;d;1;a;7;;x;;b; 23;hw;;';6;;,1;c;;.2;a;;8o;47;;b5;f;;k;13;;l1;8;;md;x;1;a7;;;t;c;;y4y;e;1; ;a;;y6atn;ca;1;l1;c;;{6s;r;1;a;7;;|58;m;1;a1;b;;£6cs;8l;1;82b;6o;;À;9;;Áa;o;;Ä3;c;;Å2;k;;Æ4;k;;Éf;2q;;Ò;c;;Ó1;1d;;Öa;16;;Ü4;v;;à7;v;;á5;l;;æ1;m;;è;6;;é4c;bg;1;t3h;29;3;a4;1n;;o;a;;u1;b;;í;5;;ö2;e;;üd;r;1; 7;;;Ā1;b;;ā3;s;;ī;5;;İ4;o;;Ō3;o;;ō;2m;;œ;8;;Ω2;e;;α1c;c1;;ε5;1s;;ω7;1x;;ϵ;8;;е;5;;–3a;1k;2;e;a;;i;9;;ℓ;d;;";
	private static ArticleNode articleRoot = null;

	private static final String[][] aExceptions = new String[][] { { "agenda", "agendas" },
			{ "alfalfa", "alfalfas" },
			{ "aurora", "auroras" },
			{ "banana", "bananas" },
			{ "barracuda", "barracudas" },
			{ "cornea", "corneas" },
			{ "nova", "novas" },
			{ "phobia", "phobias" } };
	private static final String[][] exExceptions = new String[][] {
			{ "annex", "annexes" },
			{ "complex", "complexes" },
			{ "duplex", "duplexes" },
			{ "hex", "hexes" },
			{ "index", "indices" } };
	private static final String[][] fExceptions = new String[][] { { "belief", "beliefs" },
			{ "chef", "chefs" },
			{ "chief", "chiefs" },
			{ "dwarf", "dwarfs" },
			{ "grief", "griefs" },
			{ "gulf", "gulfs" },
			{ "handkerchief", "handkerchiefs" },
			{ "kerchief", "kerchiefs" },
			{ "mischief", "mischiefs" },
			{ "muff", "muffs" },
			{ "oaf", "oafs" },
			{ "proof", "proofs" },
			{ "roof", "roofs" },
			{ "safe", "safes" },
			{ "turf", "turfs" } };
	private static final String[][] feExceptions = new String[][] { { "safe", "safes" } };
	private static String[][] ffExceptions = new String[][] { { "staff", "staff" } };
	private static final String[][] oExceptions = new String[][] { { "albino", "albinos" },
			{ "armadillo", "armadillos" },
			{ "auto", "autos" },
			{ "cameo", "cameos" },
			{ "cello", "cellos" },
			{ "combo", "combos" },
			{ "duo", "duos" },
			{ "ego", "egos" },
			{ "folio", "folios" },
			{ "halo", "halos" },
			{ "inferno", "infernos" },
			{ "lasso", "lassos" },
			{ "memento", "mementos" },
			{ "memo", "memos" },
			{ "piano", "pianos" },
			{ "photo", "photos" },
			{ "portfolio", "portfolios" },
			{ "pro", "pros" },
			{ "silo", "silos" },
			{ "solo", "solos" },
			{ "stereo", "stereos" },
			{ "studio", "studios" },
			{ "taco", "tacos" },
			{ "tattoo", "tattoos" },
			{ "tuxedo", "tuxedos" },
			{ "typo", "typos" },
			{ "veto", "vetoes" },
			{ "video", "videos" },
			{ "yo", "yos" },
			{ "zoo", "zoos" } };
	private static final String[][] onExceptions = new String[][] {
			{ "balloon", "balloons" },
			{ "carton", "cartons" },
			{ "formation", "formations" } };
	private static final String[][] umExceptions = new String[][] { { "album", "albums" }, { "stadium", "stadiums" } };
	private static final String[][] usExceptions = new String[][] {
			{ "abacus", "abacuses" },
			{ "bus", "buses" },
			{ "crocus", "crocuses" },
			{ "genus", "genera" },
			{ "octopus", "octopuses" },
			{ "rhombus", "rhombuses" },
			{ "walrus", "walruses" } };
	private static final String[][] xExceptions = new String[][] { { "fez", "fezzes" }, { "gas", "gasses" }, { "ox", "oxen" } };

	private static final String[][] irregular = new String[][] {
			{ "child", "children" },
			{ "die", "dice" },
			{ "foot", "feet" },
			{ "goose", "geese" },
			{ "louse", "lice" },
			{ "man", "men" },
			{ "mouse", "mice" },
			{ "ox", "oxen" },
			{ "person", "people" },
			{ "that", "those" },
			{ "this", "these" },
			{ "tooth", "teeth" },
			{ "woman", "women" } };
	private static final String[] onlyPlurals = new String[] { "barracks",
			"bellows",
			"cattle",
			"congratulations",
			"deer",
			"dregs",
			"eyeglasses",
			"gallows",
			"headquarters",
			"mathematics",
			"means",
			"measles",
			"mumps",
			"news",
			"oats",
			"pants",
			"pliers",
			"pajamas",
			"scissors",
			"series",
			"shears",
			"shorts",
			"species",
			"tongs",
			"tweezers",
			"vespers" };
	private static final String[] unchanging = new String[] { "advice",
			"aircraft",
			"bison",
			"corn",
			"deer",
			"equipment",
			"evidence",
			"fish",
			"gold",
			"information",
			"jewelry",
			"kin",
			"legislation",
			"luck",
			"luggage",
			"moose",
			"music",
			"oil",
			"offspring",
			"sheep",
			"silver",
			"steel",
			"swine",
			"trousers",
			"trout",
			"wheat" };

	private PluralUtil() {
		// no-op
	}

	/**
	 * Attempts to return the English article (<em>a</em> or <em>an</em>)for the provided word based
	 * on the frequency of the article appearing the English Wikipedia corpus before the specified word.
	 * 
	 * @param word The word to query the corect article for, e.g. honest
	 * @return The article, either <em>a</em> or <em>an</em>
	 */
	public static String article(String word) {
		Data result = articleResults(word);
		if (result != null) {
			return result.getArticle();
		}
		return "a";
	}

	/**
	 * Attempts to return the English article (<em>a</em> or <em>an</em>)for the provided word based
	 * on the frequency of the article appearing the English Wikipedia corpus before the specified word.
	 * 
	 * @param word The word to query the corect article for, e.g. honest
	 * @return The {@link Data} for the matching article, which contains the smallest matching prefix,
	 *         and the number of times a and and appeared before the word, as well as the recommended article
	 */
	public static Data articleResults(String word) {
		Instant start = Instant.now();
		if (articleRoot == null) {
			articleRoot = new ArticleNode();
			fill("", articleRoot, ARTICLE_DICTIONARY);
			Util.LOGGER.info("Time to fill dictionary: " + Duration.between(start, Instant.now()));
		}

		return query(word);
	}

	/**
	 * Tests that all the words in the specified phrase are lowercase,
	 * ignoring whitespace and basic punctuation.
	 * 
	 * <pre>
	 * PluralUtil.isLowerCase(null)   = false
	 * PluralUtil.isLowerCase("")     = false
	 * PluralUtil.isLowerCase("  ")   = false
	 * PluralUtil.isLowerCase("abc")  = true
	 * PluralUtil.isLowerCase("abC")  = false
	 * PluralUtil.isLowerCase("two words")  = true
	 * </pre>
	 * 
	 * @param phrase The phrase to check
	 * @return true if all the words are lowercase, false otherwise
	 */
	public static boolean isLowerCase(final String phrase) {
		if (phrase != null && phrase.length() > 0) {
			Pattern p = Pattern.compile(LOWERCASE_PATTERN);
			String[] tokens = phrase.split("\\s");
			for (String s : tokens) {
				Matcher m = p.matcher(s);
				if (!m.matches()) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * Tests that all the words in the specified phrase are title case,
	 * ignoring whitespace and basic punctuation.
	 * 
	 * <pre>
	 * PluralUtil.isTitleCase(null)   = false
	 * PluralUtil.isTitleCase("")     = false
	 * PluralUtil.isTitleCase("  ")   = false
	 * PluralUtil.isTitleCase("Abc")  = true
	 * PluralUtil.isTitleCase("aBC")  = false
	 * PluralUtil.isTitleCase("Two Words")  = true
	 * </pre>
	 * 
	 * @param phrase The phrase to check
	 * @return true if all the words are title case, false otherwise
	 */
	public static boolean isTitleCase(final String phrase) {
		if (phrase != null && phrase.length() > 0) {
			Pattern p = Pattern.compile(TITLECASE_PATTERN);
			String[] tokens = phrase.split("\\s");
			for (String s : tokens) {
				Matcher m = p.matcher(s);
				if (!m.matches()) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * Tests that all the words in the specified phrase are uppercase,
	 * ignoring whitespace and basic punctuation.
	 * 
	 * <pre>
	 * PluralUtil.isUpperCase(null)   = false
	 * PluralUtil.isUpperCase("")     = false
	 * PluralUtil.isUpperCase("  ")   = false
	 * PluralUtil.isUpperCase("ABC")  = true
	 * PluralUtil.isUpperCase("aBC")  = false
	 * PluralUtil.isUpperCase("TWO WORDS")  = true
	 * </pre>
	 * 
	 * @param phrase The phrase to check
	 * @return true if all the words are uppercase, false otherwise
	 */
	public static boolean isUpperCase(final String phrase) {
		if (phrase != null && phrase.length() > 0) {
			Pattern p = Pattern.compile(UPPERCASE_PATTERN);
			String[] tokens = phrase.split("\\s");
			for (String s : tokens) {
				Matcher m = p.matcher(s);
				if (!m.matches()) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * Attempts to return the plural of the provided singular word using
	 * basic common English patterns, does not handle individual exceptions.
	 * 
	 * @param singular The word to pluralise
	 * @return The plural of the singular word
	 */
	public static String pluralise(final String singular) {
		if (singular != null && singular.length() > 0) {
			String str = singular.toLowerCase();
			StringBuilder out = new StringBuilder(str);

			// check unchanging
			for (int i = 0; i < unchanging.length; i++) {
				if (str.equals(unchanging[i])) {
					return singular;
				}
			}
			// check only plural
			for (int i = 0; i < onlyPlurals.length; i++) {
				if (str.equals(onlyPlurals[i])) {
					return singular;
				}
			}

			// check irregular
			for (int i = 0; i < irregular.length; i++) {
				if (str.equals(irregular[i][0])) {
					return replaceWithMatchingCase(singular, irregular[i][1]);
				}
			}

			// ends with 'ex'
			if (str.endsWith("ex")) {
				// look for exceptions first exExceptions
				for (int i = 0; i < exExceptions.length; i++) {
					if (str.equals(exExceptions[i][0])) {
						return replaceWithMatchingCase(singular, exExceptions[i][1]);
					}
				}
				// change final 'ex' to 'ices'
				out.setLength(out.length() - 2);
				out.append("ices");
			}
			// ends with 'is'
			else if (str.endsWith("is")) {
				// Change final 'is' to 'es'
				out.setLength(out.length() - 2);
				out.append("es");
			}
			// ends with 'us'
			else if (str.endsWith("us")) {
				// look for exceptions first oExceptions
				for (int i = 0; i < usExceptions.length; i++) {
					if (str.equals(usExceptions[i][0])) {
						return replaceWithMatchingCase(singular, usExceptions[i][1]);
					}
				}
				// change final 'us' to 'i'
				out.setLength(out.length() - 2);
				out.append("i");
			}
			// word ends in 's', 'x', 'ch', 'z', or 'sh'
			else if (str.endsWith("s") || str.endsWith("ss") || str.endsWith("sh") || str.endsWith("ch") || str.endsWith("x")
					|| str.endsWith("z")) {
				// look for exceptions first xExceptions
				for (int i = 0; i < xExceptions.length; i++) {
					if (str.equals(xExceptions[i][0])) {
						return replaceWithMatchingCase(singular, xExceptions[i][1]);
					}
				}
				out.append("es");
			}
			// ending in 'y'
			else if (str.endsWith("y")) {
				String s = str.substring(0, str.length() - 1);
				// preceded by a vowel
				if (s.endsWith("a") || s.endsWith("e") || s.endsWith("i") || s.endsWith("o") || s.endsWith("u")) {
					out.append("s");
				} else {
					// drop the y and add ies
					out.setLength(out.length() - 1);
					out.append("ies");
				}
			}
			// ends with 'ff' or 'ffe'
			else if (str.endsWith("ff") || str.endsWith("ffe")) {
				// look for exceptions first ffExceptions
				for (int i = 0; i < ffExceptions.length; i++) {
					if (str.equals(ffExceptions[i][0])) {
						return replaceWithMatchingCase(singular, ffExceptions[i][1]);
					}
				}
				out.append("s");
			}
			// ends with 'f' (but not 'ff')
			else if (str.endsWith("f")) {
				// look for exceptions first fExceptions
				for (int i = 0; i < fExceptions.length; i++) {
					if (str.equals(fExceptions[i][0])) {
						return replaceWithMatchingCase(singular, fExceptions[i][1]);
					}
				}
				// change the 'f' to 'ves'
				out.setLength(out.length() - 1);
				out.append("ves");
			}
			// ends with 'fe' (but not ffe')
			else if (str.endsWith("fe")) {
				// look for exceptions first feExceptions
				for (int i = 0; i < feExceptions.length; i++) {
					if (str.equals(feExceptions[i][0])) {
						return replaceWithMatchingCase(singular, feExceptions[i][1]);
					}
				}
				// change the 'fe' to 'ves'
				out.setLength(out.length() - 2);
				out.append("ves");
			}
			// ends with 'o'
			else if (str.endsWith("o")) {
				// look for exceptions first oExceptions
				for (int i = 0; i < oExceptions.length; i++) {
					if (str.equals(oExceptions[i][0])) {
						return replaceWithMatchingCase(singular, oExceptions[i][1]);
					}
				}
				// add 'es'
				out.append("es");
			}
			// ends with 'um'
			else if (str.endsWith("um")) {
				// look for exceptions first oExceptions
				for (int i = 0; i < umExceptions.length; i++) {
					if (str.equals(umExceptions[i][0])) {
						return replaceWithMatchingCase(singular, umExceptions[i][1]);
					}
				}
				// change final 'um' to 'a'
				out.setLength(out.length() - 2);
				out.append("a");
			}
			// ends with 'a' but not 'ia'
			else if (str.endsWith("a")) {
				// not ending is 'ia'
				if (str.endsWith("ia")) {
					out.append("s");
					return replaceWithMatchingCase(singular, out.toString());
				}
				// look for exceptions first aExceptions
				for (int i = 0; i < aExceptions.length; i++) {
					if (str.equals(aExceptions[i][0])) {
						return replaceWithMatchingCase(singular, aExceptions[i][1]);
					}
				}
				// Change final 'a' to 'ae'
				out.setLength(out.length() - 1);
				out.append("ae");
			}
			// ends with 'on' but not 'sion'
			else if (str.endsWith("on")) {
				// not ending is 'ion'
				if (str.endsWith("sion")) {
					out.append("s");
					return replaceWithMatchingCase(singular, out.toString());
				}
				// look for exceptions first onExceptions
				for (int i = 0; i < onExceptions.length; i++) {
					if (str.equals(onExceptions[i][0])) {
						return replaceWithMatchingCase(singular, onExceptions[i][1]);
					}
				}
				// change final 'on' to 'a'
				out.setLength(out.length() - 2);
				out.append("a");
			} else {
				out.append("s");
			}

			return replaceWithMatchingCase(singular, out.toString());
		}

		return null;
	}

	/**
	 * Fashion a title case identifier from the given string.
	 * 
	 * @param phrase The string to convert
	 * @return A title case identifier. First letter upper case words with spaces between.
	 */
	public static String toTitleCase(String phrase) {
		if (phrase != null && phrase.length() > 0) {
			StringBuilder out = new StringBuilder();
			String[] tokens = phrase.split("\\s");
			for (String s : tokens) {
				out.append(out.length() > 0 ? " " : "");
				out.append(StringUtils.capitalize(s.toLowerCase()));
			}
			return out.toString();
		}
		return null;
	}

	/**
	 * Attempts to return the replacement string in the same case as the original string,
	 * e.g. if the input string is in all upper case or all lower case, the replacement
	 * string will be in a matching case.
	 * 
	 * @param original The input string with the original case
	 * @param replacement The replacement string with the case to be matched to the original
	 * @return The replacement string, with the matching case of the original if possible
	 */
	static String replaceWithMatchingCase(final String original, final String replacement) {
		if (isUpperCase(original)) {
			return replacement.toUpperCase();
		} else if (isLowerCase(original)) {
			return replacement;
		} else if (isTitleCase(original)) {
			return toTitleCase(replacement);
		}

		return replacement;
	}

	/**
	 * Recursive method used to unpack and decode the article dictionary results.
	 */
	@SuppressWarnings("boxing")
	private static String fill(final String prefix, final ArticleNode node, final String dict) {
		String[] a = dict.split(";", 4);
		if (a.length > 3) {
			a = Arrays.copyOf(a, 3);
		}
		List<Integer> n = Arrays.asList(a).stream().map(s -> {
			int r = 0;
			try {
				r = Integer.parseInt(s, 36);
			} catch (NumberFormatException nfe) {
				// swallow
			}
			return r;
		}).collect(Collectors.toList());

		node.data.setaCount(n.get(0));
		node.data.setAnCount(n.get(1));
		node.data.setPrefix(prefix);
		node.data.setArticle(n.get(0) >= n.get(1) ? "a" : "an");

		Util.LOGGER.fine(String.format("Filling for prefix:  %s  %d  %d", prefix, n.get(0), n.get(1)));

		String d2 = dict.substring(1 + String.join(";", a).length() > dict.length() ? 0 : 1 + String.join(";", a).length());
		for (int i = 0; i < n.get(2); i++) {
			if (node.get(d2.charAt(0)) == null) {
				node.put(d2.charAt(0), new ArticleNode());
			}
			d2 = fill(prefix + d2.charAt(0), node.get(d2.charAt(0)), d2.substring(1));
		}
		return d2;
	}

	/**
	 * Queries the article node data to find the matching prefix
	 * 
	 * @param word The word to query for
	 * @return the {@link Data} which contains the result
	 */
	private static Data query(final String word) {
		ArticleNode node = articleRoot;
		int sI = 0;
		char c;
		ArticleNode.Data result = null;

		do {
			c = word.charAt(sI++);
		} while ("\"‘’“”$\'-(".indexOf(c) >= 0);

		while (true) {
			result = node.data != null ? node.data : result;
			node = node.get(Character.valueOf(c));
			if (node == null)
				return result;
			try {
				c = word.charAt(sI++);
			} catch (IndexOutOfBoundsException e) {
				c = ' ';
			}
		}
	}
}

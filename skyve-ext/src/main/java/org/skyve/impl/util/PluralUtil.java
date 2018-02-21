package org.skyve.impl.util;

/**
 * Converted and enhanced Java implementation of https://github.com/rhroyston/pluralizer-js/blob/master/pluralizer.js.
 * License: MIT
 */
public class PluralUtil {

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
	private static final String[][] xExceptions = new String[][] { { "fez", "fezzes" }, { "gas", "gasses" }, { "ox", "oxen" } };
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
	private static final String[][] feExceptions = new String[][] {
			{ "safe", "safes" } };
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
	private static final String[][] usExceptions = new String[][] {
			{ "abacus", "abacuses" },
			{ "bus", "buses" },
			{ "crocus", "crocuses" },
			{ "genus", "genera" },
			{ "octopus", "octopuses" },
			{ "rhombus", "rhombuses" },
			{ "walrus", "walruses" } };
	private static final String[][] umExceptions = new String[][] { { "album", "albums" }, { "stadium", "stadiums" } };
	private static final String[][] aExceptions = new String[][] { { "agenda", "agendas" },
			{ "alfalfa", "alfalfas" },
			{ "aurora", "auroras" },
			{ "banana", "bananas" },
			{ "barracuda", "barracudas" },
			{ "cornea", "corneas" },
			{ "nova", "novas" },
			{ "phobia", "phobias" } };
	private static final String[][] onExceptions = new String[][] {
			{ "balloon", "balloons" },
			{ "carton", "cartons" },
			{ "formation", "formations" } };
	private static final String[][] exExceptions = new String[][] {
			{ "annex", "annexes" },
			{ "complex", "complexes" },
			{ "duplex", "duplexes" },
			{ "hex", "hexes" },
			{ "index", "indices" } };
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
			"offspring",
			"sheep",
			"silver",
			"swine",
			"trousers",
			"trout",
			"wheat" };
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

	private PluralUtil() {
		// no-op
	}

	/**
	 * Attempts to return the plural of the provided singular word using
	 * basic common english patterns, does not handle individual exceptions.
	 * 
	 * @param singular The word to pluralise
	 * @return The plural of the singualr word
	 */
	public static String pluralise(final String str) {
		if (str != null && str.length() > 0) {
			StringBuilder out = new StringBuilder(str);

			// check unchanging
			for (int i = 0; i < unchanging.length; i++) {
				if (str == unchanging[i]) {
					return str;
				}
			}
			// check onlyPlurals
			for (int i = 0; i < onlyPlurals.length; i++) {
				if (str == onlyPlurals[i]) {
					return str;
				}
			}
			// check irregular
			for (int i = 0; i < irregular.length; i++) {
				if (str == irregular[i][0]) {
					return irregular[i][1];
				}
			}

			// ends with 'ex'
			if (str.endsWith("ex")) {
				// look for exceptions first exExceptions
				for (int i = 0; i < exExceptions.length; i++) {
					if (str == exExceptions[i][0]) {
						return exExceptions[i][1];
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
					if (str == usExceptions[i][0]) {
						return usExceptions[i][1];
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
					if (str == xExceptions[i][0]) {
						return xExceptions[i][1];
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
				out.append("s");
			}
			// ends with 'f' (but not 'ff')
			else if (str.endsWith("f")) {
				// look for exceptions first fExceptions
				for (int i = 0; i < fExceptions.length; i++) {
					if (str == fExceptions[i][0]) {
						return fExceptions[i][1];
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
					if (str == feExceptions[i][0]) {
						return feExceptions[i][1];
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
					if (str == oExceptions[i][0]) {
						return oExceptions[i][1];
					}
				}
				// add 'es'
				out.append("es");
			}
			// ends with 'um'
			else if (str.endsWith("um")) {
				// look for exceptions first oExceptions
				for (int i = 0; i < umExceptions.length; i++) {
					if (str == umExceptions[i][0]) {
						return umExceptions[i][1];
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
					return out.toString();
				}
				// look for exceptions first aExceptions
				for (int i = 0; i < aExceptions.length; i++) {
					if (str == aExceptions[i][0]) {
						return aExceptions[i][1];
					}
				}
				// Change final 'a' to 'ae'
				out.setLength(out.length() - 1);
				out.append("ae");
			}
			// ends with 'on'
			else if (str.endsWith("on")) {
				// look for exceptions first onExceptions
				for (int i = 0; i < onExceptions.length; i++) {
					if (str == onExceptions[i][0]) {
						return onExceptions[i][1];
					}
				}
				// change final 'on' to 'a'
				out.setLength(out.length() - 2);
				out.append("a");
			} else {
				out.append("s");
			}

			return out.toString();
		}

		return null;
	}
}

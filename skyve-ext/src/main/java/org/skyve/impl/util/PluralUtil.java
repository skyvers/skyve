package org.skyve.impl.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

/**
 * Converted and enhanced Java implementation of https://github.com/rhroyston/pluralizer-js/blob/master/pluralizer.js.
 * License: MIT
 */
public class PluralUtil {

	final static String LOWERCASE_PATTERN = "\\b[a-z'\\-]+\\b";
	final static String TITLECASE_PATTERN = "\\b[A-Z][a-z'\\-]+\\b";
	final static String UPPERCASE_PATTERN = "\\b[A-Z'\\-]+\\b";

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
	 * Attempts to return the plural of the provided singular word using
	 * basic common english patterns, does not handle individual exceptions.
	 * 
	 * @param singular The word to pluralise
	 * @return The plural of the singualr word
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
			// ends with 'on'
			else if (str.endsWith("on")) {
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
}

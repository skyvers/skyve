package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

import org.junit.Test;

public class PluralUtilTest {

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testIsLowerCase() throws Exception {
		// setup the test data
		final String s1 = "lower multiple", s2 = "lower", s3 = "UPPER", s4 = "l'ower m-ultiple", s5 = "UPPER MULTIPLE", s6 = "'",
				s7 = "", s8 = null;

		// perform the method under test
		final boolean result1 = PluralUtil.isLowerCase(s1);
		final boolean result2 = PluralUtil.isLowerCase(s2);
		final boolean result3 = PluralUtil.isLowerCase(s3);
		final boolean result4 = PluralUtil.isLowerCase(s4);
		final boolean result5 = PluralUtil.isLowerCase(s5);
		final boolean result6 = PluralUtil.isLowerCase(s6);
		final boolean result7 = PluralUtil.isLowerCase(s7);
		final boolean result8 = PluralUtil.isLowerCase(s8);

		// verify the result
		assertThat(result1, is(true));
		assertThat(result2, is(true));
		assertThat(result3, is(false));
		assertThat(result4, is(true));
		assertThat(result5, is(false));
		assertThat(result6, is(false));
		assertThat(result7, is(false));
		assertThat(result8, is(false));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testIsTitleCase() throws Exception {
		// setup the test data
		final String s1 = "Title Multiple", s2 = "Title", s3 = "UPPER", s4 = "T'itle M-ultiple", s5 = "UPPER MULTIPLE", s6 = "'",
				s7 = "", s8 = null;

		// perform the method under test
		final boolean result1 = PluralUtil.isTitleCase(s1);
		final boolean result2 = PluralUtil.isTitleCase(s2);
		final boolean result3 = PluralUtil.isTitleCase(s3);
		final boolean result4 = PluralUtil.isTitleCase(s4);
		final boolean result5 = PluralUtil.isTitleCase(s5);
		final boolean result6 = PluralUtil.isTitleCase(s6);
		final boolean result7 = PluralUtil.isTitleCase(s7);
		final boolean result8 = PluralUtil.isTitleCase(s8);

		// verify the result
		assertThat(result1, is(true));
		assertThat(result2, is(true));
		assertThat(result3, is(false));
		assertThat(result4, is(true));
		assertThat(result5, is(false));
		assertThat(result6, is(false));
		assertThat(result7, is(false));
		assertThat(result8, is(false));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testIsUpperCase() throws Exception {
		// setup the test data
		final String s1 = "UPPER MULTIPLE", s2 = "UPPER", s3 = "lower", s4 = "U'PPER M-ULTIPLE", s5 = "lower multiple", s6 = "'",
				s7 = "", s8 = null;

		// perform the method under test
		final boolean result1 = PluralUtil.isUpperCase(s1);
		final boolean result2 = PluralUtil.isUpperCase(s2);
		final boolean result3 = PluralUtil.isUpperCase(s3);
		final boolean result4 = PluralUtil.isUpperCase(s4);
		final boolean result5 = PluralUtil.isUpperCase(s5);
		final boolean result6 = PluralUtil.isUpperCase(s6);
		final boolean result7 = PluralUtil.isUpperCase(s7);
		final boolean result8 = PluralUtil.isUpperCase(s8);

		// verify the result
		assertThat(result1, is(true));
		assertThat(result2, is(true));
		assertThat(result3, is(false));
		assertThat(result4, is(true));
		assertThat(result5, is(false));
		assertThat(result6, is(false));
		assertThat(result7, is(false));
		assertThat(result8, is(false));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseHandlesNulls() throws Exception {
		// setup the test data
		final String singular1 = null, singular2 = "";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular1);
		final String result2 = PluralUtil.pluralise(singular2);

		// verify the result
		assertThat(result1, is(nullValue()));
		assertThat(result2, is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluralise() throws Exception {
		// setup the test data
		final String singular1 = "site", singular2 = "inventory", singular3 = "address", singular4 = "stray", singular5 = "fey",
				singular6 = "search", singular7 = "hoof";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular1);
		final String result2 = PluralUtil.pluralise(singular2);
		final String result3 = PluralUtil.pluralise(singular3);
		final String result4 = PluralUtil.pluralise(singular4);
		final String result5 = PluralUtil.pluralise(singular5);
		final String result6 = PluralUtil.pluralise(singular6);
		final String result7 = PluralUtil.pluralise(singular7);

		// verify the result
		assertThat(result1, is("sites"));
		assertThat(result2, is("inventories"));
		assertThat(result3, is("addresses"));
		assertThat(result4, is("strays"));
		assertThat(result5, is("feys"));
		assertThat(result6, is("searches"));
		assertThat(result7, is("hooves"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithA() throws Exception {
		// setup the test data
		final String singular = "Fascia", singularException = "cornea";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("Fascias"));
		assertThat(result2, is("corneas"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithEx() throws Exception {
		// setup the test data
		final String singular = "vertex", singularException = "Index";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("vertices"));
		assertThat(result2, is("Indices"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithF() throws Exception {
		// setup the test data
		final String singular = "Wolf", singularException = "chef";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("Wolves"));
		assertThat(result2, is("chefs"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithFe() throws Exception {
		// setup the test data
		final String singular = "wife", singularException = "SAFE";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("wives"));
		assertThat(result2, is("SAFES"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithFF() throws Exception {
		// setup the test data
		final String singular = "BOFF", singularException = "staff";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("BOFFS"));
		assertThat(result2, is("staff"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithIs() throws Exception {
		// setup the test data
		final String singular = "Analysis";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);

		// verify the result
		assertThat(result1, is("Analyses"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithO() throws Exception {
		// setup the test data
		final String singular = "tomato", singularException = "VIDEO";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("tomatoes"));
		assertThat(result2, is("VIDEOS"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithOn() throws Exception {
		// setup the test data
		final String singular = "criterion", singular2 = "location", singularException = "carton";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singular2);
		final String result3 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("criteria"));
		assertThat(result2, is("locations"));
		assertThat(result3, is("cartons"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithSion() throws Exception {
		// setup the test data
		final String singular = "decision";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);

		// verify the result
		assertThat(result1, is("decisions"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithSCHXZSH() throws Exception {
		// setup the test data
		final String singular1 = "truss", singular2 = "blitz", singular3 = "fox", singular4 = "church", singularException = "ox";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular1);
		final String result2 = PluralUtil.pluralise(singular2);
		final String result3 = PluralUtil.pluralise(singular3);
		final String result4 = PluralUtil.pluralise(singular4);
		final String result5 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("trusses"));
		assertThat(result2, is("blitzes"));
		assertThat(result3, is("foxes"));
		assertThat(result4, is("churches"));
		assertThat(result5, is("oxen"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithUm() throws Exception {
		// setup the test data
		final String singular = "millenium", singularException = "stadium";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("millenia"));
		assertThat(result2, is("stadiums"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithUs() throws Exception {
		// setup the test data
		final String singular = "focus", singularException = "abacus";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("foci"));
		assertThat(result2, is("abacuses"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithY() throws Exception {
		// setup the test data
		final String singularConsonant = "city", singularVowel = "ray";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singularConsonant);
		final String result2 = PluralUtil.pluralise(singularVowel);

		// verify the result
		assertThat(result1, is("cities"));
		assertThat(result2, is("rays"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseIrregular() throws Exception {
		// setup the test data
		final String singular1 = "woman", singular2 = "person";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular1);
		final String result2 = PluralUtil.pluralise(singular2);

		// verify the result
		assertThat(result1, is("women"));
		assertThat(result2, is("people"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseOnlyPlurals() throws Exception {
		// setup the test data
		final String singular1 = "species", singular2 = "series";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular1);
		final String result2 = PluralUtil.pluralise(singular2);

		// verify the result
		assertThat(result1, is(singular1));
		assertThat(result2, is(singular2));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseReplaceIgnoresCase() throws Exception {
		// setup the test data
		final String singular1 = "EQUIPMENT", singular2 = "Equipment",
				singular3 = "Child", singular4 = "CHILD",
				singular5 = "oats", singular6 = "oAts";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular1);
		final String result2 = PluralUtil.pluralise(singular2);
		final String result3 = PluralUtil.pluralise(singular3);
		final String result4 = PluralUtil.pluralise(singular4);
		final String result5 = PluralUtil.pluralise(singular5);
		final String result6 = PluralUtil.pluralise(singular6);

		// verify the result
		assertThat(result1, is(singular1));
		assertThat(result2, is(singular2));
		assertThat(result3, is("Children"));
		assertThat(result4, is("CHILDREN"));
		assertThat(result5, is(singular5));
		assertThat(result6, is(singular6));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseUnchanging() throws Exception {
		// setup the test data
		final String singular1 = "sheep", singular2 = "deer", singular3 = "fish";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular1);
		final String result2 = PluralUtil.pluralise(singular2);
		final String result3 = PluralUtil.pluralise(singular3);

		// verify the result
		assertThat(result1, is(singular1));
		assertThat(result2, is(singular2));
		assertThat(result3, is(singular3));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceWithMatchingCaseAllLowercase() throws Exception {
		// setup the test data
		final String original = "lower", replacement = "replacement", expected = "replacement";

		// perform the method under test
		final String result = PluralUtil.replaceWithMatchingCase(original, replacement);

		// verify the result
		assertThat(result, is(expected));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceWithMatchingCaseAllUppercase() throws Exception {
		// setup the test data
		final String original = "UPPER", replacement = "replacement", expected = "REPLACEMENT";

		// perform the method under test
		final String result = PluralUtil.replaceWithMatchingCase(original, replacement);

		// verify the result
		assertThat(result, is(expected));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceWithMatchingCaseMultipleWordsLowercase() throws Exception {
		// setup the test data
		final String original = "lower multiple", replacement = "lower replacement", expected = "lower replacement";

		// perform the method under test
		final String result = PluralUtil.replaceWithMatchingCase(original, replacement);

		// verify the result
		assertThat(result, is(expected));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceWithMatchingCaseMultipleWordsTitlecase() throws Exception {
		// setup the test data
		final String original = "Title Multiple", replacement = "title replacement", expected = "Title Replacement";

		// perform the method under test
		final String result = PluralUtil.replaceWithMatchingCase(original, replacement);

		// verify the result
		assertThat(result, is(expected));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceWithMatchingCaseMultipleWordsUppercase() throws Exception {
		// setup the test data
		final String original = "UPPER MULTIPLE", replacement = "upper replacement", expected = "UPPER REPLACEMENT";

		// perform the method under test
		final String result = PluralUtil.replaceWithMatchingCase(original, replacement);

		// verify the result
		assertThat(result, is(expected));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceWithMatchingCaseTitlecase() throws Exception {
		// setup the test data
		final String original = "Title", replacement = "replacement", expected = "Replacement";

		// perform the method under test
		final String result = PluralUtil.replaceWithMatchingCase(original, replacement);

		// verify the result
		assertThat(result, is(expected));
	}

	@Test
	@SuppressWarnings({ "static-method" })
	public void testToTitleCase() throws Exception {
		// setup the test data
		final String s1 = "lower multiple", s2 = "lower", s3 = "UPPER", s4 = "l'ower m-ultiple", s5 = "UPPER MULTIPLE", s6 = "'",
				s7 = "", s8 = null;

		// perform the method under test
		final String result1 = PluralUtil.toTitleCase(s1);
		final String result2 = PluralUtil.toTitleCase(s2);
		final String result3 = PluralUtil.toTitleCase(s3);
		final String result4 = PluralUtil.toTitleCase(s4);
		final String result5 = PluralUtil.toTitleCase(s5);
		final String result6 = PluralUtil.toTitleCase(s6);
		final String result7 = PluralUtil.toTitleCase(s7);
		final String result8 = PluralUtil.toTitleCase(s8);

		// verify the result
		assertThat(result1, is("Lower Multiple"));
		assertThat(result2, is("Lower"));
		assertThat(result3, is("Upper"));
		assertThat(result4, is("L'ower M-ultiple"));
		assertThat(result5, is("Upper Multiple"));
		assertThat(result6, is("'"));
		assertThat(result7, is(nullValue()));
		assertThat(result8, is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testArticleNoResultsReturnsDefaultValue() throws Exception {
		// call the method under test
		String result = PluralUtil.article("^&*");

		// verify the result
		assertThat(result, is("a"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testArticleKnownResultReturnsA() throws Exception {
		// call the method under test
		String result = PluralUtil.article("Honey");

		// verify the result
		assertThat(result, is("a"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testArticleKnownResultReturnsAn() throws Exception {
		// call the method under test
		String result = PluralUtil.article("Honest");

		// verify the result
		assertThat(result, is("an"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testArticleResultsInstantiatesDictionaryFirstTime() throws Exception {
		ArticleNode.Data result = PluralUtil.articleResults("honey");

		assertThat(result, is(notNullValue()));
		assertThat(result.getPrefix(), is("honey"));
		assertThat(result.getArticle(), is("a"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testArticleResultsNoResultsReturnsDefaultValue() throws Exception {
		ArticleNode.Data result = PluralUtil.articleResults("^&*");

		assertThat(result, is(notNullValue()));
		assertThat(result.getPrefix(), is(""));
		assertThat(result.getArticle(), is("a"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testArticleResultsInstantiatesDictionaryOnce() throws Exception {
		ArticleNode.Data result = PluralUtil.articleResults("honey");

		assertThat(result, is(notNullValue()));
		assertThat(result.getPrefix(), is("honey"));
		assertThat(result.getArticle(), is("a"));
		assertThat(result.getaCount(), is(Integer.valueOf(1139)));
		assertThat(result.getAnCount(), is(Integer.valueOf(1)));

		// System.out.println("\n" + result);

		ArticleNode.Data result2 = PluralUtil.articleResults("honest");

		assertThat(result2, is(notNullValue()));
		assertThat(result2.getPrefix(), is("hone"));
		assertThat(result2.getArticle(), is("an"));
		assertThat(result2.getaCount(), is(Integer.valueOf(1263)));
		assertThat(result2.getAnCount(), is(Integer.valueOf(3987)));

		// System.out.println("\n" + result2);
	}
}

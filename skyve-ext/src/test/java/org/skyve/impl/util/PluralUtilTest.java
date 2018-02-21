package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

import org.junit.Test;

public class PluralUtilTest {

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
		final String singular = "fascia", singularException = "cornea";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("fascias"));
		assertThat(result2, is("corneas"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithEx() throws Exception {
		// setup the test data
		final String singular = "vertex", singularException = "index";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("vertices"));
		assertThat(result2, is("indices"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithF() throws Exception {
		// setup the test data
		final String singular = "wolf", singularException = "chef";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("wolves"));
		assertThat(result2, is("chefs"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithFe() throws Exception {
		// setup the test data
		final String singular = "wife", singularException = "safe";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("wives"));
		assertThat(result2, is("safes"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithFF() throws Exception {
		// setup the test data
		final String singular = "boff", singularException = "staff";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("boffs"));
		assertThat(result2, is("staff"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithIs() throws Exception {
		// setup the test data
		final String singular = "analysis";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);

		// verify the result
		assertThat(result1, is("analyses"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithO() throws Exception {
		// setup the test data
		final String singular = "tomato", singularException = "video";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("tomatoes"));
		assertThat(result2, is("videos"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPluraliseEndsWithOn() throws Exception {
		// setup the test data
		final String singular = "criterion", singularException = "carton";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular);
		final String result2 = PluralUtil.pluralise(singularException);

		// verify the result
		assertThat(result1, is("criteria"));
		assertThat(result2, is("cartons"));
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
	public void testPluraliseOnlySingular() throws Exception {
		// setup the test data
		final String singular1 = "wood", singular2 = "equipment";

		// perform the method under test
		final String result1 = PluralUtil.pluralise(singular1);
		final String result2 = PluralUtil.pluralise(singular2);

		// verify the result
		assertThat(result1, is(singular1));
		assertThat(result2, is(singular2));
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

}

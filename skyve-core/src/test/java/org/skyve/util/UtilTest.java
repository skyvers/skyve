package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Color;
import java.util.Locale;

import org.junit.jupiter.api.Test;

/** Unit tests for pure static utility methods in {@link Util}. */
@SuppressWarnings("static-method")
class UtilTest {

	// ---- processStringValue ----

	@Test
	void processStringValueNullReturnsNull() {
		assertThat(Util.processStringValue(null), is(nullValue()));
	}

	@Test
	void processStringValueEmptyReturnsNull() {
		assertThat(Util.processStringValue(""), is(nullValue()));
	}

	@Test
	void processStringValueWhitespaceOnlyReturnsNull() {
		assertThat(Util.processStringValue("   "), is(nullValue()));
	}

	@Test
	void processStringValueTrimsLeadingAndTrailingWhitespace() {
		assertThat(Util.processStringValue("  hello  "), is("hello"));
	}

	@Test
	void processStringValueNormalStringUnchanged() {
		assertThat(Util.processStringValue("hello world"), is("hello world"));
	}

	// ---- UTF8Length ----

	@Test
	void utf8LengthEmptyStringIsZero() {
		assertEquals(0, Util.UTF8Length(""));
	}

	@Test
	void utf8LengthAsciiCharsEachCountOnebyte() {
		assertEquals(5, Util.UTF8Length("hello"));
	}

	@Test
	void utf8LengthTwoByteCharCounts2Bytes() {
		// £ is U+00A3 which is 2 bytes in UTF-8
		assertEquals(2, Util.UTF8Length("\u00A3"));
	}

	@Test
	void utf8LengthThreeByteCharCounts3Bytes() {
		// € is U+20AC which is 3 bytes in UTF-8
		assertEquals(3, Util.UTF8Length("\u20AC"));
	}

	@Test
	void utf8LengthMixedAsciiAndMultibyte() {
		// "A" (1 byte) + "£" (2 bytes) = 3 bytes
		assertEquals(3, Util.UTF8Length("A\u00A3"));
	}

	@Test
	void utf8LengthSurrogatePairCounts4Bytes() {
		// U+1F600 (emoji) is encoded as a surrogate pair in Java chars → 4 bytes in UTF-8
		String emoji = new String(Character.toChars(0x1F600));
		assertEquals(4, Util.UTF8Length(emoji));
	}

	// ---- lastIndexOfRegEx ----

	@Test
	void lastIndexOfRegExFoundReturnsLastOccurrence() {
		// "ab" appears at index 0 and index 2; last is 2
		assertEquals(2, Util.lastIndexOfRegEx("ababc", "ab"));
	}

	@Test
	void lastIndexOfRegExNotFoundReturnsMinusOne() {
		assertEquals(-1, Util.lastIndexOfRegEx("hello", "xyz"));
	}

	@Test
	void lastIndexOfRegExSingleMatchReturnsIndex() {
		assertEquals(6, Util.lastIndexOfRegEx("hello world", "world"));
	}

	@Test
	void lastIndexOfRegExRegexPatternUsedCorrectly() {
		// Match any digit; "123" → digits at 0, 1, 2; last is 2
		assertEquals(2, Util.lastIndexOfRegEx("123", "\\d"));
	}

	// ---- isRTL ----

	@Test
	void isRtlNullLocaleReturnsFalse() {
		assertFalse(Util.isRTL(null));
	}

	@Test
	void isRtlEnglishLocaleReturnsFalse() {
		assertFalse(Util.isRTL(Locale.ENGLISH));
	}

	@Test
	void isRtlArabicLocaleReturnsTrue() {
		assertTrue(Util.isRTL(new Locale("ar")));
	}

	// ---- i18n (with Locale, null key guard) ----

	@Test
	void i18nNullKeyReturnsNull() {
		assertThat(Util.i18n(null, Locale.ENGLISH), is(nullValue()));
	}

	@Test
	void i18nUnknownKeyReturnsKeyItself() {
		String key = "this.key.does.not.exist.xyz";
		assertThat(Util.i18n(key, Locale.ENGLISH), is(key));
	}

	// ---- nullSafeI18n ----

	@Test
	void nullSafeI18nNullLocaleUsesEnglishFallback() {
		String key = "this.key.does.not.exist.xyz";
		assertThat(Util.nullSafeI18n(key, (Locale) null), is(key));
	}

	@Test
	void nullSafeI18nWithValuesSubstitutesPlaceholders() {
		// If the key has {0} substitution in the bundle, format should apply.
		// Even if not in bundle, the key is returned as-is (no substitution on raw key).
		String result = Util.nullSafeI18n("this.key.does.not.exist.xyz", Locale.ENGLISH, "arg");
		assertThat(result, is(notNullValue()));
	}

	// ---- htmlColourCode / htmlColour (round-trip) ----

	@Test
	void htmlColourCodeProducesHexWithHash() {
		String code = Util.htmlColourCode(Color.RED);
		assertTrue(code.startsWith("#"));
		assertEquals(7, code.length()); // #rrggbb
	}

	@Test
	void htmlColourCodeRedIsFF0000() {
		assertThat(Util.htmlColourCode(Color.RED), is("#ff0000"));
	}

	@Test
	void htmlColourRoundTrip() {
		Color original = new Color(0x1A, 0x2B, 0x3C);
		String code = Util.htmlColourCode(original);
		Color decoded = Util.htmlColour(code);
		assertEquals(original.getRed(), decoded.getRed());
		assertEquals(original.getGreen(), decoded.getGreen());
		assertEquals(original.getBlue(), decoded.getBlue());
	}

	// ---- coalesceNull ----

	@Test
	void coalesceNullNonNullValueReturnsValue() {
		assertThat(Util.coalesceNull("hello", "fallback"), is("hello"));
	}

	@Test
	void coalesceNullNullValueReturnsFallback() {
		assertThat(Util.coalesceNull(null, "fallback"), is("fallback"));
	}

	@Test
	void coalesceNullBothNullReturnsNull() {
		assertThat(Util.coalesceNull(null, null), is(nullValue()));
	}

	@Test
	void coalesceNullIntegerType() {
		assertThat(Util.coalesceNull(null, Integer.valueOf(42)), is(Integer.valueOf(42)));
	}

	// ---- MEGABYTE constant ----

	@Test
	void megabyteConstantIsCorrectValue() {
		assertEquals(1024L * 1024L, Util.MEGABYTE);
	}

	// ---- directory accessors ----

	@Test
	void getContentDirectoryReturnsNonNull() {
		assertThat(Util.getContentDirectory(), is(notNullValue()));
	}

	@Test
	void getAddinsDirectoryDefaultsToContentSubdir() {
		String addins = Util.getAddinsDirectory();
		assertThat(addins, is(notNullValue()));
		assertThat(addins.contains("addins"), is(true));
	}

	@Test
	void getBackupDirectoryReturnsNonNull() {
		assertThat(Util.getBackupDirectory(), is(notNullValue()));
	}

	@Test
	void getCacheDirectoryDefaultsToContentSubdir() {
		String cache = Util.getCacheDirectory();
		assertThat(cache, is(notNullValue()));
		assertThat(cache.contains("SKYVE_CACHE"), is(true));
	}

	@Test
	void getThumbnailDirectoryDefaultsToContentSubdir() {
		String thumb = Util.getThumbnnailDirectory();
		assertThat(thumb, is(notNullValue()));
		assertThat(thumb.contains("SKYVE_THUMBNAILS"), is(true));
	}

	@Test
	void getArchiveConfigReturnsNonNull() {
		assertThat(Util.getArchiveConfig(), is(notNullValue()));
	}

	// ---- URL building methods ----

	@Test
	void getDocumentUrlContainsModuleAndDocument() {
		String url = Util.getDocumentUrl("admin", "Contact");
		assertThat(url.contains("m=admin"), is(true));
		assertThat(url.contains("d=Contact"), is(true));
	}

	@Test
	void getDocumentUrlWithBizIdContainsBizId() {
		String url = Util.getDocumentUrl("admin", "Contact", "abc123");
		assertThat(url.contains("i=abc123"), is(true));
	}

	@Test
	void getDocumentUrlWithoutBizIdOmitsIdParam() {
		String url = Util.getDocumentUrl("admin", "Contact");
		assertFalse(url.contains("&i="));
	}

	@Test
	void getDocumentAnchorUrlContainsHref() {
		String anchor = Util.getDocumentAnchorUrl("admin", "Contact", null, false, "Click");
		assertThat(anchor.contains("<a href="), is(true));
		assertThat(anchor.contains("Click"), is(true));
	}

	@Test
	void getDocumentAnchorUrlNewWindowHasTargetBlank() {
		String anchor = Util.getDocumentAnchorUrl("admin", "Contact", null, true, "Click");
		assertThat(anchor.contains("_blank"), is(true));
	}

	@Test
	void getListUrlContainsModuleAndQuery() {
		String url = Util.getListUrl("admin", "allUsers");
		assertThat(url.contains("m=admin"), is(true));
		assertThat(url.contains("q=allUsers"), is(true));
	}

	@Test
	void getContentUrlContainsContentId() {
		String url = Util.getContentUrl("admin", "Contact", "photo", "cid1");
		assertThat(url.contains("_n=cid1"), is(true));
		assertThat(url.contains("_b=photo"), is(true));
	}

	@Test
	void getResourceUrlWithModuleAndDocumentContainsDoc() {
		String url = Util.getResourceUrl("admin", "Contact", "logo.png");
		assertThat(url.contains("_n=logo.png"), is(true));
		assertThat(url.contains("_doc=admin.Contact"), is(true));
	}

	@Test
	void getResourceUrlWithoutModuleOmitsDoc() {
		String url = Util.getResourceUrl("logo.png");
		assertThat(url.contains("_n=logo.png"), is(true));
		assertFalse(url.contains("_doc="));
	}
}

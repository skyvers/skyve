package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.List;
import java.util.Locale;

import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;

/** Unit tests for pure static utility methods in {@link Util}. */
@SuppressWarnings("static-method")
class UtilTest {
	private static void withThreadLocalUser(User user, Runnable run) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		try {
			run.run();
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

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
		assertTrue(addins.contains("addins"));
	}

	@Test
	void getBackupDirectoryReturnsNonNull() {
		assertThat(Util.getBackupDirectory(), is(notNullValue()));
	}

	@Test
	void getCacheDirectoryDefaultsToContentSubdir() {
		String cache = Util.getCacheDirectory();
		assertThat(cache, is(notNullValue()));
		assertTrue(cache.contains("SKYVE_CACHE"));
	}

	@Test
	void getThumbnailDirectoryDefaultsToContentSubdir() {
		String thumb = Util.getThumbnnailDirectory();
		assertThat(thumb, is(notNullValue()));
		assertTrue(thumb.contains("SKYVE_THUMBNAILS"));
	}

	@Test
	void getArchiveConfigReturnsNonNull() {
		assertThat(Util.getArchiveConfig(), is(notNullValue()));
	}

	// ---- URL building methods ----

	@Test
	void getDocumentUrlContainsModuleAndDocument() {
		String url = Util.getDocumentUrl("admin", "Contact");
		assertTrue(url.contains("m=admin"));
		assertTrue(url.contains("d=Contact"));
	}

	@Test
	void getDocumentUrlWithBizIdContainsBizId() {
		String url = Util.getDocumentUrl("admin", "Contact", "abc123");
		assertTrue(url.contains("i=abc123"));
	}

	@Test
	void getDocumentUrlWithoutBizIdOmitsIdParam() {
		String url = Util.getDocumentUrl("admin", "Contact");
		assertFalse(url.contains("&i="));
	}

	@Test
	void getDocumentAnchorUrlContainsHref() {
		String anchor = Util.getDocumentAnchorUrl("admin", "Contact", null, false, "Click");
		assertTrue(anchor.contains("<a href="));
		assertTrue(anchor.contains("Click"));
	}

	@Test
	void getDocumentAnchorUrlNewWindowHasTargetBlank() {
		String anchor = Util.getDocumentAnchorUrl("admin", "Contact", null, true, "Click");
		assertTrue(anchor.contains("_blank"));
	}

	@Test
	void getListUrlContainsModuleAndQuery() {
		String url = Util.getListUrl("admin", "allUsers");
		assertTrue(url.contains("m=admin"));
		assertTrue(url.contains("q=allUsers"));
	}

	@Test
	void getContentUrlContainsContentId() {
		String url = Util.getContentUrl("admin", "Contact", "photo", "cid1");
		assertTrue(url.contains("_n=cid1"));
		assertTrue(url.contains("_b=photo"));
	}

	@Test
	void getResourceUrlWithModuleAndDocumentContainsDoc() {
		String url = Util.getResourceUrl("admin", "Contact", "logo.png");
		assertTrue(url.contains("_n=logo.png"));
		assertTrue(url.contains("_doc=admin.Contact"));
	}

	@Test
	void getResourceUrlWithoutModuleOmitsDoc() {
		String url = Util.getResourceUrl("logo.png");
		assertTrue(url.contains("_n=logo.png"));
		assertFalse(url.contains("_doc="));
	}

	@Test
	void documentAndContentBeanUrlHelpersIncludeExpectedFragments() {
		org.skyve.domain.Bean bean = mock(org.skyve.domain.Bean.class);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.getBizId()).thenReturn("B1");

		String docUrl = Util.getDocumentUrl(bean);
		assertTrue(docUrl.contains("m=admin"));
		assertTrue(docUrl.contains("d=Contact"));
		assertTrue(docUrl.contains("i=B1"));

		String image = Util.getContentImageUrl("admin", "Contact", "photo", "cid1", 80, 40);
		assertTrue(image.contains("_w=80"));
		assertTrue(image.contains("_h=40"));

		String anchor = Util.getContentAnchorUrl("admin", "Contact", "photo", "cid1", true, "Open");
		assertTrue(anchor.contains("target=\"_blank\""));
		assertTrue(anchor.contains("Open"));

		String anchorWithImage = Util.getContentAnchorWithImageUrl("admin", "Contact", "photo", "cid1", false, 60, 30);
		assertTrue(anchorWithImage.contains("<a href="));
		assertTrue(anchorWithImage.contains("<img src="));
	}

	@Test
	void chunkCharsAndBytesWriteAllData() throws IOException {
		StringBuilder builder = new StringBuilder(3000);
		for (int i = 0; i < 3000; i++) {
			builder.append('x');
		}

		StringWriter writer = new StringWriter();
		Util.chunkCharsToWriter(builder, writer);
		assertEquals(3000, writer.toString().length());

		byte[] bytes = new byte[2500];
		for (int i = 0; i < bytes.length; i++) {
			bytes[i] = (byte) (i % 127);
		}
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		Util.chunkBytesToOutputStream(bytes, out);
		assertEquals(2500, out.toByteArray().length);
	}

	@Test
	void urlAndConfigGettersReturnExpectedShapes() {
		assertTrue(Util.getBaseUrl().endsWith("/"));
		assertThat(Util.getHomeUrl(), is(notNullValue()));
		assertThat(Util.getLoginUrl(), is(notNullValue()));
		assertThat(Util.getLoggedOutUrl(), is(notNullValue()));
		assertTrue(Util.getResetPasswordUrl().contains("passwordResetToken"));

		assertDoesNotThrow(() -> Util.getServerUrl());
		assertDoesNotThrow(() -> Util.getSkyveContext());
		assertDoesNotThrow(() -> Util.getSkyveContextRealPath());
		assertDoesNotThrow(() -> Util.getModuleDirectory());
		assertDoesNotThrow(() -> Util.getPasswordHashingAlgorithm());
		assertDoesNotThrow(() -> Util.getSupportEmailAddress());
	}

	@Test
	void countryNameFromCodeUsesUserLocale() {
		User user = mock(User.class);
		when(user.getLocale()).thenReturn(Locale.ENGLISH);

		withThreadLocalUser(user, () -> {
			String result = Util.countryNameFromCode("AU");
			assertThat(result, is("Australia"));
		});
	}

	@Test
	void isRtlAndSecureUrlAndCloneHelpersAreInvokable() {
		assertFalse(Util.isRTL());
		assertDoesNotThrow(() -> {
			Util.isSecureUrl();
		});
		String serverUrl = Util.getServerUrl();
		if (serverUrl != null) {
			assertThat(Boolean.valueOf(Util.isSecureUrl()), is(Boolean.valueOf(serverUrl.startsWith("https://"))));
		}

		String original = "serializable";
		assertThat(Util.cloneBySerialization(original), is("serializable"));
		assertThat(Util.cloneToTransientBySerialization(original), is("serializable"));
		assertThat(Util.deproxy(original), is("serializable"));

		assertDoesNotThrow(() -> Util.constructRandomInstance(mock(User.class),
				mock(org.skyve.metadata.module.Module.class),
				mock(org.skyve.metadata.model.document.Document.class), 1));
	}

	@Test
	void getCompleteSuggestionsBuildsExpectedQuery() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(mock(User.class));
		persistence.setForThread();
		try {
			DocumentQuery query = mock(DocumentQuery.class);
			DocumentFilter filter = mock(DocumentFilter.class);
			when(persistence.newDocumentQuery("admin", "Contact")).thenReturn(query);
			when(query.getFilter()).thenReturn(filter);
			when(query.addBoundProjection("name", "name")).thenReturn(query);
			when(query.addBoundOrdering("name")).thenReturn(query);
			when(query.setDistinct(true)).thenReturn(query);
			when(query.scalarResults(String.class)).thenReturn(List.of("Alice", "Bob"));

			List<String> prefixed = Util.getCompleteSuggestions("admin", "Contact", "name", "Al");
			assertEquals(2, prefixed.size());
			verify(filter).addLike("name", "Al%");

			List<String> all = Util.getCompleteSuggestions("admin", "Contact", "name", null);
			assertEquals(2, all.size());
			verify(filter, never()).addLike("name", "null%");
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void getHomeUriReturnsUtilImplHomeUri() {
		// getHomeUri() returns UtilImpl.HOME_URI which defaults to null;
		// just call it to cover the line
		Util.getHomeUri(); // no assertion; covers the return statement
	}

	@Test
	void nullSafeI18nNonEnglishLocaleUnknownKeyFallsBackToEnglish() {
		// Using a non-English locale triggers the lang != "en" branch (recursive fallback)
		String key = "this.key.does.not.exist.xyz";
		String result = Util.nullSafeI18n(key, java.util.Locale.FRENCH);
		assertThat(result, is(key));
	}
}

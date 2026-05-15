package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SafeFileNameTest {

	@Test
	void sanitiseNullReturnsUnnamed() {
		assertThat(SafeFileName.sanitise(null), is("unnamed"));
	}

	@Test
	void sanitiseBlankReturnsUnnamed() {
		assertThat(SafeFileName.sanitise("   "), is("unnamed"));
	}

	@Test
	void sanitiseSimpleNamePreserved() {
		assertThat(SafeFileName.sanitise("document"), is("document"));
	}

	@Test
	void sanitiseWithExtensionPreserved() {
		assertThat(SafeFileName.sanitise("report.pdf"), is("report.pdf"));
	}

	@Test
	void sanitiseRemovesForwardSlash() {
		String result = SafeFileName.sanitise("foo/bar.txt");
		assertFalse(result.contains("/"));
	}

	@Test
	void sanitiseRemovesBackslash() {
		String result = SafeFileName.sanitise("foo\\bar.txt");
		assertFalse(result.contains("\\"));
	}

	@Test
	void sanitiseRemovesColonCharacter() {
		String result = SafeFileName.sanitise("foo:bar.txt");
		assertFalse(result.contains(":"));
	}

	@Test
	void sanitiseRemovesAsterisk() {
		String result = SafeFileName.sanitise("foo*bar.txt");
		assertFalse(result.contains("*"));
	}

	@Test
	void sanitiseRemovesQuestionMark() {
		String result = SafeFileName.sanitise("foo?bar.txt");
		assertFalse(result.contains("?"));
	}

	@Test
	void sanitiseRemovesQuoteCharacter() {
		String result = SafeFileName.sanitise("foo\"bar.txt");
		assertFalse(result.contains("\""));
	}

	@Test
	void sanitiseRemovesAngleBrackets() {
		String result = SafeFileName.sanitise("foo<bar>.txt");
		assertFalse(result.contains("<"));
		assertFalse(result.contains(">"));
	}

	@Test
	void sanitiseRemovesPipe() {
		String result = SafeFileName.sanitise("foo|bar.txt");
		assertFalse(result.contains("|"));
	}

	@Test
	void sanitiseReplacesEmDash() {
		// em-dash (U+2013) should be replaced with hyphen-minus
		String result = SafeFileName.sanitise("foo\u2013bar.txt");
		assertFalse(result.contains("\u2013"));
	}

	@Test
	void sanitiseWindowsReservedConAppendsUnderscore() {
		String result = SafeFileName.sanitise("CON");
		assertThat(result, is("CON_"));
	}

	@Test
	void sanitiseWindowsReservedNulAppendsUnderscore() {
		String result = SafeFileName.sanitise("NUL.txt");
		assertTrue(result.startsWith("NUL_"));
	}

	@Test
	void sanitiseWindowsReservedPrnAppendsUnderscore() {
		String result = SafeFileName.sanitise("PRN");
		assertThat(result, is("PRN_"));
	}

	@Test
	void sanitiseWindowsReservedAuxAppendsUnderscore() {
		String result = SafeFileName.sanitise("AUX");
		assertThat(result, is("AUX_"));
	}

	@Test
	void sanitiseLongNameTruncatedToMaxLength() {
		String longName = "a".repeat(300);
		String result = SafeFileName.sanitise(longName);
		assertTrue(result.length() <= 255);
	}

	@Test
	void sanitiseLongNameWithExtensionTruncatedToMaxLength() {
		String longBase = "a".repeat(260);
		String result = SafeFileName.sanitise(longBase + ".txt");
		assertTrue(result.length() <= 255);
		assertTrue(result.endsWith(".txt"));
	}

	@Test
	void sanitiseExtensionLowercased() {
		String result = SafeFileName.sanitise("report.PDF");
		assertThat(result, is("report.pdf"));
	}

	@Test
	void sanitiseExtensionMixedCaseLowercased() {
		String result = SafeFileName.sanitise("data.CSV");
		assertThat(result, is("data.csv"));
	}

	@Test
	void sanitiseSpacesOnlyBaseResultIsNonEmpty() {
		// "   .txt" -> trim -> ".txt" -> dot at 0 not > 0 -> base = ".txt", ext = ""
		// result is ".txt" (valid, non-empty filename)
		String result = SafeFileName.sanitise("   .txt");
		assertNotNull(result);
		assertFalse(result.isEmpty());
	}

	@Test
	void sanitiseTrailingSpacesAndDotsRemovedFromBase() {
		// trailing dots and spaces on the base are illegal on Windows
		String result = SafeFileName.sanitise("report...");
		assertFalse(result.endsWith("..."));
	}

	@Test
	void sanitiseControlCharactersRemoved() {
		// control chars (0x00-0x1F) should be stripped
		String result = SafeFileName.sanitise("foo\u0001bar.txt");
		assertFalse(result.contains("\u0001"));
	}

	@Test
	void sanitiseNormalFilenameUnchanged() {
		String result = SafeFileName.sanitise("My Document 2024.docx");
		assertThat(result, is("My Document 2024.docx"));
	}

	@Test
	void sanitiseCom1ReservedNameAppendsUnderscore() {
		String result = SafeFileName.sanitise("COM1");
		assertTrue(result.startsWith("COM1_"));
	}

	@Test
	void sanitiseLpt9ReservedNameAppendsUnderscore() {
		String result = SafeFileName.sanitise("LPT9");
		assertTrue(result.startsWith("LPT9_"));
	}

	@Test
	void sanitiseOnlyDotsBecomesFile() {
		// After removing trailing dots, the base is empty → falls back to "file"
		String result = SafeFileName.sanitise(".....");
		assertThat(result, is("file"));
	}

	@Test
	void sanitiseLongExtensionDoesNotBreak() {
		// ext itself very long (>255 alpha chars) — trimToMax must handle it
		String longExt = "a".repeat(300);
		String result = SafeFileName.sanitise("name." + longExt);
		assertTrue(result.length() <= 255);
	}

	@Test
	void sanitiseSurrogatePairAtCutPointNotSplit() {
		// Build a base >255 chars where the 255th char position is a high surrogate
		// UTF-16 surrogate pair: U+1F600 (\uD83D\uDE00) = high + low surrogate
		String emoji = "\uD83D\uDE00"; // one emoji = 2 Java chars
		// Fill base to 254 chars with 'a', then append emoji so cut falls in the pair
		String base = "a".repeat(254) + emoji + "a".repeat(10);
		String result = SafeFileName.sanitise(base);
		assertTrue(result.length() <= 255);
		// Should NOT end with a lone high surrogate
		if (result.length() > 0) {
			char last = result.charAt(result.length() - 1);
			assertFalse(Character.isHighSurrogate(last));
		}
	}

	@Test
	void sanitiseLongExtensionWithLongBaseTrimsToMax() {
		// base = 250 'a' chars + '.' + ext = 250 'b' chars → total well over 255
		// The trimToMax should still produce ≤255 chars
		String longBase = "a".repeat(250);
		String longExt = "b".repeat(250);
		String result = SafeFileName.sanitise(longBase + "." + longExt);
		assertTrue(result.length() <= 255);
	}
}

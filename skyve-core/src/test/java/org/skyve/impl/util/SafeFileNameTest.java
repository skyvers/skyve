package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

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
		assertThat(result.contains("/"), is(false));
	}

	@Test
	void sanitiseRemovesBackslash() {
		String result = SafeFileName.sanitise("foo\\bar.txt");
		assertThat(result.contains("\\"), is(false));
	}

	@Test
	void sanitiseRemovesColonCharacter() {
		String result = SafeFileName.sanitise("foo:bar.txt");
		assertThat(result.contains(":"), is(false));
	}

	@Test
	void sanitiseRemovesAsterisk() {
		String result = SafeFileName.sanitise("foo*bar.txt");
		assertThat(result.contains("*"), is(false));
	}

	@Test
	void sanitiseRemovesQuestionMark() {
		String result = SafeFileName.sanitise("foo?bar.txt");
		assertThat(result.contains("?"), is(false));
	}

	@Test
	void sanitiseRemovesQuoteCharacter() {
		String result = SafeFileName.sanitise("foo\"bar.txt");
		assertThat(result.contains("\""), is(false));
	}

	@Test
	void sanitiseRemovesAngleBrackets() {
		String result = SafeFileName.sanitise("foo<bar>.txt");
		assertThat(result.contains("<"), is(false));
		assertThat(result.contains(">"), is(false));
	}

	@Test
	void sanitiseRemovesPipe() {
		String result = SafeFileName.sanitise("foo|bar.txt");
		assertThat(result.contains("|"), is(false));
	}

	@Test
	void sanitiseReplacesEmDash() {
		// em-dash (U+2013) should be replaced with hyphen-minus
		String result = SafeFileName.sanitise("foo\u2013bar.txt");
		assertThat(result.contains("\u2013"), is(false));
	}

	@Test
	void sanitiseWindowsReservedConAppendsUnderscore() {
		String result = SafeFileName.sanitise("CON");
		assertThat(result, is("CON_"));
	}

	@Test
	void sanitiseWindowsReservedNulAppendsUnderscore() {
		String result = SafeFileName.sanitise("NUL.txt");
		assertThat(result.startsWith("NUL_"), is(true));
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
		assertThat(result.length() <= 255, is(true));
	}

	@Test
	void sanitiseLongNameWithExtensionTruncatedToMaxLength() {
		String longBase = "a".repeat(260);
		String result = SafeFileName.sanitise(longBase + ".txt");
		assertThat(result.length() <= 255, is(true));
		assertThat(result.endsWith(".txt"), is(true));
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
		assertThat(result != null && !result.isEmpty(), is(true));
	}

	@Test
	void sanitiseTrailingSpacesAndDotsRemovedFromBase() {
		// trailing dots and spaces on the base are illegal on Windows
		String result = SafeFileName.sanitise("report...");
		assertThat(result.endsWith("..."), is(false));
	}

	@Test
	void sanitiseControlCharactersRemoved() {
		// control chars (0x00-0x1F) should be stripped
		String result = SafeFileName.sanitise("foo\u0001bar.txt");
		assertThat(result.contains("\u0001"), is(false));
	}

	@Test
	void sanitiseNormalFilenameUnchanged() {
		String result = SafeFileName.sanitise("My Document 2024.docx");
		assertThat(result, is("My Document 2024.docx"));
	}

	@Test
	void sanitiseCom1ReservedNameAppendsUnderscore() {
		String result = SafeFileName.sanitise("COM1");
		assertThat(result.startsWith("COM1_"), is(true));
	}

	@Test
	void sanitiseLpt9ReservedNameAppendsUnderscore() {
		String result = SafeFileName.sanitise("LPT9");
		assertThat(result.startsWith("LPT9_"), is(true));
	}
}

package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class UtilImplMoreTest {

	// --- processStringValue ---

	@Test
	void processStringValueNullReturnsNull() {
		assertThat(UtilImpl.processStringValue(null), is(nullValue()));
	}

	@Test
	void processStringValueEmptyReturnsNull() {
		assertThat(UtilImpl.processStringValue(""), is(nullValue()));
	}

	@Test
	void processStringValueWhitespaceOnlyReturnsNull() {
		assertThat(UtilImpl.processStringValue("   "), is(nullValue()));
	}

	@Test
	void processStringValueNormalStringReturnsTrimmed() {
		assertThat(UtilImpl.processStringValue("hello"), is("hello"));
	}

	@Test
	void processStringValueLeadingTrailingWhitespaceReturnsTrimmed() {
		assertThat(UtilImpl.processStringValue("  hello  "), is("hello"));
	}

	// --- cleanupModuleDirectory ---

	@Test
	void cleanupModuleDirectoryNullReturnsNull() {
		assertThat(UtilImpl.cleanupModuleDirectory(null), is(nullValue()));
	}

	@Test
	void cleanupModuleDirectoryEmptyReturnsEmpty() {
		assertThat(UtilImpl.cleanupModuleDirectory(""), is(""));
	}

	@Test
	void cleanupModuleDirectoryAddsModulesIfMissing() {
		String result = UtilImpl.cleanupModuleDirectory("/workspace/project/src/main/java");
		assertThat(result.endsWith("/"), is(true));
		assertThat(result.contains("modules"), is(true));
	}

	@Test
	void cleanupModuleDirectoryAlreadyEndsWithModulesTrailingSlash() {
		String result = UtilImpl.cleanupModuleDirectory("/workspace/modules/");
		assertThat(result.endsWith("/"), is(true));
		assertThat(result.contains("modules"), is(true));
	}

	@Test
	void cleanupModuleDirectoryAlreadyEndsWithModulesNoSlash() {
		String result = UtilImpl.cleanupModuleDirectory("/workspace/modules");
		assertThat(result.endsWith("/"), is(true));
		assertThat(result.contains("modules"), is(true));
	}

	@Test
	void cleanupModuleDirectoryStripsTrailingBackslash() {
		String result = UtilImpl.cleanupModuleDirectory("C:\\workspace\\project\\");
		assertThat(result, is("C:\\workspace\\project/modules/"));
	}

	@Test
	void cleanupModuleDirectoryStripsTrailingForwardSlash() {
		String result = UtilImpl.cleanupModuleDirectory("/workspace/project/");
		assertThat(result.contains("modules"), is(true));
	}
}

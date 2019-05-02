package org.skyve.impl.web;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;

public class SkyveContextListenerTest {

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupContentDirectoryHandlesEmptyStrings() throws Exception {
		// setup the test data
		final String test1 = null, test2 = "";

		// perform the method under test
		String result1 = SkyveContextListener.cleanupDirectory(test1);
		String result2 = SkyveContextListener.cleanupDirectory(test2);

		// verify the results
		assertThat(result1, is(test1));
		assertThat(result2, is(test2));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupContentDirectoryAppendsSlash() throws Exception {
		// setup the test data
		final String test1 = "C:/workspace/content",
				test2 = "C:/workspace/content/",
				test3 = "C:\\workspace\\content",
				test4 = "C:\\workspace\\content\\";
		final String expected = "C:/workspace/content/";

		// perform the method under test
		String result1 = SkyveContextListener.cleanupDirectory(test1);
		String result2 = SkyveContextListener.cleanupDirectory(test2);
		String result3 = SkyveContextListener.cleanupDirectory(test3);
		String result4 = SkyveContextListener.cleanupDirectory(test4);

		// verify the results
		assertThat(result1, is(expected));
		assertThat(result2, is(expected));
		assertThat(result3, is(expected));
		assertThat(result4, is(expected));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupModuleDirectoryHandlesEmptyStrings() throws Exception {
		// setup the test data
		final String test1 = null, test2 = "";

		// perform the method under test
		String result1 = SkyveContextListener.cleanupModuleDirectory(test1);
		String result2 = SkyveContextListener.cleanupModuleDirectory(test2);

		// verify the results
		assertThat(result1, is(test1));
		assertThat(result2, is(test2));
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testCleanupModuleDirectoryAppendsModules() throws Exception {
		// setup the test data
		final String test1 = "/src/main/java/",
				test2 = "/src/main/java",
				test3 = "\\src\\main\\java\\",
				test4 = "\\src\\main\\java";

		// perform the method under test
		String result1 = SkyveContextListener.cleanupModuleDirectory(test1);
		String result2 = SkyveContextListener.cleanupModuleDirectory(test2);
		String result3 = SkyveContextListener.cleanupModuleDirectory(test3);
		String result4 = SkyveContextListener.cleanupModuleDirectory(test4);

		// verify the results
		assertThat(result1, is(test1 + "modules/"));
		assertThat(result2, is(test2 + "/modules/"));
		assertThat(result3, is("\\src\\main\\java/modules/"));
		assertThat(result4, is(test4 + "/modules/"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCleanupModuleDirectoryWithModulesAppendsSlash() throws Exception {
		// setup the test data
		final String test1 = "/src/main/java/modules",
				test2 = "/src/main/java/modules/",
				test3 = "\\src\\main\\java\\modules",
				test4 = "\\src\\main\\java\\modules\\";

		// perform the method under test
		String result1 = SkyveContextListener.cleanupModuleDirectory(test1);
		String result2 = SkyveContextListener.cleanupModuleDirectory(test2);
		String result3 = SkyveContextListener.cleanupModuleDirectory(test3);
		String result4 = SkyveContextListener.cleanupModuleDirectory(test4);

		// verify the results
		assertThat(result1, is(test1 + "/"));
		assertThat(result2, is(test2));
		assertThat(result3, is(test3 + "/"));
		assertThat(result4, is("\\src\\main\\java\\modules" + "/"));
	}
}

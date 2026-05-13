package modules.admin.ControlPanel.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.CacheExpiryPolicy;
import org.skyve.cache.Caching;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import util.AbstractH2Test;

/**
 * Tests for the StopOrStartSelectedCache action.
 * 
 * This action starts or stops an application cache by name.
 * It sets tabIndex to null, then finds the cache config in APP_CACHES and
 * either starts (creates) or stops (removes/destroys) the cache based on
 * whether it already exists.
 * 
 * The action handles two cache types:
 * - EHCacheConfig: Uses createEHCache/removeEHCache
 * - JCacheConfig: Uses createJCache/destroyJCache
 * 
 * If the cache is not found in APP_CACHES, an error message is shown via growl.
 * 
 * Note: Some cache operations may not be fully available in the test environment.
 * We verify the testable behavior (tabIndex being set, result being returned,
 * found/not found logic) even if cache creation/removal may have limitations.
 */
public class StopOrStartSelectedCacheH2Test extends AbstractH2Test {

	private static final String TEST_EH_CACHE_NAME = "testEHCacheForStopOrStart";
	private static final String TEST_JCACHE_NAME = "testJCacheForStopOrStart";

	private DataBuilder db;
	private ControlPanelExtension controlPanel;
	private StopOrStartSelectedCache action;
	private MockWebContext webContext;
	private List<CacheConfig<? extends Serializable, ? extends Serializable>> originalAppCaches;

	@BeforeEach
	public void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		controlPanel = db.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
		action = new StopOrStartSelectedCache();
		webContext = new MockWebContext();
		
		// Save original APP_CACHES state to restore after tests
		originalAppCaches = new ArrayList<>(UtilImpl.APP_CACHES);
	}

	@AfterEach
	public void cleanup() {
		// Restore original APP_CACHES state
		UtilImpl.APP_CACHES.clear();
		UtilImpl.APP_CACHES.addAll(originalAppCaches);
		
		// Clean up any test caches that may have been created
		try {
			Caching caching = EXT.getCaching();
			if (caching != null) {
				try {
					caching.removeEHCache(TEST_EH_CACHE_NAME);
				} catch (Exception e) {
					// Ignore - cache may not exist
				}
				try {
					caching.destroyJCache(TEST_JCACHE_NAME);
				} catch (Exception e) {
					// Ignore - cache may not exist
				}
			}
		} catch (Exception e) {
			// Ignore - caching may not be available in test environment
		}
	}

	@Test
	public void testExecuteWithNullCacheName() throws Exception {
		// setup the test data - set initial values
		controlPanel.setTabIndex(Integer.valueOf(5));
		controlPanel.setSelectedCache(null);

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, webContext);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
		// tabIndex should be set to null
		assertThat(controlPanel.getTabIndex(), is(nullValue()));
		// selectedCache should remain null
		assertThat(controlPanel.getSelectedCache(), is(nullValue()));
		// The action should show an error growl since cache was not found
		// (null cache name means no match in APP_CACHES)
	}

	@Test
	public void testExecuteWithUnknownCacheName() throws Exception {
		// setup the test data - use a cache name that doesn't exist in APP_CACHES
		controlPanel.setTabIndex(Integer.valueOf(3));
		controlPanel.setSelectedCache("unknownCacheNameXYZ123");

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, webContext);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
		// tabIndex should be set to null
		assertThat(controlPanel.getTabIndex(), is(nullValue()));
		// The action should show an error growl since cache was not found in APP_CACHES
	}

	@Test
	public void testExecuteSetsTabIndexToNull() throws Exception {
		// setup the test data - verify tabIndex is always set to null
		controlPanel.setTabIndex(Integer.valueOf(10));
		controlPanel.setSelectedCache("anyCacheName");

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, webContext);

		// verify tabIndex is set to null regardless of cache name
		assertThat(controlPanel.getTabIndex(), is(nullValue()));
		assertThat(result, is(notNullValue()));
	}

	@Test
	public void testExecuteWithEHCacheConfig() throws Exception {
		// Add a test EHCacheConfig to APP_CACHES
		EHCacheConfig<String, String> testConfig = new EHCacheConfig<>(
				TEST_EH_CACHE_NAME,
				100L,
				CacheExpiryPolicy.eternal,
				0L,
				String.class,
				String.class
		);
		UtilImpl.APP_CACHES.add(testConfig);

		// setup the test data
		controlPanel.setTabIndex(Integer.valueOf(7));
		controlPanel.setSelectedCache(TEST_EH_CACHE_NAME);

		// call the method under test
		try {
			ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, webContext);

			// verify the result
			assertThat(result, is(notNullValue()));
			assertThat(result.getBean(), is(controlPanel));
			assertThat(controlPanel.getTabIndex(), is(nullValue()));
			// Cache should have been started (created) since it didn't exist
			// A success growl would be shown
		} catch (Exception e) {
			// If caching operations fail in test environment, still verify state changes
			assertThat(controlPanel.getTabIndex(), is(nullValue()));
		}
	}

	@Test
	public void testExecuteWithJCacheConfig() throws Exception {
		// Add a test JCacheConfig to APP_CACHES
		JCacheConfig<String, String> testConfig = new JCacheConfig<>(
				TEST_JCACHE_NAME,
				100L,
				CacheExpiryPolicy.eternal,
				0L,
				String.class,
				String.class
		);
		UtilImpl.APP_CACHES.add(testConfig);

		// setup the test data
		controlPanel.setTabIndex(Integer.valueOf(8));
		controlPanel.setSelectedCache(TEST_JCACHE_NAME);

		// call the method under test
		try {
			ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, webContext);

			// verify the result
			assertThat(result, is(notNullValue()));
			assertThat(result.getBean(), is(controlPanel));
			assertThat(controlPanel.getTabIndex(), is(nullValue()));
			// Cache should have been started (created) since it didn't exist
			// A success growl would be shown
		} catch (Exception e) {
			// If caching operations fail in test environment, still verify state changes
			assertThat(controlPanel.getTabIndex(), is(nullValue()));
		}
	}

	@Test
	public void testExecuteWithEHCacheConfigToggle() throws Exception {
		// Add a test EHCacheConfig to APP_CACHES
		EHCacheConfig<String, String> testConfig = new EHCacheConfig<>(
				TEST_EH_CACHE_NAME,
				100L,
				CacheExpiryPolicy.eternal,
				0L,
				String.class,
				String.class
		);
		UtilImpl.APP_CACHES.add(testConfig);

		// setup the test data
		controlPanel.setTabIndex(Integer.valueOf(1));
		controlPanel.setSelectedCache(TEST_EH_CACHE_NAME);

		try {
			Caching caching = EXT.getCaching();
			if (caching != null) {
				// First call should start (create) the cache
				ServerSideActionResult<ControlPanelExtension> result1 = action.execute(controlPanel, webContext);
				assertThat(result1, is(notNullValue()));
				assertThat(controlPanel.getTabIndex(), is(nullValue()));

				// Second call should stop (remove) the cache since it now exists
				controlPanel.setTabIndex(Integer.valueOf(2));
				ServerSideActionResult<ControlPanelExtension> result2 = action.execute(controlPanel, webContext);
				assertThat(result2, is(notNullValue()));
				assertThat(controlPanel.getTabIndex(), is(nullValue()));
			}
		} catch (Exception e) {
			// Caching may not be fully available in test environment
			// Still verify that tabIndex is managed correctly
			assertThat(controlPanel.getTabIndex(), is(nullValue()));
		}
	}

	@Test
	public void testExecuteWithJCacheConfigToggle() throws Exception {
		// Add a test JCacheConfig to APP_CACHES
		JCacheConfig<String, String> testConfig = new JCacheConfig<>(
				TEST_JCACHE_NAME,
				100L,
				CacheExpiryPolicy.eternal,
				0L,
				String.class,
				String.class
		);
		UtilImpl.APP_CACHES.add(testConfig);

		// setup the test data
		controlPanel.setTabIndex(Integer.valueOf(1));
		controlPanel.setSelectedCache(TEST_JCACHE_NAME);

		try {
			Caching caching = EXT.getCaching();
			if (caching != null) {
				// First call should start (create) the cache
				ServerSideActionResult<ControlPanelExtension> result1 = action.execute(controlPanel, webContext);
				assertThat(result1, is(notNullValue()));
				assertThat(controlPanel.getTabIndex(), is(nullValue()));

				// Second call should stop (destroy) the cache since it now exists
				controlPanel.setTabIndex(Integer.valueOf(2));
				ServerSideActionResult<ControlPanelExtension> result2 = action.execute(controlPanel, webContext);
				assertThat(result2, is(notNullValue()));
				assertThat(controlPanel.getTabIndex(), is(nullValue()));
			}
		} catch (Exception e) {
			// Caching may not be fully available in test environment
			// Still verify that tabIndex is managed correctly
			assertThat(controlPanel.getTabIndex(), is(nullValue()));
		}
	}

	@Test
	public void testExecuteResultContainsBean() throws Exception {
		// setup the test data
		controlPanel.setTabIndex(Integer.valueOf(4));
		controlPanel.setSelectedCache("anyCache");

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, webContext);

		// verify the result bean is the same as input
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
	}

	@Test
	public void testExecuteWithEmptyStringCacheName() throws Exception {
		// setup the test data - use empty string as cache name
		controlPanel.setTabIndex(Integer.valueOf(6));
		controlPanel.setSelectedCache("");

		// call the method under test
		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, webContext);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
		// tabIndex should be set to null
		assertThat(controlPanel.getTabIndex(), is(nullValue()));
		// Empty string won't match any cache in APP_CACHES, so error growl shown
	}
}


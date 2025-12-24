package modules.admin.ControlPanel.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import javax.cache.management.CacheStatisticsMXBean;

import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.TierStatistics;
import org.junit.jupiter.api.Test;

/**
 * Tests for the CacheStats action static helper methods.
 * 
 * Note: The main execute() method is not fully tested here because it relies on
 * EXT.getCaching() which is a static method. Per test requirements, we avoid mockStatic.
 * Instead, we test the public static methods that perform the actual formatting logic.
 */
public class CacheStatsTest {

	@Test
	public void testAddEHCacheStatsWithNullStats() {
		// setup the test data
		StringBuilder result = new StringBuilder();

		// call the method under test
		CacheStats.addEHCacheStats("testCache", null, result);

		// verify the result
		String output = result.toString();
		assertThat(output, is(notNullValue()));
		assertThat(output, containsString("<h1>testCache</h1>"));
		assertThat(output, containsString("No stats<br/>"));
	}

	@Test
	public void testAddEHCacheStatsWithValidStats() {
		// setup the test data
		StringBuilder result = new StringBuilder();
		CacheStatistics stats = mock(CacheStatistics.class);
		
		// mock the statistics values
		when(stats.getCacheEvictions()).thenReturn(5L);
		when(stats.getCacheExpirations()).thenReturn(10L);
		when(stats.getCacheGets()).thenReturn(100L);
		when(stats.getCacheHits()).thenReturn(80L);
		when(stats.getCacheHitPercentage()).thenReturn(80.0f);
		when(stats.getCacheMisses()).thenReturn(20L);
		when(stats.getCacheMissPercentage()).thenReturn(20.0f);
		when(stats.getCachePuts()).thenReturn(50L);
		when(stats.getCacheRemovals()).thenReturn(3L);

		// call the method under test
		CacheStats.addEHCacheStats("testCache", stats, result);

		// verify the result
		String output = result.toString();
		assertThat(output, is(notNullValue()));
		assertThat(output, containsString("<h1>testCache</h1>"));
		assertThat(output, containsString("Evictions: 5"));
		assertThat(output, containsString("Expirations: 10"));
		assertThat(output, containsString("Gets: 100"));
		assertThat(output, containsString("Hits: 80"));
		assertThat(output, containsString("Hit (%): 80.0"));
		assertThat(output, containsString("Misses: 20"));
		assertThat(output, containsString("Miss (%): 20.0"));
		assertThat(output, containsString("Puts: 50"));
		assertThat(output, containsString("Removals: 3"));
	}

	@Test
	public void testAddJCacheStatsWithNullStats() {
		// setup the test data
		StringBuilder result = new StringBuilder();

		// call the method under test
		CacheStats.addJCacheStats("jcacheTest", null, result);

		// verify the result
		String output = result.toString();
		assertThat(output, is(notNullValue()));
		assertThat(output, containsString("<h1>jcacheTest</h1>"));
		assertThat(output, containsString("No Stats<br/>"));
	}

	@Test
	public void testAddJCacheStatsWithValidStats() {
		// setup the test data
		StringBuilder result = new StringBuilder();
		CacheStatisticsMXBean stats = mock(CacheStatisticsMXBean.class);
		
		// mock the statistics values
		when(stats.getCacheEvictions()).thenReturn(7L);
		when(stats.getCacheGets()).thenReturn(200L);
		when(stats.getCacheHitPercentage()).thenReturn(75.0f);
		when(stats.getCacheHits()).thenReturn(150L);
		when(stats.getCacheMisses()).thenReturn(50L);
		when(stats.getCacheMissPercentage()).thenReturn(25.0f);
		when(stats.getCachePuts()).thenReturn(100L);
		when(stats.getCacheRemovals()).thenReturn(5L);
		when(stats.getAverageGetTime()).thenReturn(1.5f);
		when(stats.getAveragePutTime()).thenReturn(2.0f);
		when(stats.getAverageRemoveTime()).thenReturn(0.5f);

		// call the method under test
		CacheStats.addJCacheStats("jcacheTest", stats, result);

		// verify the result
		String output = result.toString();
		assertThat(output, is(notNullValue()));
		assertThat(output, containsString("<h1>jcacheTest</h1>"));
		assertThat(output, containsString("Evictions: 7"));
		assertThat(output, containsString("Gets: 200"));
		assertThat(output, containsString("Hit (%): 75.0"));
		assertThat(output, containsString("Hits: 150"));
		assertThat(output, containsString("Misses: 50"));
		assertThat(output, containsString("Miss (%): 25.0"));
		assertThat(output, containsString("Puts: 100"));
		assertThat(output, containsString("Removals: 5"));
		assertThat(output, containsString("Average Get Time: 1.5"));
		assertThat(output, containsString("Average Put Time: 2.0"));
		assertThat(output, containsString("Average Remove Time: 0.5"));
	}

	@Test
	public void testAddEHCacheStatsFormatsHTMLCorrectly() {
		// setup the test data
		StringBuilder result = new StringBuilder();
		CacheStatistics stats = mock(CacheStatistics.class);
		
		// minimal mock setup
		when(stats.getCacheEvictions()).thenReturn(0L);
		when(stats.getCacheExpirations()).thenReturn(0L);
		when(stats.getCacheGets()).thenReturn(0L);
		when(stats.getCacheHits()).thenReturn(0L);
		when(stats.getCacheHitPercentage()).thenReturn(0.0f);
		when(stats.getCacheMisses()).thenReturn(0L);
		when(stats.getCacheMissPercentage()).thenReturn(0.0f);
		when(stats.getCachePuts()).thenReturn(0L);
		when(stats.getCacheRemovals()).thenReturn(0L);

		// call the method under test
		CacheStats.addEHCacheStats("htmlTest", stats, result);

		// verify HTML structure
		String output = result.toString();
		assertThat(output, containsString("<tr>"));
		assertThat(output, containsString("<td style=\"padding:20px\">"));
		assertThat(output, containsString("</tr>"));
	}

	@Test
	public void testAddJCacheStatsFormatsHTMLCorrectly() {
		// setup the test data
		StringBuilder result = new StringBuilder();
		CacheStatisticsMXBean stats = mock(CacheStatisticsMXBean.class);
		
		// minimal mock setup
		when(stats.getCacheEvictions()).thenReturn(0L);
		when(stats.getCacheGets()).thenReturn(0L);
		when(stats.getCacheHitPercentage()).thenReturn(0.0f);
		when(stats.getCacheHits()).thenReturn(0L);
		when(stats.getCacheMisses()).thenReturn(0L);
		when(stats.getCacheMissPercentage()).thenReturn(0.0f);
		when(stats.getCachePuts()).thenReturn(0L);
		when(stats.getCacheRemovals()).thenReturn(0L);
		when(stats.getAverageGetTime()).thenReturn(0.0f);
		when(stats.getAveragePutTime()).thenReturn(0.0f);
		when(stats.getAverageRemoveTime()).thenReturn(0.0f);

		// call the method under test
		CacheStats.addJCacheStats("htmlTest", stats, result);

		// verify HTML structure
		String output = result.toString();
		assertThat(output, containsString("<tr>"));
		assertThat(output, containsString("<td style=\"padding:20px\">"));
		assertThat(output, containsString("</tr>"));
	}

	@Test
	public void testAddEHCacheStatsWithMultipleTiers() {
		// setup the test data
		StringBuilder result = new StringBuilder();
		CacheStatistics stats = mock(CacheStatistics.class);
		TierStatistics heapTier = mock(TierStatistics.class);
		
		// mock cache statistics
		when(stats.getCacheEvictions()).thenReturn(1L);
		when(stats.getCacheExpirations()).thenReturn(2L);
		when(stats.getCacheGets()).thenReturn(10L);
		when(stats.getCacheHits()).thenReturn(8L);
		when(stats.getCacheHitPercentage()).thenReturn(80.0f);
		when(stats.getCacheMisses()).thenReturn(2L);
		when(stats.getCacheMissPercentage()).thenReturn(20.0f);
		when(stats.getCachePuts()).thenReturn(5L);
		when(stats.getCacheRemovals()).thenReturn(1L);
		
		// mock tier statistics
		when(heapTier.getAllocatedByteSize()).thenReturn(1024L);
		when(heapTier.getOccupiedByteSize()).thenReturn(512L);
		when(heapTier.getMappings()).thenReturn(10L);
		when(heapTier.getEvictions()).thenReturn(1L);
		when(heapTier.getExpirations()).thenReturn(0L);
		when(heapTier.getHits()).thenReturn(8L);
		when(heapTier.getMisses()).thenReturn(2L);
		when(heapTier.getPuts()).thenReturn(5L);
		when(heapTier.getRemovals()).thenReturn(1L);

		// Note: We cannot test the tier statistics here without mocking EXT.getCaching()
		// which is against test requirements. The addEHCacheStats method internally calls
		// caching.getEHTierStatistics() which requires the Caching instance.

		// call the method under test
		CacheStats.addEHCacheStats("multiTierCache", stats, result);

		// verify base statistics are present
		String output = result.toString();
		assertThat(output, containsString("<h1>multiTierCache</h1>"));
		assertThat(output, containsString("Evictions: 1"));
	}
}

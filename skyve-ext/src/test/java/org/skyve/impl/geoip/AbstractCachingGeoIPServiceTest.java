package org.skyve.impl.geoip;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.ehcache.Cache;
import org.ehcache.PersistentCacheManager;
import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.cache.GeoIPCacheConfig;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.IPGeolocation;

@SuppressWarnings({ "static-method", "resource" })
public class AbstractCachingGeoIPServiceTest {
	@Test
	public void geolocateReturnsCachedValueWithoutCallingDelegate() throws Exception {
		DefaultCaching caching = DefaultCaching.get();
		PersistentCacheManager ehManager = mock(PersistentCacheManager.class);
		Cache<String, IPGeolocation> cache = mock(Cache.class);
		GeoIPCacheConfig originalConfig = UtilImpl.GEO_IP_CACHE;
		Object originalEhManager = getField(caching, "ehCacheManager");

		IPGeolocation cached = new IPGeolocation("Adelaide",
				"SA",
				"AU",
				new GeometryFactory().createPoint(new Coordinate(138.6d, -34.9d)));
		CountingGeoIPService service = new CountingGeoIPService(IPGeolocation.EMPTY);

		try {
			UtilImpl.GEO_IP_CACHE = new GeoIPCacheConfig(10L, 0L, 0L, 5);
			setField(caching, "ehCacheManager", ehManager);
			when(ehManager.getCache(UtilImpl.GEO_IP_CACHE.getName(), String.class, IPGeolocation.class)).thenReturn(cache);
			when(cache.get("203.0.113.21")).thenReturn(cached);

			IPGeolocation result = service.geolocate("203.0.113.21");

			assertSame(cached, result);
			assertEquals(0, service.invocations);
			verify(cache).get("203.0.113.21");
			verifyNoMoreInteractions(cache);
		}
		finally {
			UtilImpl.GEO_IP_CACHE = originalConfig;
			setField(caching, "ehCacheManager", originalEhManager);
		}
	}

	@Test
	public void geolocateCachesDelegateValueOnMiss() throws Exception {
		DefaultCaching caching = DefaultCaching.get();
		PersistentCacheManager ehManager = mock(PersistentCacheManager.class);
		Cache<String, IPGeolocation> cache = mock(Cache.class);
		GeoIPCacheConfig originalConfig = UtilImpl.GEO_IP_CACHE;
		Object originalEhManager = getField(caching, "ehCacheManager");

		IPGeolocation computed = new IPGeolocation("Melbourne",
				"VIC",
				"AU",
				new GeometryFactory().createPoint(new Coordinate(144.9d, -37.8d)));
		CountingGeoIPService service = new CountingGeoIPService(computed);

		try {
			UtilImpl.GEO_IP_CACHE = new GeoIPCacheConfig(10L, 0L, 0L, 5);
			setField(caching, "ehCacheManager", ehManager);
			when(ehManager.getCache(UtilImpl.GEO_IP_CACHE.getName(), String.class, IPGeolocation.class)).thenReturn(cache);
			when(cache.get("203.0.113.22")).thenReturn(null);

			IPGeolocation result = service.geolocate("203.0.113.22");

			assertSame(computed, result);
			assertEquals(1, service.invocations);
			verify(cache).get("203.0.113.22");
			verify(cache).put("203.0.113.22", computed);
		}
		finally {
			UtilImpl.GEO_IP_CACHE = originalConfig;
			setField(caching, "ehCacheManager", originalEhManager);
		}
	}

	private static Object getField(Object target, String name) throws Exception {
		Field field = DefaultCaching.class.getDeclaredField(name);
		field.setAccessible(true);
		return field.get(target);
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		Field field = DefaultCaching.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}

	private static final class CountingGeoIPService extends AbstractCachingGeoIPService {
		private final IPGeolocation result;
		private int invocations;

		private CountingGeoIPService(IPGeolocation result) {
			this.result = result;
		}

		@Override
		protected IPGeolocation doGeolocation(String ipAddress) {
			invocations++;
			return result;
		}
	}
}
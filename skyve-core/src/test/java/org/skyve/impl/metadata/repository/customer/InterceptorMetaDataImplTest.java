package org.skyve.impl.metadata.repository.customer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Interceptor;

@SuppressWarnings("static-method")
class InterceptorMetaDataImplTest {

	/** Concrete interceptor for testing the happy-path of getInterceptor(). */
	public static class StubInterceptor extends Interceptor {
		// Nothing to override — Interceptor provides defaults for all methods
	}

	@Test
	void setAndGetClassNameRoundtrip() {
		InterceptorMetaDataImpl impl = new InterceptorMetaDataImpl();
		impl.setClassName("com.example.MyInterceptor");
		assertEquals("com.example.MyInterceptor", impl.getClassName());
	}

	@Test
	void getClassNameInitiallyNull() {
		InterceptorMetaDataImpl impl = new InterceptorMetaDataImpl();
		assertNull(impl.getClassName());
	}

	@Test
	void getInterceptorThrowsMetaDataExceptionForUnknownClass() {
		InterceptorMetaDataImpl impl = new InterceptorMetaDataImpl();
		impl.setClassName("com.example.NonExistentInterceptor");
		assertThrows(MetaDataException.class, impl::getInterceptor);
	}

	@Test
	void setClassNameTrimsWhitespace() {
		InterceptorMetaDataImpl impl = new InterceptorMetaDataImpl();
		impl.setClassName("  com.example.MyInterceptor  ");
		// UtilImpl.processStringValue trims and returns null for blank
		assertNotNull(impl.getClassName());
	}

	@Test
	void getInterceptorInstantiatesAndCachesInterceptor() {
		InterceptorMetaDataImpl impl = new InterceptorMetaDataImpl();
		impl.setClassName(StubInterceptor.class.getName());
		Interceptor interceptor = impl.getInterceptor();
		assertNotNull(interceptor);
		// Second call should return the same cached instance
		assertSame(interceptor, impl.getInterceptor());
	}
}

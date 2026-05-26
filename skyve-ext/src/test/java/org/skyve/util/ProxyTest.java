package org.skyve.util;

import static org.junit.Assert.assertSame;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.lang.reflect.Method;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;

@SuppressWarnings("static-method")
class ProxyTest {
	public static class Counter {
		private int value = 0;

		public int getValue() {
			return value;
		}

		public void increment() {
			value++;
		}

		public void throwingMethod() {
			throw new IllegalArgumentException("original-cause");
		}
	}

	@Test
	void proxyDelegateInvokeCallsIncrementOnRealObject() throws Throwable {
		Counter counter = new Counter();
		ProxyDelegate<Counter> delegate = new ProxyDelegate<>();
		Method method = Counter.class.getMethod("increment");
		delegate.invoke(counter, counter, method, new Object[0]);
		assertEquals(1, counter.getValue());
	}

	@Test
	void isProxyReturnsFalseForNonProxy() {
		Counter counter = new Counter();
		assertFalse(Proxy.isProxy(counter));
	}

	@Test
	void isProxyReturnsFalseForNull() {
		assertFalse(Proxy.isProxy(null));
	}

	@Test
	void proxyDelegateRethrowsCauseOfInvocationTargetException() throws Throwable {
		// covers ProxyDelegate L34-35: InvocationTargetException catch re-throws e.getCause()
		Counter counter = new Counter();
		ProxyDelegate<Counter> delegate = new ProxyDelegate<>();
		Method method = Counter.class.getMethod("throwingMethod");
		try {
			delegate.invoke(counter, counter, method, new Object[0]);
			fail("Expected exception");
		} catch (IllegalArgumentException e) {
			assertEquals("original-cause", e.getMessage());
		}
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	void ofCreatesProxyWithDelegateMethodCalls() {
		Counter counter = new Counter();
		Counter proxy = Proxy.of(counter, new ProxyDelegate<>());
		proxy.increment();
		// Default delegate calls through to proxied object
		assertEquals(1, counter.getValue());
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	void ofCreatesProxyWithOverriddenDelegate() {
		Counter counter = new Counter();
		Counter proxy = Proxy.of(counter, new ProxyDelegate<Counter>() {
			private static final long serialVersionUID = 1L;
			@Override
			public Object invoke(Counter p, Counter proxied, Method method, Object[] args) throws Throwable {
				if ("getValue".equals(method.getName())) {
					return Integer.valueOf(99);
				}
				return super.invoke(p, proxied, method, args);
			}
		});
		assertEquals(99, proxy.getValue());
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	void ofTransientCreatesProxyDelegatingThroughToOriginal() {
		Counter counter = new Counter();
		Counter proxy = Proxy.ofTransient(counter, new ProxyDelegate<>());
		proxy.increment();
		assertEquals(1, counter.getValue());
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	void deproxyReturnsOriginalObject() {
		Counter counter = new Counter();
		Counter proxy = Proxy.of(counter, new ProxyDelegate<>());
		assertSame(counter, Proxy.deproxy(proxy));
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	void reproxySetsNewProxiedObject() {
		Counter counter = new Counter();
		Counter proxy = Proxy.of(counter, new ProxyDelegate<>());
		Counter replacement = new Counter();
		Proxy.reproxy(proxy, replacement);
		assertSame(replacement, Proxy.deproxy(proxy));
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	void isProxyReturnsTrueForProxy() {
		Counter counter = new Counter();
		Counter proxy = Proxy.of(counter, new ProxyDelegate<>());
		assertTrue(Proxy.isProxy(proxy));
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	void getDelegateReturnsOriginalDelegate() {
		Counter counter = new Counter();
		ProxyDelegate<Counter> delegate = new ProxyDelegate<>();
		Counter proxy = Proxy.of(counter, delegate);
		assertSame(delegate, Proxy.getDelegate(proxy));
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	void ofProxyThrowsIllegalArgumentException() {
		// Covers Proxy.java line 109-110: proxying an already-proxied object throws
		Counter counter = new Counter();
		Counter proxy = Proxy.of(counter, new ProxyDelegate<>());
		boolean threw = false;
		try {
			Proxy.of(proxy, new ProxyDelegate<>());
		}
		catch (@SuppressWarnings("unused") IllegalArgumentException e) {
			threw = true;
		}
		assertTrue(threw, "Expected IllegalArgumentException when proxying a proxy");
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	public void deproxyOnNonProxyThrowsDomainException() {
		// Covers Proxy.java lines 185-186: deproxy on a non-proxy throws DomainException
		Counter counter = new Counter();
		boolean threw = false;
		try {
			Proxy.deproxy(counter);
		}
		catch (@SuppressWarnings("unused") DomainException e) {
			threw = true;
		}
		assertTrue(threw, "Expected DomainException when deproxying a non-proxy");
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	void reproxyOnNonProxyThrowsDomainException() {
		// Covers Proxy.java lines 200-201: reproxy on a non-proxy throws DomainException
		Counter counter = new Counter();
		Counter other = new Counter();
		boolean threw = false;
		try {
			Proxy.reproxy(counter, other);
		}
		catch (@SuppressWarnings("unused") DomainException e) {
			threw = true;
		}
		assertTrue(threw, "Expected DomainException when reproxying a non-proxy");
	}

	@Disabled("ByteBuddy requires --add-opens java.base/java.lang not set in surefire")
	@Test
	void getDelegateOnNonProxyThrowsDomainException() {
		// Covers Proxy.java lines 237-238: getDelegate on a non-proxy throws DomainException
		Counter counter = new Counter();
		boolean threw = false;
		try {
			Proxy.getDelegate(counter);
		}
		catch (@SuppressWarnings("unused") DomainException e) {
			threw = true;
		}
		assertTrue(threw, "Expected DomainException when getting delegate from a non-proxy");
	}
}

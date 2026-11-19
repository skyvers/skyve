package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;

@SuppressWarnings({"static-method", "java:S116", "java:S5778"})
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

	public static class FieldCompatibleProxy extends Counter {
		public Counter _skyve_proxied;
		public ProxyDelegate<Counter> _skyve_delegate;
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
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
				() -> delegate.invoke(counter, counter, method, new Object[0]));
		assertEquals("original-cause", e.getMessage());
	}

	@Test
	void isProxyReturnsTrueForObjectWithProxyField() {
		assertTrue(Proxy.isProxy(new FieldCompatibleProxy()));
	}

	@Test
	void deproxyReturnsFieldCompatibleProxiedObject() {
		Counter counter = new Counter();
		FieldCompatibleProxy proxy = new FieldCompatibleProxy();
		proxy._skyve_proxied = counter;

		assertSame(counter, Proxy.deproxy((Counter) proxy));
	}

	@Test
	void reproxySetsFieldCompatibleProxiedObject() {
		Counter replacement = new Counter();
		FieldCompatibleProxy proxy = new FieldCompatibleProxy();

		Proxy.reproxy(proxy, replacement);

		assertSame(replacement, proxy._skyve_proxied);
	}

	@Test
	void getDelegateReturnsFieldCompatibleDelegate() {
		ProxyDelegate<Counter> delegate = new ProxyDelegate<>();
		FieldCompatibleProxy proxy = new FieldCompatibleProxy();
		proxy._skyve_delegate = delegate;

		assertSame(delegate, Proxy.getDelegate((Counter) proxy));
	}

	@Test
	void ofRejectsFieldCompatibleProxyBeforeCreatingByteBuddyProxy() {
		FieldCompatibleProxy proxy = new FieldCompatibleProxy();

		assertThrows(IllegalArgumentException.class, () -> Proxy.of(proxy, new ProxyDelegate<>()));
	}

	@Test
	void deproxyOnPlainObjectThrowsDomainException() {
		assertThrows(DomainException.class, () -> Proxy.deproxy(new Counter()));
	}

	@Test
	void reproxyOnPlainObjectThrowsDomainException() {
		assertThrows(DomainException.class, () -> Proxy.reproxy(new Counter(), new Counter()));
	}

	@Test
	void getDelegateOnPlainObjectThrowsDomainException() {
		assertThrows(DomainException.class, () -> Proxy.getDelegate(new Counter()));
	}

	@Test
	void ofCreatesProxyWithDelegateMethodCalls() {
		Counter counter = new Counter();
		Counter proxy = Proxy.of(counter, new ProxyDelegate<>());
		proxy.increment();
		// Default delegate calls through to proxied object
		assertEquals(1, counter.getValue());
	}

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

	@Test
	void ofTransientCreatesProxyDelegatingThroughToOriginal() {
		Counter counter = new Counter();
		Counter proxy = Proxy.ofTransient(counter, new ProxyDelegate<>());
		proxy.increment();
		assertEquals(1, counter.getValue());
	}

	@Test
	void deproxyReturnsOriginalObject() {
		Counter counter = new Counter();
		Counter proxy = Proxy.of(counter, new ProxyDelegate<>());
		assertSame(counter, Proxy.deproxy(proxy));
	}

	@Test
	void reproxySetsNewProxiedObject() {
		Counter counter = new Counter();
		Counter proxy = Proxy.of(counter, new ProxyDelegate<>());
		Counter replacement = new Counter();
		Proxy.reproxy(proxy, replacement);
		assertSame(replacement, Proxy.deproxy(proxy));
	}

	@Test
	void isProxyReturnsTrueForProxy() {
		Counter counter = new Counter();
		Counter proxy = Proxy.of(counter, new ProxyDelegate<>());
		assertTrue(Proxy.isProxy(proxy));
	}

	@Test
	void getDelegateReturnsOriginalDelegate() {
		Counter counter = new Counter();
		ProxyDelegate<Counter> delegate = new ProxyDelegate<>();
		Counter proxy = Proxy.of(counter, delegate);
		assertSame(delegate, Proxy.getDelegate(proxy));
	}

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

	@Test
	void deproxyOnNonProxyThrowsDomainException() {
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

	// ---- one proxy class serves every delegate ----

	/** Proxied by {@link #aSpecificDelegateFirstDoesNotStopOtherDelegatesBeingUsed()} only, so that test decides which delegate is first. */
	public static class SpecificFirst extends Counter {
		// nothing to add
	}

	/** Proxied by {@link #transientProxiesAcceptAnyDelegateToo()} only. */
	public static class TransientSpecificFirst extends Counter {
		// nothing to add
	}

	public static class DoublingDelegate<T> extends ProxyDelegate<T> {
		private static final long serialVersionUID = 1L;

		@Override
		public Object invoke(T proxy, T proxied, Method method, Object[] args) throws Throwable {
			Object result = super.invoke(proxy, proxied, method, args);
			return (result instanceof Integer value) ? Integer.valueOf(value.intValue() * 2) : result;
		}
	}

	public static class NegatingDelegate<T> extends ProxyDelegate<T> {
		private static final long serialVersionUID = 1L;

		@Override
		public Object invoke(T proxy, T proxied, Method method, Object[] args) throws Throwable {
			Object result = super.invoke(proxy, proxied, method, args);
			return (result instanceof Integer value) ? Integer.valueOf(- value.intValue()) : result;
		}
	}

	private static <T extends Counter> T incremented(T counter) {
		counter.increment();
		return counter;
	}

	@Test
	void aSpecificDelegateFirstDoesNotStopOtherDelegatesBeingUsed() {
		SpecificFirst doubled = Proxy.of(incremented(new SpecificFirst()), new DoublingDelegate<>());
		SpecificFirst plain = Proxy.of(incremented(new SpecificFirst()), new ProxyDelegate<>());
		SpecificFirst negated = Proxy.of(incremented(new SpecificFirst()), new NegatingDelegate<>());
		assertEquals(2, doubled.getValue());
		assertEquals(1, plain.getValue());
		assertEquals(-1, negated.getValue());
		// the three proxies share the one generated class
		assertSame(doubled.getClass(), plain.getClass());
		assertSame(doubled.getClass(), negated.getClass());
		assertSame(SpecificFirst.class, doubled.getClass().getSuperclass());
	}

	@Test
	void transientProxiesAcceptAnyDelegateToo() {
		TransientSpecificFirst negated = Proxy.ofTransient(incremented(new TransientSpecificFirst()), new NegatingDelegate<>());
		TransientSpecificFirst plain = Proxy.ofTransient(incremented(new TransientSpecificFirst()), new ProxyDelegate<>());
		TransientSpecificFirst doubled = Proxy.ofTransient(incremented(new TransientSpecificFirst()), new DoublingDelegate<>());
		assertEquals(-1, negated.getValue());
		assertEquals(1, plain.getValue());
		assertEquals(2, doubled.getValue());
		assertSame(negated.getClass(), doubled.getClass());
	}

	@Test
	void transientAndSerializableProxiesOfAClassAreDifferentClasses() {
		Counter serializable = Proxy.of(new Counter(), new ProxyDelegate<>());
		Counter transientProxied = Proxy.ofTransient(new Counter(), new ProxyDelegate<>());
		assertNotSame(serializable.getClass(), transientProxied.getClass());
		assertSame(serializable.getClass(), Proxy.of(new Counter(), new DoublingDelegate<>()).getClass());
	}

	// ---- where the proxy class is defined ----

	@Test
	void theProxyClassIsDefinedBesideTheProxiedClass() {
		Counter proxy = Proxy.of(new Counter(), new ProxyDelegate<>());
		Class<?> proxyClass = proxy.getClass();
		assertSame(Counter.class.getClassLoader(), proxyClass.getClassLoader());
		assertEquals(Counter.class.getPackageName(), proxyClass.getPackageName());
		assertTrue(java.io.Serializable.class.isAssignableFrom(proxyClass));
	}

	@Test
	void aTransientProxySurvivesSerialization() throws Exception {
		Counter counter = incremented(new Counter());
		Counter proxy = Proxy.ofTransient(counter, new DoublingDelegate<>());
		assertEquals(2, proxy.getValue());

		// the proxy class is Serializable although Counter is not
		java.io.Serializable serializable = (java.io.Serializable) proxy;
		java.io.ByteArrayOutputStream bytes = new java.io.ByteArrayOutputStream();
		try (java.io.ObjectOutputStream out = new java.io.ObjectOutputStream(bytes)) {
			out.writeObject(serializable);
		}
		Counter restored;
		try (java.io.ObjectInputStream in = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(bytes.toByteArray()))) {
			restored = (Counter) in.readObject();
		}

		// the proxy class was found again, the delegate came with it and the transient proxied object did not
		assertSame(proxy.getClass(), restored.getClass());
		assertTrue(Proxy.isProxy(restored));
		assertTrue(Proxy.getDelegate(restored) instanceof DoublingDelegate);
		assertNull(Proxy.deproxy(restored));
		Proxy.reproxy(restored, counter);
		assertEquals(2, restored.getValue());
	}

	/**
	 * A class that cannot be looked up into has its proxy class injected reflectively,
	 * which the JVM only allows when it is started with {@code --add-opens java.base/java.lang=ALL-UNNAMED}.
	 *
	 * @return whether the JVM allowed the injection
	 */
	private static boolean injectionAllowed(Runnable proxying) {
		try {
			proxying.run();
			return true;
		}
		catch (UnsupportedOperationException e) {
			assertTrue(e.getMessage().contains("Cannot define class using reflection"), e.getMessage());
			return false;
		}
	}

	@Test
	void aBootstrapClassIsProxiedByInjection() {
		assertNull(java.util.ArrayList.class.getClassLoader());
		java.util.ArrayList<String> list = new java.util.ArrayList<>();
		injectionAllowed(() -> {
			java.util.ArrayList<String> proxy = Proxy.of(list, new ProxyDelegate<>());
			assertTrue(Proxy.isProxy(proxy));
			assertSame(list, Proxy.deproxy(proxy));
			proxy.add("through the proxy");
			assertEquals(1, list.size());
		});
	}

	@Test
	@SuppressWarnings("java:S8714") // more than one throwable is an acceptable outcome, which assertThrows() cannot express
	void aClassInAModuleThatIsNotOpenFallsBackToInjection() {
		// java.sql.Timestamp has a class loader, but java.sql does not open its packages, so it cannot be looked up into.
		assertNotNull(java.sql.Timestamp.class.getClassLoader());
		java.sql.Timestamp timestamp = new java.sql.Timestamp(0L);
		try {
			Proxy.of(timestamp, new ProxyDelegate<>());
			fail("A class in the platform class loader is not expected to be proxied");
		}
		catch (UnsupportedOperationException e) {
			// the JVM was not started with the option that allows injection
			assertTrue(e.getMessage().contains("Cannot define class using reflection"), e.getMessage());
		}
		catch (NoClassDefFoundError e) {
			// A known limit: where injection is allowed, the proxy class lands in the platform class loader,
			// which cannot see Skyve's ProxyDelegate, so the proxy class cannot be used.
			assertTrue(e.getMessage().contains("ProxyDelegate"), e.getMessage());
		}
	}
}

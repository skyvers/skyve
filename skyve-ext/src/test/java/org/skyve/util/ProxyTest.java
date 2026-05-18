package org.skyve.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.lang.reflect.Method;

import org.junit.Test;

@SuppressWarnings("static-method")
public class ProxyTest {

	public static class Counter {
		private int value = 0;

		public int getValue() {
			return value;
		}

		public void increment() {
			value++;
		}
	}

	@Test
	public void proxyDelegateDefaultInvokeCallsRealMethod() throws Throwable {
		Counter counter = new Counter();
		ProxyDelegate<Counter> delegate = new ProxyDelegate<>();
		Method method = Counter.class.getMethod("getValue");
		Object result = delegate.invoke(counter, counter, method, new Object[0]);
		assertEquals(Integer.valueOf(0), result);
	}

	@Test
	public void proxyDelegateInvokeCallsIncrementOnRealObject() throws Throwable {
		Counter counter = new Counter();
		ProxyDelegate<Counter> delegate = new ProxyDelegate<>();
		Method method = Counter.class.getMethod("increment");
		delegate.invoke(counter, counter, method, new Object[0]);
		assertEquals(1, counter.getValue());
	}

	@Test
	public void isProxyReturnsFalseForNonProxy() {
		Counter counter = new Counter();
		assertFalse(Proxy.isProxy(counter));
	}

	@Test
	public void isProxyReturnsFalseForNull() {
		assertFalse(Proxy.isProxy(null));
	}
}

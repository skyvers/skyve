package org.skyve.util;

import java.io.Serializable;
import java.lang.reflect.Method;

/**
 * A class to accept and process delegated method calls from a Proxy.
 * Extend this class and override the invoke method to implement behaviour around the delegated method calls from the proxy.
 * Extension classes can be stateful as each proxy can be created with a new instance of a delegate.
 * Delegates need to be able to be serialized.
 * 
 * @author mike
 *
 * @param <T>
 */
public class ProxyDelegate <T extends Object> implements Serializable {
	private static final long serialVersionUID = 7580433700386384620L;

	/**
	 * Override this method to do stuff around the public method calls.
	 * Call super.invoke() to make the proxied method call.
	 * @param proxy	The proxy object.
	 * @param proxied	The proxied object.
	 * @param method	The method that will be called.
	 * @param args	The arguments to call the method with.
	 * @return	The call result (if applicable)
	 * @throws Throwable
	 */
	public Object invoke(T proxy, T proxied, Method method, Object[] args) throws Throwable {
		return method.invoke(proxied, args);
	}
}

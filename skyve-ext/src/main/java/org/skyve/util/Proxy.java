package org.skyve.util;

import java.io.Serializable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.skyve.domain.messages.DomainException;

import net.bytebuddy.ByteBuddy;
import net.bytebuddy.description.modifier.Visibility;
import net.bytebuddy.dynamic.loading.ClassLoadingStrategy;
import net.bytebuddy.implementation.InvocationHandlerAdapter;
import net.bytebuddy.jar.asm.Opcodes;
import net.bytebuddy.matcher.ElementMatchers;

/**
 * Create a Serializable dynamic proxy for an object instance that will route all public methods through the invoke method of its ProxyDelegate.
 * The proxy will be a sub-class of the instance given, so the instance's class must be non-final.
 * The default invoke method implementation will make the call to the proxied object.
 * Method .of() and .ofTransient() take an object and a delegate and proxy the public calls into the delegate's invoke method.
 * Call super invoke on the delegate to make the proxied method call.
 * Method ofTransient creates the proxied object as a field with modifier "transient" on the subclass (but both will give you back a Serializable proxy).
 * Proxy.deproxy() will give you the original proxied object.
 * Proxy.reproxy() allows you to set the proxied object.
 * 
 * @author mike
 */
public class Proxy {
	/**
	 * Proxied class to Proxy sub-class (with non-transient proxied field added).
	 */
	private static final ConcurrentMap<Class<? extends Object>, Class<? extends Serializable>> CLASS_TO_PROXY_CLASS_WITH_SERIALIZABLE_PROXIED = new ConcurrentHashMap<>();

	/**
	 * Proxied class to Proxy sub-class (with transient proxied field add).
	 */
	private static final ConcurrentMap<Class<? extends Object>, Class<? extends Serializable>> CLASS_TO_PROXY_CLASS_WITH_TRANSIENT_PROXIED = new ConcurrentHashMap<>();

	/**
	 * The proxied field name added to the sub-class.
	 */
	private static final String PROXIED_FIELD_NAME = "_skyve_proxied";

	/**
	 * The delegate field name added to the sub-class.
	 */
	private static final String DELEGATE_FIELD_NAME = "_skyve_delegate";

	/**
	 * Private class used to hide the invocation handler used.
	 */
	private static class ProxyInvocationHandler <T extends Object> implements InvocationHandler {
		@Override
		@SuppressWarnings("unchecked")
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			Class<?> proxyClass = proxy.getClass();
			T proxied = (T) proxyClass.getDeclaredField(PROXIED_FIELD_NAME).get(proxy);
			ProxyDelegate<T> delegate = (ProxyDelegate<T>) proxyClass.getDeclaredField(DELEGATE_FIELD_NAME).get(proxy);
			return delegate.invoke((T) proxy, proxied, method, args);
		}
	}
	
	/**
	 * Disallow instantiation
	 */
	private Proxy() {
		// nothing to see here
	}
	
	/**
	 * Proxy the proxied object delegating the method calls to the delegate.
	 * @param <T>
	 * @param proxied Must be an instanceof of a non-final class
	 * @param delegate	Used to process the method calls made
	 * @return	The proxy that represents the proxied object.
	 */
	public static <T extends Object> T of(T proxied, ProxyDelegate<T> delegate) {
		return of(proxied, delegate, false);
	}
	
	/**
	 * Proxy the proxied object delegating the method calls to the delegate.
	 * The proxied object is stored in a transient field on the proxy.
	 * @param <T>
	 * @param proxied Must be an instanceof of a non-final class
	 * @param delegate	Used to process the method calls made
	 * @return	The proxy that represents the proxied object.
	 */
	public static <T extends Object> T ofTransient(T proxied, ProxyDelegate<T> delegate) {
		return of(proxied, delegate, true);
	}
	
	/**
	 * Create a subclass of the proxied class and return a proxy instance for proxied.
	 * @param <T>
	 * @param proxied
	 * @param delegate
	 * @param proxiedTransient
	 * @return	The proxy instance.
	 */
	@SuppressWarnings({"unchecked"})
	private static <T extends Object> T of(T proxied, ProxyDelegate<T> delegate, boolean proxiedTransient) {
		if (Proxy.isProxy(proxied)) {
			throw new IllegalArgumentException("proxied argument is a Proxy");
		}
		Class<?> proxiedClass = proxied.getClass();
		Class<?> delegateClass = delegate.getClass();

		// Cater for the bootstrap class loader
		final ClassLoader classLoader = (proxiedClass.getClassLoader() == null) ? Thread.currentThread().getContextClassLoader() : proxiedClass.getClassLoader();

		// Only proxy public methods so that the proxied object can still be serialized
		// Ensure that we inject the new class definition into the proxied class' class loader so that Serialization can work.
		ProxyInvocationHandler<T> handler = new ProxyInvocationHandler<>();
		Class<T> proxyClass = null;
		if (proxiedTransient) {
			proxyClass = (Class<T>) CLASS_TO_PROXY_CLASS_WITH_TRANSIENT_PROXIED.computeIfAbsent(
											proxiedClass,
											k -> (Class<? extends Serializable>) new ByteBuddy()
													.subclass(proxiedClass)
													.method(ElementMatchers.isPublic()).intercept(InvocationHandlerAdapter.of(handler))
													.implement(Serializable.class)
													.defineField(PROXIED_FIELD_NAME, proxiedClass, Opcodes.ACC_PUBLIC | Opcodes.ACC_TRANSIENT)
													.defineField(DELEGATE_FIELD_NAME, delegateClass, Visibility.PUBLIC)
													.make()
													.load(classLoader, ClassLoadingStrategy.Default.INJECTION)
													.getLoaded());
		}
		else {
			proxyClass = (Class<T>) CLASS_TO_PROXY_CLASS_WITH_SERIALIZABLE_PROXIED.computeIfAbsent(
											proxiedClass,
											k -> (Class<? extends Serializable>) new ByteBuddy()
													.subclass(proxiedClass)
													.method(ElementMatchers.isPublic()).intercept(InvocationHandlerAdapter.of(handler))
													.implement(Serializable.class)
													.defineField(PROXIED_FIELD_NAME, proxiedClass, Visibility.PUBLIC)
													.defineField(DELEGATE_FIELD_NAME, delegateClass, Visibility.PUBLIC)
													.make()
													.load(classLoader, ClassLoadingStrategy.Default.INJECTION)
													.getLoaded());
		}
										
		// Instantiate and set
		try {
			T proxy = proxyClass.getDeclaredConstructor().newInstance();
			proxyClass.getDeclaredField(PROXIED_FIELD_NAME).set(proxy, proxied);
			proxyClass.getDeclaredField(DELEGATE_FIELD_NAME).set(proxy, delegate);
			return proxy;
		}
		catch (Exception e) {
			throw new DomainException("Could not create a proxy for " + proxied, e);
		}
	}
	
	/**
	 * Return the proxied object from a proxy.
	 * @param <T>
	 * @param proxy	The proxy to deproxy.
	 * @return	The proxied object.
	 */
	@SuppressWarnings("unchecked")
	public static <T extends Object> T deproxy(T proxy) {
		try {
			return (T) proxy.getClass().getDeclaredField(PROXIED_FIELD_NAME).get(proxy);
		}
		catch (Exception e) {
			throw new DomainException("Could not deproxy " + proxy, e);
		}
	}

	/**
	 * Set the proxied object for a proxy.
	 * @param <T>
	 * @param proxy	The proxy to reproxy.
	 * @param proxied	The new proxied object.
	 */
	public static <T extends Object> void reproxy(T proxy, T proxied) {
		try {
			proxy.getClass().getDeclaredField(PROXIED_FIELD_NAME).set(proxy, proxied);
		}
		catch (Exception e) {
			throw new DomainException("Could not reproxy " + proxied + " to proxy " + proxy, e);
		}
	}
	
	/**
	 * Determine if the given object is a proxy or not
	 * @param proxy	The potential proxy object.
	 * @return	true if it is a proxy, otherwise false
	 */
	@SuppressWarnings("null")
	public static boolean isProxy(Object proxy) {
		boolean result = (proxy != null);

		if (result) {
			try {
				proxy.getClass().getDeclaredField(PROXIED_FIELD_NAME);
			}
			catch (@SuppressWarnings("unused") NoSuchFieldException e) {
				result = false;
			}
		}
		
		return result;
	}
	
	/**
	 * Get the delegate used with this proxy.
	 * @param <T>
	 * @param proxy	The proxy to get the delegate for.
	 * @return	The delegate object.
	 */
	@SuppressWarnings("unchecked")
	public static <T extends Object> ProxyDelegate<T> getDelegate(T proxy) {
		try {
			return (ProxyDelegate<T>) proxy.getClass().getDeclaredField(DELEGATE_FIELD_NAME).get(proxy);
		}
		catch (NoSuchFieldException | IllegalAccessException e) {
			throw new DomainException("Could not get delegate from proxy " + proxy, e);
		}
	}
}

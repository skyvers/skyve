package org.skyve.util;

import java.io.IOException;
import java.io.Serializable;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.skyve.domain.messages.DomainException;

import net.bytebuddy.ByteBuddy;
import net.bytebuddy.description.modifier.Visibility;
import net.bytebuddy.dynamic.DynamicType.Loaded;
import net.bytebuddy.dynamic.DynamicType.Unloaded;
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
	private static final String PROXY_CREATION_FAILED = "Could not create a proxy for ";

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

		Class<T> proxyClass = (Class<T>) (proxiedTransient ?
											CLASS_TO_PROXY_CLASS_WITH_TRANSIENT_PROXIED.computeIfAbsent(proxiedClass, k -> defineProxyClass(k, true)) :
											CLASS_TO_PROXY_CLASS_WITH_SERIALIZABLE_PROXIED.computeIfAbsent(proxiedClass, k -> defineProxyClass(k, false)));

		// Instantiate and set
		try {
			T proxy = proxyClass.getDeclaredConstructor().newInstance();
			proxyClass.getDeclaredField(PROXIED_FIELD_NAME).set(proxy, proxied);
			proxyClass.getDeclaredField(DELEGATE_FIELD_NAME).set(proxy, delegate);
			return proxy;
		}
		catch (Exception e) {
			throw new DomainException(PROXY_CREATION_FAILED + proxied, e);
		}
	}
	
	/**
	 * Generates the proxy sub-class of a proxied class and defines it alongside the proxied class.
	 *
	 * <p>Only public methods are proxied so that the proxied object can still be serialized.
	 * The delegate field takes any {@link ProxyDelegate}, as the one class is used for every
	 * proxy of the proxied class whatever delegate each is given.
	 *
	 * @param proxiedClass	The class to sub-class. Must be non-final.
	 * @param proxiedTransient	Whether the field for the proxied object is transient.
	 * @return	The proxy sub-class.
	 * @throws DomainException	If the sub-class cannot be generated.
	 */
	@SuppressWarnings("unchecked")
	private static Class<? extends Serializable> defineProxyClass(Class<?> proxiedClass, boolean proxiedTransient) {
		// Cater for the bootstrap class loader
		ClassLoader classLoader = (proxiedClass.getClassLoader() == null) ? Thread.currentThread().getContextClassLoader() : proxiedClass.getClassLoader();
		int proxiedModifiers = proxiedTransient ? (Opcodes.ACC_PUBLIC | Opcodes.ACC_TRANSIENT) : Opcodes.ACC_PUBLIC;

		try (Unloaded<?> unloaded = new ByteBuddy()
				.subclass(proxiedClass)
				.method(ElementMatchers.isPublic()).intercept(InvocationHandlerAdapter.of(new ProxyInvocationHandler<>()))
				.implement(Serializable.class)
				.defineField(PROXIED_FIELD_NAME, proxiedClass, proxiedModifiers)
				.defineField(DELEGATE_FIELD_NAME, ProxyDelegate.class, Visibility.PUBLIC)
				.make()) {
			try (Loaded<?> loaded = unloaded.load(classLoader, loadingStrategy(proxiedClass))) {
				return (Class<? extends Serializable>) loaded.getLoaded();
			}
		}
		catch (IOException e) {
			throw new DomainException(PROXY_CREATION_FAILED + proxiedClass, e);
		}
	}

	/**
	 * Determines how to define the proxy sub-class in the proxied class' class loader, which is where
	 * it has to be for Serialization to work.
	 *
	 * <p>The sub-class is defined through a lookup on the proxied class wherever the proxied class allows that,
	 * which is the case for application classes and needs no JVM options.
	 * A class the bootstrap class loader loaded, or one in a module that is not open to Skyve, does not allow it.
	 * Its sub-class is injected reflectively instead, which only works when the JVM is started with
	 * {@code --add-opens java.base/java.lang=ALL-UNNAMED}.
	 *
	 * @param proxiedClass	The class being sub-classed.
	 * @return	The strategy to load the sub-class with.
	 */
	private static ClassLoadingStrategy<ClassLoader> loadingStrategy(Class<?> proxiedClass) {
		if (proxiedClass.getClassLoader() != null) {
			try {
				return ClassLoadingStrategy.UsingLookup.of(MethodHandles.privateLookupIn(proxiedClass, MethodHandles.lookup()));
			}
			catch (@SuppressWarnings("unused") IllegalAccessException e) {
				// the proxied class is not open to us - inject instead
			}
		}
		return ClassLoadingStrategy.Default.INJECTION;
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

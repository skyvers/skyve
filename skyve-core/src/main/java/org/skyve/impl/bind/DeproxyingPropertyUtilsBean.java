package org.skyve.impl.bind;

import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.beanutils.PropertyUtilsBean;
import org.skyve.impl.util.UtilImpl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Resolves BeanUtils property access against deproxied Hibernate entities.
 *
 * <p>The adapter unwraps CGLib proxies before delegating to the standard
 * {@link PropertyUtilsBean} implementation so that property lookup works on the
 * concrete runtime type.
 */
public class DeproxyingPropertyUtilsBean extends PropertyUtilsBean {
	/**
	 * Resolves a simple property after deproxying the bean instance.
	 *
	 * @param bean the bean instance, possibly a Hibernate proxy
	 * @param name the property name
	 * @return the resolved property value
	 * @throws IllegalAccessException if the property accessor is not accessible
	 * @throws InvocationTargetException if the accessor throws an exception
	 * @throws NoSuchMethodException if the property does not exist
	 */
	@Override
	public @Nullable Object getSimpleProperty(@SuppressWarnings("null") @Nonnull Object bean, @SuppressWarnings("null") @Nonnull String name)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		return super.getSimpleProperty(deproxiedBean, name);
	}

	/**
	 * Sets a simple property after deproxying the bean and value instances.
	 *
	 * @param bean the bean instance, possibly a Hibernate proxy
	 * @param name the property name
	 * @param value the property value
	 * @throws IllegalAccessException if the property accessor is not accessible
	 * @throws InvocationTargetException if the accessor throws an exception
	 * @throws NoSuchMethodException if the property does not exist
	 */
	@Override
	public void setSimpleProperty(@SuppressWarnings("null") @Nonnull Object bean,
									@SuppressWarnings("null") @Nonnull String name,
									@Nullable Object value)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		Object deproxiedValue = UtilImpl.deproxy(value);
		super.setSimpleProperty(deproxiedBean, name, deproxiedValue);
	}

	/**
	 * Resolves the declared type of a property on the deproxied bean.
	 *
	 * @param bean the bean instance, possibly a Hibernate proxy
	 * @param name the property name
	 * @return the property type
	 * @throws IllegalAccessException if the property accessor is not accessible
	 * @throws InvocationTargetException if the accessor throws an exception
	 * @throws NoSuchMethodException if the property does not exist
	 */
	@Override
	public @Nullable Class<?> getPropertyType(@SuppressWarnings("null") @Nonnull Object bean, @SuppressWarnings("null") @Nonnull String name)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		return super.getPropertyType(deproxiedBean, name);
	}
	
	/**
	 * Resolves the descriptor of a property on the deproxied bean.
	 *
	 * @param bean the bean instance, possibly a Hibernate proxy
	 * @param name the property name
	 * @return the property descriptor
	 * @throws IllegalAccessException if the property accessor is not accessible
	 * @throws InvocationTargetException if the accessor throws an exception
	 * @throws NoSuchMethodException if the property does not exist
	 */
	@Override
	public @Nullable PropertyDescriptor getPropertyDescriptor(@SuppressWarnings("null") @Nonnull Object bean,
																@SuppressWarnings("null") @Nonnull String name)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		return super.getPropertyDescriptor(deproxiedBean, name);
	}
	
	/**
	 * Resolves an indexed property on the deproxied bean.
	 *
	 * @param bean the bean instance, possibly a Hibernate proxy
	 * @param name the property name
	 * @param index the collection or array index
	 * @return the indexed property value
	 * @throws IllegalAccessException if the property accessor is not accessible
	 * @throws InvocationTargetException if the accessor throws an exception
	 * @throws NoSuchMethodException if the property does not exist
	 */
	@Override
	public @Nullable Object getIndexedProperty(@SuppressWarnings("null") @Nonnull Object bean,
												@SuppressWarnings("null") @Nonnull String name,
												int index)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		return super.getIndexedProperty(deproxiedBean, name, index);
	}
	
	/**
	 * Sets an indexed property after deproxying the bean and value instances.
	 *
	 * @param bean the bean instance, possibly a Hibernate proxy
	 * @param name the property name
	 * @param index the collection or array index
	 * @param value the property value
	 * @throws IllegalAccessException if the property accessor is not accessible
	 * @throws InvocationTargetException if the accessor throws an exception
	 * @throws NoSuchMethodException if the property does not exist
	 */
	@Override
	public void setIndexedProperty(@SuppressWarnings("null") @Nonnull Object bean,
									@SuppressWarnings("null") @Nonnull String name,
									int index,
									@Nullable Object value)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		Object deproxiedValue = UtilImpl.deproxy(value);
		super.setIndexedProperty(deproxiedBean, name, index, deproxiedValue);
	}
	
	/**
	 * Resolves a mapped property on the deproxied bean.
	 *
	 * @param bean the bean instance, possibly a Hibernate proxy
	 * @param name the property name
	 * @param key the map key
	 * @return the mapped property value
	 * @throws IllegalAccessException if the property accessor is not accessible
	 * @throws InvocationTargetException if the accessor throws an exception
	 * @throws NoSuchMethodException if the property does not exist
	 */
	@Override
	public @Nullable Object getMappedProperty(@SuppressWarnings("null") @Nonnull Object bean,
												@SuppressWarnings("null") @Nonnull String name,
												@SuppressWarnings("null") @Nonnull String key)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		return super.getMappedProperty(deproxiedBean, name, key);
	}
	
	/**
	 * Sets a mapped property after deproxying the bean and value instances.
	 *
	 * @param bean the bean instance, possibly a Hibernate proxy
	 * @param name the property name
	 * @param key the map key
	 * @param value the property value
	 * @throws IllegalAccessException if the property accessor is not accessible
	 * @throws InvocationTargetException if the accessor throws an exception
	 * @throws NoSuchMethodException if the property does not exist
	 */
	@Override
	public void setMappedProperty(@SuppressWarnings("null") @Nonnull Object bean,
									@SuppressWarnings("null") @Nonnull String name,
									@SuppressWarnings("null") @Nonnull String key,
									@Nullable Object value)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		Object deproxiedValue = UtilImpl.deproxy(value);
		super.setMappedProperty(deproxiedBean, name, key, deproxiedValue);
	}
}

package org.skyve.impl.bind;

import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.beanutils.PropertyUtilsBean;
import org.skyve.impl.util.UtilImpl;

/**
 * This class overrides the normal PropertyUtils calls but ensures that
 * the arguments are not CGLib proxies dished up by hibernate.
 * The proxies do not play well, especially when they are base class 
 * references to subclasses - these need to be inflated to get the 
 * properties from the particular instance.
 * 
 * @author mike
 */
public class DeproxyingPropertyUtilsBean extends PropertyUtilsBean {
	@Override
	public Object getSimpleProperty(Object bean, String name)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		return super.getSimpleProperty(deproxiedBean, name);
	}

	@Override
	public void setSimpleProperty(Object bean, String name, Object value)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		Object deproxiedValue = UtilImpl.deproxy(value);
		super.setSimpleProperty(deproxiedBean, name, deproxiedValue);
	}

	@Override
	public Class<?> getPropertyType(Object bean, String name)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		return super.getPropertyType(deproxiedBean, name);
	}
	
	@Override
	public PropertyDescriptor getPropertyDescriptor(Object bean, String name)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		return super.getPropertyDescriptor(deproxiedBean, name);
	}
	
	@Override
	public Object getIndexedProperty(Object bean, String name, int index)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		return super.getIndexedProperty(deproxiedBean, name, index);
	}
	
	@Override
	public void setIndexedProperty(Object bean, String name, int index, Object value)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		Object deproxiedValue = UtilImpl.deproxy(value);
		super.setIndexedProperty(deproxiedBean, name, index, deproxiedValue);
	}
	
	@Override
	public Object getMappedProperty(Object bean, String name, String key)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		return super.getMappedProperty(deproxiedBean, name, key);
	}
	
	@Override
	public void setMappedProperty(Object bean, String name, String key, Object value)
	throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		Object deproxiedBean = UtilImpl.deproxy(bean);
		Object deproxiedValue = UtilImpl.deproxy(value);
		super.setMappedProperty(deproxiedBean, name, key, deproxiedValue);
	}
}

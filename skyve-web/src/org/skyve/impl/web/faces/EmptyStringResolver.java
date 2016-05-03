package org.skyve.impl.web.faces;

import java.beans.FeatureDescriptor;
import java.util.Iterator;

import javax.el.ELContext;
import javax.el.ELResolver;

/**
 * This class is required to ensure that non-BeanMapAdapter EL expressions yield null for empty strings.
 * EL in JSF 2.2 (JEE 7) insists on setting string properties to "" - FFS!!!
 * This resolver is registered in the skyve web faces-config.xml.
 * 
 * @author mike
 */
public class EmptyStringResolver extends ELResolver {
	@Override
	public Class<?> getCommonPropertyType(ELContext context, Object base) {
		return String.class;
	}

	@Override
	public Iterator<FeatureDescriptor> getFeatureDescriptors(ELContext context, Object base) {
		return null;
	}

	@Override
	public Class<?> getType(ELContext context, Object base, Object property) {
		return null;
	}

	@Override
	public Object getValue(ELContext context, Object base, Object property) {
		return null;
	}

	@Override
	public boolean isReadOnly(ELContext context, Object base, Object property) {
		return true;
	}

	@Override
	public void setValue(ELContext context, Object base, Object property, Object value) {
		// it's read only
	}

	@Override
	public Object convertToType(ELContext context, Object obj, Class<?> targetType) {
		if ((obj == null) && String.class.equals(targetType)) {
			context.setPropertyResolved(true);
			return null;
		}
		
		return null;
	}
}

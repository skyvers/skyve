package org.skyve.impl.bind;

import java.beans.FeatureDescriptor;
import java.util.Iterator;

import javax.el.ELContext;
import javax.el.ELResolver;

import org.skyve.domain.Bean;

/**
 * Used to Resolve static/dynamic Skyve beans using BindUtil instead of bean introspection alone.
 * @author mike
 */
public class BindingELResolver extends ELResolver {
	public BindingELResolver() {
		// nothing to see here
	}
	
	@Override
	public Class<?> getType(ELContext context, Object base, Object property) {
		if ((base instanceof Bean) && (property instanceof String)) {
			context.setPropertyResolved(true);
			return BindUtil.getPropertyType((Bean) base, (String) property);
		}
		return null;
	}

	@Override
	public Object getValue(ELContext context, Object base, Object property) {
		if ((base instanceof Bean) && (property instanceof String)) {
			context.setPropertyResolved(base, property);
			return BindUtil.get((Bean) base, (String) property);
		}
		return null;
	}
	
	@Override
	public void setValue(ELContext context, Object base, Object property, Object val) {
		if ((base instanceof Bean) && (property instanceof String)) {
			context.setPropertyResolved(base, property);
			BindUtil.set((Bean) base, (String) property, val);
		}
	}

	@Override
	public boolean isReadOnly(ELContext context, Object base, Object property) {
		if ((base instanceof Bean) && (property instanceof String)) {
			context.setPropertyResolved(true);
			BindUtil.isMutable((Bean) base, (String) property);
		}
		return false;
	}

	@Override
	public Iterator<FeatureDescriptor> getFeatureDescriptors(ELContext context, Object base) {
		return null;
	}

	@Override
	public Class<?> getCommonPropertyType(ELContext context, Object base) {
		return Object.class;
	}
}

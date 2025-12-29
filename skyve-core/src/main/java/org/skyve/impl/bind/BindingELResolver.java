package org.skyve.impl.bind;

import org.skyve.domain.Bean;

import jakarta.el.ELContext;
import jakarta.el.ELResolver;

/**
 * Used to Resolve static/dynamic Skyve beans using BindUtil instead of bean introspection alone.
 * @author mike
 */
class BindingELResolver extends ELResolver {
	BindingELResolver() {
		// nothing to see here
	}
	
	@Override
	public Class<?> getType(ELContext context, Object base, Object property) {
		if ((base instanceof Bean bean) && (property instanceof String binding)) {
			context.setPropertyResolved(true);
			return BindUtil.getPropertyType(bean, binding);
		}
		return null;
	}

	@Override
	public Object getValue(ELContext context, Object base, Object property) {
		if ((base instanceof Bean bean) && (property instanceof String binding)) {
			context.setPropertyResolved(base, property);
			return BindUtil.get(bean, binding);
		}
		return null;
	}
	
	@Override
	public void setValue(ELContext context, Object base, Object property, Object val) {
		if ((base instanceof Bean bean) && (property instanceof String binding)) {
			context.setPropertyResolved(base, property);
			BindUtil.set(bean, binding, val);
		}
	}

	@Override
	public boolean isReadOnly(ELContext context, Object base, Object property) {
		if ((base instanceof Bean bean) && (property instanceof String simplePropertyName)) {
			context.setPropertyResolved(true);
			return BindUtil.isMutable(bean, simplePropertyName);
		}
		return false;
	}

	@Override
	public Class<?> getCommonPropertyType(ELContext context, Object base) {
		return Object.class;
	}
}

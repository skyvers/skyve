package org.skyve.impl.bind;

import org.skyve.domain.Bean;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
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
	public @Nullable Class<?> getType(@SuppressWarnings("null") @Nonnull ELContext context, @Nullable Object base, @Nullable Object property) {
		if ((base instanceof Bean bean) && (property instanceof String binding)) {
			context.setPropertyResolved(true);
			return BindUtil.getPropertyType(bean, binding);
		}
		return null;
	}

	@Override
	public @Nullable Object getValue(@SuppressWarnings("null") @Nonnull ELContext context, @Nullable Object base, @Nullable Object property) {
		if ((base instanceof Bean bean) && (property instanceof String binding)) {
			context.setPropertyResolved(base, property);
			return BindUtil.get(bean, binding);
		}
		return null;
	}
	
	@Override
	public void setValue(@SuppressWarnings("null") @Nonnull ELContext context,
						@Nullable Object base,
						@Nullable Object property,
						@Nullable Object val) {
		if ((base instanceof Bean bean) && (property instanceof String binding)) {
			context.setPropertyResolved(base, property);
			BindUtil.set(bean, binding, val);
		}
	}

	@Override
	public boolean isReadOnly(@SuppressWarnings("null") @Nonnull ELContext context, @Nullable Object base, @Nullable Object property) {
		if ((base instanceof Bean bean) && (property instanceof String simplePropertyName)) {
			context.setPropertyResolved(true);
			return BindUtil.isMutable(bean, simplePropertyName);
		}
		return false;
	}

	@Override
	public @Nonnull Class<?> getCommonPropertyType(@SuppressWarnings("null") @Nonnull ELContext context, @Nullable Object base) {
		return Object.class;
	}
}

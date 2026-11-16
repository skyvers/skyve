package org.skyve.impl.web.faces;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.el.ELContext;
import jakarta.el.ELResolver;

/**
 * Normalises EL conversions so empty-string assignments can resolve to {@code null} for string targets.
 *
 * <p>This resolver is registered in the Skyve JSF {@code faces-config.xml} chain and intentionally
 * behaves as read-only for standard property resolution methods.
 *
 * This class is required to ensure that non-BeanMapAdapter EL expressions yield null for empty strings.
 * EL in JSF 2.2 (JEE 7) insists on setting string properties to "" - FFS!!!
 * 
 * @author mike
 */
public class EmptyStringResolver extends ELResolver {
	/**
	 * Returns the common property type this resolver participates in.
	 *
	 * @param context the active EL context
	 * @param base the base object for the current resolution step
	 * @return {@link String} to indicate this resolver only targets string conversion scenarios
	 */
	@Override
	public @Nonnull Class<?> getCommonPropertyType(@SuppressWarnings("null") @Nonnull ELContext context, @Nullable Object base) {
		return String.class;
	}

	/**
	 * Returns no type metadata because this resolver does not perform property typing.
	 *
	 * @param context the active EL context
	 * @param base the base object for the current resolution step
	 * @param property the property being resolved
	 * @return {@code null} because this resolver only participates in conversion
	 */
	@Override
	public @Nullable Class<?> getType(@SuppressWarnings("null") @Nonnull ELContext context, @Nullable Object base, @Nullable Object property) {
		return null;
	}

	/**
	 * Returns no value because this resolver does not handle direct property lookups.
	 *
	 * @param context the active EL context
	 * @param base the base object for the current resolution step
	 * @param property the property being resolved
	 * @return {@code null} because value resolution is delegated to other resolvers
	 */
	@Override
	public @Nullable Object getValue(@SuppressWarnings("null") @Nonnull ELContext context, @Nullable Object base, @Nullable Object property) {
		return null;
	}

	/**
	 * Declares this resolver as read-only.
	 *
	 * @param context the active EL context
	 * @param base the base object for the current resolution step
	 * @param property the property being resolved
	 * @return {@code true} because this resolver never writes target properties
	 */
	@Override
	public boolean isReadOnly(@SuppressWarnings("null") @Nonnull ELContext context, @Nullable Object base, @Nullable Object property) {
		return true;
	}

	/**
	 * Ignores assignment requests because this resolver is read-only.
	 *
	 * @param context the active EL context
	 * @param base the base object for the current resolution step
	 * @param property the property being assigned
	 * @param value the value being assigned
	 */
	@Override
	public void setValue(@SuppressWarnings("null") @Nonnull ELContext context,
						@Nullable Object base,
						@Nullable Object property,
						@Nullable Object value) {
		// it's read only
	}

	/**
	 * Converts an empty string assignment path into {@code null} for string targets.
	 *
	 * @param <T> the requested conversion target type
	 * @param context the active EL context
	 * @param obj the value to convert
	 * @param targetType the requested conversion target type
	 * @return {@code null} for string targets when the source is {@code null}; otherwise {@code null}
	 *         so that downstream resolvers can continue handling conversion
	 */
	@Override
	public @Nullable <T extends Object> T convertToType(@SuppressWarnings("null") @Nonnull ELContext context,
														@Nullable Object obj,
														@SuppressWarnings("null") @Nonnull Class<T> targetType) {
		if ((obj == null) && String.class.equals(targetType)) {
			context.setPropertyResolved(true);
		}
		
		return null;
	}
}

package org.skyve.impl.bind;

import java.util.Set;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.el.ELContext;
import jakarta.el.ELResolver;
import jakarta.el.ImportHandler;

/**
 * Resolves a top-level identifier in an EL expression as an enumeration.
 *
 * <p>An EL expression can name the enumerations of the document it is evaluated against,
 * for example {@code bean.status eq Status.open}, and those of the documents that one is
 * related to, so that {@code bean.contact.type eq ContactType.person} works too.
 * An enumeration that is already known for the root document or bean class
 * (see {@link #remembered(String)}) is answered for directly. Otherwise a related document's
 * enumeration is looked for (see {@link #find(String)}), but only for a name that is not
 * an imported class, so no expression that resolved before resolves differently.
 * The Skyve variables, and names that are bound elsewhere, are never answered for.
 *
 * <p>Threading: not thread-safe; one instance belongs to one EL processor.
 */
abstract class EnumELResolver extends ELResolver {
	private static final Set<String> VARIABLES = Set.of(ELExpressionEvaluator.BEAN_VARIABLE,
															ELExpressionEvaluator.USER_VARIABLE,
															ELExpressionEvaluator.STASH_VARIABLE);

	private final Set<String> boundNames;

	/**
	 * Creates a resolver.
	 *
	 * @param boundNames the simple names of enumerations that are bound in the EL context already, which are never resolved here
	 */
	EnumELResolver(@Nonnull Set<String> boundNames) {
		this.boundNames = boundNames;
	}

	/**
	 * Returns what is already known for the name: the root's own enumeration, or what was found for it earlier.
	 * What is returned takes precedence over an imported class of the same name.
	 *
	 * @param name the simple name of the enumeration
	 * @return the known value, or {@code null} when there is none
	 */
	@SuppressWarnings("static-method")
	@Nullable Object remembered(@Nonnull String name) {
		return null;
	}

	/**
	 * Finds the enumeration of that name declared closest to the root document, amongst its related documents.
	 *
	 * @param name the simple name of the enumeration
	 * @return what the name evaluates to, or {@code null} when no related document declares it
	 */
	abstract @Nullable Object find(@Nonnull String name);

	@Override
	public Object getValue(ELContext context, Object base, Object property) {
		if ((base != null) || (! (property instanceof String name)) || VARIABLES.contains(name) || boundNames.contains(name)) {
			return null;
		}
		Object result = remembered(name);
		if ((result == null) && (! isImported(context, name))) {
			result = find(name);
		}
		if (result != null) {
			context.setPropertyResolved(base, property);
		}
		return result;
	}

	/**
	 * Determines whether EL would resolve the name itself, as an imported class or static member.
	 */
	private static boolean isImported(@Nonnull ELContext context, @Nonnull String name) {
		ImportHandler imports = context.getImportHandler();
		return (imports.resolveClass(name) != null) || (imports.resolveStatic(name) != null);
	}

	@Override
	public Class<?> getType(ELContext context, Object base, Object property) {
		return null; // an enumeration name is not assignable
	}

	@Override
	public void setValue(ELContext context, Object base, Object property, Object value) {
		// an enumeration name is not assignable
	}

	@Override
	public boolean isReadOnly(ELContext context, Object base, Object property) {
		return false; // not handled here
	}

	@Override
	public Class<?> getCommonPropertyType(ELContext context, Object base) {
		return null;
	}
}

package org.skyve;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.SortedMap;

import org.skyve.domain.number.NumberGenerator;
import org.skyve.domain.types.formatters.Formatter;
import org.skyve.domain.types.formatters.Formatters;
import org.skyve.impl.domain.number.NumberGeneratorStaticSingleton;
import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.Customisations;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The central static facade for the Skyve core API.
 *
 * <p>{@code CORE} provides thread-scoped access to the fundamental runtime services:
 * persistence, the authenticated user, the customer context, the metadata repository,
 * and formatting utilities. All services are bound to the <em>current thread's execution
 * context</em>; do not share retrieved objects across thread boundaries.
 *
 * <p>The companion class {@code EXT} (in {@code skyve-ext}) provides additional services
 * such as mail, content management, job scheduling, and tag management that depend on a
 * heavier runtime layer.
 *
 * <p>Threading: not instantiable; all methods are static and delegate to thread-local
 * state managed by the framework's persistence and security infrastructure.
 *
 * @see org.skyve.EXT
 * @see org.skyve.persistence.Persistence
 * @see org.skyve.metadata.repository.Repository
 */
public class CORE {
	/**
	 * Disallow instantiation
	 */
	private CORE() {
		// no-op
	}
	
	/**
	 * Returns the {@link Persistence} instance bound to the current thread.
	 *
	 * <p>The returned instance is the primary gateway for all database operations:
	 * saving, deleting, querying, and transaction management. It also exposes the
	 * current {@link User} and {@link Customer}.
	 *
	 * <p>Threading: the returned object is thread-confined — never share it across
	 * threads. It is valid only for the duration of the current request or job execution.
	 *
	 * @return the thread-local {@link Persistence}; never {@code null}
	 */
	public static @Nonnull Persistence getPersistence() {
		return AbstractPersistence.get();
	}
	
	/**
	 * Creates a new {@link Ordering} specification for use in queries and sort operations.
	 *
	 * <p>An {@code Ordering} pairs a binding path with a {@link SortDirection} and is
	 * consumed by {@link org.skyve.persistence.DocumentQuery} column ordering and by
	 * {@link org.skyve.util.Binder} collection sorting.
	 *
	 * @param by   the dot-separated Skyve binding expression to sort on; must not be {@code null}
	 * @param sort the direction to sort; must not be {@code null}
	 * @return a new {@link Ordering}; never {@code null}
	 */
	public static @Nonnull Ordering newOrdering(@Nonnull String by, @Nonnull SortDirection sort) {
		return new OrderingImpl(by, sort);
	}
	
	/**
	 * Returns the application-wide {@link NumberGenerator}.
	 *
	 * <p>The number generator produces autonomous, gap-free sequential numbers for
	 * document number fields declared with {@code @NumberGenerated} or equivalent
	 * metadata. It is backed by a database sequence and operates in its own transaction
	 * so that numbers are never reused, even on rollback.
	 *
	 * @return the {@link NumberGenerator} singleton; never {@code null}
	 * @see org.skyve.domain.number.NumberGenerator
	 */
	public static @Nonnull NumberGenerator getNumberGenerator() {
		return NumberGeneratorStaticSingleton.get();
	}
	
	/**
	 * Returns the authenticated {@link User} for the current thread of execution.
	 *
	 * <p>Equivalent to {@code CORE.getPersistence().getUser()}. The user carries
	 * identity, role membership, customer context, and access-control information.
	 *
	 * <p>Threading: the returned object is thread-confined.
	 *
	 * @return the current thread's {@link User}; never {@code null}
	 */
	public static @Nonnull User getUser() {
		return AbstractPersistence.get().getUser();
	}

	/**
	 * Returns the {@link Customer} for the current thread of execution.
	 *
	 * <p>Equivalent to {@code CORE.getUser().getCustomer()}. The customer is the
	 * multi-tenancy root: it governs which modules, documents, and overrides are active
	 * for the current request.
	 *
	 * <p>Threading: the returned object is thread-confined.
	 *
	 * @return the current thread's {@link Customer}; never {@code null}
	 */
	public static @Nonnull Customer getCustomer() {
		return AbstractPersistence.get().getUser().getCustomer();
	}

	/**
	 * Returns the conversation-scoped stash map for the current thread.
	 *
	 * <p>The stash is a {@link SortedMap} keyed by arbitrary strings where action and
	 * Bizlet code can store transient state that must survive across multiple request
	 * round-trips within the same conversation (e.g. wizard steps). The map is serialised
	 * and stored in the conversation cache, so every value placed in it must be
	 * {@link java.io.Serializable}. Minimise the volume and size of stored objects to
	 * avoid excessive memory and serialisation overhead.
	 *
	 * <p>Threading: the returned map is thread-confined to the current request; do not
	 * pass it to other threads.
	 *
	 * @return the thread-local stash map; never {@code null}
	 */
	public static @Nonnull SortedMap<String, Object> getStash() {
		return AbstractPersistence.get().getStash();
	}
	
	/**
	 * Returns the {@link Repository} for the current runtime.
	 *
	 * <p>The repository is the programmatic gateway to all Skyve metadata: modules,
	 * documents, views, queries, roles, and router definitions. Most metadata is more
	 * conveniently accessed via {@link Customer} (e.g. {@code CORE.getCustomer().getModule(...)}),
	 * but the repository exposes lower-level and customer-agnostic operations such as
	 * retrieving all customers or accessing shared repository state.
	 *
	 * <p>The repository is a long-lived singleton that is safe to use from any thread.
	 *
	 * @return the application {@link Repository}; never {@code null}
	 */
	public static @Nonnull Repository getRepository() {
		return ProvidedRepositoryFactory.get();
	}
	
	/**
	 * Returns the active {@link Customisations} implementation for this deployment.
	 *
	 * <p>{@link Customisations} is an SPI that allows a project to override framework
	 * behaviours such as password validation, login redirect, and menu rendering. The
	 * implementation is registered via the deployment's JSON configuration and is a
	 * long-lived singleton safe to call from any thread.
	 *
	 * @return the {@link Customisations} singleton; never {@code null}
	 */
	public static @Nonnull Customisations getCustomisations() {
		return CustomisationsStaticSingleton.get();
	}

	/**
	 * Formats {@code valueToFormat} using one of Skyve's built-in named formatters.
	 *
	 * <p>Returns an empty string if {@code valueToFormat} is {@code null}.
	 * The {@code name} parameter selects from the set of formatters declared in
	 * {@link org.skyve.metadata.FormatterName}; each corresponds to a standard Skyve
	 * display convention (e.g. date patterns, decimal scales, currency symbols).
	 *
	 * @param <T>            the value type accepted by the named formatter
	 * @param name           the built-in formatter to use; must not be {@code null}
	 * @param valueToFormat  the value to format; may be {@code null}
	 * @return the formatted string, or {@code ""} if {@code valueToFormat} is {@code null}
	 */
	@SuppressWarnings("unchecked")
	public static @Nonnull <T> String format(@Nonnull FormatterName name, @Nullable T valueToFormat) {
		if (valueToFormat == null) {
			return "";
		}
		return ((Formatter<T>) name.getFormatter()).toDisplayValue(valueToFormat);
	}

	/**
	 * Formats {@code valueToFormat} using a custom formatter registered by name.
	 *
	 * <p>Custom formatters are declared in the deployment's Customisations JSON and
	 * looked up by name at runtime. If no formatter is registered for {@code name},
	 * this method falls back to {@code valueToFormat.toString()}.
	 * Returns an empty string if {@code valueToFormat} is {@code null}.
	 *
	 * @param <T>            the value type
	 * @param name           the name of a custom formatter registered via Customisations; must not be {@code null}
	 * @param valueToFormat  the value to format; may be {@code null}
	 * @return the formatted string, {@code ""} if {@code valueToFormat} is {@code null},
	 *         or {@code valueToFormat.toString()} if no formatter is registered for {@code name}
	 */
	public static @Nonnull <T> String format(@Nonnull String name, @Nullable T valueToFormat) {
		if (valueToFormat == null) {
			return "";
		}
		Formatter<T> formatter = Formatters.get(name);
		if (formatter == null) {
			return valueToFormat.toString();
		}
		return formatter.toDisplayValue(valueToFormat);
	}

	/**
	 * Creates a new {@link SimpleDateFormat} for the given pattern, using {@link Locale#ENGLISH}
	 * with lenient parsing disabled.
	 *
	 * <p>{@link SimpleDateFormat} is not thread-safe; a new instance is returned on every
	 * call. Callers should not share the returned instance across threads.
	 *
	 * @param formatString a {@link SimpleDateFormat}-compatible pattern string; must not be {@code null}
	 * @return a new, non-lenient {@link SimpleDateFormat}; never {@code null}
	 */
	public static @Nonnull SimpleDateFormat getDateFormat(@Nonnull String formatString) {
		SimpleDateFormat result = new SimpleDateFormat(formatString, Locale.ENGLISH);
		result.setLenient(false);
		return result;
	}
	
	/**
	 * Creates a new {@link DecimalFormat} for the given pattern.
	 *
	 * <p>{@link DecimalFormat} is not thread-safe; a new instance is returned on every
	 * call. Callers should not share the returned instance across threads.
	 *
	 * @param formatString a {@link DecimalFormat}-compatible pattern string; must not be {@code null}
	 * @return a new {@link DecimalFormat}; never {@code null}
	 */
	public static @Nonnull DecimalFormat getDecimalFormat(@Nonnull String formatString) {
		return new DecimalFormat(formatString);
	}
	
	/**
	 * Returns a {@link SimpleDateFormat} for the ISO-8601 date format {@code yyyy-MM-dd}.
	 *
	 * <p>Used throughout the framework for serialising and deserialising date-only values
	 * in JSON, XML, and import/export pipelines. A new instance is returned on every call
	 * because {@link SimpleDateFormat} is not thread-safe.
	 *
	 * @return a new {@link SimpleDateFormat} for {@code yyyy-MM-dd}; never {@code null}
	 */
	public static @Nonnull SimpleDateFormat getSerializableDateFormat() {
		return getDateFormat("yyyy-MM-dd");
	}

	/**
	 * Returns a {@link SimpleDateFormat} for the ISO-8601 time format {@code HH:mm:ss}.
	 *
	 * <p>Used throughout the framework for serialising and deserialising time-only values
	 * in JSON, XML, and import/export pipelines. A new instance is returned on every call
	 * because {@link SimpleDateFormat} is not thread-safe.
	 *
	 * @return a new {@link SimpleDateFormat} for {@code HH:mm:ss}; never {@code null}
	 */
	public static @Nonnull SimpleDateFormat getSerializableTimeFormat() {
		return getDateFormat("HH:mm:ss");
	}
}

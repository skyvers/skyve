package util.sail.call;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The {@code <parameter/>}s of one {@code <call/>}, checked against the names its call takes and read as types, with
 * messages that name the call, so a mistake in a script says where it is. Every problem is an
 * {@link IllegalArgumentException}: a script that gives a call a wrong parameter can't run, so the executor ends the
 * interaction whatever its test strategy.
 * <p>
 * A blank value counts as left out.
 * <p>
 * Threading: thread-confined, as its call is.
 */
public final class SailCallParameters {
	private final String call;
	private final Map<String, String> values;

	/**
	 * Checks that the step gives only the parameters the call takes.
	 *
	 * <p>Complexity: O(p log p) in the number of parameters given.
	 *
	 * @param call	the call's name, for messages, such as {@code Offline}
	 * @param values	the parameters, with {@code {run}} and {@code {var:name}} already substituted; kept, not copied
	 * @param allowed	the names the call takes
	 * @throws IllegalArgumentException	naming each parameter the call doesn't take, and those it does
	 */
	public SailCallParameters(@Nonnull String call, @Nonnull Map<String, String> values, @Nonnull Set<String> allowed) {
		this.call = call;
		this.values = values;
		Set<String> unknown = new TreeSet<>(values.keySet());
		unknown.removeAll(allowed);
		if (! unknown.isEmpty()) {
			throw new IllegalArgumentException(String.format("%s doesn't take %s %s: it takes %s",
																call,
																(unknown.size() == 1) ? "the parameter" : "the parameters",
																unknown,
																allowed.isEmpty() ? "none" : new TreeSet<>(allowed).toString()));
		}
	}

	/**
	 * Returns the name of the call these are the parameters of.
	 *
	 * @return the name, as the messages give it
	 */
	public @Nonnull String call() {
		return call;
	}

	/**
	 * Returns a parameter the call can't do without.
	 *
	 * @param name	the parameter's name
	 * @return its value, not blank
	 * @throws IllegalArgumentException	if it is left out or blank
	 */
	public @Nonnull String required(@Nonnull String name) {
		String result = optional(name);
		if (result == null) {
			throw new IllegalArgumentException(call + " needs the parameter " + name);
		}
		return result;
	}

	/**
	 * Returns a parameter that may be left out.
	 *
	 * @param name	the parameter's name
	 * @return its value as given, or null if it is left out or blank
	 */
	public @Nullable String optional(@Nonnull String name) {
		String result = values.get(name);
		return ((result == null) || result.isBlank()) ? null : result;
	}

	/**
	 * Returns a parameter that is {@code true} or {@code false}, in any case.
	 *
	 * @param name	the parameter's name
	 * @param fallback	the value when it is left out
	 * @return the value
	 * @throws IllegalArgumentException	if it is something else
	 */
	public boolean bool(@Nonnull String name, boolean fallback) {
		String value = optional(name);
		if (value == null) {
			return fallback;
		}
		return switch (value.trim().toLowerCase(Locale.ROOT)) {
			case "true" -> true;
			case "false" -> false;
			default -> throw new IllegalArgumentException(String.format("%s's %s must be true or false, not %s", call, name, value));
		};
	}

	/**
	 * Returns a parameter that is a whole number, at least a minimum.
	 *
	 * @param name	the parameter's name
	 * @param fallback	the value when it is left out, which needn't reach the minimum
	 * @param minimum	the smallest value allowed
	 * @return the value
	 * @throws IllegalArgumentException	if it isn't a whole number, or is below the minimum
	 */
	public int integer(@Nonnull String name, int fallback, int minimum) {
		String value = optional(name);
		if (value == null) {
			return fallback;
		}
		int result;
		try {
			result = Integer.parseInt(value.trim());
		}
		catch (NumberFormatException e) {
			throw new IllegalArgumentException(String.format("%s's %s must be a whole number, not %s", call, name, value), e);
		}
		if (result < minimum) {
			throw new IllegalArgumentException(String.format("%s's %s must be %d or more, not %d",
																call,
																name,
																Integer.valueOf(minimum),
																Integer.valueOf(result)));
		}
		return result;
	}

	/**
	 * Returns a parameter the call can't do without that is a whole number, at least a minimum.
	 *
	 * @param name	the parameter's name
	 * @param minimum	the smallest value allowed
	 * @return the value
	 * @throws IllegalArgumentException	if it is left out, isn't a whole number, or is below the minimum
	 */
	public int requiredInteger(@Nonnull String name, int minimum) {
		required(name);
		return integer(name, minimum, minimum);
	}

	/**
	 * Returns a parameter that is a comma separated list.
	 *
	 * <p>Complexity: O(n) in the value's length.
	 *
	 * @param name	the parameter's name
	 * @return the items, trimmed, without empty ones; empty if it is left out
	 */
	public @Nonnull List<String> list(@Nonnull String name) {
		String value = optional(name);
		if (value == null) {
			return Collections.emptyList();
		}
		String[] items = value.split(",");
		List<String> result = new ArrayList<>(items.length);
		for (String item : items) {
			String trimmed = item.trim();
			if (! trimmed.isEmpty()) {
				result.add(trimmed);
			}
		}
		return result;
	}

	/**
	 * Returns a parameter that names one of an enum's constants, in any case, with or without underscores:
	 * {@code TimedOut}, {@code timed_out} and {@code TIMED_OUT} all name {@code TIMED_OUT}.
	 *
	 * <p>Complexity: O(c) in the number of constants.
	 *
	 * @param <E>	the enum
	 * @param name	the parameter's name
	 * @param type	the enum's class
	 * @param fallback	the value when it is left out
	 * @return the constant
	 * @throws IllegalArgumentException	if it names none of them, listing them
	 */
	public @Nonnull <E extends Enum<E>> E choice(@Nonnull String name, @Nonnull Class<E> type, @Nonnull E fallback) {
		String value = optional(name);
		return (value == null) ? fallback : constant(name, type, value);
	}

	/**
	 * Returns a parameter the call can't do without that names one of an enum's constants, as {@link #choice} reads it.
	 *
	 * <p>Complexity: O(c) in the number of constants.
	 *
	 * @param <E>	the enum
	 * @param name	the parameter's name
	 * @param type	the enum's class
	 * @return the constant
	 * @throws IllegalArgumentException	if it is left out, or names none of them, listing them
	 */
	public @Nonnull <E extends Enum<E>> E requiredChoice(@Nonnull String name, @Nonnull Class<E> type) {
		return constant(name, type, required(name));
	}

	/** Returns the enum constant a parameter's value names, or says which it could name. */
	private @Nonnull <E extends Enum<E>> E constant(@Nonnull String name, @Nonnull Class<E> type, @Nonnull String value) {
		String wanted = value.trim().replace("_", "");
		E[] constants = type.getEnumConstants();
		for (E constant : constants) {
			if (constant.name().replace("_", "").equalsIgnoreCase(wanted)) {
				return constant;
			}
		}
		List<String> names = new ArrayList<>(constants.length);
		for (E constant : constants) {
			names.add(constant.name());
		}
		throw new IllegalArgumentException(String.format("%s's %s must be one of %s, not %s", call, name, names, value));
	}
}

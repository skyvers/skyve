package org.skyve.domain.types.converters;

import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.user.User;

import jakarta.xml.bind.annotation.XmlTransient;

/**
 * Abstract semantic validator invoked after a successful {@link Converter#fromDisplayValue}
 * call to perform domain-level validation of the converted value.
 *
 * <p>Implementations add {@link org.skyve.domain.messages.Message} instances to the
 * supplied {@link ValidationException} rather than throwing immediately, allowing the
 * framework to accumulate multiple validation errors in a single pass and present them
 * all to the user at once.
 *
 * <p>A {@code Validator} is associated with a {@link Converter} via
 * {@link Converter#getValidator()}. The framework calls
 * {@link #validate(org.skyve.metadata.user.User, Object, String, String, Converter, ValidationException)}
 * in the view binding pipeline and during import operations.
 *
 * @param <T> the domain type this validator operates on
 * @see Converter
 */
@XmlTransient
public abstract class Validator<T extends Object> {
/**
	 * Validates the converted value and appends any constraint violations to
	 * {@code e} as {@link org.skyve.domain.messages.Message} instances.
	 *
	 * <p>Implementations must <em>not</em> throw {@code e}; the framework does that
	 * after all validators have run.
	 *
	 * @param user                 the current user, for locale and permission context; must not be {@code null}
	 * @param value                the converted value to validate; may be {@code null}
	 * @param binding              the bean binding path this value came from; used as the message binding
	 * @param localisedDisplayName the UI label for the attribute, used in error messages
	 * @param converter            the converter that produced {@code value}, for formatting error messages
	 * @param e                    the exception to add {@link org.skyve.domain.messages.Message} entries to; must not be {@code null}
	 */
	public abstract void validate(User user,
									T value,
									String binding,
									String localisedDisplayName,
									Converter<T> converter,
									ValidationException e);
}

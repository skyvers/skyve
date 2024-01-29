package org.skyve.domain.types.converters;

import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.user.User;

import jakarta.xml.bind.annotation.XmlTransient;

@XmlTransient
public abstract class Validator<T extends Object> {
	public abstract void validate(User user,
									T value,
									String binding,
									String localisedDisplayName,
									Converter<T> converter,
									ValidationException e);
}

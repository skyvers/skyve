package org.skyve.domain.types.converters;

import javax.xml.bind.annotation.XmlTransient;

import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.user.User;

@XmlTransient
public abstract class Validator<T extends Object> {
	public abstract void validate(User user,
									T value,
									String binding,
									String displayName,
									Converter<T> converter,
									ValidationException e);
}

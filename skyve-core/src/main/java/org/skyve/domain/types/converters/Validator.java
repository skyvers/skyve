package org.skyve.domain.types.converters;

import javax.xml.bind.annotation.XmlTransient;

import org.skyve.domain.messages.ValidationException;

@XmlTransient
public abstract class Validator<T extends Object> {
	public abstract void validate(T value,
									String binding,
									String displayName,
									Converter<T> converter,
									ValidationException e);
}

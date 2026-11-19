package org.skyve.impl.cdi;

import java.io.Serializable;

import org.skyve.CORE;
import org.skyve.domain.number.NumberGenerator;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.enterprise.inject.Alternative;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class NumberGeneratorInjectable implements NumberGenerator, Serializable {
	private static final long serialVersionUID = -7226319192709654523L;

	@Override
	public @Nonnull String next(@Nullable String prefix, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String fieldName, int minimumLength) {
		return CORE.getNumberGenerator().next(prefix, moduleName, documentName, fieldName, minimumLength);
	}
}

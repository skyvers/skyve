package org.skyve.impl.domain.types.jaxb;

import org.skyve.domain.types.OptimisticLock;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlSchemaType(name = "string")
//@XmlJavaTypeAdapter(OptimisticLockMapper.class)
/**
 * JAXB {@link XmlAdapter} that converts between a serialised {@code String}
 * and an {@link OptimisticLock} domain type.
 *
 * <p>On unmarshalling, parses the string using
 * {@link OptimisticLock#OptimisticLock(String)}.  On marshalling, calls
 * {@link OptimisticLock#toString()}.  A {@code null} input produces a
 * {@code null} output in both directions.
 *
 * <p>Threading: stateless; instances are safe for concurrent use.
 *
 * @see OptimisticLock
 */
public class OptimisticLockMapper extends XmlAdapter<String, OptimisticLock> {
	@Override
	public @Nullable OptimisticLock unmarshal(@Nullable String lock) throws Exception {
		return (lock == null) ? null : new OptimisticLock(lock);
	}

	@Override
	public @Nullable String marshal(@Nullable OptimisticLock lock) throws Exception {
		return (lock == null) ? null : lock.toString();
	}
}

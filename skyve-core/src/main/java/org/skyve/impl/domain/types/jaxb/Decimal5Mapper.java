package org.skyve.impl.domain.types.jaxb;

import java.math.BigDecimal;

import org.skyve.domain.types.Decimal5;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlJavaTypeAdapter(Decimal5Mapper.class)
/**
 * JAXB {@link XmlAdapter} that converts between XML's {@link java.math.BigDecimal}
 * and Skyve's {@link org.skyve.domain.types.Decimal5} domain type.
 *
 * <p>On unmarshalling, wraps the JAXB-supplied {@code BigDecimal} in a new
 * {@link org.skyve.domain.types.Decimal5}.  A {@code null} input produces a
 * {@code null} output in both directions.
 *
 * <p>Threading: stateless; instances are safe for concurrent use.
 *
 * @see org.skyve.domain.types.Decimal5
 */
public class Decimal5Mapper extends XmlAdapter<BigDecimal, Decimal5> {
	@Override
	public @Nullable Decimal5 unmarshal(@Nullable BigDecimal value) throws Exception {
		return (value == null) ? null : new Decimal5(value);
	}

	@Override
	public @Nullable BigDecimal marshal(@Nullable Decimal5 value) throws Exception {
		return (value == null) ? null : value.bigDecimalValue();
	}
}

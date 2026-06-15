package org.skyve.impl.domain.types.jaxb;

import java.math.BigDecimal;

import org.skyve.domain.types.Decimal10;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlJavaTypeAdapter(Decimal10Mapper.class)
/**
 * JAXB {@link XmlAdapter} that converts between XML's {@link java.math.BigDecimal}
 * and Skyve's {@link org.skyve.domain.types.Decimal10} domain type.
 *
 * <p>On unmarshalling, wraps the JAXB-supplied {@code BigDecimal} in a new
 * {@link org.skyve.domain.types.Decimal10}.  A {@code null} input produces a
 * {@code null} output in both directions.
 *
 * <p>Threading: stateless; instances are safe for concurrent use.
 *
 * @see org.skyve.domain.types.Decimal10
 */
public class Decimal10Mapper extends XmlAdapter<BigDecimal, Decimal10> {
	@Override
	public Decimal10 unmarshal(BigDecimal value) throws Exception {
		return (value == null) ? null : new Decimal10(value);
	}

	@Override
	public BigDecimal marshal(Decimal10 value) throws Exception {
		return (value == null) ? null : value.bigDecimalValue();
	}
}

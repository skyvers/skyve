package org.skyve.impl.domain.types.jaxb;

import java.math.BigDecimal;

import org.skyve.domain.types.Decimal10;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlJavaTypeAdapter(Decimal10Mapper.class)
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

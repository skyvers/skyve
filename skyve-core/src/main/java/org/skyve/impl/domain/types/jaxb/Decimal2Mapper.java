package org.skyve.impl.domain.types.jaxb;

import java.math.BigDecimal;

import org.skyve.domain.types.Decimal2;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlJavaTypeAdapter(Decimal2Mapper.class)
public class Decimal2Mapper extends XmlAdapter<BigDecimal, Decimal2> {
	@Override
	public Decimal2 unmarshal(BigDecimal value) throws Exception {
		return (value == null) ? null : new Decimal2(value);
	}

	@Override
	public BigDecimal marshal(Decimal2 value) throws Exception {
		return (value == null) ? null : value.bigDecimalValue();
	}
}

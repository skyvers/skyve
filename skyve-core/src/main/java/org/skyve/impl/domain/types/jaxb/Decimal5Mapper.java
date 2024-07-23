package org.skyve.impl.domain.types.jaxb;

import java.math.BigDecimal;

import org.skyve.domain.types.Decimal5;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlJavaTypeAdapter(Decimal5Mapper.class)
public class Decimal5Mapper extends XmlAdapter<BigDecimal, Decimal5> {
	@Override
	public Decimal5 unmarshal(BigDecimal value) throws Exception {
		return (value == null) ? null : new Decimal5(value);
	}

	@Override
	public BigDecimal marshal(Decimal5 value) throws Exception {
		return (value == null) ? null : value.bigDecimalValue();
	}
}

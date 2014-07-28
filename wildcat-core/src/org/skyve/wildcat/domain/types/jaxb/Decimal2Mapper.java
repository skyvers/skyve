package org.skyve.wildcat.domain.types.jaxb;

import java.math.BigDecimal;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.skyve.domain.types.Decimal2;

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

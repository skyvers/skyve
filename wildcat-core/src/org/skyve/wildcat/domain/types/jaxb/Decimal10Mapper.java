package org.skyve.wildcat.domain.types.jaxb;

import java.math.BigDecimal;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.skyve.domain.types.Decimal10;

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

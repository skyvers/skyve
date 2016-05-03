package org.skyve.impl.domain.types.jaxb;

import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.skyve.domain.types.Timestamp;

//@XmlSchemaType(name = "dateTime")
//@XmlJavaTypeAdapter(TimestampMapper.class)
public class TimestampMapper extends XmlAdapter<Date, Timestamp> {
	@Override
	public Timestamp unmarshal(Date date) throws Exception {
		return (date == null) ? null : new Timestamp(date.getTime());
	}

	@Override
	public Date marshal(Timestamp timestamp) throws Exception {
		return timestamp;
	}
}

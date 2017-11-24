package org.skyve.impl.domain.types.jaxb;

import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.skyve.domain.types.DateTime;

//@XmlSchemaType(name = "dateTime")
//@XmlJavaTypeAdapter(DateTimeMapper.class)
public class DateTimeMapper extends XmlAdapter<Date, DateTime> {
	@Override
	public DateTime unmarshal(Date date) throws Exception {
		return (date == null) ? null : new DateTime(date.getTime());
	}

	@Override
	public Date marshal(DateTime dateTime) throws Exception {
		return dateTime;
	}
}

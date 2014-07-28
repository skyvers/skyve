package org.skyve.wildcat.domain.types.jaxb;

import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.skyve.domain.types.DateOnly;

//@XmlSchemaType(name = "date")
//@XmlJavaTypeAdapter(DateOnlyMapper.class)
public class DateOnlyMapper extends XmlAdapter<Date, DateOnly> {
	@Override
	public DateOnly unmarshal(Date date) throws Exception {
		return (date == null) ? null : new DateOnly(date.getTime());
	}

	@Override
	public Date marshal(DateOnly dateOnly) throws Exception {
		return dateOnly;
	}
}

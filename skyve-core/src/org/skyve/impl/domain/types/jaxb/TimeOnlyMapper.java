package org.skyve.impl.domain.types.jaxb;

import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.skyve.domain.types.TimeOnly;

//@XmlSchemaType(name = "time")
//@XmlJavaTypeAdapter(TimeOnlyMapper.class)
public class TimeOnlyMapper extends XmlAdapter<Date, TimeOnly> {
	@Override
	public TimeOnly unmarshal(Date time) throws Exception {
		return (time == null) ? null : new TimeOnly(time.getTime());
	}

	@Override
	public Date marshal(TimeOnly timeOnly) throws Exception {
		return timeOnly;
	}
}

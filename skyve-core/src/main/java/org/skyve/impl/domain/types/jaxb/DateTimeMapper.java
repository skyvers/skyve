package org.skyve.impl.domain.types.jaxb;

import java.util.Date;

import org.skyve.domain.types.DateTime;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlSchemaType(name = "dateTime")
//@XmlJavaTypeAdapter(DateTimeMapper.class)
/**
 * JAXB {@link XmlAdapter} that converts between the XML {@code xs:dateTime} type
 * ({@link java.util.Date}) and Skyve's {@link org.skyve.domain.types.DateTime} domain type.
 *
 * <p>On unmarshalling, wraps the JAXB-supplied {@code Date} in a new
 * {@link org.skyve.domain.types.DateTime} preserving the epoch millisecond value.
 * A {@code null} input produces a {@code null} output in both directions.
 *
 * <p>Threading: stateless; instances are safe for concurrent use.
 *
 * @see org.skyve.domain.types.DateTime
 */
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

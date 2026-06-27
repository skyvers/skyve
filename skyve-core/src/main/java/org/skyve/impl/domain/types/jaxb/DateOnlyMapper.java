package org.skyve.impl.domain.types.jaxb;

import java.util.Date;

import org.skyve.domain.types.DateOnly;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlSchemaType(name = "date")
//@XmlJavaTypeAdapter(DateOnlyMapper.class)
/**
 * JAXB {@link XmlAdapter} that converts between the XML {@code xs:date} type
 * ({@link java.util.Date}) and Skyve's {@link DateOnly} domain type.
 *
 * <p>On unmarshalling, wraps the JAXB-supplied {@code Date} in a new
 * {@link DateOnly} preserving the epoch millisecond value.  On marshalling,
 * returns the underlying {@code Date}.  A {@code null} input produces a
 * {@code null} output in both directions.
 *
 * <p>Threading: stateless; instances are safe for concurrent use.
 *
 * @see DateOnly
 */
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
